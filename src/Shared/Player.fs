module Player

open Catan
open Aether

let create id =
    let resourceCards =
        [Brick; Lumber; Wool; Grain; Ore]
        |> List.map (fun r -> r, 0)
        |> Map

    let developmentCards =
        [VictoryPoint; Knight; YearOfPlenty; RoadBuilding; Monopoly]
        |> List.map (fun r -> r, 0)
        |> Map  

    { PlayerId = PlayerId id
      VictoryPoints = 0
      ResourceCards = resourceCards
      DevelopmentCards = developmentCards
      Communities = []
      Roads = []
      ArmySize = 0 }

let addResource (resource : ResourceCard) (n : int) =
    Optic.map Player.ResourceCards_ (Map.mapValueAtKey resource ((+) n))
        
let removeResource (resource : ResourceCard) (n : int) =
    Optic.map Player.ResourceCards_ (Map.mapValueAtKey resource ((+) n))

let addResources (resources : Map<ResourceCard, int>) =
    resources
    |> Map.toList        
    |> List.unzip
    ||> List.map2 addResource
    |> List.reduce (>>)

let removeResources (resources : Map<ResourceCard, int>) =
    resources
    |> Map.toList
    |> List.unzip
    ||> List.map2 removeResource
    |> List.reduce (>>)

let toCollectedResources (roll : int) (terrainMap : Map<TerrainIndex, TerrainTile>) (robberIndex : TerrainIndex) (player : Player) =
    let resourceLocations =
        terrainMap
        |> Map.filter (fun k v -> k <> robberIndex)
        |> Map.toList
        |> List.choose (fun (k, v) ->
            match v with
            | Fertile terrain when terrain.Number = roll ->
                let resource =
                    Terrain.toResourceCard terrain.TerrainType

                let terrainIndex =
                    Terrain.toVertices k       

                Some (resource, terrainIndex)
            | _ -> None)

    let resources =
        resourceLocations
        |> List.map (fun (k, v) ->
            let n =
                player.Communities
                |> List.sumBy (fun infrastructure ->
                    match infrastructure.CommunityType with
                    | Settlement ->
                        if List.contains infrastructure.Location v then 1 else 0
                    | City ->
                        if List.contains infrastructure.Location v then 2 else 0)
            k, n)
        |> Map    

    resources

let numberToRob (player : Player) = player.ResourceCards.Count / 2

let longestRoad (opponentVertices : Vertex list) (player : Player) =
    let paths = List.map (fun (Road path) -> path) player.Roads
        
    paths
    |> List.map (fun path -> Path.longestChain path paths opponentVertices)
    |> List.max

let calculateVictoryPoints (achievements : Map<AchievementCard, PlayerId option>) (player : Player) =
    let infrastructurePoints =
        player.Communities
        |> List.sumBy (fun infrastructure ->
            match infrastructure.CommunityType with
            | Settlement -> 1
            | City -> 2)

    let achievementPoints =
        achievements
        |> Map.toList
        |> List.sumBy (fun (k, v) ->
            match v with
            | Some playerId when playerId = player.PlayerId -> 2 
            | _ -> 0)

    infrastructurePoints + achievementPoints

let updateVictoryPoints (achievements : Map<AchievementCard, PlayerId option>) (player : Player) =
    Optic.set Player.VictoryPoints_ (calculateVictoryPoints achievements player) player

