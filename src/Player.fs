module Catan.Player
let createPlayer id =
    {
        PlayerId = PlayerId id
        VictoryPoints = 0
        ResourceCards = Map.empty
        DevelopmentCards = Map.empty
        Communities = []
        Roads = []
        ArmySize = 0
    }

let addResource (resource : ResourceCard) (n : int) (player : Player) =
    {
        player with
            ResourceCards =
                player.ResourceCards
                |> Map.mapValueAtKey resource ((+) n)
    } 

let removeResource (resource : ResourceCard) (n : int) (player : Player) =
    {
        player with
            ResourceCards =
                player.ResourceCards
                |> Map.mapValueAtKey resource ((+) -n)
    } 

let addResources (x : Map<ResourceCard, int>) =
    x
    |> Map.toList        
    |> List.unzip
    ||> List.map2 addResource
    |> List.reduce (>>)

let removeResources (x : Map<ResourceCard, int>) =
    x
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
                    ResourceCard.terrainToResource terrain.TerrainType

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

let numberToRob (player : Player) =
    player.ResourceCards.Count / 2

let longestRoad (opponentVertices : Vertex list) (player : Player) =
    let paths =
        player.Roads
        |> List.map (fun (Road path) -> path)

    paths
    |> List.map (fun path -> 
        Path.longestChainFromPath 
            path 
            paths
            opponentVertices)
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
    {
        player with
            VictoryPoints =
                calculateVictoryPoints achievements player
    }
