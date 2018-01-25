module GameState

open Catan
open Aether
open Aether.Operators

let mapActivePlayer (f : Player -> Player) (gameState : GameState) =
    { gameState with ActivePlayer = f gameState.ActivePlayer }

let mapOtherPlayers (f : Player -> Player) (gameState : GameState) =
    { gameState with OtherPlayers = List.map f gameState.OtherPlayers }

let mapPlayers (f : Player -> Player) =
    mapActivePlayer f
    >> mapOtherPlayers f

let nextTurn (gameState : GameState) =
    { gameState with
        ActivePlayer = gameState.OtherPlayers.Head
        OtherPlayers = gameState.OtherPlayers.Tail @ [gameState.ActivePlayer] }

let collectResources (roll : int) (gameState : GameState) =
    let resourcesRequiredByPlayers =
        gameState.ActivePlayer :: gameState.OtherPlayers
        |> List.map (fun player ->
            let collectedResources =
                Player.toCollectedResources 
                    roll 
                    gameState.Terrain 
                    gameState.RobberLocation player
            player.PlayerId, collectedResources)
        |> Map

    let resourcesRequired =
        gameState.ActivePlayer :: gameState.OtherPlayers
        |> List.map (fun player ->
            Player.toCollectedResources 
                roll 
                gameState.Terrain 
                gameState.RobberLocation
                player)
        |> List.reduce (fun a b -> Map.map (fun k v -> v + b.[k]) a)

    let enoughResources =
        Map.map (fun k v -> v <= gameState.ResourcePool.[k]) resourcesRequired

    let reviseCollection (resources : Map<ResourceCard, int>) =
        Map.map (fun k v -> if enoughResources.[k] then v else 0) resources

    let totalResourcesCollected = reviseCollection resourcesRequired

    let resourcesCollected =
        Map.map (fun k v -> reviseCollection v) resourcesRequiredByPlayers
 
    gameState
    |> Optic.map GameState.ResourcePool_ 
        (Map.map (fun k v -> v - totalResourcesCollected.[k]))
    |> mapPlayers (fun player -> 
        player
        |> Player.addResources (resourcesCollected.[player.PlayerId]))

let updateAchievements (gameState : GameState) =
    let opponentVertices =
        gameState.OtherPlayers
        |> List.collect (fun player -> player.Communities)
        |> List.map (fun community -> community.Location)

    gameState
    |> Optic.map GameState.AchievementCards_ (Map.map (fun k v ->
        match k with
        | LongestRoad ->
            let player, roadLength =
                gameState.ActivePlayer :: gameState.OtherPlayers
                |> List.map (fun player -> player, Player.longestRoad opponentVertices player)
                |> List.maxBy snd
            if roadLength >= 5 then Some player.PlayerId else None
        | LargestArmy ->
            let player =
                gameState.ActivePlayer :: gameState.OtherPlayers
                |> List.maxBy (fun player -> player.ArmySize)
            if player.ArmySize >= 3 then Some player.PlayerId else None))

let buildcommunity (community : Community) (gameState : GameState) =
    let resourceCost =
        match community.CommunityType with
        | Settlement -> Map [ (Brick, 1); (Lumber, 1); (Wool, 1); (Grain, 1) ]
        | City -> Map [ (Ore, 3); (Grain, 2) ]

    gameState
    |> Optic.map (GameState.ActivePlayer_ >-> Player.Communities_)
        (List.filter (fun c -> c.Location <> community.Location) 
        >> List.append [community])
    |> Optic.map GameState.ActivePlayer_
        (Player.removeResources resourceCost
        >> Player.updateVictoryPoints gameState.AchievementCards)
    |> Optic.map GameState.ResourcePool_ (Map.map (fun k v -> v + resourceCost.[k]))

let buildRoad (road : Road) (gameState : GameState) =
    let resourceCost = Map [ (Brick, 1); (Lumber, 1) ]

    gameState
    |> Optic.map (GameState.ActivePlayer_ >-> Player.Roads_) (fun roads -> road :: roads)
    |> Optic.map GameState.ActivePlayer_ (Player.removeResources resourceCost) 
    |> Optic.map GameState.ResourcePool_ (Map.map (fun k v -> v + resourceCost.[k]))
    |> updateAchievements
    |> mapPlayers (Player.updateVictoryPoints gameState.AchievementCards)

let robPlayer (rand : System.Random) (playerId : PlayerId) (gameState : GameState) =
    let robbedPlayer =
        gameState.OtherPlayers
        |> List.tryFind (fun player -> player.PlayerId = playerId)
        |> Option.defaultWith (failwith "Invalid player id.")

    let resources =
        robbedPlayer.ResourceCards
        |> Map.toList
        |> List.collect (fun (k, v) -> List.replicate v k)

    let stolenResource = resources.[rand.Next(0, resources.Length)]

    gameState
    |> Optic.map GameState.ActivePlayer_ (Player.addResource stolenResource 1)
    |> Optic.map GameState.OtherPlayers_ (List.map (fun player ->
        if player.PlayerId = playerId then
            Player.removeResource stolenResource 1 player
        else
            player))


let rob (resourcesByPlayer : Map<PlayerId, Map<ResourceCard, int>>) (gameState : GameState) =
    gameState
    |> mapPlayers (fun player ->
        Player.removeResources (resourcesByPlayer.[player.PlayerId]) player)

let drawDevelopmentCard (gameState : GameState) =
    let resourceCost = Map [ (Wool, 1); (Grain, 1); (Ore, 1) ]

    match gameState.DevelopmentCards with
    | [] -> failwith "Cannot draw from empty deck"
    | (card :: cards) ->

        gameState
        |> Optic.map GameState.ActivePlayer_ (Player.removeResources resourceCost)
        |> Optic.map (GameState.ActivePlayer_ >-> Player.DevelopmentCards_)
            (Map.mapValueAtKey card ((+) 1))
        |> Optic.set GameState.DevelopmentCards_ cards
        |> Optic.map GameState.ResourcePool_ (Map.map (fun k v -> v + resourceCost.[k]))

let playDevelopmentCard (rand : System.Random) (developmentAction : DevelopmentCardAction) (gameState : GameState) =
    let transform : GameState -> GameState =
        match developmentAction with
        | VictoryPointAction -> failwith "Should win automatically"
        | KnightAction (terrainIndex, playerId) ->
            Optic.map (GameState.ActivePlayer_ >-> Player.ArmySize_) ((+) 1)
            >> Optic.set GameState.RobberLocation_ terrainIndex
            >> robPlayer rand playerId
        | YearOfPlentyAction resources ->
            Optic.map (GameState.ActivePlayer_ >-> Player.ResourceCards_)
                (Map.map (fun k v ->
                    v + List.sumBy (fun r -> if r = k then 1 else 0) resources))
            >> Optic.map GameState.ResourcePool_
                (Map.map (fun k v ->
                    v - List.sumBy (fun r -> if r = k then 1 else 0) resources))
        | RoadBuildingAction paths ->
            let newRoads = List.map Road paths
            Optic.map (GameState.ActivePlayer_ >-> Player.Roads_) ((@) newRoads)
        | MonopolyAction resource ->
            let stolenCount =
                gameState.OtherPlayers
                |> List.sumBy (fun player -> player.ResourceCards.[resource])

            Optic.map (GameState.ActivePlayer_ >-> Player.ResourceCards_)
                (Map.mapValueAtKey resource ((+) stolenCount))
            >> mapOtherPlayers
                (Optic.set (Player.ResourceCards_ >-> Optics.Map.key_ resource) 0)

    let card = DevelopmentCard.fromDevelopmentAction developmentAction

    gameState
    |> transform
    |> Optic.map (GameState.ActivePlayer_ >-> Player.DevelopmentCards_)
        (Map.mapValueAtKey card ((+) -1))
    |> updateAchievements
    |> mapPlayers (Player.updateVictoryPoints gameState.AchievementCards)
    
let maritimeTrade (trade : MaritimeTrade) (gameState : GameState) =
    gameState
    |> Optic.map GameState.ResourcePool_
        (Map.mapValueAtKey trade.TradedResource ((+) trade.TradedQuantity)
        >> Map.mapValueAtKey trade.RecievedResource ((+) -1))
    |> Optic.map GameState.ActivePlayer_
        (Player.addResource trade.RecievedResource 1
        >> Player.removeResource trade.TradedResource trade.TradedQuantity)   

let trade (trade : TradeInfo) (gameState : GameState) =
    gameState
    |> mapPlayers (fun player ->
        if player.PlayerId = trade.OfferingPlayerId then
            player
            |> Player.addResources trade.DesiredResources
            |> Player.removeResources trade.OfferedResources
        else if player.PlayerId = trade.RecievingPlayerId then
            player
            |> Player.addResources trade.OfferedResources
            |> Player.removeResources trade.DesiredResources
        else
            player)

let checkForVictory (gameState : GameState) =
    let cardPoints = gameState.ActivePlayer.DevelopmentCards.[VictoryPoint]

    let otherPoints = gameState.ActivePlayer.VictoryPoints

    let totalPoints = cardPoints + otherPoints

    if totalPoints >= 10 then
        gameState
        |> Optic.set (GameState.ActivePlayer_ >-> Player.VictoryPoints_) totalPoints
        |> Optic.set GameState.Winner_ gameState.ActivePlayer.PlayerId
    else
        gameState

let init (rand : System.Random) =

    let developmentCards =
        [ List.replicate 5 VictoryPoint
          List.replicate 14 Knight
          List.replicate 2 YearOfPlenty
          List.replicate 2 RoadBuilding
          List.replicate 2 Monopoly ] 
        |> List.concat
        |> Helpers.shuffle rand

    let resources = [Brick; Lumber; Wool; Grain; Ore]

    let resourceBank =
        resources
        |> List.map (fun resource -> resource, 19)
        |> Map

    let desertLocation = TerrainIndex (rand.Next(0, 19))

    let terrainIndices =
        [0 .. 18]
        |> List.map TerrainIndex
        |> List.filter ((<>) desertLocation)
        
    let tokensAreFair (tokens : int list) =
        let terrainToToken =
            List.zip terrainIndices tokens
            |> Map    

        let adjacentTokens =
            List.zip terrainIndices tokens
            |> List.map (fun (terrainIndex, n) -> 
                let adjacentIndices =
                    Terrain.adjacentTiles terrainIndex
                
                let adjacentTokens =
                    adjacentIndices
                    |> List.choose (fun i -> Map.tryFind i terrainToToken) 
                
                n :: adjacentTokens)

        adjacentTokens
        |> List.forall (fun tokens ->
            let numberOfRedTokens =
                tokens
                |> List.filter (fun n -> n = 6 || n = 8)
                |> List.length
            numberOfRedTokens < 2)

    let numberTokens =
        [2; 3; 3; 4; 4; 5; 5; 6; 6; 8; 8; 9; 9; 10; 10; 11; 11; 12]
        |> Helpers.shuffleUntil rand tokensAreFair

    let terrainIsFair (terrain : TerrainType list) =
        let indexToTerrain =
            List.zip terrainIndices terrain
            |> Map    

        let adjacentTerrainTypes =
            List.zip terrainIndices terrain
            |> List.map (fun (terrainIndex, terrain) -> 
                let adjacentIndices =
                   Terrain.adjacentTiles terrainIndex
                
                let adjacentTerrain =
                    adjacentIndices
                    |> List.choose (fun i -> Map.tryFind i indexToTerrain) 
                
                terrain :: adjacentTerrain)

        adjacentTerrainTypes
        |> List.forall (fun terrains ->
            let maxIndenticalAdjacentTerrain =
                terrains
                |> List.countBy id
                |> List.map snd
                |> List.max
            maxIndenticalAdjacentTerrain < 3)

    let terrainList =
        [ 3, Hills
          4, Forest
          4, Pasture
          4, Fields
          3, Mountains ]
        |> List.collect (fun (n, t) -> List.replicate n t)
        |> Helpers.shuffleUntil rand terrainIsFair

    let makeTerrain terrain n =
        { TerrainType = terrain; Number = n }

    let terrainMap =
        List.map2 makeTerrain terrainList numberTokens
        |> List.map Fertile
        |> List.zip terrainIndices
        |> Map
        |> Map.add desertLocation Desert

    let harbors =
        resources
        |> List.map Harbor.ResourceHarbor
        |> List.append (List.replicate 4 Harbor.ThreeForOneHarbor)
        |> Helpers.shuffle rand
        |> List.mapi (fun i harbor -> HarborIndex i, harbor)
        |> Map

    let achievementCards = Map [LongestRoad, None; LargestArmy, None]

    { ActivePlayer = Player.create 1
      OtherPlayers = List.map Player.create [1; 2; 3]
      ResourcePool = resourceBank
      DevelopmentCards = developmentCards
      Terrain = terrainMap
      Harbors = harbors
      RobberLocation = desertLocation
      AchievementCards = achievementCards
      Winner = None }
