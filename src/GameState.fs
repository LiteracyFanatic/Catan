module Catan.GameState
let mapActivePlayer (f : Player -> Player) (gameState : GameState) =
    {
        gameState with
            ActivePlayer = f gameState.ActivePlayer
    }

let mapOtherPlayers (f : Player -> Player) (gameState : GameState) =
    {
        gameState with
            OtherPlayers = List.map f gameState.OtherPlayers
    }

let mapPlayers (f : Player -> Player) =
    mapActivePlayer f
    >> mapOtherPlayers f

let nextTurn (gameState : GameState) =
    {
        gameState with
            ActivePlayer =
                gameState.OtherPlayers.Head
            OtherPlayers =
                gameState.OtherPlayers.Tail @ [gameState.ActivePlayer]
    }

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

    let totalResourcesRequired =
        resourcesRequired
        |> List.reduce (fun a b -> Map.map (fun k v -> v + b.[k]) a)

    let enoughResources =
        totalResourcesRequired
        |> Map.map (fun k v -> v <= gameState.ResourcePool.[k])

    let reviseCollection (resources : Map<ResourceCard, int>) =
        resources
        |> Map.map (fun k v -> if enoughResources.[k] then v else 0)

    let totalResourcesCollected =
        reviseCollection totalResourcesRequired

    let resourcesCollectedByPlayers =
        resourcesRequiredByPlayers
        |> Map.map (fun k v -> reviseCollection v)

    {
        gameState with
            ResourcePool =
                gameState.ResourcePool
                |> Map.map ( fun k v -> v - totalResourcesCollected.[k])
    } 
    |> mapPlayers (fun player -> 
        player
        |> Player.addResources (resourcesCollectedByPlayers.[player.PlayerId])
    )

let updateAchievements (gameState : GameState) =
    let opponentVertices =
        gameState.OtherPlayers
        |> List.collect (fun player -> player.Communities)
        |> List.map (fun infrastructure -> infrastructure.Location)

    {
        gameState with
            AchievementCards =
                gameState.AchievementCards
                |> Map.map (fun k v ->
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
                        if player.ArmySize >= 3 then Some player.PlayerId else None)
    }

let buildInfrastructure (infrastructure : Community) (gameState : GameState) =
    let resourceCost =
        match infrastructure.CommunityType with
        | Settlement -> Map [ (Brick, 1); (Lumber, 1); (Wool, 1); (Grain, 1) ]
        | City -> Map [ (Ore, 3); (Grain, 2) ]

    {
        gameState with
            ActivePlayer =
                { 
                    gameState.ActivePlayer with
                        Communities =
                            gameState.ActivePlayer.Communities
                            |> List.filter (fun x -> 
                                x.Location <> infrastructure.Location)
                            |> List.append [infrastructure]
                } 
                |> Player.removeResources resourceCost
                |> Player.updateVictoryPoints gameState.AchievementCards
            ResourcePool =
                gameState.ResourcePool
                |> Map.map (fun k v -> v + resourceCost.[k])
    }

let buildRoad (road : Road) (gameState : GameState) =
    let resourceCost =
        Map [ (Brick, 1); (Lumber, 1) ]

    {
        gameState with
            ActivePlayer = 
                {
                    gameState.ActivePlayer with
                        Roads = road :: gameState.ActivePlayer.Roads
                }
                |> Player.removeResources resourceCost
            ResourcePool =
                gameState.ResourcePool
                |> Map.map (fun k v -> v + resourceCost.[k])
    }
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

    let stolenResource =
        resources.[rand.Next(0, resources.Length)]

    {
        gameState with
            ActivePlayer =
                gameState.ActivePlayer
                |> Player.addResource stolenResource 1
            OtherPlayers =
                gameState.OtherPlayers
                |> List.map (fun player ->
                    if player.PlayerId = playerId then
                        Player.removeResource stolenResource 1 player
                    else
                        player
                )
    }

let rob (resourcesByPlayer : Map<PlayerId, Map<ResourceCard, int>>) (gameState : GameState) =
    gameState
    |> mapPlayers (fun player ->
        player
        |> Player.removeResources (resourcesByPlayer.[player.PlayerId]))

let drawDevelopmentCard (gameState : GameState) =
    let resourceCost =
        Map [ (Wool, 1); (Grain, 1); (Ore, 1) ]

    match gameState.DevelopmentCards with
    | [] -> failwith "Cannot draw from empty deck"
    | (card :: cards) ->
        {
            gameState with
                ActivePlayer =
                    {
                        gameState.ActivePlayer with
                            DevelopmentCards =
                                gameState.ActivePlayer.DevelopmentCards
                                |> Map.mapValueAtKey card ((+) 1)
                    } 
                    |> Player.removeResources resourceCost
                DevelopmentCards =
                    cards
                ResourcePool =
                    gameState.ResourcePool
                    |> Map.map (fun k v -> v + resourceCost.[k])
        }

let playDevelopmentCard (rand : System.Random) (developmentAction : DevelopmentCardAction) (gameState : GameState) =
    let transform : GameState -> GameState =
        match developmentAction with
        | VictoryPointAction -> failwith "Should win automatically"
        | KnightAction (terrainIndex, playerId) ->
            mapActivePlayer (fun player -> 
                { 
                    player with
                        ArmySize = player.ArmySize + 1
                }) 
            >> (fun g -> 
                {
                    g with
                        RobberLocation =
                            terrainIndex                            
                })
            >> robPlayer rand playerId
        | YearOfPlentyAction resources ->
            mapActivePlayer (fun player ->
                {
                    player with
                        ResourceCards =
                            player.ResourceCards
                            |> Map.map (fun k v ->
                                v + List.sumBy (fun resource ->
                                    if resource = k then 1 else 0
                                ) resources)
                }) 
                >> (fun g -> 
                {
                    g with
                        ResourcePool =
                            g.ResourcePool
                            |> Map.map (fun k v ->
                                v - List.sumBy (fun resource ->
                                    if resource = k then 1 else 0) 
                                        resources)
                })
        | RoadBuildingAction paths ->
            mapActivePlayer (fun player ->
                {
                    player with
                        Roads =
                            player.Roads @ List.map Road paths
                })
        | MonopolyAction resource ->
            let stolenCount =
                gameState.OtherPlayers
                |> List.sumBy (fun player -> player.ResourceCards.[resource])

            mapActivePlayer (fun player ->
                {
                    player with
                        ResourceCards =
                            player.ResourceCards
                            |> Map.mapValueAtKey resource ((+) stolenCount)
                }) 
                >> mapOtherPlayers (fun player ->
                {
                    player with
                        ResourceCards =
                            player.ResourceCards
                            |> Map.add resource 0
                })
    
    gameState
    |> transform 
    |> mapActivePlayer (fun player ->
        let card = DevelopmentCard.fromDevelopmentAction developmentAction

        {
            player with
                DevelopmentCards =
                    player.DevelopmentCards
                    |> Map.mapValueAtKey card ((+) -1)
        })
    |> updateAchievements
    |> mapPlayers (Player.updateVictoryPoints gameState.AchievementCards)

let maritimeTrade (trade : MaritimeTrade) (gameState : GameState) =
    {
        gameState with
            ResourcePool =
                gameState.ResourcePool
                |> Map.mapValueAtKey trade.TradedResource ((+) trade.TradedQuantity)
                |> Map.mapValueAtKey trade.RecievedResource ((+) -1)
            ActivePlayer =
                gameState.ActivePlayer
                |> Player.addResource trade.RecievedResource 1
                |> Player.removeResource trade.TradedResource trade.TradedQuantity
    }

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
    let cardPoints =
        gameState.ActivePlayer.DevelopmentCards.[VictoryPoint]

    let otherPoints =
        gameState.ActivePlayer.VictoryPoints

    let totalPoints = cardPoints + otherPoints

    if totalPoints >= 10 then
        {
            gameState with
                ActivePlayer =
                    {
                        gameState.ActivePlayer with
                            VictoryPoints =
                                totalPoints
                    }
                Winner =
                    Some gameState.ActivePlayer.PlayerId                
        }
    else
        gameState

let init (rand : System.Random) =

    let developmentCards =
        [
            List.replicate 5 VictoryPoint
            List.replicate 14 Knight
            List.replicate 2 YearOfPlenty
            List.replicate 2 RoadBuilding
            List.replicate 2 Monopoly
        ] 
        |> List.concat
        |> Helpers.shuffle rand

    let resources =
        [Brick; Lumber; Wool; Grain; Ore]

    let resourceBank =
        resources
        |> List.map (fun resource -> resource, 19)
        |> Map

    let desertLocation =
        TerrainIndex (rand.Next(0, 19))

    let terrainIndices =
        [0 .. 18]
        |> List.map TerrainIndex
        |> List.filter ((<>) desertLocation)
        
    let tokenPredicate (tokens : int list) =
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
        |> Helpers.shuffleUntil rand tokenPredicate

    let makeTerrain terrain n =
        {
            TerrainType = terrain
            Number = n
        }

    let terrainPredicate (terrain : TerrainType list) =
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
        [
            List.replicate 3 Hills
            List.replicate 4 Forest
            List.replicate 4 Pasture
            List.replicate 4 Fields
            List.replicate 3 Mountains
        ]
        |> List.concat
        |> Helpers.shuffleUntil rand terrainPredicate
    
    let terrainMap =
        List.map2 makeTerrain terrainList numberTokens
        |> List.map Fertile
        |> List.zip terrainIndices
        |> Map
        |> Map.add desertLocation Desert

    let harbors =
        resources
        |> List.map (Harbor.ResourceHarbor)
        |> List.append (List.replicate 4 Harbor.ThreeForOneHarbor)
        |> Helpers.shuffle rand
        |> List.mapi (fun i harbor -> HarborIndex i, harbor)
        |> Map

    let achievementCards =
        [
            LongestRoad, None
            LargestArmy, None
        ]
        |> Map   

    {
        ActivePlayer = Player.createPlayer 1
        OtherPlayers = List.map Player.createPlayer [1; 2; 3]
        ResourcePool = resourceBank
        DevelopmentCards = developmentCards
        Terrain = terrainMap
        Harbors = harbors
        RobberLocation = desertLocation
        AchievementCards = achievementCards
        Winner = None
    }
