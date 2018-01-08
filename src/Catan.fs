module Catan.Domain

module Map =
    let mapValueAtKey (key : 'Key) (f : 'T -> 'T) (map : Map<'Key, 'T>) =
        Map.add key (f map.[key]) map

type PlayerId = PlayerId of int

type TerrainIndex = TerrainIndex of int
type HarborIndex = HarborIndex of int

type Vertex = {
    X : int
    Y : int
}

[<CustomEquality; NoComparison>]
type Path = 
    | Path of Vertex * Vertex

    override x.Equals(y) =
        match y with
        | :? Path as y ->
            let (Path (a, b)) = x
            let (Path (c, d)) = y
            set [a; b] = set [c; d]
        | _ -> false

    override x.GetHashCode() =
        match x with
        | Path (a, b) ->
            let obj =
                box (a, b)
            obj.GetHashCode()

type TerrainType =
    | Hills
    | Forest
    | Pasture
    | Fields
    | Mountains

type Terrain = {
    TerrainType : TerrainType
    Number : int
}

type TerrainTile =
    | Fertile of Terrain
    | Desert

type ResourceCard =
    | Brick
    | Lumber
    | Wool
    | Grain
    | Ore

let terrainToResource = function
    | Hills -> Brick
    | Forest -> Lumber
    | Pasture -> Wool
    | Fields -> Grain
    | Mountains -> Ore

type Harbor =
    | Resource of ResourceCard
    | ThreeForOne

type MaritimeTrade = {
    TradedResource : ResourceCard
    TradedQuantity : int
    RecievedResource : ResourceCard
}

type TradeOffer = {
    OfferingPlayerId : PlayerId
    DesiredResources : Map<ResourceCard, int>
    OfferedResources : Map<ResourceCard, int>
}

type TradeInfo = {
    OfferingPlayerId : PlayerId
    RecievingPlayerId : PlayerId
    DesiredResources : Map<ResourceCard, int>
    OfferedResources : Map<ResourceCard, int>
}

type Trade =
    | SuggestedTrade of TradeOffer
    | AcceptedTrade of TradeInfo
    | ConfirmedTrade of TradeInfo

type DevelopmentCard =
    | VictoryPoint
    | Knight
    | YearOfPlenty
    | RoadBuilding
    | Monopoly

type DevelopmentCardAction =
    | VictoryPointAction
    | KnightAction of TerrainIndex * PlayerId
    | YearOfPlentyAction of ResourceCard list
    | RoadBuildingAction of Path list
    | MonopolyAction of ResourceCard

let developmentActionToCard = function
    | VictoryPointAction -> VictoryPoint
    | KnightAction _ -> Knight
    | YearOfPlentyAction _ -> YearOfPlenty
    | RoadBuildingAction _ -> RoadBuilding
    | MonopolyAction _ -> Monopoly

type AchievementCard =
    | LongestRoad
    | LargestArmy

let originToVertices (vertex : Vertex) =
    [(0, 0); (1, 0); (2, 0); (0, 1); (1, 1); (2, 1)]
    |> List.map (fun (x, y) -> { X = x + vertex.X; Y = y + vertex.Y })

let (|From|_|) (lower : int) (upper : int) (x : int) =
    if x >= lower && x <= upper then Some x else None

let terrainIndexToVertices (TerrainIndex i) =
    match i with
    | From 0 2 i -> originToVertices ({ X = 2 * i + 2; Y = 0 })
    | From 3 6 i -> originToVertices ({ X = 2 * (i - 3) + 1; Y = 1 })
    | From 7 11 i -> originToVertices ({ X = 2 * (i - 7); Y = 2 })
    | From 12 15 i -> originToVertices ({ X = 2 * (i - 12) + 1; Y = 3 })
    | From 16 18 i -> originToVertices ({ X = 2 * (i - 16) + 2; Y = 4 })
    | _ -> failwith "Terrain index out of bounds"

let toAdjacentTiles (TerrainIndex i) =
    let ns =
        match i with
        | 0 -> [1; 3; 4]
        | 1 -> [2; 4; 5]
        | 2 -> [1; 5; 6]
        | 3 -> [0; 4; 7; 8]
        | 4 -> [0; 1; 3; 5; 8; 9]
        | 5 -> [1; 2; 4; 6; 9; 10]
        | 6 -> [2; 5; 10; 11]
        | 7 -> [3; 8; 12]
        | 8 -> [3; 4; 7; 9; 12; 13]
        | 9 -> [4; 5; 8; 10; 13; 14]
        | 10 -> [5; 6; 9; 11; 14; 15]
        | 11 -> [6; 10; 15]
        | 12 -> [7; 8; 13; 16]
        | 13 -> [8; 9; 12; 14; 16; 17]
        | 14 -> [9; 10; 13; 15; 17; 18]
        | 15 -> [10; 11; 14; 18]
        | 16 -> [12; 13; 17]
        | 17 -> [13; 14; 16; 18]
        | 18 -> [14; 15; 17]
        | _ -> failwith "Terrain index out of bounds"
    List.map TerrainIndex ns    

let harborIndexToVertices (HarborIndex i) =
    match i with
    | 0 -> [ { X = 4; Y = 0 }; { X = 5; Y = 0 } ]
    | 1 -> [ { X = 7; Y = 0 }; { X = 8; Y = 0 } ]
    | 2 -> [ { X = 9; Y = 1 }; { X = 9; Y = 2 } ]
    | 3 -> [ { X = 9; Y = 3 }; { X = 9; Y = 4 } ]
    | 4 -> [ { X = 7; Y = 5 }; { X = 8; Y = 5 } ]
    | 5 -> [ { X = 4; Y = 5 }; { X = 5; Y = 5 } ]
    | 6 -> [ { X = 1; Y = 4 }; { X = 2; Y = 4 } ]
    | 7 -> [ { X = 0; Y = 2 }; { X = 0; Y = 3 } ]
    | 9 -> [ { X = 1; Y = 1 }; { X = 2; Y = 1 } ]
    | _ -> failwith "Harbor index out of bounds"

type BuildingType =
    | Settlement
    | City    

type Infrastructure = {
    BuildingType : BuildingType
    Location : Vertex
}

type Road = Road of Path

type Player = {
    PlayerId : PlayerId
    VictoryPoints : int
    ResourceCards : Map<ResourceCard, int>
    DevelopmentCards : Map<DevelopmentCard, int>
    Infrastructure : Infrastructure list
    Roads : Road list
    ArmySize : int
}

type GameState = {
    ActivePlayer : Player
    OtherPlayers : Player list
    ResourceBank : Map<ResourceCard, int>
    DevelopmentCards : DevelopmentCard list
    Terrain : Map<TerrainIndex, TerrainTile>
    Harbors : Map<HarborIndex, Harbor>
    RobberLocation : TerrainIndex
    AchievementCards : Map<AchievementCard, PlayerId option>
    Winner : PlayerId option
}

let roll (rand : System.Random) () =
    rand.Next(1, 7), rand.Next(1, 7)

let shuffle (rand : System.Random) (cards : 'T list) : 'T list =
    let innerShuffle cards =
        let cards' =
            List.toArray cards

        let swap (arr : 'T []) a b =
            let temp = arr.[a]
            arr.[a] <- arr.[b]
            arr.[b] <- temp

        for i in cards.Length - 1 .. -1 .. 1 do
            let j = rand.Next(0, i + 1)
            swap cards' i j

        List.ofArray cards'

    let shuffle10 =
        List.replicate 10 innerShuffle 
        |> List.reduce (>>)

    shuffle10 cards    

let shuffleUntil rand f cards =
    let rec loop cards =
        if f cards then cards else loop (shuffle rand cards)

    loop (shuffle rand cards)

let createPlayer id =
    {
        PlayerId = PlayerId id
        VictoryPoints = 0
        ResourceCards = Map.empty
        DevelopmentCards = Map.empty
        Infrastructure = []
        Roads = []
        ArmySize = 0
    }

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
        |> shuffle rand

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
                    toAdjacentTiles terrainIndex
                
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
        |> shuffleUntil rand tokenPredicate

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
                    toAdjacentTiles terrainIndex
                
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
        |> shuffleUntil rand terrainPredicate
    
    let terrainMap =
        List.map2 makeTerrain terrainList numberTokens
        |> List.map Fertile
        |> List.zip terrainIndices
        |> Map
        |> Map.add desertLocation Desert

    let harbors =
        resources
        |> List.map (Harbor.Resource)
        |> List.append (List.replicate 4 Harbor.ThreeForOne)
        |> shuffle rand
        |> List.mapi (fun i harbor -> HarborIndex i, harbor)
        |> Map

    let achievementCards =
        [
            LongestRoad, None
            LargestArmy, None
        ]
        |> Map   

    {
        ActivePlayer = createPlayer 1
        OtherPlayers = List.map createPlayer [1; 2; 3]
        ResourceBank = resourceBank
        DevelopmentCards = developmentCards
        Terrain = terrainMap
        Harbors = harbors
        RobberLocation = desertLocation
        AchievementCards = achievementCards
        Winner = None
    }


module Player =
    let toCollectedResources (roll : int) (terrainMap : Map<TerrainIndex, TerrainTile>) (robberIndex : TerrainIndex) (player : Player) =
        let resourceLocations =
            terrainMap
            |> Map.filter (fun k v -> k <> robberIndex)
            |> Map.toList
            |> List.choose (fun (k, v) ->
                match v with
                | Fertile terrain when terrain.Number = roll ->
                    let resource =
                        terrainToResource terrain.TerrainType

                    let terrainIndex =
                        terrainIndexToVertices k       

                    Some (resource, terrainIndex)
                | _ -> None)

        let resources =
            resourceLocations
            |> List.map (fun (k, v) ->
                let n =
                    player.Infrastructure
                    |> List.sumBy (fun infrastructure ->
                        match infrastructure.BuildingType with
                        | Settlement ->
                            if List.contains infrastructure.Location v then 1 else 0
                        | City ->
                            if List.contains infrastructure.Location v then 2 else 0)
                k, n)
            |> Map    

        resources

    let numberToRob (player : Player) =
        player.ResourceCards.Count / 2

    let calculateVictoryPoints (achievements : Map<AchievementCard, PlayerId option>) (player : Player) =
        let infrastructurePoints =
            player.Infrastructure
            |> List.sumBy (fun infrastructure ->
                match infrastructure.BuildingType with
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

    let pathsIntersect (Path (a, b)) (Path (c, d)) =
        a = c || a = d || b = c || b = d

    let pathsFromVertex (a : Vertex) (opponentVertices : Vertex list) =
        if List.contains a opponentVertices then
            []
        else
            [
                { X = a.X + 1; Y = a.Y }
                { X = a.X - 1; Y = a.Y }
                { X = a.X; Y = a.Y + 1 }
                { X = a.X; Y = a.Y - 1 }
            ]
            |> List.map (fun b -> Path (a, b))

    let adjacentPaths (Path (a, b) as path) (opponentVertices : Vertex list) =
        pathsFromVertex a opponentVertices @ pathsFromVertex b opponentVertices
        |> List.filter ((<>) path)

    let longestRoadStartingFromPath (path : Path) (roadLocations : Path list) (opponentVertices : Vertex list) =
        let rec loop (chain : Path list) n =
            match roadLocations with
            | [] -> failwith "Player shouldn't be able to have no roads."
            | _ ->
                let adjacent =
                    adjacentPaths chain.Head opponentVertices

                let intersectingPaths =
                    roadLocations
                    |> List.filter (fun p ->
                        let isAdjacent = 
                            List.contains p adjacent
                        let isInChain = 
                            List.contains p chain
                        let isFork =
                            if chain.Length > 1 then
                                pathsIntersect chain.[1] p
                            else
                                false
                        isAdjacent && not isInChain && not isFork)

                match intersectingPaths with
                | [] -> n
                | _ ->
                    intersectingPaths                
                    |> List.map (fun p -> loop (p :: chain) (n + 1))
                    |> List.max

        loop [path] 1

    let longestRoad (opponentVertices : Vertex list) (player : Player) =
        let paths =
            player.Roads
            |> List.map (fun (Road path) -> path)

        paths
        |> List.map (fun path -> 
            longestRoadStartingFromPath 
                path 
                paths
                opponentVertices)
        |> List.max

    let makePlayer paths =
        {
            PlayerId = PlayerId 0
            VictoryPoints = 0
            ResourceCards = Map []
            DevelopmentCards = Map []
            Infrastructure = []
            Roads = List.map Road paths
            ArmySize = 0
        }

    let testPaths =
        [
            (4, 0), (5, 0)
            (5, 0), (6, 0)
            (6, 0), (6, 1)

            (6, 0), (7, 0)
            (7, 0), (8, 0)
            (8, 0), (8, 1)
            (8, 1), (7, 1)
            (7, 1), (6, 1)

            (7, 1), (7, 2)

            (4, 0), (4, 1)
            (4, 1), (5, 1)
            (5, 1), (6, 1)

            (4, 4), (5, 4)
            (5, 4), (6, 4)
            (5, 4), (5, 3)
        ]
        |> List.map (fun ((a, b), (c, d)) -> 
            Path ({ X = a; Y =  b }, { X = c; Y =  d }))

    let opponentVertices =
        [
            (6, 0)
            (8, 1)
            (6, 1)
        ]
        |> List.map (fun (a, b) -> { X = a; Y =  b })

    testPaths
    |> makePlayer
    |> longestRoad opponentVertices


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

    let removeResources (x : Map<ResourceCard, int>) =
        x
        |> Map.toList
        |> List.unzip
        ||> List.map2 removeResource
        |> List.reduce (>>)

    let addResources (x : Map<ResourceCard, int>) =
        x
        |> Map.toList        
        |> List.unzip
        ||> List.map2 addResource
        |> List.reduce (>>)

module GameState =
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

    let updateAchievements (gameState : GameState) =
        let opponentVertices =
            gameState.OtherPlayers
            |> List.collect (fun player -> player.Infrastructure)
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
            |> Map.map (fun k v -> v <= gameState.ResourceBank.[k])

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
                ResourceBank =
                    gameState.ResourceBank
                    |> Map.map ( fun k v -> v - totalResourcesCollected.[k])
        } 
        |> mapPlayers (fun player -> 
            player
            |> Player.addResources (resourcesCollectedByPlayers.[player.PlayerId])
        )

    let buildInfrastructure (infrastructure : Infrastructure) (gameState : GameState) =

        let resourceCost =
            match infrastructure.BuildingType with
            | Settlement -> Map [ (Brick, 1); (Lumber, 1); (Wool, 1); (Grain, 1) ]
            | City -> Map [ (Ore, 3); (Grain, 2) ]

        {
            gameState with
                ActivePlayer =
                    { 
                        gameState.ActivePlayer with
                            Infrastructure =
                                gameState.ActivePlayer.Infrastructure
                                |> List.filter (fun x -> 
                                    x.Location <> infrastructure.Location)
                                |> List.append [infrastructure]
                    } 
                    |> Player.removeResources resourceCost
                    |> Player.updateVictoryPoints gameState.AchievementCards
                ResourceBank =
                    gameState.ResourceBank
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
                ResourceBank =
                    gameState.ResourceBank
                    |> Map.map (fun k v -> v + resourceCost.[k])
        }
        |> updateAchievements
        |> mapPlayers (Player.updateVictoryPoints gameState.AchievementCards)

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
                    ResourceBank =
                        gameState.ResourceBank
                        |> Map.map (fun k v -> v + resourceCost.[k])
            }

    let playDevelopMentCard (rand : System.Random) (developmentAction : DevelopmentCardAction) (gameState : GameState) =
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
                            ResourceBank =
                                g.ResourceBank
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
            let card = developmentActionToCard developmentAction

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
                ResourceBank =
                    gameState.ResourceBank
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

let checkIfWon (gameState : GameState) =
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

let rand = System.Random()

let game = init rand

game.Terrain
|> Map.toList
|> List.map (fun (k, v) ->
    match v with
    | Fertile x -> x.TerrainType.ToString()
    | Desert -> "Desert")
|> List.iter (printfn "%s")

game.Terrain
|> Map.toList
|> List.map (fun (k, v) ->
    match v with
    | Fertile x -> x.Number.ToString()
    | Desert -> "Desert")
|> List.iter (printfn "%s")

(*

TODO:

Validators
Refactor
Modules
Lenses

Server
Commands
Client

*)