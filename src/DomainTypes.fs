namespace Catan

type ResourceCard =
    | Brick
    | Lumber
    | Wool
    | Grain
    | Ore

type TerrainIndex = TerrainIndex of int

type TerrainType =
    | Hills
    | Forest
    | Pasture
    | Fields
    | Mountains

type Terrain = 
    { TerrainType : TerrainType
      Number : int }

type TerrainTile =
    | Fertile of Terrain
    | Desert

type Vertex =
    { X : int
      Y : int }

type CommunityType =
    | Settlement
    | City    

type Community =
    { CommunityType : CommunityType
      Location : Vertex }

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

type Road = Road of Path

type HarborIndex = HarborIndex of int

type Harbor =
    | ResourceHarbor of ResourceCard
    | ThreeForOneHarbor

type AchievementCard =
    | LongestRoad
    | LargestArmy

type PlayerId = PlayerId of int

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

type Player = 
    { PlayerId : PlayerId
      VictoryPoints : int
      ResourceCards : Map<ResourceCard, int>
      DevelopmentCards : Map<DevelopmentCard, int>
      Communities : Community list
      Roads : Road list
      ArmySize : int }
    
    static member PlayerId_ =
        (fun x -> x.PlayerId), (fun v x -> { x with PlayerId = v })
    static member VictoryPoints_ =
        (fun x -> x.VictoryPoints), (fun v x -> { x with VictoryPoints = v })
    static member ResourceCards_ =
        (fun x -> x.ResourceCards), (fun v x -> { x with ResourceCards = v })
    static member DevelopmentCards_ =
        (fun x -> x.DevelopmentCards), (fun v x -> { x with DevelopmentCards = v })
    static member Communities_ =
        (fun x -> x.Communities), (fun v x -> { x with Communities = v })
    static member Roads_ =
        (fun x -> x.Roads), (fun v x -> { x with Roads = v })
    static member ArmySize_ =
        (fun x -> x.ArmySize), (fun v x -> { x with ArmySize = v })
    

type MaritimeTrade = 
    { TradedResource : ResourceCard
      TradedQuantity : int
      RecievedResource : ResourceCard }

type TradeOffer =
    { OfferingPlayerId : PlayerId
      DesiredResources : Map<ResourceCard, int>
      OfferedResources : Map<ResourceCard, int> }

type TradeInfo =
    { OfferingPlayerId : PlayerId
      RecievingPlayerId : PlayerId
      DesiredResources : Map<ResourceCard, int>
      OfferedResources : Map<ResourceCard, int> }

type Trade =
    | SuggestedTrade of TradeOffer
    | AcceptedTrade of TradeInfo
    | ConfirmedTrade of TradeInfo

type GameState =
    { ActivePlayer : Player
      OtherPlayers : Player list
      ResourcePool : Map<ResourceCard, int>
      DevelopmentCards : DevelopmentCard list
      Terrain : Map<TerrainIndex, TerrainTile>
      Harbors : Map<HarborIndex, Harbor>
      RobberLocation : TerrainIndex
      AchievementCards : Map<AchievementCard, PlayerId option>
      Winner : PlayerId option }

    static member ActivePlayer_ =
        (fun x -> x.ActivePlayer), (fun v x -> { x with ActivePlayer = v })
    static member OtherPlayers_ =
        (fun x -> x.OtherPlayers), (fun v x -> { x with OtherPlayers = v })
    static member ResourcePool_ =
        (fun x -> x.ResourcePool), (fun v x -> { x with ResourcePool = v })
    static member DevelopmentCards_ =
        (fun x -> x.DevelopmentCards), (fun v (x : GameState) -> { x with DevelopmentCards = v })
    static member Terrain_ =
        (fun x -> x.Terrain), (fun v x -> { x with Terrain = v })
    static member Harbors_ =
        (fun x -> x.Harbors), (fun v x -> { x with Harbors = v })
    static member RobberLocation_ =
        (fun x -> x.RobberLocation), (fun v x -> { x with RobberLocation = v })
    static member AchievementCards_ =
        (fun x -> x.AchievementCards), (fun v x -> { x with AchievementCards = v })
    static member Winner_ =
        (fun x -> x.Winner), (fun v x -> { x with Winner = Some v })


module Map =
    let mapValueAtKey (key : 'Key) (f : 'T -> 'T) (map : Map<'Key, 'T>) =
        Map.add key (f map.[key]) map

module Helpers =
    let roll (rand : System.Random) () = rand.Next(1, 7), rand.Next(1, 7)

    let shuffle (rand : System.Random) (cards : 'T list) : 'T list =
        let innerShuffle cards =
            let cards' = List.toArray cards

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