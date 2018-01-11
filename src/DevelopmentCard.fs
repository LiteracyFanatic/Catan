module DevelopmentCard

open Catan

let fromDevelopmentAction = function
    | VictoryPointAction -> VictoryPoint
    | KnightAction _ -> Knight
    | YearOfPlentyAction _ -> YearOfPlenty
    | RoadBuildingAction _ -> RoadBuilding
    | MonopolyAction _ -> Monopoly

