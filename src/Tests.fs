module Tests

open Catan

let makePlayer paths =
    { PlayerId = PlayerId 0
      VictoryPoints = 0
      ResourceCards = Map []
      DevelopmentCards = Map []
      Communities = []
      Roads = List.map Road paths
      ArmySize = 0 }

let testPaths =
    [ (4, 0), (5, 0)
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
      (5, 4), (5, 3) ]
    |> List.map (fun ((a, b), (c, d)) -> 
        Path ({ X = a; Y =  b }, { X = c; Y =  d }))

let opponentVertices =
    [ (6, 0)
      (8, 1)
      (6, 1) ]
    |> List.map (fun (a, b) -> { X = a; Y =  b })

let n =
    testPaths
    |> makePlayer
    |> Player.longestRoad opponentVertices

let rand = System.Random()

let game = GameState.init rand

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