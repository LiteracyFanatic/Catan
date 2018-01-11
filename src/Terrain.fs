module Terrain

open Catan

let toResourceCard = function
    | Hills -> Brick
    | Forest -> Lumber
    | Pasture -> Wool
    | Fields -> Grain
    | Mountains -> Ore

let private (|From|_|) (lower : int) (upper : int) (x : int) =
    if x >= lower && x <= upper then Some x else None

let toVertices (TerrainIndex i) =
    let corner =
        match i with
        | From 0 2 i -> { X = 2 * i + 2; Y = 0 }
        | From 3 6 i -> { X = 2 * (i - 3) + 1; Y = 1 }
        | From 7 11 i -> { X = 2 * (i - 7); Y = 2 }
        | From 12 15 i -> { X = 2 * (i - 12) + 1; Y = 3 }
        | From 16 18 i -> { X = 2 * (i - 16) + 2; Y = 4 }
        | _ -> failwith "Terrain index out of bounds"

    Vertex.toCorners corner    

let adjacentTiles (TerrainIndex i) =
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
