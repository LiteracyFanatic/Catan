module Vertex

open Catan

let toCorners (vertex : Vertex) =
    [(0, 0); (1, 0); (2, 0); (0, 1); (1, 1); (2, 1)]
    |> List.map (fun (x, y) -> { X = x + vertex.X; Y = y + vertex.Y })
