module Catan.Path
let intersect (Path (a, b)) (Path (c, d)) =
    a = c || a = d || b = c || b = d

let fromVertex (a : Vertex) (opponentVertices : Vertex list) =
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

let adjacent (Path (a, b) as path) (opponentVertices : Vertex list) =
    fromVertex a opponentVertices @ fromVertex b opponentVertices
    |> List.filter ((<>) path)

let longestChainFromPath (path : Path) (roadLocations : Path list) (opponentVertices : Vertex list) =
    let rec loop (chain : Path list) n =
        match roadLocations with
        | [] -> failwith "Player shouldn't be able to have no roads."
        | _ ->
            let adjacent =
                adjacent chain.Head opponentVertices

            let intersectingPaths =
                roadLocations
                |> List.filter (fun p ->
                    let isAdjacent = 
                        List.contains p adjacent
                    let isInChain = 
                        List.contains p chain
                    let isFork =
                        if chain.Length > 1 then
                            intersect chain.[1] p
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
