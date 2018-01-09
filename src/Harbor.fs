module Harbor

open Catan

let toVertices (HarborIndex i) =
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
