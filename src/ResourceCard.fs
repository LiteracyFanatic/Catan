module Catan.ResourceCard
let terrainToResource = function
    | Hills -> Brick
    | Forest -> Lumber
    | Pasture -> Wool
    | Fields -> Grain
    | Mountains -> Ore
    