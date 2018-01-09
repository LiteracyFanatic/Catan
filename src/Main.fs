open Suave

[<EntryPoint>]
let main argv = 
    startWebServer defaultConfig Server.app
    0