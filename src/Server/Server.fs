module Server

open System.IO
open System.Net
open Suave
open Suave.Successful
open Suave.Web
open Suave.Utils.Choice
open Suave.Operators
open Suave.Filters
open Suave.Html
open Newtonsoft.Json

let clientPath = Path.Combine("..","Client/public") |> Path.GetFullPath 
let port = 8085us

let config =
  { defaultConfig with 
      homeFolder = Some clientPath
      bindings = [ HttpBinding.create HTTP (IPAddress.Parse "0.0.0.0") port ] }


let newGame () =
    let gameState = GameState.init (System.Random())
    JsonConvert.SerializeObject(gameState, Formatting.Indented)

let gameState =
    GET 
    >=> path "/api/game-state"
    >=> warbler (fun _ -> OK (newGame ()))
    >=> Writers.setMimeType "text/plain"

let app =
    choose [
        gameState
        Files.browseHome     
    ]

startWebServer config app