open Suave
open Suave.Successful
open Suave.Web
open Suave.Utils.Choice
open Suave.Operators
open Suave.Filters
open Suave.Html
open Newtonsoft.Json
open Catan

let newGame () =
    let gameState =
        Domain.init (System.Random())

    let json =
        JsonConvert.SerializeObject(gameState, Formatting.Indented)

    p ["style", "white-space: pre-wrap;"] [Text json]
    |> htmlToString

let app =
    GET 
    >=> path "/"
    >=> warbler (fun _ ->
        OK (newGame ()))

startWebServer defaultConfig app