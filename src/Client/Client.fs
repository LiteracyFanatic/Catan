module Client

type Model =
  { Counter : int
    Text : string }

type Msg = 
  | Increment 
  | Decrement
  | GetGameState
  | UpdateText of Result<string, exn>

open Elmish
open Fable.PowerPack.Fetch
open Fable.PowerPack

let init () =
  { Counter = 0; Text = "" }, Cmd.none
  
let update (msg:Msg) model =
  match msg with
  | Increment -> { model with Counter = model.Counter + 1 }, Cmd.none
  | Decrement -> { model with Counter = model.Counter - 1 }, Cmd.none
  | GetGameState -> 
      let cmd =
        Cmd.ofPromise
          (fun a ->
            promise {
              let! res = fetch "/api/game-state" a
              return! res.text()
              }) 
          [] 
          (Ok >> UpdateText) 
          (Error >> UpdateText)  
      model, cmd
  | UpdateText (Ok text) -> { model with Text = text }, Cmd.none
  | UpdateText (Error exn) -> { model with Text = exn.Message }, Cmd.none     

open Fable.Helpers.React.Props
module R = Fable.Helpers.React

open Fable.Core.JsInterop
importAll "./sass/main.sass"

let view model dispatch =

  R.div []
    [ R.button [ OnClick (fun _ -> dispatch Decrement) ] [ R.str "-" ]
      R.div [] [ R.str (sprintf "%A" model.Counter) ]
      R.button [ OnClick (fun _ -> dispatch Increment) ] [ R.str "+ " ]
      R.textarea [ Value model.Text ] [] 
      R.button [ OnClick (fun _ -> dispatch GetGameState)] [ R.str "Get Game State" ] ]

open Elmish.React

Program.mkProgram init update view
|> Program.withReact "elmish-app"
|> Program.run
