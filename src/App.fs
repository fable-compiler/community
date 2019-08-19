module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Elmish architecture and samples at https://elmish.github.io/
*)

open System.Text.RegularExpressions
open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fable.SimpleHttp
open Fulma
open Global

let init(): Model * Cmd<Msg> =
    let model = Model.Empty
    let cmd =
        async {
            let! (_, json) = Http.get "community.json"
            return ResourcesLoaded json
        } |> Cmd.OfAsync.result
    model, cmd

let shuffle (org:_ list) =
    let rng = System.Random()
    let arr = Array.ofList org
    let max = arr.Length - 1
    let randomSwap (arr:_[]) i =
        let pos = rng.Next(max)
        let tmp = arr.[pos]
        arr.[pos] <- arr.[i]
        arr.[i] <- tmp
        arr
    [|0..max|] |> Array.fold randomSwap arr |> Array.toList

// TODO: Truncate?
let resetVisibleResources (model:Model) =
    { model with VisibleResources = model.Resources }

let update (msg:Msg) (model:Model) =
    match msg with    
    | ToggleBurger ->
        { model with BurgerOpen = not model.BurgerOpen }, Cmd.none
    | ResourcesLoaded json ->
        let resources = Resource.FromJsonArray json |> shuffle
        { model with Resources = resources }
        |> resetVisibleResources, Cmd.none
    | SearchTextUdpdated text ->
        let terms: string list =
            match text.Trim() with
            | text when text.Length > 2 ->
                // TODO: Discard terms if their length is less than 2?
                Regex.Split(text, @"(?:\s*,\s*)|\s+") |> Array.toList
            | _ -> []
        let model =
            match terms with
            | [] -> resetVisibleResources model
            | terms ->
                let regex =
                    Regex(terms
                          |> Seq.map (fun x -> "(?=.*" + Regex.Escape(x) + ")")
                          |> String.concat "", RegexOptions.IgnoreCase)
                { model with VisibleResources =
                                model.Resources |> List.filter (fun r ->
                                    regex.IsMatch(r.title + "\n" + r.description)) }
        { model with SearchText = text }, Cmd.none

// VIEW (rendered with React)

let footer() =
    footer [
      Class "footer"
      Style [BackgroundColor "dodgerblue"]
    ] [ 
      Content.content [] [
        p [] [
          str "Fable "
          a [ Href "https://github.com/fable-compiler/Fable" ]
            [ str "source code" ]
          str " is licensed "
          a [ Href "http://opensource.org/licenses/mit-license.php" ]
            [ str "MIT" ]
          str "."
    ] ] ]

let searchBox (model:Model) dispatch =
    Field.div [ ]
        [ //Label.label [ ] [ str "Search" ]
          Control.div [ ]
            [ Input.text [ Input.Placeholder "Search resources"
                           Input.Props [AutoFocus true]
                           Input.Value model.SearchText
                           Input.OnChange (fun ev -> SearchTextUdpdated ev.Value |> dispatch)
                         ]
            ]
        ]

let resourceItem (r: Resource) =
    div [ Class "resource-item"
          Key (string r.hash) ]
        [ Heading.h5 [] [
            a [Href r.link] [str r.title]
            str "  "
            Tag.tag [ Tag.Color IsSuccess ] [str <| r.category.ToString().ToLower()]
          ]
          Heading.h6 [Heading.IsSubtitle] [str r.description] ]

let view (model:Model) dispatch =
  fragment []
      [ Navbar.view model.BurgerOpen dispatch
        Section.section [Section.CustomClass "main-section"] [
            Columns.columns [] [
                Column.column [
                    Column.Width (Screen.All, Column.IsThreeFifths)
                    Column.Offset (Screen.All, Column.IsOneFifth)
                ] [
                    div [Class "intro-text"] [
                        Heading.h4 [] [str "Find resources from the community!"]
                        Heading.h6 [Heading.IsSubtitle] [
                            str "Let's grow together. Send us a PR to "
                            a [Href "https://github.com/fable-compiler/community/edit/master/public/community.json"] [str "add your awesome project"]
                            str "."
                        ]

                    ]
                    searchBox model dispatch
                    ul [Class "resource-list"]
                        (List.map resourceItem model.VisibleResources)
                ]
            ]
        ]
        footer()
      ]

// App
Program.mkProgram init update view
|> Program.withReactSynchronous "elmish-app"
// |> Program.withConsoleTrace
|> Program.run
