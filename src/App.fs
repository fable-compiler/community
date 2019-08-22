module App

open System.Text.RegularExpressions
open Elmish
open Elmish.React
open Fable.Core
open Fable.React
open Fable.React.Props
open Fable.SimpleHttp
open Fulma
open Fable.FontAwesome
open Global

let [<Literal>] GITHUB_JSON = "https://raw.githubusercontent.com/fable-compiler/community/master/public/community.json"

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

let init(): Model * Cmd<Msg> =
    let model = Model.Empty
    let cmd =
        let msg = Resource.FromJsonArray >> shuffle >> ResourcesLoaded
        async {
            #if !DEBUG
            try
                let! (_, json) = Http.get GITHUB_JSON
                return msg json
            with ex ->
                printfn "Cannot get JSON from Github, using deployed version: %s" ex.Message
            #endif
                let! (_, json) = Http.get "community.json"
                return msg json
        } |> Cmd.OfAsync.result
    model, cmd

// TODO: Truncate?
let resetVisibleResources (model:Model) =
    { model with VisibleResources = model.Resources }

let update (msg:Msg) (model:Model) =
    match msg with    
    | ToggleBurger ->
        { model with BurgerOpen = not model.BurgerOpen }, Cmd.none
    | ResourcesLoaded resources ->
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

let resourceItem =
    FunctionComponent.Of((fun (p: {| key: string; resource: Resource|}) ->
        let reportLink =
            "https://github.com/fable-compiler/community/issues/new"
            + "?title=" + JS.encodeURIComponent(p.resource.title + " needs edit")
            + "&body=" + JS.encodeURIComponent("Please explain the reason.")

        let category = p.resource.category.ToString().ToLower()
            
        div [ Class "resource-item" ]
            [ Heading.h5 [] [
                a [Href p.resource.link] [str p.resource.title]
                str "  "
                Tag.tag [ Tag.CustomClass (sprintf "is-%s" category)] [str category ]
                str " "
                a [Href reportLink; Target "_blank"]
                  [Fa.i [ Fa.Solid.PencilAlt; Fa.Size Fa.FaExtraSmall ] []]
              ]
              Heading.h6 [Heading.IsSubtitle] [str p.resource.description] ]),
        displayName = "Resource",
        memoizeWith = fun p1 p2 -> p1.key = p2.key)          

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
                    ul [Class "resource-list"] (model.VisibleResources |> List.map (fun r ->
                        resourceItem {| resource = r; key = string r.hash|}))
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
