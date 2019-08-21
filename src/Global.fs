module Global

open Fable.Core

[<StringEnum>]
type ResourceCategory =
    | Library
    | Tool
    | Tutorial
    | Video
    | Template
    | Sample

let private stringHash (s: string) =
    let mutable h = 5381
    for i = 0 to s.Length - 1 do
        h <- (h * 33) ^^^ (int s.[i])
    h

type Resource =
    { title: string
      description: string
      link: string
      category: ResourceCategory
      hash: int }
      static member FromJsonArray(json: string) =
        let rs: Resource[] = JS.JSON.parse(json) :?> _
        rs |> Seq.map (fun r ->
            { r with hash = stringHash(r.title + r.link) })
        |> List.ofSeq

type Model =
    { BurgerOpen: bool
      Resources: Resource list
      VisibleResources: Resource list
      SearchText: string }
    static member Empty =
        { BurgerOpen = false
          Resources = []
          VisibleResources = []
          SearchText = "" }

type Msg =
    | ToggleBurger
    | ResourcesLoaded of Resource list
    | SearchTextUdpdated of string
