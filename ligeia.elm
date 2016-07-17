import Html exposing (Html, Attribute, text, div, input, button, a, img)
import Html.App exposing (beginnerProgram)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String


main =
  beginnerProgram { model = model, view = view, update = update }


type alias Model = 
  { location : String,
    body : SirenDocument }

sirenDoc = 
  { properties = { title = "mytitle"
                 , description = "mydesc" } 
  , actions = [ { name = "start-game" 
                , method = "POST"
                , href = "http://localhost:1337/hywit/void"
                , fields = [ { name = "foo"
                             , type' = "text" 
                             , value = "nought" } ] } ]
  , links = [ { rel = ["lol", "hello"], href = "http://wherever" }
            , { rel = ["ok", "wut"], href = "http://lolever" }
            , { rel = ["yes", "image"]
              , href = "http://localhost:1337/images/grue.png" }
            , { rel = ["image"]
              , href = "http://localhost:1337/images/cave.png" }] }

model : Model
model = 
  { location = "http://hyperwizard.azurewebsites.net/hywit/void",
    body = sirenDoc }

-- UPDATE

type alias SirenLinkRel = String
type alias SirenHref = String

type alias SirenLink = 
  { rel: List SirenLinkRel,
    href: SirenHref }

type alias SirenProperties = 
  { title: String,
    description: String }

type alias SirenField = 
  { name: String,
    type': String,
    value: String }

type alias SirenAction = 
  { name: String,
    method: String,
    href: SirenHref,
    fields: List SirenField }

type alias SirenDocument = 
  { properties: SirenProperties,
    actions: List SirenAction,
    links: List SirenLink }

type Msg = NewLocation String | GoToLink String

update msg oldModel =
  case msg of
  NewLocation loc -> { oldModel | location = loc }
  GoToLink loc -> { oldModel | location = loc }

renderImageLink link =
  img [ src link.href ] [] 

renderAnchorLink link =
  a [ onClick (GoToLink link.href) ] [ text link.href ]

renderLink link =
  if List.member "image" link.rel then renderImageLink link else renderAnchorLink link

renderStringProperty key value = 
  div [] [ div [ style [ ("font-weight", "bold") ] ] [ text key ] 
         , div [] [ text value ] ]  

renderProperties properties =
  div [] [ renderStringProperty "title" properties.title
         , renderStringProperty "description" properties.description ] 

renderAction action = 
  div [] [ button [] [ text action.name ] 
         , text action.href ]

renderActions actions =
  let rendered = List.map renderAction actions
  in 
  div [] rendered

renderDocument doc =
  let (imageLinks, anchorLinks) = 
    List.partition (\lnk -> List.member "image" lnk.rel) doc.links
  in 
  div []
    [ renderProperties doc.properties
    , renderActions doc.actions
    , div [] (List.map renderAnchorLink anchorLinks)
    , div [] (List.map renderImageLink imageLinks) ]

view model =
  div []
    [ input [ value model.location, onInput NewLocation, myStyle ] []
    , button [ onClick (GoToLink "I don't even know") ] [ text "->" ]
    , div [] [ text model.location ]
    , renderDocument model.body ]

myStyle =
  style
    [ ("width", "80%")
    , ("text-align", "center")
    ]
