import Array exposing (Array)
import Dict exposing (Dict)
import Html exposing (Html, Attribute, text, div, input, button, a, img)
import Html.App exposing (program)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import Http exposing (send, defaultSettings, empty, Value(..), Response, RawError(..), Error(..))
import String
import Task exposing (Task, andThen, mapError, succeed, fail)
import Json.Decode exposing (Decoder, int, string, object3, (:=), customDecoder, decodeValue)

main =
  program { init = init
          , view = view
          , update = update
          , subscriptions = subscriptions }

type alias Model =
  { location : String
  , debug : String
  , body : SirenDocument }

sirenDoc : SirenDocument
sirenDoc =
  { class = Nothing
  , properties =
    Just { title = "mytitle"
         , description = "mydesc" }
  , actions =
    Just [ { name = "start-game"
         , method = "POST"
         , href = "http://localhost:1337/hywit/void"
         , fields = [ { name = "foo"
                      , type' = "text"
                      , value = "nought" } ] } ]
  , links =
    Just [ { rel = ["lol", "hello"], href = "http://wherever" }
         , { rel = ["ok", "wut"], href = "http://lolever" }
         , { rel = ["yes", "image"]
           , href = "http://localhost:1337/images/grue.png" }
         , { rel = ["image"]
           , href = "http://localhost:1337/images/cave.png" }] }

model : Model
model =
  { location = "http://hyperwizard.azurewebsites.net/hywit/void"
  , debug = ""
  , body = sirenDoc }

init : (Model, Cmd Msg)
init = (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

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

type alias SirenActions =
  List SirenAction

type alias SirenClass =
  List String

type alias SirenLinks =
  List SirenLink

type alias SirenDocument =
  { class: Maybe SirenClass
  , properties: Maybe SirenProperties
  , actions: Maybe SirenActions
  , links: Maybe SirenLinks }

type JVal
  = JString String
  | JBool Bool
  | JInt Int
  | JList (List JVal)
  | JDict (Dict String JVal)


nameVal = JString "Gamgee"
typeVal = JString "text"
valVal = JString "nought"

arr = JList [ nameVal, typeVal, valVal]

--dctVal = Dict.insert ("name" nameVal Dict.empty)

-- DECODERS

sirenFieldDecoder : Decoder SirenField
sirenFieldDecoder =
    object3 SirenField
        ("name" := string)
        ("type" := string)
        ("value" := string)
      
sirenFieldListDecoder : Decoder (List SirenField)
sirenFieldListDecoder = Json.Decode.list sirenFieldDecoder

lazy : (() -> Decoder a) -> Decoder a
lazy getDecoder =
    customDecoder Json.Decode.value <|
       \rawValue ->
           decodeValue (getDecoder ()) rawValue
          
type Tree
  = Leaf String
  | Branch Tree Tree

type JsonVal
  = JsonStr String
  | JsonBool Bool
  | JsonInt Int
  | JsonFloat Float
  | JsonList (List JsonVal)
  | JsonDict (Dict String JsonVal)

decodeStr : String -> Decoder JsonVal
decodeStr s = Json.Decode.succeed (JsonStr s)

decodeBool : Bool -> Decoder JsonVal
decodeBool b = Json.Decode.succeed (JsonBool b)

decodeInt : Int -> Decoder JsonVal
decodeInt n = Json.Decode.succeed (JsonInt n)

decodeFloat : Float -> Decoder JsonVal
decodeFloat f = Json.Decode.succeed (JsonFloat f)

decodeList : List JsonVal -> Decoder JsonVal
decodeList lst = Json.Decode.succeed (JsonList lst)

decodeDict : Dict String JsonVal -> Decoder JsonVal
decodeDict dct = Json.Decode.succeed (JsonDict dct)

valDecoder : Decoder JsonVal
valDecoder =
  Json.Decode.oneOf
    [ Json.Decode.float `Json.Decode.andThen` decodeFloat
    , Json.Decode.int `Json.Decode.andThen` decodeInt
    , Json.Decode.bool `Json.Decode.andThen` decodeBool
    , Json.Decode.string `Json.Decode.andThen` decodeStr
    , Json.Decode.dict (lazy (\_ -> valDecoder)) `Json.Decode.andThen` decodeDict
    , Json.Decode.list (lazy (\_ -> valDecoder)) `Json.Decode.andThen` decodeList ]

type Msg
  = NewLocation String
  | GoToLink String
  | FetchSucceed String
  | FetchFail Http.Error

promoteError : RawError -> Error
promoteError rawError =
  case rawError of
    RawTimeout -> Timeout
    RawNetworkError -> NetworkError

handleResponse : (String -> Task Error a) -> Response -> Task Error a
handleResponse handle response =
  if 200 <= response.status && response.status < 300 then
      case response.value of
        Text str ->
            handle str
        _ ->
            fail (UnexpectedPayload "Response body is a blob, expecting a string.")
  else
    fail (BadResponse response.status response.statusText)


getStr : String -> Task Error String
getStr url =
  let request =
        { verb = "GET"
        , headers = [ ("Accept", "application/vnd.siren+json") ]
        , url = url
        , body = empty
        }
  in
      mapError promoteError (send defaultSettings request)
      `andThen` handleResponse succeed

fetchCommand : String -> Cmd Msg
fetchCommand loc =
  Task.perform FetchFail FetchSucceed (getStr loc)
 
update : Msg -> Model -> (Model, Cmd Msg)
update msg oldModel =
  case msg of
  NewLocation loc -> ({ oldModel | location = loc }, Cmd.none)
  GoToLink loc -> ({ oldModel | location = loc }, fetchCommand loc)
  FetchSucceed s -> ({ oldModel | debug = s }, Cmd.none)
  FetchFail e -> ({ oldModel | debug = "ohhh" }, Cmd.none)

-- VIEW

renderImageLink : SirenLink -> Html Msg
renderImageLink link =
  img [ src link.href ] []

renderAnchorLink : SirenLink -> Html Msg
renderAnchorLink link =
  a [ onClick (GoToLink link.href) ] [ text link.href ]

renderLink : SirenLink -> Html Msg
renderLink link =
  if List.member "image" link.rel then renderImageLink link else renderAnchorLink link

renderStringProperty : String -> String -> Html Msg
renderStringProperty key value =
  div [] [ div [ style [ ("font-weight", "bold") ] ] [ text key ]
         , div [] [ text value ] ]

renderProperties : Maybe SirenProperties -> Html Msg
renderProperties maybeProps =
  case maybeProps of
  Just properties ->
    div [] [ renderStringProperty "title" properties.title
           , renderStringProperty "description" properties.description ]
  Nothing ->
    div [] []

renderField field =
  div [] [ div [ style [ ("font-weight", "bold") ] ] [ text field.name ]
         , input [ value field.value ] [ text field.name ] ]

renderAction action =
  div [] [ div [] (List.map renderField action.fields)
         , button [] [ text action.name ]
         , text action.href ]

renderActions : Maybe SirenActions -> Html Msg
renderActions maybeActions =
  case maybeActions of
  Just actions ->
    let rendered = List.map renderAction actions
    in
    div [] rendered
  Nothing ->
    div [] []

renderDocument : SirenDocument -> Html Msg
renderDocument doc =
  let (imageLinks, anchorLinks) =
    case doc.links of
    Just links ->
      List.partition (\lnk -> List.member "image" lnk.rel) links
    Nothing ->
      ([], [])
  in
  div []
    [ renderProperties doc.properties
    , renderActions doc.actions
    , div [] (List.map renderAnchorLink anchorLinks)
    , div [] (List.map renderImageLink imageLinks) ]

view : Model -> Html Msg
view model =
  div []
    [ input [ value model.location, onInput NewLocation, myStyle ] []
    , button [ onClick (GoToLink "http://localhost:1337/hywit/void") ] [ text "->" ]
    , div [] [ text ("loc " ++ model.location) ]
    , div [] [ text ("dbg " ++ model.debug) ]
    , renderDocument model.body ]

myStyle =
  style
    [ ("width", "80%")
    , ("text-align", "center")
    ]