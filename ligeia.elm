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
  , body : ResponseDocument }

sirenDoc : SirenDocument
sirenDoc =
  { class = Nothing
  , properties =
    Just (JsonDict (Dict.fromList [("title", JsonStr "my-title"), ("description", JsonStr "my-description")]))
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
  , body = SirenDoc sirenDoc }

init : (Model, Cmd Msg)
init = (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- UPDATE

type JsonVal
  = JsonStr String
  | JsonBool Bool
  | JsonInt Int
  | JsonFloat Float
  | JsonNull
  | JsonList (List JsonVal)
  | JsonDict (Dict String JsonVal)

type alias SirenLinkRel = String
type alias SirenHref = String

type alias SirenLink =
  { rel: List SirenLinkRel,
    href: SirenHref }

type alias SirenProperties = JsonVal

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



decodeNull : Decoder JsonVal
decodeNull = Json.Decode.null JsonNull

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
    [ Json.Decode.null JsonNull
    , Json.Decode.float `Json.Decode.andThen` decodeFloat
    , Json.Decode.int `Json.Decode.andThen` decodeInt
    , Json.Decode.bool `Json.Decode.andThen` decodeBool
    , Json.Decode.string `Json.Decode.andThen` decodeStr
    , Json.Decode.dict (lazy (\_ -> valDecoder)) `Json.Decode.andThen` decodeDict
    , Json.Decode.list (lazy (\_ -> valDecoder)) `Json.Decode.andThen` decodeList ]

type Msg
  = NewLocation String
  | GoToLink String
  | FetchSucceed ResponseDocument
  | FetchFail Http.Error

type ResponseDocument
  = TextDoc String
  | JsonDoc JsonVal
  | SirenDoc SirenDocument

type SirenParseResult
  = InvalidSiren JsonVal
  | ValidSiren SirenDocument

type alias JsonParseError = (String, JsonVal)

parseClassElement : JsonVal -> Result JsonParseError String
parseClassElement json =
  case json of
    JsonStr str -> Ok str
    _ -> Err ("parseClassElement: Unexpected json instead of string.", json)

combineClassResult : (Result JsonParseError String) -> Result JsonParseError (List String) -> Result JsonParseError (List String)
combineClassResult res acc =
  case acc of
    Ok lst ->
      case res of
        Ok s -> Ok (List.append lst [s])
        Err e -> Err e
    Err _ -> acc

toListOfClasses : List (Result JsonParseError String) -> Result JsonParseError (List String)
toListOfClasses resultList =
  List.foldl combineClassResult (Ok []) resultList

parseClass : JsonVal -> Result JsonParseError SirenClass
parseClass json =
  case json of
    JsonList lst ->
      let
        resultList : List (Result JsonParseError String)
        resultList = List.map parseClassElement lst
        -- List (Result JsonParseError String) -> Result JsonParseError (List String)
        classes : Result JsonParseError SirenClass
        classes = toListOfClasses resultList
      in
        classes
    _ -> Err ("parseClass: Unexpected json instead of list of strings.", json)

combineLinkResult : (Result JsonParseError SirenLink) -> Result JsonParseError SirenLinks -> Result JsonParseError SirenLinks
combineLinkResult res acc =
  case acc of
    Ok lst ->
      case res of
        Ok s -> Ok (List.append lst [s])
        Err e -> Err e
    Err _ -> acc

toListOfLinks : List (Result JsonParseError SirenLink) -> Result JsonParseError SirenLinks
toListOfLinks resultList =
  List.foldl combineLinkResult (Ok []) resultList

parseLink : JsonVal -> Result JsonParseError SirenLink
parseLink json =
  case json of
    JsonDict dct ->
      let (maybeRel, maybeHref) = (Dict.get "rel" dct, Dict.get "href" dct)
      in
        case (maybeRel, maybeHref) of
          (Just rel, Just href) ->
            Ok { rel = [ "relrelrel" ]
               , href = "http://void" }
          _ ->
            Err ("parseLink: Missing parts of link.", json)
    _ ->
      Err ("parseLink: Unexpected json instead of link.", json)

parseLinks : JsonVal -> Result JsonParseError SirenLinks
parseLinks json =
  case json of
    JsonList lst ->
      let
        resultList : List (Result JsonParseError SirenLink)
        resultList = List.map parseLink lst
        links : Result JsonParseError SirenLinks
        links = toListOfLinks resultList
      in
        links
    _ -> Err ("parseLinks: Unexpected json instead of list of links.", json)

liftError : Maybe (Result JsonParseError a) -> Result JsonParseError (Maybe a)
liftError maybeResult =
  case maybeResult of
    Just (Ok v) -> Ok (Just v)
    Just (Err e) -> Err e
    Nothing -> Ok Nothing

toSiren : JsonVal -> SirenParseResult
toSiren json =
  case json of
    JsonDict jd ->
      let
        maybeClassJson = Dict.get "class" jd -- Just JsonVal | Nothing
        maybePropertiesJson = Dict.get "properties" jd
        maybeActionsJson = Dict.get "actions" jd
        maybeLinksJson = Dict.get "links" jd
        maybeClassResult = Maybe.map parseClass maybeClassJson -- Just (Result String SirenClass) | Nothing
        maybeLinksResult = Maybe.map parseLinks maybeLinksJson -- Just (Result String SirenLinks) | Nothing
        classResult = liftError maybeClassResult
        linksResult = liftError maybeLinksResult
        -- (Just (Result String SirenClass), Just (Result String SirenLinks))
        -- (Result String (Maybe SirenClass, Maybe SirenLinks))
      in
        case maybeClassResult of
          Just classResult ->
            case classResult of
              Ok sirenClass ->
                ValidSiren { class = Just sirenClass
                           , properties = maybePropertiesJson
                           , actions = Nothing
                           , links = Nothing }
              Err sirenClassErr ->
                InvalidSiren json
          Nothing ->
            ValidSiren { class = Nothing
                       , properties = Nothing
                       , actions = Nothing
                       , links = Nothing }
    _ -> InvalidSiren json

--        ValidSiren { class = Maybe.map parseClass maybeClass
--                   , properties = Maybe.map parseProperties maybeProperties
--                   , actions = Maybe.map parseActions maybeActions
--                   , links = Maybe.map parseLinks maybeLinks }

promoteError : RawError -> Error
promoteError rawError =
  case rawError of
    RawTimeout -> Timeout
    RawNetworkError -> NetworkError

handleRes : (ResponseDocument -> Task Error ResponseDocument) -> Response -> Task Error ResponseDocument
handleRes handle response =
  if 200 <= response.status && response.status < 300 then
      case response.value of
        Text str ->
            let x = Json.Decode.decodeString valDecoder str in
            case x of
              Ok jdoc ->
                let doc = case toSiren jdoc of
                            ValidSiren sdoc -> SirenDoc sdoc
                            InvalidSiren _ -> JsonDoc jdoc
                in
                  handle doc
              Err err -> handle (TextDoc str)
        _ ->
            fail (UnexpectedPayload "Response body is a blob, expecting a string.")
  else
    fail (BadResponse response.status response.statusText)

handleResponse : (String -> Task Error String) -> Response -> Task Error String
handleResponse handle response =
  if 200 <= response.status && response.status < 300 then
      case response.value of
        Text str ->
            handle str
        _ ->
            fail (UnexpectedPayload "Response body is a blob, expecting a string.")
  else
    fail (BadResponse response.status response.statusText)

getDocument : String -> Task Error ResponseDocument
getDocument url =
  let request =
        { verb = "GET"
        , headers = [ ("Accept", "application/vnd.siren+json") ]
        , url = url
        , body = empty
        }
  in
      mapError promoteError (send defaultSettings request)
      `andThen` handleRes succeed

fetchCommand : String -> Cmd Msg
fetchCommand loc =
  Task.perform FetchFail FetchSucceed (getDocument loc)

update : Msg -> Model -> (Model, Cmd Msg)
update msg oldModel =
  case msg of
  NewLocation loc -> ({ oldModel | location = loc }, Cmd.none)
  GoToLink loc -> ({ oldModel | location = loc }, fetchCommand loc)
  FetchSucceed doc -> ({ oldModel | debug = "success", body = doc }, Cmd.none)
  FetchFail e -> ({ oldModel | debug = "failure" }, Cmd.none)

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

renderJson : JsonVal -> Html Msg
renderJson json =
  case json of
    JsonStr s -> text s
    JsonBool b -> text (if b then "true" else "false")
    JsonInt n -> text "int"
    JsonFloat f -> text "float"
    JsonNull -> text "null"
    JsonList lst -> div [] (List.map renderJson lst)
    JsonDict dct ->
      let
        dctLst : List (String, JsonVal)
        dctLst = Dict.toList dct
      in
        div [] (List.map (\(key, value) -> div [] [ div [ style [ ("font-weight", "bold") ] ] [ text key ]
                                                  , renderJson value ]) dctLst)

renderProperties : Maybe SirenProperties -> Html Msg
renderProperties maybeProps =
  case maybeProps of
  Just properties ->
    div [] [ (Html.h3 [] [ text "properties" ]), renderJson properties ]
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

renderClasses : Maybe SirenClass -> Html Msg
renderClasses maybeClasses =
  case maybeClasses of
  Just classes ->
    let rendered = List.map text classes
    in
    div [] (Html.h3 [] [ (text "class") ] :: rendered)
  Nothing ->
    div [] []

renderSirenDocument : SirenDocument -> Html Msg
renderSirenDocument doc =
  let (imageLinks, anchorLinks) =
    case doc.links of
    Just links ->
      List.partition (\lnk -> List.member "image" lnk.rel) links
    Nothing ->
      ([], [])
  in
  div []
    [ renderClasses doc.class
    , renderProperties doc.properties
    , renderActions doc.actions
    , div [] (List.map renderAnchorLink anchorLinks)
    , div [] (List.map renderImageLink imageLinks) ]

renderDocument : ResponseDocument -> Html Msg
renderDocument doc =
  case doc of
    SirenDoc sirenDoc -> renderSirenDocument sirenDoc
    JsonDoc jsonDoc -> div [] [ text "json document", renderJson jsonDoc ]
    TextDoc textDoc -> div [] [ text textDoc ]

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
