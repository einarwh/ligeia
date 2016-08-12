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
           , class = Nothing
           , method = "POST"
           , href = "http://localhost:1337/hywit/void"
           , title = Nothing
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
type alias SirenLinkRels = List SirenLinkRel
type alias SirenHref = String

type alias SirenLink =
  { rel: SirenLinkRels,
    href: SirenHref }

type alias SirenProperties = JsonVal

type alias SirenField =
  { name: String,
    type': String,
    value: String }

type alias SirenFields = List SirenField

type alias SirenAction =
  { name: String
  , class: Maybe String
  , method: String
  , href: SirenHref
  , title: Maybe String
  , fields: SirenFields }

type alias SirenActions = List SirenAction

type alias SirenClass = String

type alias SirenClasses = List SirenClass

type alias SirenLinks = List SirenLink

type alias SirenDocument =
  { class: Maybe SirenClasses
  , properties: Maybe SirenProperties
  , actions: Maybe SirenActions
  , links: Maybe SirenLinks }

-- DECODERS

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

parseClasses : JsonVal -> Result JsonParseError SirenClasses
parseClasses json =
  case json of
    JsonList lst ->
      let
        resultList : List (Result JsonParseError String)
        resultList = List.map parseClassElement lst
        -- List (Result JsonParseError String) -> Result JsonParseError (List String)
        classes : Result JsonParseError SirenClasses
        classes = toListOfClasses resultList
      in
        classes
    _ -> Err ("parseClasses: Unexpected json instead of list of strings.", json)

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

parseLinkRelation : JsonVal -> Result JsonParseError SirenLinkRel
parseLinkRelation json =
  case json of
    JsonStr str -> Ok str
    _ -> Err ("parseLinkRelation: Unexpected json instead of string.", json)

combineLinkRelationResult : (Result JsonParseError SirenLinkRel) -> Result JsonParseError SirenLinkRels -> Result JsonParseError SirenLinkRels
combineLinkRelationResult res acc =
  case acc of
    Ok lst ->
      case res of
        Ok s -> Ok (List.append lst [s])
        Err e -> Err e
    Err _ -> acc

combineResults : (Result JsonParseError a) -> Result JsonParseError (List a) -> Result JsonParseError (List a)
combineResults res acc =
  case acc of
    Ok lst ->
      case res of
        Ok s -> Ok (List.append lst [s])
        Err e -> Err e
    Err _ -> acc

toListOfLinkRelations : List (Result JsonParseError SirenLinkRel) -> Result JsonParseError SirenLinkRels
toListOfLinkRelations resultList =
  List.foldl combineLinkRelationResult (Ok []) resultList

parseLinkRelations : JsonVal -> Result JsonParseError SirenLinkRels
parseLinkRelations json =
  case json of
    JsonList lst ->
      let
        resultList : List (Result JsonParseError SirenLinkRel)
        resultList = List.map parseLinkRelation lst
        relations : Result JsonParseError SirenLinkRels
        relations = toListOfLinkRelations resultList
      in
        relations
    _ -> Err ("parseLinkRelations: Unexpected json instead of list of strings.", json)

parseHref : JsonVal -> Result JsonParseError SirenHref
parseHref json =
  case json of
    JsonStr s -> Ok s
    _ -> Err ("parseHref: Unexpected json instead of href.", json)

parseLink : JsonVal -> Result JsonParseError SirenLink
parseLink json =
  case json of
    JsonDict dct ->
      let (maybeRels, maybeHref) = (Dict.get "rel" dct, Dict.get "href" dct)
      in
        case (maybeRels, maybeHref) of
          (Just rels, Just href) ->
            let
              linkRelsResult = parseLinkRelations rels
              linkHrefResult = parseHref href
            in
              case (linkRelsResult, linkHrefResult) of
                (Ok rs, Ok hf) -> Ok { rel = rs, href = hf }
                (Err e1, _) -> Err e1
                (_, Err e2) -> Err e2
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

toListOfActions : List (Result JsonParseError SirenAction) -> Result JsonParseError SirenActions
toListOfActions resultList =
  List.foldl combineResults (Ok []) resultList

--type alias SirenAction =-
--  { name: String
--  , class: Maybe String
--  , method: String
--  , href: SirenHref
--  , fields: List SirenField }

parseString : JsonVal -> Result JsonParseError String
parseString json =
  case json of
    JsonStr s -> Ok s
    _ -> Err ("parseString: Unexpected json instead of string.", json)

parseFields : JsonVal -> Result JsonParseError SirenFields
parseFields json =
  case json of
    JsonList lst ->
      Err ("Not implemented", json)
    _ -> Err ("parseFields: Unexpected json instead of list of Siren fields", json)

parseAction : JsonVal -> Result JsonParseError SirenAction
parseAction json =
  case json of
    JsonDict dct ->
      let
        maybeNameJson = Dict.get "name" dct
        maybeClassJson = Dict.get "class" dct
        maybeMethodJson = Dict.get "method" dct
        maybeHrefJson = Dict.get "href" dct
        maybeTitleJson = Dict.get "title" dct
        maybeTypeJson = Dict.get "type" dct
        maybeFieldsJson = Dict.get "fields" dct
        maybeNameResult = Maybe.map parseString maybeNameJson -- Maybe (Result JPE String)
        maybeClassResult = Maybe.map parseClasses maybeClassJson -- Maybe (Result JPE SirenClasses)
        maybeMethodResult = Maybe.map parseString maybeMethodJson -- Maybe (Result JPE String)
        maybeHrefResult = Maybe.map parseHref maybeHrefJson -- Maybe (Result JPE SirenHref)
        maybeTitleResult = Maybe.map parseString maybeTitleJson -- Maybe (Result JPE String)
        maybeFieldsResult = Maybe.map parseFields maybeFieldsJson -- Maybe (Result JPE SirenFields)
      in
        Err ("parseAction: Not implemented.", json)
    _ ->
      Err ("parseAction: Unexpected json instead of action.", json)

parseActions : JsonVal -> Result JsonParseError SirenActions
parseActions json =
  case json of
    JsonList lst ->
      let
        resultList : List (Result JsonParseError SirenAction)
        resultList = List.map parseAction lst
        links : Result JsonParseError SirenActions
        links = toListOfActions resultList
      in
        links
    _ -> Err ("parseLinks: Unexpected json instead of list of links.", json)

liftError : Maybe (Result JsonParseError a) -> Result JsonParseError (Maybe a)
liftError maybeResult =
  case maybeResult of
    Just (Ok v) -> Ok (Just v)
    Just (Err e) -> Err e
    Nothing -> Ok Nothing

combineSirenResults : Result JsonParseError (Maybe SirenClasses) -> Result JsonParseError (Maybe SirenLinks) -> Result JsonParseError (Maybe SirenClasses, Maybe SirenLinks)
combineSirenResults classResult linksResult =
  case classResult of
    Err classError -> Err classError
    Ok maybeClass ->
      case linksResult of
        Err linksError -> Err linksError
        Ok maybeLinks ->
          Ok (maybeClass, maybeLinks)

toSiren : JsonVal -> SirenParseResult
toSiren json =
  case json of
    JsonDict jd ->
      let
        maybeClassJson = Dict.get "class" jd -- Just JsonVal | Nothing
        maybePropertiesJson = Dict.get "properties" jd
        maybeActionsJson = Dict.get "actions" jd
        maybeLinksJson = Dict.get "links" jd
        maybeClassResult = Maybe.map parseClasses maybeClassJson -- Just (Result JPE SirenClasses) | Nothing
        maybeActionsResult = Maybe.map parseActions maybeActionsJson -- Just (Result JPE SirenActions) | Nothing
        maybeLinksResult = Maybe.map parseLinks maybeLinksJson -- Just (Result JPE SirenLinks) | Nothing
        classResult = liftError maybeClassResult -- Result JPE (Maybe SirenClasses)
        actionsResult = liftError maybeActionsResult -- Result JPE (Maybe SirenActions)
        linksResult = liftError maybeLinksResult -- Result JPE (Maybe SirenLinks)
        combinedResults : Result JsonParseError (Maybe SirenClasses, Maybe SirenLinks)
        combinedResults = combineSirenResults classResult linksResult
        -- (Just (Result String SirenClasses), Just (Result String SirenLinks))
        -- (Result String (Maybe SirenClasses, Maybe SirenLinks))
      in
        case combinedResults of
          Err e -> InvalidSiren json
          Ok (maybeClass, maybeLinks) ->
            ValidSiren { class = maybeClass
                       , properties = maybePropertiesJson
                       , actions = Nothing
                       , links = maybeLinks }
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
  a [ onClick (GoToLink link.href) ] ((text link.href) :: List.map text link.rel)

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

renderClasses : Maybe SirenClasses -> Html Msg
renderClasses maybeClasses =
  case maybeClasses of
  Just classes ->
    let rendered = List.map text classes
    in
    div [] (Html.h3 [] [ (text "class") ] :: rendered)
  Nothing ->
    div [] []

renderAnchorLinkItem anchorLink =
  Html.li [] [ (renderAnchorLink anchorLink) ]

renderAnchorLinks anchorLinks =
  Html.ul [] (List.map renderAnchorLinkItem anchorLinks)

renderSirenDocument : SirenDocument -> Html Msg
renderSirenDocument doc =
  let (imageLinks, anchorLinks) =
    case doc.links of
    Just links ->
      List.partition (\lnk -> List.member "image" lnk.rel || List.member "view" lnk.rel) links
    Nothing ->
      ([], [])
  in
  div []
    [ renderClasses doc.class
    , renderProperties doc.properties
    , renderActions doc.actions
    , div [] (Html.h3 [] [ text "links" ] :: [ (renderAnchorLinks anchorLinks) ])
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
