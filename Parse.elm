module Parse exposing (..)

import Dict exposing (Dict)
import Json exposing (..)
import Siren exposing (..)

type alias JsonParseError = (String, JsonVal)

type SirenParseResult
  = InvalidSiren JsonVal
  | ValidSiren SirenDocument

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

parseDictValue : Dict String JsonVal -> String -> (JsonVal -> Result JsonParseError a) -> Result JsonParseError (Maybe a)
parseDictValue dct key parser = liftError (Maybe.map parser (Dict.get key dct))

parseAction : JsonVal -> Result JsonParseError SirenAction
parseAction json =
  case json of
    JsonDict dct ->
      let
        nameResult = parseDictValue dct "name" parseString
        classResult = parseDictValue dct "class" parseClasses
        methodResult = parseDictValue dct "method" parseString
        hrefResult = parseDictValue dct "href" parseHref
        titleResult = parseDictValue dct "title" parseString
        fieldsResult = parseDictValue dct "fields" parseFields
      in
        case (nameResult, classResult, methodResult, hrefResult, titleResult, fieldsResult) of
          (Ok maybeName, Ok maybeClass, Ok maybeMethod, Ok maybeHref, Ok maybeTitle, Ok maybeFields) ->
            case (maybeName, maybeClass, maybeMethod, maybeHref, maybeTitle, maybeFields) of
              (Just nameValue, _, _, Just hrefValue, _, _) ->
                Ok { name = nameValue
                   , class = maybeClass
                   , method = maybeMethod
                   , href = hrefValue
                   , title = maybeTitle
                   , fields = maybeFields }
              _ ->
                Err ("Missing mandatory part of action.", json)
          (Err e1, _, _, _, _, _) -> Err e1
          (_, Err e2, _, _, _, _) -> Err e2
          (_, _, Err e3, _, _, _) -> Err e3
          (_, _, _, Err e4, _, _) -> Err e4
          (_, _, _, _, Err e5, _) -> Err e5
          (_, _, _, _, _, Err e6) -> Err e6
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
