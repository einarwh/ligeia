module Render exposing (..)

import Dict exposing (Dict)
import Html exposing (Html, Attribute, text, div, input, button, a, img)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Json exposing (..)
import Model exposing (..)
import Siren exposing (..)

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

renderAction : SirenAction -> Html Msg
renderAction action =
  let renderedFields =
    case action.fields of
      Just fields -> List.map renderField fields
      Nothing -> []
  in
    div [] [ div [] renderedFields
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
