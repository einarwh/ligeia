module View exposing (view)

import Html exposing (Html, Attribute, text, div, input, button, a, img)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)

import Model exposing (..)
import Render exposing (..)

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
