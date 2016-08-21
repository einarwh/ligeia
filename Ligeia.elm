import Html.App exposing (program)

import Init exposing (init)
import Model exposing (Model, Msg(..))
import Update exposing (update)
import View exposing (view)

main =
  program { init = init
          , view = view
          , update = update
          , subscriptions = subscriptions }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
