module Update exposing (update)

import Model exposing (Model, Msg(..))
import Fetch exposing (fetchCommand)

update : Msg -> Model -> (Model, Cmd Msg)
update msg oldModel =
  case msg of
  NewLocation loc -> ({ oldModel | location = loc }, Cmd.none)
  GoToLink loc -> ({ oldModel | location = loc }, fetchCommand loc)
  FetchSucceed doc -> ({ oldModel | debug = "success", body = doc }, Cmd.none)
  FetchFail e -> ({ oldModel | debug = "failure" }, Cmd.none)
