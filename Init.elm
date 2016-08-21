module Init exposing (init)

import Dict exposing (Dict)

import Model exposing (Model, Msg(..), ResponseDocument(..))
import Json exposing (..)
import Siren exposing (SirenDocument)

sirenDoc : SirenDocument
sirenDoc =
  { class = Nothing
  , properties =
    Just (JsonDict (Dict.fromList [("title", JsonStr "my-title"), ("description", JsonStr "my-description")]))
  , actions =
    Just [ { name = "start-game"
           , class = Nothing
           , method = Just "POST"
           , href = "http://localhost:1337/hywit/void"
           , title = Nothing
           , fields = Just [ { name = "foo"
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
