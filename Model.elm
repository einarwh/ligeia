module Model exposing (..)

import Http exposing (..)
import Json exposing (..)
import Siren exposing (..)

type alias Model =
  { location : String
  , debug : String
  , body : ResponseDocument }

type Msg
  = NewLocation String
  | GoToLink String
  | FetchSucceed ResponseDocument
  | FetchFail Http.Error

type ResponseDocument
  = TextDoc String
  | JsonDoc JsonVal
  | SirenDoc SirenDocument
