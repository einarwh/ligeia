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
  | FetchSucceed FetchResponse
  | FetchFail Http.Error

type alias FetchResponse = (String, ResponseDocument)

type ResponseDocument
  = TextDoc String
  | JsonDoc JsonVal
  | SirenDoc SirenDocument
