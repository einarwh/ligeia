module Json exposing (..)

import Dict exposing (Dict)

type JsonVal
  = JsonStr String
  | JsonBool Bool
  | JsonInt Int
  | JsonFloat Float
  | JsonNull
  | JsonList (List JsonVal)
  | JsonDict (Dict String JsonVal)
