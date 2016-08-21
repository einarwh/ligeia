module JsonDecoder exposing (valDecoder)

import Dict exposing (Dict)
import Json.Decode exposing (Decoder, int, string, object3, (:=), customDecoder, decodeValue)
import Json exposing (..)

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
