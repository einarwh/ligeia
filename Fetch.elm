module Fetch exposing (fetchCommand)

import Http exposing (send, defaultSettings, empty, Value(..), Response, RawError(..), Error(..))
import Json.Decode exposing (decodeString)
import Task exposing (Task, andThen, mapError, succeed, fail)

import Model exposing (..)
import JsonDecoder exposing (valDecoder)
import Parse exposing (..)

promoteError : RawError -> Error
promoteError rawError =
  case rawError of
    RawTimeout -> Timeout
    RawNetworkError -> NetworkError

handleRes : (ResponseDocument -> Task Error ResponseDocument) -> Response -> Task Error ResponseDocument
handleRes handle response =
  if 200 <= response.status && response.status < 300 then
      case response.value of
        Text str ->
            let x = decodeString valDecoder str in
            case x of
              Ok jdoc ->
                let doc = case toSiren jdoc of
                            ValidSiren sdoc -> SirenDoc sdoc
                            InvalidSiren _ -> JsonDoc jdoc
                in
                  handle doc
              Err err -> handle (TextDoc str)
        _ ->
            fail (UnexpectedPayload "Response body is a blob, expecting a string.")
  else
    fail (BadResponse response.status response.statusText)

getDocument : String -> Task Error ResponseDocument
getDocument url =
  let request =
        { verb = "GET"
        , headers = [ ("Accept", "application/vnd.siren+json") ]
        , url = url
        , body = empty
        }
  in
      mapError promoteError (send defaultSettings request)
      `andThen` handleRes succeed

fetchCommand : String -> Cmd Msg
fetchCommand loc =
  Task.perform FetchFail FetchSucceed (getDocument loc)
