module GetWithHeaders(get, Headers, postJson) where

{-| This library takes from elm-http the minimum needed to
    add HTTP headers to the request and return them from the response.

# useful methods
@docs get

# super useful methods that don't really belong in this file
@docs postJson

# handy type alias
@docs Headers

 -}

import Http exposing (send, empty, defaultSettings
    , Response, Value(..)
    , Error(..), RawError(..))
import Task exposing (Task, andThen, mapError, succeed, fail)
import Json.Decode as Json
import Json.Encode
import Dict exposing (Dict)

{-| Headers are represented as a list of key-value tuples.
 -}
type alias Headers = List (String, String)

{-| Given headers and a body, return an HTTP GET task
    that produces headers and a decoded body.
 -}
get: Json.Decoder value -> Headers -> String -> Task Error (Headers, value)
get decoder headers url =
  let
    request =
      {
          verb = "GET",
          headers = headers,
          url =  url,
          body = Http.empty
      }
    in
      fromJsonWithHeaders decoder (send defaultSettings request)

{-| Post Json data with Content-Type set to application/json
    You may also send and receive other headers.

    Use the Json.Encode package to convert your body into Json.Value
 -}
postJson: Json.Decoder value -> Headers -> String -> Json.Value -> Task Error (Headers, value)
postJson decoder headers url jsonBody =
  let
    request =
      {
          verb = "POST",
          headers = headers ++ [("Content-Type", "application/json")],
          url =  url,
          body = Http.string (Json.Encode.encode 0 jsonBody)
      }
    in
      fromJsonWithHeaders decoder (send defaultSettings request)


fromJsonWithHeaders : Json.Decoder a -> Task RawError Response -> Task Error (Headers, a)
fromJsonWithHeaders decoder response =
  let decode str =
        case Json.decodeString decoder str of
          Ok v -> succeed v
          Err msg -> fail (UnexpectedPayload msg)
  in
      mapError promoteError response
        `andThen` handleResponseWithHeaders decode


promoteError : RawError -> Error
promoteError rawError =
  case rawError of
    RawTimeout -> Timeout
    RawNetworkError -> NetworkError


handleResponseWithHeaders : (String -> Task Error a) -> Response -> Task Error (Headers, a)
handleResponseWithHeaders handle response =
  case 200 <= response.status && response.status < 300 of
    False ->
        fail (BadResponse response.status response.statusText)
    True ->
        case response.value of
          Text str ->
            str
              |> handle
              |> Task.map (\v -> ((Dict.toList response.headers), v))
          _ -> fail (UnexpectedPayload "Response body is a blob, expecting a string.")
