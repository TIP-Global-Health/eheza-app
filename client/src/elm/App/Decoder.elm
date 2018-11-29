module App.Decoder exposing (decodeVersion)

import App.Model exposing (Version)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeVersion : Decoder Version
decodeVersion =
    decode Version
        |> required "build" string
