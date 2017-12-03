module Measurement.Decoder exposing (..)

import Gizra.Json exposing (decodeJsonInString)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Measurement.Model exposing (..)


decodeDropZoneFile : Decoder DropZoneFile
decodeDropZoneFile =
    decode DropZoneFile
        |> requiredAt [ "detail", "file", "xhr", "responseText" ]
            (decodeJsonInString (field "url" string))
