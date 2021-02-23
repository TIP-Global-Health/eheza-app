module Measurement.Decoder exposing (decodeDropZoneFile, decodeDropZoneText)

import Gizra.Json exposing (decodeJsonInString)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Measurement.Model exposing (..)


decodeDropZoneFile : Decoder DropZoneFile
decodeDropZoneFile =
    succeed DropZoneFile
        |> requiredAt [ "detail", "file", "xhr", "responseText" ]
            (decodeJsonInString (field "url" string))


decodeDropZoneText : Decoder String
decodeDropZoneText =
    at [ "detail", "text" ] string
