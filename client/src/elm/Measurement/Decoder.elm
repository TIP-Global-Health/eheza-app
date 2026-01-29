module Measurement.Decoder exposing (decodeDropZoneFile)

import Gizra.Json exposing (decodeJsonInString)
import Json.Decode exposing (Decoder, field, string, succeed)
import Json.Decode.Pipeline exposing (requiredAt)
import Measurement.Model exposing (DropZoneFile)


decodeDropZoneFile : Decoder DropZoneFile
decodeDropZoneFile =
    succeed DropZoneFile
        |> requiredAt [ "detail", "file", "xhr", "responseText" ]
            (decodeJsonInString (field "url" string))
