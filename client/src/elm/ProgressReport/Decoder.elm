module ProgressReport.Decoder
    exposing
        ( decodeProgressReport
        )

import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed, value)
import RemoteData exposing (RemoteData(Success), WebData)


{-| Decodes progress report of a Child.
@todo: add handling to the real structure
-}
decodeProgressReport : Decoder (WebData String)
decodeProgressReport =
    value
        |> andThen
            (\value ->
                succeed <| Success <| toString value
            )
