module Activity.Decoder
    exposing
        ( decodeChildActivityDates
        )

import Activity.Model exposing (ChildActivityDates)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Utils.Json exposing (decodeDate)


decodeChildActivityDates : Decoder ChildActivityDates
decodeChildActivityDates =
    decode ChildActivityDates
        |> required "date_picture" (nullable decodeDate)
        |> required "date_height" (nullable decodeDate)
        |> required "date_muac" (nullable decodeDate)
        |> optional "date_progress_report" (nullable decodeDate) Nothing
        |> required "date_weight" (nullable decodeDate)
