module Child.Decoder
    exposing
        ( decodeChild
        , decodeWeight
        , decodeWeightList
        )

import Activity.Decoder exposing (decodeChildActivityDates)
import Child.Model exposing (..)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Utils.Json exposing (decodeInt)


decodeChild : Decoder Child
decodeChild =
    decode Child
        |> required "label" string
        |> optionalAt [ "avatar", "styles", "large" ] string "http://placehold.it/350x150"
        |> required "mother" (nullable decodeInt)
        |> required "lastExamination" (nullable decodeInt)
        |> custom decodeChildActivityDates


decodeWeight : Decoder Weight
decodeWeight =
    decode Weight
        |> required "weight"
            (string
                |> andThen
                    (\val ->
                        case String.toFloat val of
                            Ok value ->
                                succeed value

                            Err message ->
                                fail message
                    )
            )


decodeWeightList : Decoder (List Weight)
decodeWeightList =
    list decodeWeight
