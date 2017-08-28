module Child.Decoder
    exposing
        ( decodeChild
        )

import Activity.Decoder exposing (decodeChildActivityDates)
import Child.Model exposing (..)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import RemoteData exposing (RemoteData(NotAsked))
import Utils.Json exposing (decodeInt)


decodeChild : Decoder Child
decodeChild =
    decode Child
        |> required "label" string
        |> optionalAt [ "avatar", "styles", "patient-photo" ] string "https://placehold.it/200x200"
        |> required "mother" (nullable decodeInt)
        |> hardcoded NotAsked
        |> hardcoded Nothing
        |> custom decodeChildActivityDates
        |> required "gender" decodeGender


decodeGender : Decoder Gender
decodeGender =
    string
        |> andThen
            (\gender ->
                if gender == "female" then
                    succeed Female
                else
                    succeed Male
            )
