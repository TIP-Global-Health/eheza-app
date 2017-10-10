module Backend.Child.Decoder
    exposing
        ( decodeChild
        )

import Backend.Child.Model exposing (..)
import Drupal.Restful exposing (decodeEntityId)
import Gizra.Json exposing (decodeInt)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Utils.Json exposing (decodeDate)


decodeChild : Decoder Child
decodeChild =
    decode Child
        |> required "label" string
        -- The default avatar comes from SASS , not from the Model.
        |> optionalAt [ "avatar", "styles", "patient-photo" ] string ""
        |> required "mother" (nullable decodeEntityId)
        |> required "sibling" (nullable decodeEntityId)
        |> hardcoded []
        |> required "date_birth" decodeDate
        |> required "gender" decodeGender


decodeGender : Decoder Gender
decodeGender =
    string
        |> andThen
            (\gender ->
                if gender == "female" then
                    succeed Female
                else if gender == "male" then
                    succeed Male
                else
                    fail (gender ++ " is not a recognized 'type' for Gender.")
            )
