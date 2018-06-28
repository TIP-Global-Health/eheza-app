module Backend.Child.Decoder
    exposing
        ( decodeChild
        )

import Backend.Child.Model exposing (..)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Restful.Endpoint exposing (decodeEntityId)


decodeChild : Decoder Child
decodeChild =
    decode Child
        |> required "label" string
        -- We're accommodating the JSON from the backend and the JSON
        -- we store in the cache.
        |> custom
            (oneOf
                [ map Just <| at [ "avatar", "styles", "patient-photo" ] string
                , map Just <| field "avatar" string
                , succeed Nothing
                ]
            )
        |> required "mother" (nullable decodeEntityId)
        |> required "sibling" (nullable decodeEntityId)
        |> required "date_birth" decodeYYYYMMDD
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
