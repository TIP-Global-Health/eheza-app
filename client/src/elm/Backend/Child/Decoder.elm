module Backend.Child.Decoder exposing (decodeChild)

import Backend.Child.Model exposing (..)
import Backend.Patient.Decoder exposing (decodeGender)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeChild : Decoder Child
decodeChild =
    decode Child
        |> required "label" string
        -- There 3 are first, middle and second names.
        -- We do not pull actual values from server yet.
        |> hardcoded ""
        |> hardcoded Nothing
        |> hardcoded ""
        -- National ID Number
        |> hardcoded Nothing
        -- We're accommodating the JSON from the backend and the JSON
        -- we store in the cache.
        |> custom
            (oneOf
                [ map Just <| at [ "avatar", "styles", "patient-photo" ] string
                , map Just <| field "avatar" string
                , succeed Nothing
                ]
            )
        |> required "mother" (nullable decodeEntityUuid)
        |> required "date_birth" decodeYYYYMMDD
        -- Is birth date estimated
        |> hardcoded False
        |> required "gender" decodeGender
        -- Mode of delivery
        |> hardcoded Nothing
        -- Ubudehe
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing
        |> hardcoded Nothing



-- |> hardcoded Nothing
