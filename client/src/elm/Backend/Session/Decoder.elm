module Backend.Session.Decoder exposing (decodeSession)

import Backend.Clinic.Decoder exposing (decodeClinicType)
import Backend.Session.Model exposing (..)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, nullable, succeed)
import Json.Decode.Pipeline exposing (optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)


{-| Decodes the JSON sent by /api/sessions
-}
decodeSession : Decoder Session
decodeSession =
    succeed Session
        |> requiredAt [ "scheduled_date", "value" ] decodeYYYYMMDD
        |> optionalAt [ "scheduled_date", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> required "clinic" decodeEntityUuid
        |> required "clinic_type" decodeClinicType
