module Backend.Clinic.Decoder exposing (decodeClinic, decodeClinicType)

import Backend.Clinic.Model exposing (Clinic, ClinicType(..))
import Json.Decode exposing (Decoder, andThen, fail, nullable, string, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeClinic : Decoder Clinic
decodeClinic =
    succeed Clinic
        |> required "label" string
        |> required "health_center" decodeEntityUuid
        |> required "group_type" decodeClinicType
        |> optional "village" (nullable decodeEntityUuid) Nothing


decodeClinicType : Decoder ClinicType
decodeClinicType =
    string
        |> andThen
            (\s ->
                case s of
                    "achi" ->
                        succeed Achi

                    "chw" ->
                        succeed Chw

                    "fbf" ->
                        succeed Fbf

                    "pmtct" ->
                        succeed Pmtct

                    "sorwathe" ->
                        succeed Sorwathe

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized ClinicType"
            )
