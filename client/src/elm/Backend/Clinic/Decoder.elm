module Backend.Clinic.Decoder exposing (decodeClinic)

import Backend.Clinic.Model exposing (..)
import Json.Decode exposing (Decoder, andThen, dict, fail, field, int, list, map, map2, nullable, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeClinic : Decoder Clinic
decodeClinic =
    decode Clinic
        |> required "label" string
        |> required "health_center" decodeEntityUuid
        |> required "group_type" decodeClinicType


decodeClinicType : Decoder ClinicType
decodeClinicType =
    string
        |> andThen
            (\s ->
                case s of
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
