module Backend.Child.Decoder exposing (decodeChild, decodeModeOfDelivery)

import Backend.Child.Model exposing (..)
import Backend.Participant.Decoder exposing (decodeGender, decodeUbudehe)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, bool, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeChild : Decoder Child
decodeChild =
    decode Child
        |> required "label" string
        |> optional "first_name" string ""
        |> optional "middle_name" (nullable string) Nothing
        |> optional "second_name" string ""
        |> optional "national_id_number" (nullable string) Nothing
        |> optional "avatar" (nullable string) Nothing
        |> optional "mother" (nullable decodeEntityUuid) Nothing
        |> required "date_birth" decodeYYYYMMDD
        |> optional "birth_date_estimated" bool False
        |> required "gender" decodeGender
        |> optional "mode_of_delivery" (nullable decodeModeOfDelivery) Nothing
        |> optional "ubudehe" (nullable decodeUbudehe) Nothing
        |> optional "mother_name" (nullable string) Nothing
        |> optional "mother_national_id" (nullable string) Nothing
        |> optional "father_name" (nullable string) Nothing
        |> optional "father_national_id" (nullable string) Nothing
        |> optional "caregiver_name" (nullable string) Nothing
        |> optional "caregiver_national_id" (nullable string) Nothing
        |> optional "province" (nullable string) Nothing
        |> optional "district" (nullable string) Nothing
        |> optional "sector" (nullable string) Nothing
        |> optional "cell" (nullable string) Nothing
        |> optional "village" (nullable string) Nothing
        |> optional "phone_number" (nullable string) Nothing
        |> optional "health_center" (nullable decodeEntityUuid) Nothing


decodeModeOfDelivery : Decoder ModeOfDelivery
decodeModeOfDelivery =
    string
        |> andThen
            (\mode ->
                case mode of
                    "svd-episiotomy" ->
                        succeed <| VaginalDelivery (Spontaneous True)

                    "svd-no-episiotomy" ->
                        succeed <| VaginalDelivery (Spontaneous False)

                    "vd-vacuum" ->
                        succeed <| VaginalDelivery WithVacuumExtraction

                    "cesarean-delivery" ->
                        succeed <| CesareanDelivery

                    _ ->
                        fail (mode ++ " is not a recognized ModeOfDelivery")
            )
