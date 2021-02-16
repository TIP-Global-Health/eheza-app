module Backend.AcuteIllnessEncounter.Decoder exposing (decodeAcuteIllnessEncounter)

import Backend.AcuteIllnessEncounter.Model exposing (..)
import Backend.AcuteIllnessEncounter.Utils exposing (acuteIllnessDiagnosisFromString)
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, bool, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeAcuteIllnessEncounter : Decoder AcuteIllnessEncounter
decodeAcuteIllnessEncounter =
    succeed AcuteIllnessEncounter
        |> required "individual_participant" decodeEntityUuid
        |> requiredAt [ "scheduled_date", "value" ] decodeYYYYMMDD
        |> optionalAt [ "scheduled_date", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> optional "sequence_number" decodeInt 1
        |> required "acute_illness_diagnosis" decodeAcuteIllnessDiagnosis
        |> optional "shard" (nullable decodeEntityUuid) Nothing


decodeAcuteIllnessDiagnosis : Decoder AcuteIllnessDiagnosis
decodeAcuteIllnessDiagnosis =
    string
        |> andThen
            (\s ->
                acuteIllnessDiagnosisFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized AcuteIllnessDiagnosis" |> fail)
            )
