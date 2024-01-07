module Backend.NCDEncounter.Decoder exposing (decodeNCDDiagnosis, decodeNCDEncounter)

import Backend.NCDEncounter.Model exposing (..)
import Backend.NCDEncounter.Types exposing (..)
import EverySet
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, fail, list, map, nullable, string, succeed)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)
import Utils.Json exposing (decodeWithFallback)


decodeNCDEncounter : Decoder NCDEncounter
decodeNCDEncounter =
    let
        decodeDiagnoses =
            map
                (\items ->
                    if List.isEmpty items then
                        EverySet.singleton NoNCDDiagnosis

                    else
                        EverySet.fromList items
                )
            <|
                list (decodeWithFallback NoNCDDiagnosis decodeNCDDiagnosis)
    in
    succeed NCDEncounter
        |> required "individual_participant" decodeEntityUuid
        |> requiredAt [ "scheduled_date", "value" ] decodeYYYYMMDD
        |> optionalAt [ "scheduled_date", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> optional "ncd_diagnoses" decodeDiagnoses (EverySet.singleton NoNCDDiagnosis)
        |> optional "shard" (nullable decodeEntityUuid) Nothing


decodeNCDDiagnosis : Decoder NCDDiagnosis
decodeNCDDiagnosis =
    string
        |> andThen
            (\diagnosis ->
                case diagnosis of
                    "hypertension-stage1" ->
                        succeed DiagnosisHypertensionStage1

                    "hypertension-stage2" ->
                        succeed DiagnosisHypertensionStage2

                    "hypertension-stage3" ->
                        succeed DiagnosisHypertensionStage3

                    "diabetes-initial" ->
                        succeed DiagnosisDiabetesInitial

                    "diabetes-recurrent" ->
                        succeed DiagnosisDiabetesRecurrent

                    "renal-complications" ->
                        succeed DiagnosisRenalComplications

                    "none" ->
                        succeed NoNCDDiagnosis

                    _ ->
                        fail <|
                            diagnosis
                                ++ " is not a recognized NCDDiagnosis"
            )
