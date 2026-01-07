module Backend.HealthyStartEncounter.Decoder exposing (decodeHealthyStartEncounter)

import Backend.HealthyStartEncounter.Model exposing (..)
import Backend.HealthyStartEncounter.Types exposing (HealthyStartDiagnosis(..), HealthyStartEncounterType(..))
import EverySet
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, fail, list, map, nullable, string, succeed)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)
import Utils.Json exposing (decodeEverySet, decodeWithFallback)


decodeHealthyStartEncounter : Decoder HealthyStartEncounter
decodeHealthyStartEncounter =
    let
        decodeDiagnoses =
            map
                (\items ->
                    if List.isEmpty items then
                        EverySet.singleton NoHealthyStartDiagnosis

                    else
                        EverySet.fromList items
                )
            <|
                list (decodeWithFallback NoHealthyStartDiagnosis decodeHealthyStartDiagnosis)
    in
    succeed HealthyStartEncounter
        |> required "individual_participant" decodeEntityUuid
        |> requiredAt [ "scheduled_date", "value" ] decodeYYYYMMDD
        |> optionalAt [ "scheduled_date", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> required "healthy_start_encounter_type" (decodeWithFallback NurseEncounter decodeHealthyStartEncounterType)
        |> optional "healthy_start_diagnoses" decodeDiagnoses (EverySet.singleton NoHealthyStartDiagnosis)
        |> optional "past_healthy_start_diagnoses" decodeDiagnoses (EverySet.singleton NoHealthyStartDiagnosis)
        |> optional "healthy_start_indicators" (decodeEverySet decodeHealthyStartIndicator) EverySet.empty
        |> optional "next_visit_date" (nullable decodeYYYYMMDD) Nothing
        |> optional "shard" (nullable decodeEntityUuid) Nothing


decodeHealthyStartEncounterType : Decoder HealthyStartEncounterType
decodeHealthyStartEncounterType =
    string
        |> andThen
            (\encounterType ->
                case encounterType of
                    "nurse" ->
                        succeed NurseEncounter

                    "chw" ->
                        succeed ChwEncounter

                    _ ->
                        fail <|
                            encounterType
                                ++ " is not a recognized HealthyStartEncounterType"
            )


decodeHealthyStartDiagnosis : Decoder HealthyStartDiagnosis
decodeHealthyStartDiagnosis =
    string
        |> andThen
            (\diagnosis ->
                case diagnosis of
                    "none" ->
                        succeed NoHealthyStartDiagnosis

                    _ ->
                        fail <|
                            diagnosis
                                ++ " is not a recognized HealthyStartDiagnosis"
            )


decodeHealthyStartIndicator : Decoder HealthyStartIndicator
decodeHealthyStartIndicator =
    string
        |> andThen
            (\value ->
                case value of
                    "none" ->
                        succeed NoHealthyStartIndicators

                    _ ->
                        fail <|
                            value
                                ++ " is not a recognized HealthyStartIndicator"
            )
