module Backend.HealthyStartEncounter.Decoder exposing (decodeHealthyStartEncounter)

import Backend.HealthyStartEncounter.Model exposing (..)
import Backend.HealthyStartEncounter.Types exposing (HealthyStartEncounterType(..))
import Backend.PrenatalEncounter.Decoder exposing (decodePrenatalDiagnosis)
import Backend.PrenatalEncounter.Model exposing (PrenatalIndicator(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
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
                        EverySet.singleton NoPrenatalDiagnosis

                    else
                        EverySet.fromList items
                )
            <|
                list (decodeWithFallback NoPrenatalDiagnosis decodePrenatalDiagnosis)
    in
    succeed HealthyStartEncounter
        |> required "individual_participant" decodeEntityUuid
        |> requiredAt [ "scheduled_date", "value" ] decodeYYYYMMDD
        |> optionalAt [ "scheduled_date", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> required "healthy_start_encounter_type" (decodeWithFallback NurseEncounter decodeHealthyStartEncounterType)
        |> optional "healthy_start_diagnoses" decodeDiagnoses (EverySet.singleton NoPrenatalDiagnosis)
        |> optional "past_healthy_start_diagnoses" decodeDiagnoses (EverySet.singleton NoPrenatalDiagnosis)
        |> optional "healthy_start_indicators" (decodeEverySet decodePrenatalIndicatorLocal) EverySet.empty
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


decodePrenatalIndicatorLocal : Decoder PrenatalIndicator
decodePrenatalIndicatorLocal =
    string
        |> andThen
            (\value ->
                case value of
                    "past-labs-completed" ->
                        succeed IndicatorHistoryLabsCompleted

                    "none" ->
                        succeed NoPrenatalIndicators

                    _ ->
                        fail <|
                            value
                                ++ " is not a recognized PrenatalIndicator"
            )
