module Backend.PrenatalEncounter.Decoder exposing (decodePrenatalEncounter)

import Backend.PrenatalEncounter.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, bool, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)
import Utils.Json exposing (decodeWithFallback)


decodePrenatalEncounter : Decoder PrenatalEncounter
decodePrenatalEncounter =
    succeed PrenatalEncounter
        |> required "individual_participant" decodeEntityUuid
        |> requiredAt [ "scheduled_date", "value" ] decodeYYYYMMDD
        |> optionalAt [ "scheduled_date", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> required "prenatal_encounter_type" (decodeWithFallback NurseEncounter decodePrenatalEncounterType)
        |> optional "prenatal_diagnoses"
            (map
                (\items ->
                    if List.isEmpty items then
                        EverySet.singleton NoPrenatalDiagnosis

                    else
                        EverySet.fromList items
                )
             <|
                list (decodeWithFallback NoPrenatalDiagnosis decodePrenatalDiagnosis)
            )
            (EverySet.singleton NoPrenatalDiagnosis)
        |> optional "shard" (nullable decodeEntityUuid) Nothing


decodePrenatalEncounterType : Decoder PrenatalEncounterType
decodePrenatalEncounterType =
    string
        |> andThen
            (\encounterType ->
                case encounterType of
                    "nurse" ->
                        succeed NurseEncounter

                    "chw-1" ->
                        succeed ChwFirstEncounter

                    "chw-2" ->
                        succeed ChwSecondEncounter

                    "chw-3" ->
                        succeed ChwThirdPlusEncounter

                    "chw-postpartum" ->
                        succeed ChwPostpartumEncounter

                    _ ->
                        fail <|
                            encounterType
                                ++ " is not a recognized PrenatalEncounterType"
            )


decodePrenatalDiagnosis : Decoder PrenatalDiagnosis
decodePrenatalDiagnosis =
    string
        |> andThen
            (\diagnosis ->
                case diagnosis of
                    "prescribe-mebendezole" ->
                        succeed DiagnosisPrescribeMebendezole

                    "imminent-delivery" ->
                        succeed DiagnosisImminentDelivery

                    "none" ->
                        succeed NoPrenatalDiagnosis

                    _ ->
                        fail <|
                            diagnosis
                                ++ " is not a recognized PrenatalDiagnosis"
            )
