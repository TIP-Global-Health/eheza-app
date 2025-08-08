module Backend.NutritionEncounter.Decoder exposing (decodeNutritionEncounter, decodeNutritionEncounterType)

import Backend.NutritionEncounter.Model exposing (..)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, bool, fail, nullable, string, succeed)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)
import Utils.Json exposing (decodeWithFallback)


decodeNutritionEncounter : Decoder NutritionEncounter
decodeNutritionEncounter =
    succeed NutritionEncounter
        |> required "individual_participant" decodeEntityUuid
        |> requiredAt [ "scheduled_date", "value" ] decodeYYYYMMDD
        |> optionalAt [ "scheduled_date", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> optional "nutrition_encounter_type"
            (decodeWithFallback NutritionEncounterUnknown decodeNutritionEncounterType)
            NutritionEncounterUnknown
        |> required "deleted" (decodeWithFallback False bool)
        |> optional "shard" (nullable decodeEntityUuid) Nothing


decodeNutritionEncounterType : Decoder NutritionEncounterType
decodeNutritionEncounterType =
    string
        |> andThen
            (\encounterType ->
                case encounterType of
                    "nurse" ->
                        succeed NutritionEncounterNurse

                    "chw" ->
                        succeed NutritionEncounterCHW

                    "unknown" ->
                        succeed NutritionEncounterUnknown

                    _ ->
                        fail <|
                            encounterType
                                ++ " is not a recognized NutritionEncounterType"
            )
