module Backend.NutritionEncounter.Decoder exposing (decodeNutritionEncounter, decodeNutritionEncounterType)

import Backend.NutritionEncounter.Model exposing (..)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, fail, nullable, string, succeed)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)
import Utils.Json exposing (decodeWithFallback)


decodeNutritionEncounter : Decoder NutritionEncounter
decodeNutritionEncounter =
    succeed NutritionEncounter
        |> required "individual_participant" decodeEntityUuid
        |> required "start_date" decodeYYYYMMDD
        |> optional "end_date" (nullable decodeYYYYMMDD) Nothing
        |> optional "nutrition_encounter_type" (decodeWithFallback NutritionEncounterUnknown decodeNutritionEncounterType) NutritionEncounterUnknown
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
