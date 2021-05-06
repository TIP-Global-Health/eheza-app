module Backend.PrenatalEncounter.Decoder exposing (decodePrenatalEncounter)

import Backend.PrenatalEncounter.Model exposing (..)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, bool, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)


decodePrenatalEncounter : Decoder PrenatalEncounter
decodePrenatalEncounter =
    succeed PrenatalEncounter
        |> required "individual_participant" decodeEntityUuid
        |> requiredAt [ "scheduled_date", "value" ] decodeYYYYMMDD
        |> optionalAt [ "scheduled_date", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> optional "prenatal_encounter_type" decodePrenatalEncounterTypeWithDefault NurseEncounter
        |> optional "shard" (nullable decodeEntityUuid) Nothing


decodePrenatalEncounterTypeWithDefault : Decoder PrenatalEncounterType
decodePrenatalEncounterTypeWithDefault =
    oneOf
        [ decodePrenatalEncounterType
        , succeed NurseEncounter
        ]


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
                        succeed ChwThirdEncounter

                    "chw-postpartum" ->
                        succeed ChwPostpartumEncounter

                    _ ->
                        fail <|
                            encounterType
                                ++ " is not a recognized PrenatalEncounterType"
            )
