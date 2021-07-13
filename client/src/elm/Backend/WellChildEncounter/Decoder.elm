module Backend.WellChildEncounter.Decoder exposing (decodeWellChildEncounter)

import Backend.WellChildEncounter.Model exposing (..)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, bool, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)


decodeWellChildEncounter : Decoder WellChildEncounter
decodeWellChildEncounter =
    succeed WellChildEncounter
        |> required "individual_participant" decodeEntityUuid
        |> requiredAt [ "scheduled_date", "value" ] decodeYYYYMMDD
        |> optionalAt [ "scheduled_date", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> optional "well_child_encounter_type" decodeWellChildEncounterType PediatricCare
        |> optional "shard" (nullable decodeEntityUuid) Nothing


decodeWellChildEncounterType : Decoder WellChildEncounterType
decodeWellChildEncounterType =
    string
        |> andThen
            (\encounterType ->
                case encounterType of
                    "pediatric-care" ->
                        succeed PediatricCare

                    "newborn-exam" ->
                        succeed NewbornExam

                    _ ->
                        fail <|
                            encounterType
                                ++ " is not a recognized WellChildEncounterType"
            )
