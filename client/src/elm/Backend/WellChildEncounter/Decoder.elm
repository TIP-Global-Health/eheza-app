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
        |> optional "well_child_encounter_type" decodeWellChildEncounterType PediatricCareRecurrent
        |> optional "encounter_notes" (map (List.head >> Maybe.withDefault NoEncounterNotes) <| list decodeEncounterNote) NoEncounterNotes
        |> optional "shard" (nullable decodeEntityUuid) Nothing


decodeWellChildEncounterType : Decoder WellChildEncounterType
decodeWellChildEncounterType =
    string
        |> andThen
            (\encounterType ->
                case encounterType of
                    "newborn-exam" ->
                        succeed NewbornExam

                    "pediatric-care-birth-to-6w" ->
                        succeed PediatricCareBirthTo6Weeks

                    "pediatric-care-6w" ->
                        succeed PediatricCare6Weeks

                    "pediatric-care-10w" ->
                        succeed PediatricCare10Weeks

                    "pediatric-care-14w" ->
                        succeed PediatricCare14Weeks

                    "pediatric-care-6m" ->
                        succeed PediatricCare6Months

                    "pediatric-care-9m" ->
                        succeed PediatricCare9Months

                    "pediatric-care-12m" ->
                        succeed PediatricCare12Months

                    "pediatric-care-15m" ->
                        succeed PediatricCare15Months

                    "pediatric-care-18m" ->
                        succeed PediatricCare18Months

                    "pediatric-care" ->
                        succeed PediatricCareRecurrent

                    _ ->
                        fail <|
                            encounterType
                                ++ " is not a recognized WellChildEncounterType"
            )


decodeEncounterNote : Decoder EncounterNote
decodeEncounterNote =
    string
        |> andThen
            (\note ->
                case note of
                    "triggered-ai-encounter" ->
                        succeed NoteTriggeredAcuteIllnessEncounter

                    "none" ->
                        succeed NoEncounterNotes

                    _ ->
                        fail <|
                            note
                                ++ " is not a recognized EncounterNote"
            )
