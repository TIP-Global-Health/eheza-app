module Backend.WellChildEncounter.Decoder exposing (decodeWellChildEncounter)

import Backend.WellChildEncounter.Model exposing (..)
import EverySet
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
        |> optional "encounter_notes"
            (map
                (List.head >> Maybe.withDefault NoEncounterNotes)
             <|
                list decodeEncounterNote
            )
            NoEncounterNotes
        |> optional "encounter_warnings"
            (map
                (\warnings ->
                    if List.isEmpty warnings then
                        EverySet.singleton NoEncounterWarnings

                    else
                        EverySet.fromList warnings
                )
             <|
                list decodeEncounterWarning
            )
            (EverySet.singleton NoEncounterWarnings)
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


decodeEncounterWarning : Decoder EncounterWarning
decodeEncounterWarning =
    string
        |> andThen
            (\warning ->
                case warning of
                    "warning-ecd-milestone-behind" ->
                        succeed WarningECDMilestoneBehind

                    "warning-ecd-milestone-refer-to-specialist" ->
                        succeed WarningECDMilestoneReferToSpecialist

                    "no-ecd-milstone-warning" ->
                        succeed NoECDMilstoneWarning

                    "warning-head-circumference-microcephaly" ->
                        succeed WarningHeadCircumferenceMicrocephaly

                    "warning-head-circumference-macrocephaly" ->
                        succeed WarningHeadCircumferenceMacrocephaly

                    "no-head-circumference-warning" ->
                        succeed NoHeadCircumferenceWarning

                    "none" ->
                        succeed NoEncounterWarnings

                    _ ->
                        fail <|
                            warning
                                ++ " is not a recognized EncounterWarning"
            )
