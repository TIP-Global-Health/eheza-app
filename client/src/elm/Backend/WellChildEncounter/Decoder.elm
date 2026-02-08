module Backend.WellChildEncounter.Decoder exposing (decodeEncounterWarning, decodeWellChildEncounter, decodeWellChildEncounterType)

import Backend.Measurement.Decoder exposing (decodeSkippedForm)
import Backend.WellChildEncounter.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, bool, fail, list, map, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)
import Utils.Json exposing (decodeEverySet, decodeWithFallback)


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
        |> optional "encounter_warnings" decodeEncounterWarnings (EverySet.singleton NoEncounterWarnings)
        |> optional "skipped_forms" (decodeEverySet decodeSkippedForm) EverySet.empty
        |> required "deleted" (decodeWithFallback False bool)
        |> optional "shard" (nullable decodeEntityUuid) Nothing


decodeWellChildEncounterType : Decoder WellChildEncounterType
decodeWellChildEncounterType =
    string
        |> andThen
            (\encounterType ->
                case encounterType of
                    "pediatric-care" ->
                        succeed PediatricCare

                    "pediatric-care-chw" ->
                        succeed PediatricCareChw

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


decodeEncounterWarnings : Decoder (EverySet EncounterWarning)
decodeEncounterWarnings =
    oneOf
        [ map
            (\warnings ->
                if List.isEmpty warnings then
                    EverySet.singleton NoEncounterWarnings

                else
                    EverySet.fromList warnings
            )
          <|
            list decodeEncounterWarning
        , succeed (EverySet.singleton NoEncounterWarnings)
        ]


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
