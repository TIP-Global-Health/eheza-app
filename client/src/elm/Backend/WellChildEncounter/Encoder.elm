module Backend.WellChildEncounter.Encoder exposing (encodeWellChildEncounter)

import Backend.WellChildEncounter.Model exposing (..)
import EverySet
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfExists)


{-| Encodes a `WellChildEncounter`.
-}
encodeWellChildEncounter : WellChildEncounter -> List ( String, Value )
encodeWellChildEncounter encounter =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD encounter.startDate )
            , ( "value2", maybe encodeYYYYMMDD encounter.endDate )
            ]
      )
    , ( "individual_participant", encodeEntityUuid encounter.participant )
    , ( "well_child_encounter_type", encodeWellChildEncounterType encounter.encounterType )
    , ( "encounter_notes", list encodeEncounterNote [ encounter.encounterNote ] )
    , ( "encounter_warnings"
      , list encodeEncounterWarning
            (if EverySet.isEmpty encounter.encounterWarnings then
                List.singleton NoEncounterWarnings

             else
                EverySet.toList encounter.encounterWarnings
            )
      )
    , ( "deleted", bool False )
    , ( "type", string "well_child_encounter" )
    ]
        ++ encodeIfExists "shard" encounter.shard encodeEntityUuid


encodeWellChildEncounterType : WellChildEncounterType -> Value
encodeWellChildEncounterType encounterType =
    string <|
        case encounterType of
            NewbornExam ->
                "newborn-exam"

            PediatricCareBirthTo6Weeks ->
                "pediatric-care-birth-to-6w"

            PediatricCare6Weeks ->
                "pediatric-care-6w"

            PediatricCare10Weeks ->
                "pediatric-care-10w"

            PediatricCare14Weeks ->
                "pediatric-care-14w"

            PediatricCare6Months ->
                "pediatric-care-6m"

            PediatricCare9Months ->
                "pediatric-care-9m"

            PediatricCare12Months ->
                "pediatric-care-12m"

            PediatricCare15Months ->
                "pediatric-care-15m"

            PediatricCare18Months ->
                "pediatric-care-18m"

            PediatricCareRecurrent ->
                "pediatric-care"


encodeEncounterNote : EncounterNote -> Value
encodeEncounterNote note =
    string <|
        case note of
            NoteTriggeredAcuteIllnessEncounter ->
                "triggered-ai-encounter"

            NoEncounterNotes ->
                "none"


encodeEncounterWarning : EncounterWarning -> Value
encodeEncounterWarning warning =
    string <|
        case warning of
            WarningECDMilestoneBehind ->
                "warning-ecd-milestone-behind"

            WarningECDMilestoneReferToSpecialist ->
                "warning-ecd-milestone-refer-to-specialist"

            NoECDMilstoneWarning ->
                "no-ecd-milstone-warning"

            WarningHeadCircumferenceMicrocephaly ->
                "warning-head-circumference-microcephaly"

            WarningHeadCircumferenceMacrocephaly ->
                "warning-head-circumference-macrocephaly"

            NoHeadCircumferenceWarning ->
                "no-head-circumference-warning"

            NoEncounterWarnings ->
                "none"
