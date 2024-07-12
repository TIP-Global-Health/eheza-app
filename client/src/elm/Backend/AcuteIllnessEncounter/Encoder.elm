module Backend.AcuteIllnessEncounter.Encoder exposing (encodeAcuteIllnessDiagnosis, encodeAcuteIllnessEncounter, encodeAcuteIllnessEncounterType)

import Backend.AcuteIllnessEncounter.Model exposing (..)
import Backend.AcuteIllnessEncounter.Types exposing (..)
import Backend.AcuteIllnessEncounter.Utils exposing (acuteIllnessDiagnosisToString)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfSet)


{-| Encodes a `AcuteIllnessEncounter`.
-}
encodeAcuteIllnessEncounter : AcuteIllnessEncounter -> List ( String, Value )
encodeAcuteIllnessEncounter session =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD session.startDate )
            , ( "value2", maybe encodeYYYYMMDD session.endDate )
            ]
      )
    , ( "sequence_number", int session.sequenceNumber )
    , ( "individual_participant", encodeEntityUuid session.participant )
    , ( "ai_encounter_type", encodeAcuteIllnessEncounterType session.encounterType )
    , ( "acute_illness_diagnosis", encodeAcuteIllnessDiagnosis session.diagnosis )
    , ( "deleted", bool False )
    , ( "type", string "acute_illness_encounter" )
    ]
        ++ encodeIfSet "shard" session.shard encodeEntityUuid


encodeAcuteIllnessEncounterType : AcuteIllnessEncounterType -> Value
encodeAcuteIllnessEncounterType encounterType =
    string <|
        case encounterType of
            AcuteIllnessEncounterNurse ->
                "nurse-encounter"

            AcuteIllnessEncounterNurseSubsequent ->
                "nurse-encounter-subsequent"

            AcuteIllnessEncounterCHW ->
                "chw-encounter"


encodeAcuteIllnessDiagnosis : AcuteIllnessDiagnosis -> Value
encodeAcuteIllnessDiagnosis diagnosis =
    acuteIllnessDiagnosisToString diagnosis |> string
