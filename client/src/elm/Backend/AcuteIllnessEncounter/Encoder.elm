module Backend.AcuteIllnessEncounter.Encoder exposing (encodeAcuteIllnessDiagnosis, encodeAcuteIllnessEncounter, encodeAcuteIllnessEncounterType)

import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounter)
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis, AcuteIllnessEncounterType(..))
import Backend.AcuteIllnessEncounter.Utils exposing (acuteIllnessDiagnosisToString)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (Value, bool, int, object, string)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfSet)


{-| Encodes a `AcuteIllnessEncounter`.
-}
encodeAcuteIllnessEncounter : AcuteIllnessEncounter -> List ( String, Value )
encodeAcuteIllnessEncounter encounter =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD encounter.startDate )
            , ( "value2", maybe encodeYYYYMMDD encounter.endDate )
            ]
      )
    , ( "sequence_number", int encounter.sequenceNumber )
    , ( "individual_participant", encodeEntityUuid encounter.participant )
    , ( "ai_encounter_type", encodeAcuteIllnessEncounterType encounter.encounterType )
    , ( "acute_illness_diagnosis", encodeAcuteIllnessDiagnosis encounter.diagnosis )
    , ( "deleted", bool encounter.deleted )
    , ( "type", string "acute_illness_encounter" )
    ]
        ++ encodeIfSet "shard" encounter.shard encodeEntityUuid


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
