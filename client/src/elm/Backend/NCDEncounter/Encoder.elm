module Backend.NCDEncounter.Encoder exposing (encodeNCDDiagnosis, encodeNCDEncounter)

import Backend.NCDEncounter.Model exposing (..)
import Backend.NCDEncounter.Types exposing (..)
import EverySet
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfSet)


encodeNCDEncounter : NCDEncounter -> List ( String, Value )
encodeNCDEncounter encounter =
    let
        diagnosesWithDefault diagnoses =
            if EverySet.isEmpty diagnoses then
                List.singleton NoNCDDiagnosis

            else
                EverySet.toList diagnoses
    in
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD encounter.startDate )
            , ( "value2", maybe encodeYYYYMMDD encounter.endDate )
            ]
      )
    , ( "individual_participant", encodeEntityUuid encounter.participant )
    , ( "ncd_diagnoses", list encodeNCDDiagnosis (diagnosesWithDefault encounter.diagnoses) )
    , ( "deleted", bool False )
    , ( "type", string "ncd_encounter" )
    ]
        ++ encodeIfSet "shard" encounter.shard encodeEntityUuid


encodeNCDDiagnosis : NCDDiagnosis -> Value
encodeNCDDiagnosis diagnosis =
    string <|
        case diagnosis of
            DiagnosisHypertensionStage1 ->
                "hypertension-stage1"

            DiagnosisHypertensionStage2 ->
                "hypertension-stage2"

            DiagnosisHypertensionStage3 ->
                "hypertension-stage3"

            DiagnosisDiabetesInitial ->
                "diabetes-initial"

            DiagnosisDiabetesRecurrent ->
                "diabetes-recurrent"

            DiagnosisRenalComplications ->
                "renal-complications"

            NoNCDDiagnosis ->
                "none"
