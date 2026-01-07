module Backend.HealthyStartEncounter.Encoder exposing (encodeHealthyStartEncounter)

import Backend.HealthyStartEncounter.Model exposing (..)
import Backend.HealthyStartEncounter.Types exposing (HealthyStartDiagnosis(..), HealthyStartEncounterType(..))
import EverySet
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeEverySet, encodeIfSet)


{-| Encodes a `HealthyStartEncounter`.
-}
encodeHealthyStartEncounter : HealthyStartEncounter -> List ( String, Value )
encodeHealthyStartEncounter encounter =
    let
        diagnosesWithDefault diagnoses =
            if EverySet.isEmpty diagnoses then
                List.singleton NoHealthyStartDiagnosis

            else
                EverySet.toList diagnoses

        healthyStartIndicators =
            if not <| EverySet.isEmpty encounter.indicators then
                [ ( "healthy_start_indicators", encodeEverySet encodeHealthyStartIndicator encounter.indicators ) ]

            else
                []
    in
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD encounter.startDate )
            , ( "value2", maybe encodeYYYYMMDD encounter.endDate )
            ]
      )
    , ( "individual_participant", encodeEntityUuid encounter.participant )
    , ( "healthy_start_encounter_type", encodeHealthyStartEncounterType encounter.encounterType )
    , ( "healthy_start_diagnoses", list encodeHealthyStartDiagnosis (diagnosesWithDefault encounter.diagnoses) )
    , ( "past_healthy_start_diagnoses", list encodeHealthyStartDiagnosis (diagnosesWithDefault encounter.pastDiagnoses) )
    , ( "next_visit_date", maybe encodeYYYYMMDD encounter.nextVisitDate )
    , ( "deleted", bool False )
    , ( "type", string "healthy_start_encounter" )
    ]
        ++ healthyStartIndicators
        ++ encodeIfSet "shard" encounter.shard encodeEntityUuid


encodeHealthyStartEncounterType : HealthyStartEncounterType -> Value
encodeHealthyStartEncounterType encounterType =
    string <|
        case encounterType of
            NurseEncounter ->
                "nurse"

            ChwEncounter ->
                "chw"


encodeHealthyStartDiagnosis : HealthyStartDiagnosis -> Value
encodeHealthyStartDiagnosis diagnosis =
    string <|
        case diagnosis of
            NoHealthyStartDiagnosis ->
                "none"


encodeHealthyStartIndicator : HealthyStartIndicator -> Value
encodeHealthyStartIndicator value =
    string <|
        case value of
            NoHealthyStartIndicators ->
                "none"
