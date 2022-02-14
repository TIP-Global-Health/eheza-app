module Backend.PrenatalEncounter.Encoder exposing (encodePrenatalEncounter)

import Backend.PrenatalEncounter.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeIfExists)


{-| Encodes a `PrenatalEncounter`.
-}
encodePrenatalEncounter : PrenatalEncounter -> List ( String, Value )
encodePrenatalEncounter encounter =
    [ ( "scheduled_date"
      , object
            [ ( "value", encodeYYYYMMDD encounter.startDate )
            , ( "value2", maybe encodeYYYYMMDD encounter.endDate )
            ]
      )
    , ( "individual_participant", encodeEntityUuid encounter.participant )
    , ( "prenatal_encounter_type", encodePrenatalEncounterType encounter.encounterType )
    , ( "prenatal_diagnosis"
      , list encodePrenatalDiagnosis
            (if EverySet.isEmpty encounter.diagnosis then
                List.singleton NoPrenatalDiagnosis

             else
                EverySet.toList encounter.diagnosis
            )
      )
    , ( "deleted", bool False )
    , ( "type", string "prenatal_encounter" )
    ]
        ++ encodeIfExists "shard" encounter.shard encodeEntityUuid


encodePrenatalEncounterType : PrenatalEncounterType -> Value
encodePrenatalEncounterType encounterType =
    string <|
        case encounterType of
            NurseEncounter ->
                "nurse"

            ChwFirstEncounter ->
                "chw-1"

            ChwSecondEncounter ->
                "chw-2"

            ChwThirdPlusEncounter ->
                "chw-3"

            ChwPostpartumEncounter ->
                "chw-postpartum"


encodePrenatalDiagnosis : PrenatalDiagnosis -> Value
encodePrenatalDiagnosis diagnosis =
    string <|
        case diagnosis of
            DiagnosisPrescribeMebendezole ->
                "prescribe-mebendezole"

            DiagnosisEclampsia ->
                "diagnosis-eclampsia"

            DiagnosisMiscarriageOrMolarPregnancy ->
                "diagnosis-miscarriage-molar-pregnancy"

            DiagnosisPlacentaPreviaOrPlacentalAbruptionOrUterineRupture ->
                "diagnosis-placenta-previa-placental-abruption-uterine-rupture"

            DiagnosisUterineRuptureOrObstructedLaborOrPlacentalAbruption ->
                "diagnosis-uterine-rupture-obstructed-labor-placental-abruption"

            DiagnosisEctopicPregnancy ->
                "diagnosis-ectopic-pregnancy"

            DiagnosisPROM ->
                "diagnosis-prom"

            DiagnosisPPROM ->
                "diagnosis-pprom"

            DiagnosisHyperemesisGravidum ->
                "diagnosis-hyperemesis-gravidum"

            DiagnosisImminentDelivery ->
                "imminent-delivery"

            DiagnosisLaborAndDelivery ->
                "labor"

            NoPrenatalDiagnosis ->
                "none"
