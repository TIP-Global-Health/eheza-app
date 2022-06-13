module Backend.PrenatalEncounter.Encoder exposing (encodePrenatalDiagnosis, encodePrenatalEncounter)

import Backend.PrenatalEncounter.Model exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
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
    , ( "prenatal_diagnoses"
      , list encodePrenatalDiagnosis
            (if EverySet.isEmpty encounter.diagnoses then
                List.singleton NoPrenatalDiagnosis

             else
                EverySet.toList encounter.diagnoses
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
            DiagnosisChronicHypertensionImmediate ->
                "chronic-hypertension-immediate"

            DiagnosisChronicHypertensionAfterRecheck ->
                "chronic-hypertension-recheck"

            DiagnosisGestationalHypertensionImmediate ->
                "gestational-hypertension-immediate"

            DiagnosisGestationalHypertensionAfterRecheck ->
                "gestational-hypertension-recheck"

            DiagnosisModeratePreeclampsiaImmediate ->
                "moderate-preeclampsia-immediate"

            DiagnosisModeratePreeclampsiaAfterRecheck ->
                "moderate-preeclampsia-recheck"

            DiagnosisSeverePreeclampsiaImmediate ->
                "severe-preeclampsia-immediate"

            DiagnosisSeverePreeclampsiaAfterRecheck ->
                "severe-preeclampsia-recheck"

            DiagnosisEclampsia ->
                "eclampsia"

            DiagnosisHIV ->
                "hiv"

            DiagnosisHIVDetectableViralLoad ->
                "hiv-detectable-viral-load"

            DiagnosisDiscordantPartnership ->
                "partner-hiv"

            DiagnosisSyphilis ->
                "syphilis"

            DiagnosisSyphilisWithComplications ->
                "syphilis-complications"

            DiagnosisNeurosyphilis ->
                "neurosyphilis"

            DiagnosisHepatitisB ->
                "hepatitis-b"

            DiagnosisMalaria ->
                "malaria"

            DiagnosisMalariaMedicatedContinued ->
                "malaria-continued"

            DiagnosisMalariaWithAnemia ->
                "malaria-anemia"

            DiagnosisMalariaWithAnemiaMedicatedContinued ->
                "malaria-anemia-continued"

            DiagnosisMalariaWithSevereAnemia ->
                "malaria-severe-anemia"

            DiagnosisModerateAnemia ->
                "anemia"

            DiagnosisSevereAnemia ->
                "severe-anemia"

            DiagnosisSevereAnemiaWithComplications ->
                "severe-anemia-complications"

            DiagnosisMiscarriage ->
                "miscarriage"

            DiagnosisMolarPregnancy ->
                "molar-pregnancy"

            DiagnosisPlacentaPrevia ->
                "placenta-previa"

            DiagnosisPlacentalAbruption ->
                "placental-abruption"

            DiagnosisUterineRupture ->
                "uterine-rupture"

            DiagnosisObstructedLabor ->
                "obstructed-labor"

            DiagnosisPostAbortionSepsis ->
                "post-abortion-sepsis"

            DiagnosisEctopicPregnancy ->
                "ectopic-pregnancy"

            DiagnosisPROM ->
                "prom"

            DiagnosisPPROM ->
                "pprom"

            DiagnosisHyperemesisGravidum ->
                "hyperemesis-gravidum"

            DiagnosisHyperemesisGravidumBySymptoms ->
                "hyperemesis-gravidum-by-symptoms"

            DiagnosisMaternalComplications ->
                "maternal-complications"

            DiagnosisInfection ->
                "infection"

            DiagnosisImminentDelivery ->
                "imminent-delivery"

            DiagnosisLaborAndDelivery ->
                "labor"

            DiagnosisHeartburn ->
                "heartburn"

            DiagnosisHeartburnPersistent ->
                "heartburn-persistent"

            DiagnosisDeepVeinThrombosis ->
                "dvt"

            DiagnosisPelvicPainIntense ->
                "pelvic-pain-intense"

            DiagnosisUrinaryTractInfection ->
                "urinary-tract-infection"

            DiagnosisUrinaryTractInfectionContinued ->
                "urinary-tract-infection-continued"

            DiagnosisPyelonephritis ->
                "pyelonephritis"

            DiagnosisCandidiasis ->
                "candidiasis"

            DiagnosisCandidiasisContinued ->
                "candidiasis-continued"

            DiagnosisGonorrhea ->
                "gonorrhea"

            DiagnosisGonorrheaContinued ->
                "gonorrhea-continued"

            DiagnosisTrichomonasOrBacterialVaginosis ->
                "trichomonas-or-bv"

            DiagnosisTrichomonasOrBacterialVaginosisContinued ->
                "trichomonas-or-bv-continued"

            DiagnosisTuberculosis ->
                "tuberculosis"

            DiagnosisDepressionPossible ->
                "depression-possible"

            DiagnosisDepressionHighlyPossible ->
                "depression-highly-possible"

            DiagnosisDepressionProbable ->
                "depression-probable"

            DiagnosisSuicideRisk ->
                "suicide-risk"

            NoPrenatalDiagnosis ->
                "none"
