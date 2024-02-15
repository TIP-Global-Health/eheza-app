module Backend.PrenatalEncounter.Encoder exposing (encodePrenatalDiagnosis, encodePrenatalEncounter, encodePrenatalEncounterType)

import Backend.PrenatalEncounter.Model exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import EverySet
import Gizra.NominalDate exposing (encodeYYYYMMDD)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (encodeEntityUuid)
import Utils.Json exposing (encodeEverySet, encodeIfSet)


{-| Encodes a `PrenatalEncounter`.
-}
encodePrenatalEncounter : PrenatalEncounter -> List ( String, Value )
encodePrenatalEncounter encounter =
    let
        diagnosesWithDefault diagnoses =
            if EverySet.isEmpty diagnoses then
                List.singleton NoPrenatalDiagnosis

            else
                EverySet.toList diagnoses

        prenatalIndicators =
            if not <| EverySet.isEmpty encounter.indicators then
                [ ( "prenatal_indicators", encodeEverySet encodePrenatalIndicator encounter.indicators ) ]

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
    , ( "prenatal_encounter_type", encodePrenatalEncounterType encounter.encounterType )
    , ( "prenatal_diagnoses", list encodePrenatalDiagnosis (diagnosesWithDefault encounter.diagnoses) )
    , ( "past_prenatal_diagnoses", list encodePrenatalDiagnosis (diagnosesWithDefault encounter.pastDiagnoses) )
    , ( "deleted", bool False )
    , ( "type", string "prenatal_encounter" )
    ]
        ++ prenatalIndicators
        ++ encodeIfSet "shard" encounter.shard encodeEntityUuid


encodePrenatalEncounterType : PrenatalEncounterType -> Value
encodePrenatalEncounterType encounterType =
    string <|
        case encounterType of
            NurseEncounter ->
                "nurse"

            NursePostpartumEncounter ->
                "nurse-postpartum"

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

            DiagnosisModeratePreeclampsiaInitialPhase ->
                "moderate-preeclampsia-initial"

            DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ->
                "moderate-preeclampsia-initial-ega-37+"

            DiagnosisModeratePreeclampsiaRecurrentPhase ->
                "moderate-preeclampsia-recurrent"

            DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus ->
                "moderate-preeclampsia-recurrent-ega-37+"

            DiagnosisSeverePreeclampsiaInitialPhase ->
                "severe-preeclampsia-initial"

            DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ->
                "severe-preeclampsia-initial-ega-37+"

            DiagnosisSeverePreeclampsiaRecurrentPhase ->
                "severe-preeclampsia-recurrent"

            DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus ->
                "severe-preeclampsia-recurrent-ega-37+"

            DiagnosisEclampsia ->
                "eclampsia"

            DiagnosisHIVInitialPhase ->
                "hiv"

            DiagnosisHIVRecurrentPhase ->
                "hiv-recurrent"

            DiagnosisHIVDetectableViralLoadInitialPhase ->
                "hiv-detectable-viral-load-initial"

            DiagnosisHIVDetectableViralLoadRecurrentPhase ->
                "hiv-detectable-viral-load"

            DiagnosisDiscordantPartnershipInitialPhase ->
                "partner-hiv"

            DiagnosisDiscordantPartnershipRecurrentPhase ->
                "partner-hiv-recurrent"

            DiagnosisSyphilisInitialPhase ->
                "syphilis-initial"

            DiagnosisSyphilisRecurrentPhase ->
                "syphilis"

            DiagnosisSyphilisWithComplicationsInitialPhase ->
                "syphilis-complications-initial"

            DiagnosisSyphilisWithComplicationsRecurrentPhase ->
                "syphilis-complications"

            DiagnosisNeurosyphilisInitialPhase ->
                "neurosyphilis-initial"

            DiagnosisNeurosyphilisRecurrentPhase ->
                "neurosyphilis"

            DiagnosisHepatitisBInitialPhase ->
                "hepatitis-b-initial"

            DiagnosisHepatitisBRecurrentPhase ->
                "hepatitis-b"

            DiagnosisMalariaInitialPhase ->
                "malaria"

            DiagnosisMalariaRecurrentPhase ->
                "malaria-recurrent"

            DiagnosisMalariaMedicatedContinuedInitialPhase ->
                "malaria-continued"

            DiagnosisMalariaMedicatedContinuedRecurrentPhase ->
                "malaria-continued-recurrent"

            DiagnosisMalariaWithAnemiaInitialPhase ->
                "malaria-anemia"

            DiagnosisMalariaWithAnemiaRecurrentPhase ->
                "malaria-anemia-recurrent"

            DiagnosisMalariaWithAnemiaMedicatedContinuedInitialPhase ->
                "malaria-anemia-continued"

            DiagnosisMalariaWithAnemiaMedicatedContinuedRecurrentPhase ->
                "malaria-anemia-continued-recurrent"

            DiagnosisMalariaWithSevereAnemiaInitialPhase ->
                "malaria-severe-anemia"

            DiagnosisMalariaWithSevereAnemiaRecurrentPhase ->
                "malaria-severe-anemia-recurrent"

            DiagnosisModerateAnemiaInitialPhase ->
                "anemia-initial"

            DiagnosisModerateAnemiaRecurrentPhase ->
                "anemia"

            DiagnosisSevereAnemiaInitialPhase ->
                "severe-anemia-initial"

            DiagnosisSevereAnemiaRecurrentPhase ->
                "severe-anemia"

            DiagnosisSevereAnemiaWithComplicationsInitialPhase ->
                "severe-anemia-complications-initial"

            DiagnosisSevereAnemiaWithComplicationsRecurrentPhase ->
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

            DiagnosisSevereVomiting ->
                "severe-vomiting"

            DiagnosisSevereVomitingBySymptoms ->
                "severe-vomiting-by-symptoms"

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

            DiagnosisPelvicPainContinued ->
                "pelvic-pain-continued"

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

            DiagnosisDiabetesInitialPhase ->
                "diabetes-initial"

            DiagnosisDiabetesRecurrentPhase ->
                "diabetes"

            DiagnosisGestationalDiabetesInitialPhase ->
                "gestational-diabetes-initial"

            DiagnosisGestationalDiabetesRecurrentPhase ->
                "gestational-diabetes"

            DiagnosisRhesusNegativeInitialPhase ->
                "rhesus-negative-initial"

            DiagnosisRhesusNegativeRecurrentPhase ->
                "rhesus-negative"

            DiagnosisDepressionNotLikely ->
                "depression-not-likely"

            DiagnosisDepressionPossible ->
                "depression-possible"

            DiagnosisDepressionHighlyPossible ->
                "depression-highly-possible"

            DiagnosisDepressionProbable ->
                "depression-probable"

            DiagnosisSuicideRisk ->
                "suicide-risk"

            DiagnosisOther ->
                "other"

            DiagnosisPostpartumAbdominalPain ->
                "postpartum-abdominal-pain"

            DiagnosisPostpartumUrinaryIncontinence ->
                "postpartum-urinary-incontinence"

            DiagnosisPostpartumHeadache ->
                "postpartum-headache"

            DiagnosisPostpartumFatigue ->
                "postpartum-fatigue"

            DiagnosisPostpartumFever ->
                "postpartum-fever"

            DiagnosisPostpartumPerinealPainOrDischarge ->
                "postpartum-perineal-pain-discharge"

            DiagnosisPostpartumInfection ->
                "postpartum-infection"

            DiagnosisPostpartumExcessiveBleeding ->
                "postpartum-excessive-bleeding"

            DiagnosisPostpartumEarlyMastitisOrEngorgment ->
                "postpartum-early-mastitis-engorgment"

            DiagnosisPostpartumMastitis ->
                "postpartum-mastitis"

            NoPrenatalDiagnosis ->
                "none"


encodePrenatalIndicator : PrenatalIndicator -> Value
encodePrenatalIndicator value =
    string <|
        case value of
            IndicatorHistoryLabsCompleted ->
                "past-labs-completed"

            NoPrenatalIndicators ->
                "none"
