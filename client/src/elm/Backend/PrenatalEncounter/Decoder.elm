module Backend.PrenatalEncounter.Decoder exposing (decodePrenatalDiagnosis, decodePrenatalEncounter, decodePrenatalEncounterType)

import Backend.PrenatalEncounter.Model exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import EverySet
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, fail, list, map, nullable, string, succeed)
import Json.Decode.Pipeline exposing (optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)
import Utils.Json exposing (decodeEverySet, decodeWithFallback)


decodePrenatalEncounter : Decoder PrenatalEncounter
decodePrenatalEncounter =
    let
        decodeDiagnoses =
            map
                (\items ->
                    if List.isEmpty items then
                        EverySet.singleton NoPrenatalDiagnosis

                    else
                        EverySet.fromList items
                )
            <|
                list (decodeWithFallback NoPrenatalDiagnosis decodePrenatalDiagnosis)
    in
    succeed PrenatalEncounter
        |> required "individual_participant" decodeEntityUuid
        |> requiredAt [ "scheduled_date", "value" ] decodeYYYYMMDD
        |> optionalAt [ "scheduled_date", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> required "prenatal_encounter_type" (decodeWithFallback NurseEncounter decodePrenatalEncounterType)
        |> optional "prenatal_diagnoses" decodeDiagnoses (EverySet.singleton NoPrenatalDiagnosis)
        |> optional "past_prenatal_diagnoses" decodeDiagnoses (EverySet.singleton NoPrenatalDiagnosis)
        |> optional "prenatal_indicators" (decodeEverySet decodePrenatalIndicator) EverySet.empty
        |> optional "next_visit_date" (nullable decodeYYYYMMDD) Nothing
        |> optional "shard" (nullable decodeEntityUuid) Nothing


decodePrenatalEncounterType : Decoder PrenatalEncounterType
decodePrenatalEncounterType =
    string
        |> andThen
            (\encounterType ->
                case encounterType of
                    "nurse" ->
                        succeed NurseEncounter

                    "nurse-postpartum" ->
                        succeed NursePostpartumEncounter

                    "chw-1" ->
                        succeed ChwFirstEncounter

                    "chw-2" ->
                        succeed ChwSecondEncounter

                    "chw-3" ->
                        succeed ChwThirdPlusEncounter

                    "chw-postpartum" ->
                        succeed ChwPostpartumEncounter

                    _ ->
                        fail <|
                            encounterType
                                ++ " is not a recognized PrenatalEncounterType"
            )


decodePrenatalDiagnosis : Decoder PrenatalDiagnosis
decodePrenatalDiagnosis =
    string
        |> andThen
            (\diagnosis ->
                case diagnosis of
                    "chronic-hypertension-immediate" ->
                        succeed DiagnosisChronicHypertensionImmediate

                    "chronic-hypertension-recheck" ->
                        succeed DiagnosisChronicHypertensionAfterRecheck

                    "gestational-hypertension-immediate" ->
                        succeed DiagnosisGestationalHypertensionImmediate

                    "gestational-hypertension-recheck" ->
                        succeed DiagnosisGestationalHypertensionAfterRecheck

                    "moderate-preeclampsia-initial" ->
                        succeed DiagnosisModeratePreeclampsiaInitialPhase

                    "moderate-preeclampsia-initial-ega-37+" ->
                        succeed DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus

                    "moderate-preeclampsia-recurrent" ->
                        succeed DiagnosisModeratePreeclampsiaRecurrentPhase

                    "moderate-preeclampsia-recurrent-ega-37+" ->
                        succeed DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus

                    "severe-preeclampsia-initial" ->
                        succeed DiagnosisSeverePreeclampsiaInitialPhase

                    "severe-preeclampsia-initial-ega-37+" ->
                        succeed DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus

                    "severe-preeclampsia-recurrent" ->
                        succeed DiagnosisSeverePreeclampsiaRecurrentPhase

                    "severe-preeclampsia-recurrent-ega-37+" ->
                        succeed DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus

                    "eclampsia" ->
                        succeed DiagnosisEclampsia

                    "hiv" ->
                        succeed DiagnosisHIVInitialPhase

                    "hiv-recurrent" ->
                        succeed DiagnosisHIVRecurrentPhase

                    "hiv-detectable-viral-load-initial" ->
                        succeed DiagnosisHIVDetectableViralLoadInitialPhase

                    "hiv-detectable-viral-load" ->
                        succeed DiagnosisHIVDetectableViralLoadRecurrentPhase

                    "partner-hiv" ->
                        succeed DiagnosisDiscordantPartnershipInitialPhase

                    "partner-hiv-recurrent" ->
                        succeed DiagnosisDiscordantPartnershipRecurrentPhase

                    "syphilis-initial" ->
                        succeed DiagnosisSyphilisInitialPhase

                    "syphilis" ->
                        succeed DiagnosisSyphilisRecurrentPhase

                    "syphilis-complications-initial" ->
                        succeed DiagnosisSyphilisWithComplicationsInitialPhase

                    "syphilis-complications" ->
                        succeed DiagnosisSyphilisWithComplicationsRecurrentPhase

                    "neurosyphilis-initial" ->
                        succeed DiagnosisNeurosyphilisInitialPhase

                    "neurosyphilis" ->
                        succeed DiagnosisNeurosyphilisRecurrentPhase

                    "hepatitis-b-initial" ->
                        succeed DiagnosisHepatitisBInitialPhase

                    "hepatitis-b" ->
                        succeed DiagnosisHepatitisBRecurrentPhase

                    "malaria" ->
                        succeed DiagnosisMalariaInitialPhase

                    "malaria-recurrent" ->
                        succeed DiagnosisMalariaRecurrentPhase

                    "malaria-continued" ->
                        succeed DiagnosisMalariaMedicatedContinuedInitialPhase

                    "malaria-continued-recurrent" ->
                        succeed DiagnosisMalariaMedicatedContinuedRecurrentPhase

                    "malaria-anemia" ->
                        succeed DiagnosisMalariaWithAnemiaInitialPhase

                    "malaria-anemia-recurrent" ->
                        succeed DiagnosisMalariaWithAnemiaRecurrentPhase

                    "malaria-anemia-continued" ->
                        succeed DiagnosisMalariaWithAnemiaMedicatedContinuedInitialPhase

                    "malaria-anemia-continued-recurrent" ->
                        succeed DiagnosisMalariaWithAnemiaMedicatedContinuedRecurrentPhase

                    "malaria-severe-anemia" ->
                        succeed DiagnosisMalariaWithSevereAnemiaInitialPhase

                    "malaria-severe-anemia-recurrent" ->
                        succeed DiagnosisMalariaWithSevereAnemiaRecurrentPhase

                    "anemia-initial" ->
                        succeed DiagnosisModerateAnemiaInitialPhase

                    "anemia" ->
                        succeed DiagnosisModerateAnemiaRecurrentPhase

                    "severe-anemia-initial" ->
                        succeed DiagnosisSevereAnemiaInitialPhase

                    "severe-anemia" ->
                        succeed DiagnosisSevereAnemiaRecurrentPhase

                    "severe-anemia-complications-initial" ->
                        succeed DiagnosisSevereAnemiaWithComplicationsInitialPhase

                    "severe-anemia-complications" ->
                        succeed DiagnosisSevereAnemiaWithComplicationsRecurrentPhase

                    "miscarriage" ->
                        succeed DiagnosisMiscarriage

                    "molar-pregnancy" ->
                        succeed DiagnosisMolarPregnancy

                    "placenta-previa" ->
                        succeed DiagnosisPlacentaPrevia

                    "placental-abruption" ->
                        succeed DiagnosisPlacentalAbruption

                    "uterine-rupture" ->
                        succeed DiagnosisUterineRupture

                    "obstructed-labor" ->
                        succeed DiagnosisObstructedLabor

                    "post-abortion-sepsis" ->
                        succeed DiagnosisPostAbortionSepsis

                    "ectopic-pregnancy" ->
                        succeed DiagnosisEctopicPregnancy

                    "prom" ->
                        succeed DiagnosisPROM

                    "pprom" ->
                        succeed DiagnosisPPROM

                    "hyperemesis-gravidum" ->
                        succeed DiagnosisHyperemesisGravidum

                    "hyperemesis-gravidum-by-symptoms" ->
                        succeed DiagnosisHyperemesisGravidumBySymptoms

                    "severe-vomiting" ->
                        succeed DiagnosisSevereVomiting

                    "severe-vomiting-by-symptoms" ->
                        succeed DiagnosisSevereVomitingBySymptoms

                    "maternal-complications" ->
                        succeed DiagnosisMaternalComplications

                    "infection" ->
                        succeed DiagnosisInfection

                    "imminent-delivery" ->
                        succeed DiagnosisImminentDelivery

                    "labor" ->
                        succeed DiagnosisLaborAndDelivery

                    "heartburn" ->
                        succeed DiagnosisHeartburn

                    "heartburn-persistent" ->
                        succeed DiagnosisHeartburnPersistent

                    "dvt" ->
                        succeed DiagnosisDeepVeinThrombosis

                    "pelvic-pain-intense" ->
                        succeed DiagnosisPelvicPainIntense

                    "pelvic-pain-continued" ->
                        succeed DiagnosisPelvicPainContinued

                    "urinary-tract-infection" ->
                        succeed DiagnosisUrinaryTractInfection

                    "urinary-tract-infection-continued" ->
                        succeed DiagnosisUrinaryTractInfectionContinued

                    "pyelonephritis" ->
                        succeed DiagnosisPyelonephritis

                    "candidiasis" ->
                        succeed DiagnosisCandidiasis

                    "candidiasis-continued" ->
                        succeed DiagnosisCandidiasisContinued

                    "gonorrhea" ->
                        succeed DiagnosisGonorrhea

                    "trichomonas-or-bv" ->
                        succeed DiagnosisTrichomonasOrBacterialVaginosis

                    "trichomonas-or-bv-continued" ->
                        succeed DiagnosisTrichomonasOrBacterialVaginosisContinued

                    "tuberculosis" ->
                        succeed DiagnosisTuberculosis

                    "diabetes-initial" ->
                        succeed DiagnosisDiabetesInitialPhase

                    "diabetes" ->
                        succeed DiagnosisDiabetesRecurrentPhase

                    "gestational-diabetes-initial" ->
                        succeed DiagnosisGestationalDiabetesInitialPhase

                    "gestational-diabetes" ->
                        succeed DiagnosisGestationalDiabetesRecurrentPhase

                    "rhesus-negative-initial" ->
                        succeed DiagnosisRhesusNegativeInitialPhase

                    "rhesus-negative" ->
                        succeed DiagnosisRhesusNegativeRecurrentPhase

                    "depression-not-likely" ->
                        succeed DiagnosisDepressionNotLikely

                    "depression-possible" ->
                        succeed DiagnosisDepressionPossible

                    "depression-highly-possible" ->
                        succeed DiagnosisDepressionHighlyPossible

                    "depression-probable" ->
                        succeed DiagnosisDepressionProbable

                    "suicide-risk" ->
                        succeed DiagnosisSuicideRisk

                    "high-risk-of-preeclampsia-initial" ->
                        succeed DiagnosisHighRiskOfPreeclampsiaInitialPhase

                    "high-risk-of-preeclampsia-recurrent" ->
                        succeed DiagnosisHighRiskOfPreeclampsiaRecurrentPhase

                    "moderate-risk-of-preeclampsia" ->
                        succeed DiagnosisModerateRiskOfPreeclampsia

                    "other" ->
                        succeed DiagnosisOther

                    "postpartum-abdominal-pain" ->
                        succeed DiagnosisPostpartumAbdominalPain

                    "postpartum-urinary-incontinence" ->
                        succeed DiagnosisPostpartumUrinaryIncontinence

                    "postpartum-headache" ->
                        succeed DiagnosisPostpartumHeadache

                    "postpartum-fatigue" ->
                        succeed DiagnosisPostpartumFatigue

                    "postpartum-fever" ->
                        succeed DiagnosisPostpartumFever

                    "postpartum-perineal-pain-discharge" ->
                        succeed DiagnosisPostpartumPerinealPainOrDischarge

                    "postpartum-infection" ->
                        succeed DiagnosisPostpartumInfection

                    "postpartum-excessive-bleeding" ->
                        succeed DiagnosisPostpartumExcessiveBleeding

                    "postpartum-early-mastitis-engorgment" ->
                        succeed DiagnosisPostpartumEarlyMastitisOrEngorgment

                    "postpartum-mastitis" ->
                        succeed DiagnosisPostpartumMastitis

                    "none" ->
                        succeed NoPrenatalDiagnosis

                    _ ->
                        fail <|
                            diagnosis
                                ++ " is not a recognized PrenatalDiagnosis"
            )


decodePrenatalIndicator : Decoder PrenatalIndicator
decodePrenatalIndicator =
    string
        |> andThen
            (\value ->
                case value of
                    "past-labs-completed" ->
                        succeed IndicatorHistoryLabsCompleted

                    "adequate-gwg" ->
                        succeed IndicatorAdequateGWG

                    "inadequate-gwg" ->
                        succeed IndicatorInadequateGWG

                    "none" ->
                        succeed NoPrenatalIndicators

                    _ ->
                        fail <|
                            value
                                ++ " is not a recognized PrenatalIndicator"
            )
