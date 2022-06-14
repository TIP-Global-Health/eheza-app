module Backend.PrenatalEncounter.Decoder exposing (decodePrenatalDiagnosis, decodePrenatalEncounter)

import Backend.PrenatalEncounter.Model exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (decodeYYYYMMDD)
import Json.Decode exposing (Decoder, andThen, at, bool, dict, fail, field, int, list, map, map2, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (decodeEntityUuid)
import Utils.Json exposing (decodeWithFallback)


decodePrenatalEncounter : Decoder PrenatalEncounter
decodePrenatalEncounter =
    succeed PrenatalEncounter
        |> required "individual_participant" decodeEntityUuid
        |> requiredAt [ "scheduled_date", "value" ] decodeYYYYMMDD
        |> optionalAt [ "scheduled_date", "value2" ] (nullable decodeYYYYMMDD) Nothing
        |> required "prenatal_encounter_type" (decodeWithFallback NurseEncounter decodePrenatalEncounterType)
        |> optional "prenatal_diagnoses"
            (map
                (\items ->
                    if List.isEmpty items then
                        EverySet.singleton NoPrenatalDiagnosis

                    else
                        EverySet.fromList items
                )
             <|
                list (decodeWithFallback NoPrenatalDiagnosis decodePrenatalDiagnosis)
            )
            (EverySet.singleton NoPrenatalDiagnosis)
        |> optional "shard" (nullable decodeEntityUuid) Nothing


decodePrenatalEncounterType : Decoder PrenatalEncounterType
decodePrenatalEncounterType =
    string
        |> andThen
            (\encounterType ->
                case encounterType of
                    "nurse" ->
                        succeed NurseEncounter

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

                    "moderate-preeclampsia-immediate" ->
                        succeed DiagnosisModeratePreeclampsiaImmediate

                    "moderate-preeclampsia-recheck" ->
                        succeed DiagnosisModeratePreeclampsiaAfterRecheck

                    "severe-preeclampsia-immediate" ->
                        succeed DiagnosisSeverePreeclampsiaImmediate

                    "severe-preeclampsia-recheck" ->
                        succeed DiagnosisSeverePreeclampsiaAfterRecheck

                    "eclampsia" ->
                        succeed DiagnosisEclampsia

                    "hiv" ->
                        succeed DiagnosisHIV

                    "hiv-detectable-viral-load" ->
                        succeed DiagnosisHIVDetectableViralLoad

                    "partner-hiv" ->
                        succeed DiagnosisDiscordantPartnership

                    "syphilis" ->
                        succeed DiagnosisSyphilis

                    "syphilis-complications" ->
                        succeed DiagnosisSyphilisWithComplications

                    "neurosyphilis" ->
                        succeed DiagnosisNeurosyphilis

                    "hepatitis-b" ->
                        succeed DiagnosisHepatitisB

                    "malaria" ->
                        succeed DiagnosisMalaria

                    "malaria-continued" ->
                        succeed DiagnosisMalariaMedicatedContinued

                    "malaria-anemia" ->
                        succeed DiagnosisMalariaWithAnemia

                    "malaria-anemia-continued" ->
                        succeed DiagnosisMalariaWithAnemiaMedicatedContinued

                    "malaria-severe-anemia" ->
                        succeed DiagnosisMalariaWithSevereAnemia

                    "anemia" ->
                        succeed DiagnosisModerateAnemia

                    "severe-anemia" ->
                        succeed DiagnosisSevereAnemia

                    "severe-anemia-complications" ->
                        succeed DiagnosisSevereAnemiaWithComplications

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

                    "depression-possible" ->
                        succeed DiagnosisDepressionPossible

                    "depression-highly-possible" ->
                        succeed DiagnosisDepressionHighlyPossible

                    "depression-probable" ->
                        succeed DiagnosisDepressionProbable

                    "suicide-risk" ->
                        succeed DiagnosisSuicideRisk

                    "none" ->
                        succeed NoPrenatalDiagnosis

                    _ ->
                        fail <|
                            diagnosis
                                ++ " is not a recognized PrenatalDiagnosis"
            )
