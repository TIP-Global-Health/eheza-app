module Backend.PrenatalEncounter.Types exposing (..)


type PrenatalDiagnosis
    = DiagnosisChronicHypertensionImmediate
    | DiagnosisChronicHypertensionAfterRecheck
    | DiagnosisGestationalHypertensionImmediate
    | DiagnosisGestationalHypertensionAfterRecheck
    | DiagnosisModeratePreeclampsiaImmediate
    | DiagnosisModeratePreeclampsiaAfterRecheck
    | DiagnosisSeverePreeclampsiaImmediate
    | DiagnosisSeverePreeclampsiaAfterRecheck
    | DiagnosisEclampsia
    | DiagnosisHIV
    | DiagnosisHIVDetectableViralLoad
    | DiagnosisDiscordantPartnership
    | DiagnosisSyphilis
    | DiagnosisSyphilisWithComplications
    | DiagnosisNeurosyphilis
    | DiagnosisHepatitisB
    | DiagnosisMalaria
    | DiagnosisMalariaMedicatedContinued
    | DiagnosisMalariaWithAnemia
    | DiagnosisMalariaWithAnemiaMedicatedContinued
    | DiagnosisMalariaWithSevereAnemia
    | DiagnosisModerateAnemia
    | DiagnosisSevereAnemia
    | DiagnosisSevereAnemiaWithComplications
    | DiagnosisMiscarriage
    | DiagnosisMolarPregnancy
    | DiagnosisPlacentaPrevia
    | DiagnosisPlacentalAbruption
    | DiagnosisUterineRupture
    | DiagnosisObstructedLabor
    | DiagnosisPostAbortionSepsis
    | DiagnosisEctopicPregnancy
    | DiagnosisPROM
    | DiagnosisPPROM
    | DiagnosisHyperemesisGravidum
    | DiagnosisHyperemesisGravidumBySymptoms
    | DiagnosisMaternalComplications
    | DiagnosisInfection
    | DiagnosisImminentDelivery
    | DiagnosisLaborAndDelivery
    | DiagnosisHeartburn
    | DiagnosisHeartburnPersistent
    | DiagnosisDeepVeinThrombosis
    | DiagnosisPelvicPainIntense
    | DiagnosisPelvicPainContinued
    | DiagnosisUrinaryTractInfection
    | DiagnosisUrinaryTractInfectionContinued
    | DiagnosisPyelonephritis
    | DiagnosisCandidiasis
    | DiagnosisCandidiasisContinued
    | DiagnosisGonorrhea
    | DiagnosisGonorrheaContinued
    | DiagnosisTrichomonasOrBacterialVaginosis
    | DiagnosisTrichomonasOrBacterialVaginosisContinued
    | DiagnosisTuberculosis
    | DiagnosisDepressionPossible
    | DiagnosisDepressionHighlyPossible
    | DiagnosisDepressionProbable
    | DiagnosisSuicideRisk
    | NoPrenatalDiagnosis
