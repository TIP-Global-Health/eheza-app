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
    | DiagnosisDiscordantPartnership
    | DiagnosisSyphilis
    | DiagnosisSyphilisWithComplications
    | DiagnosisNeurosyphilis
    | DiagnosisHepatitisB
    | DiagnosisMalaria
    | DiagnosisMalariaWithAnemia
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
      --  Diagnosed by dangher signs and is an emergency referral diagnosis.
    | DiagnosisHyperemesisGravidum
      --  Diagnosed by symptoms and is not an emergency referral diagnosis.
    | DiagnosisHyperemesisGravidumBySymptoms
    | DiagnosisMaternalComplications
    | DiagnosisInfection
    | DiagnosisImminentDelivery
    | DiagnosisLaborAndDelivery
    | DiagnosisHeartburn
    | DiagnosisHeartburnPersistent
    | DiagnosisDeepVeinThrombosis
    | DiagnosisPelvicPainIntense
    | DiagnosisUrinaryTractInfection
    | DiagnosisPyelonephritis
    | DiagnosisCandidiasis
    | DiagnosisGonorrhea
    | DiagnosisTrichomonasOrBacterialVaginosis
    | DiagnosisTuberculosis
    | NoPrenatalDiagnosis
