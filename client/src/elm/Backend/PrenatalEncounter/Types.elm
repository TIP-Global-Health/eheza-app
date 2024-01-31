module Backend.PrenatalEncounter.Types exposing (..)


type PrenatalDiagnosis
    = DiagnosisChronicHypertensionImmediate
    | DiagnosisChronicHypertensionAfterRecheck
    | DiagnosisGestationalHypertensionImmediate
    | DiagnosisGestationalHypertensionAfterRecheck
    | DiagnosisModeratePreeclampsiaInitialPhase
    | DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus
    | DiagnosisModeratePreeclampsiaRecurrentPhase
    | DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus
    | DiagnosisSeverePreeclampsiaInitialPhase
    | DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus
    | DiagnosisSeverePreeclampsiaRecurrentPhase
    | DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus
    | DiagnosisEclampsia
      -- | DiagnosisHIV
    | DiagnosisHIVInitialPhase
    | DiagnosisHIVRecurrentPhase
      -- | DiagnosisHIVDetectableViralLoad
    | DiagnosisHIVDetectableViralLoadInitialPhase
    | DiagnosisHIVDetectableViralLoadRecurrentPhase
      -- | DiagnosisDiscordantPartnership
    | DiagnosisDiscordantPartnershipInitialPhase
    | DiagnosisDiscordantPartnershipRecurrentPhase
      -- | DiagnosisSyphilis
    | DiagnosisSyphilisInitialPhase
    | DiagnosisSyphilisRecurrentPhase
      -- | DiagnosisSyphilisWithComplications
    | DiagnosisSyphilisWithComplicationsInitialPhase
    | DiagnosisSyphilisWithComplicationsRecurrentPhase
      -- | DiagnosisNeurosyphilis
    | DiagnosisNeurosyphilisInitialPhase
    | DiagnosisNeurosyphilisRecurrentPhase
      -- | DiagnosisHepatitisB
    | DiagnosisHepatitisBInitialPhase
    | DiagnosisHepatitisBRecurrentPhase
      -- | DiagnosisMalaria
    | DiagnosisMalariaInitialPhase
    | DiagnosisMalariaRecurrentPhase
      -- | DiagnosisMalariaMedicatedContinued
    | DiagnosisMalariaMedicatedContinuedInitialPhase
    | DiagnosisMalariaMedicatedContinuedRecurrentPhase
      -- | DiagnosisMalariaWithAnemia
    | DiagnosisMalariaWithAnemiaInitialPhase
    | DiagnosisMalariaWithAnemiaRecurrentPhase
      -- | DiagnosisMalariaWithAnemiaMedicatedContinued
    | DiagnosisMalariaWithAnemiaMedicatedContinuedInitialPhase
    | DiagnosisMalariaWithAnemiaMedicatedContinuedRecurrentPhase
      -- | DiagnosisMalariaWithSevereAnemia
    | DiagnosisMalariaWithSevereAnemiaInitialPhase
    | DiagnosisMalariaWithSevereAnemiaRecurrentPhase
      -- | DiagnosisModerateAnemia
    | DiagnosisModerateAnemiaInitialPhase
    | DiagnosisModerateAnemiaRecurrentPhase
      -- | DiagnosisSevereAnemia
    | DiagnosisSevereAnemiaInitialPhase
    | DiagnosisSevereAnemiaRecurrentPhase
      -- | DiagnosisSevereAnemiaWithComplications
    | DiagnosisSevereAnemiaWithComplicationsInitialPhase
    | DiagnosisSevereAnemiaWithComplicationsRecurrentPhase
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
    | DiagnosisSevereVomiting
    | DiagnosisSevereVomitingBySymptoms
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
      -- | DiagnosisDiabetes
    | DiagnosisDiabetesInitialPhase
    | DiagnosisDiabetesRecurrentPhase
      -- | DiagnosisGestationalDiabetes
    | DiagnosisGestationalDiabetesInitialPhase
    | DiagnosisGestationalDiabetesRecurrentPhase
      -- | DiagnosisRhesusNegative
    | DiagnosisRhesusNegativeInitialPhase
    | DiagnosisRhesusNegativeRecurrentPhase
    | DiagnosisDepressionNotLikely
    | DiagnosisDepressionPossible
    | DiagnosisDepressionHighlyPossible
    | DiagnosisDepressionProbable
    | DiagnosisSuicideRisk
    | DiagnosisOther
      -- For Postpartum only:
    | DiagnosisPostpartumAbdominalPain
    | DiagnosisPostpartumUrinaryIncontinence
    | DiagnosisPostpartumHeadache
    | DiagnosisPostpartumFatigue
    | DiagnosisPostpartumFever
    | DiagnosisPostpartumPerinealPainOrDischarge
    | DiagnosisPostpartumInfection
    | DiagnosisPostpartumExcessiveBleeding
    | DiagnosisPostpartumEarlyMastitisOrEngorgment
    | DiagnosisPostpartumMastitis
    | NoPrenatalDiagnosis
