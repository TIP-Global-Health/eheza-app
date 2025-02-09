module Backend.AcuteIllnessEncounter.Types exposing (..)

import Backend.Entities exposing (..)
import Backend.NCDEncounter.Types exposing (NCDProgressReportInitiator)
import Backend.PatientRecord.Model exposing (PatientRecordInitiator)


type AcuteIllnessEncounterType
    = AcuteIllnessEncounterNurse
    | AcuteIllnessEncounterNurseSubsequent
    | AcuteIllnessEncounterCHW


type AcuteIllnessDiagnosis
    = DiagnosisSevereCovid19
    | DiagnosisPneuminialCovid19
    | DiagnosisLowRiskCovid19
    | DiagnosisMalariaComplicated
    | DiagnosisMalariaUncomplicated
    | DiagnosisMalariaUncomplicatedAndPregnant
    | DiagnosisGastrointestinalInfectionComplicated
    | DiagnosisGastrointestinalInfectionUncomplicated
    | DiagnosisSimpleColdAndCough
    | DiagnosisRespiratoryInfectionComplicated
    | DiagnosisRespiratoryInfectionUncomplicated
    | DiagnosisFeverOfUnknownOrigin
    | DiagnosisUndeterminedMoreEvaluationNeeded
    | DiagnosisTuberculosisSuspect
    | NoAcuteIllnessDiagnosis
      -- Obsolete
    | DiagnosisCovid19Suspect


type AcuteIllnessProgressReportInitiator
    = InitiatorEncounterPage
    | InitiatorIndividualNutritionProgressReport NutritionEncounterId
    | InitiatorWellChildProgressReport WellChildEncounterId
    | InitiatorGroupNutritionProgressReport SessionId PersonId
    | InitiatorPatientRecord PatientRecordInitiator PersonId
    | InitiatorNCDProgressReport NCDProgressReportInitiator
    | InitiatorChildScoreboardProgressReport ChildScoreboardEncounterId
