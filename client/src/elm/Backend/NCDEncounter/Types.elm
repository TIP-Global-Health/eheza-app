module Backend.NCDEncounter.Types exposing (..)

import Backend.Entities exposing (NCDEncounterId)


type NCDDiagnosis
    = DiagnosisHypertensionStage1
    | DiagnosisHypertensionStage2
    | DiagnosisHypertensionStage3
    | DiagnosisDiabetesInitial
    | DiagnosisDiabetesRecurrent
    | DiagnosisRenalComplications
    | NoNCDDiagnosis


type NCDProgressReportInitiator
    = InitiatorEncounterPage NCDEncounterId
    | InitiatorRecurrentEncounterPage NCDEncounterId
