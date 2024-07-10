module Backend.Reports.Utils exposing (..)

import Backend.Reports.Model exposing (..)


genderFromString : String -> Maybe Gender
genderFromString s =
    case s of
        "female" ->
            Just Female

        "male" ->
            Just Male

        _ ->
            Nothing


allAcuteIllnessDiagnoses : List AcuteIllnessDiagnosis
allAcuteIllnessDiagnoses =
    [ DiagnosisCovid19Suspect
    , DiagnosisSevereCovid19
    , DiagnosisPneuminialCovid19
    , DiagnosisLowRiskCovid19
    , DiagnosisMalariaComplicated
    , DiagnosisMalariaUncomplicated
    , DiagnosisMalariaUncomplicatedAndPregnant
    , DiagnosisGastrointestinalInfectionComplicated
    , DiagnosisGastrointestinalInfectionUncomplicated
    , DiagnosisSimpleColdAndCough
    , DiagnosisRespiratoryInfectionComplicated
    , DiagnosisRespiratoryInfectionUncomplicated
    , DiagnosisFeverOfUnknownOrigin
    , DiagnosisUndeterminedMoreEvaluationNeeded
    , DiagnosisTuberculosisSuspect
    ]
