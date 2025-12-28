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


allPrenatalDiagnoses : List PrenatalDiagnosis
allPrenatalDiagnoses =
    [ DiagnosisChronicHypertension
    , DiagnosisGestationalHypertension
    , DiagnosisModeratePreeclampsia
    , DiagnosisSeverePreeclampsia
    , DiagnosisEclampsia
    , DiagnosisHIV
    , DiagnosisHIVDetectableViralLoad
    , DiagnosisDiscordantPartnership
    , DiagnosisSyphilis
    , DiagnosisSyphilisWithComplications
    , DiagnosisNeurosyphilis
    , DiagnosisHepatitisB
    , DiagnosisMalaria
    , DiagnosisMalariaWithAnemia
    , DiagnosisMalariaWithSevereAnemia
    , DiagnosisModerateAnemia
    , DiagnosisSevereAnemia
    , DiagnosisSevereAnemiaWithComplications
    , DiagnosisMiscarriage
    , DiagnosisMolarPregnancy
    , DiagnosisPlacentaPrevia
    , DiagnosisPlacentalAbruption
    , DiagnosisUterineRupture
    , DiagnosisObstructedLabor
    , DiagnosisPostAbortionSepsis
    , DiagnosisEctopicPregnancy
    , DiagnosisPROM
    , DiagnosisPPROM
    , DiagnosisHyperemesisGravidum
    , DiagnosisSevereVomiting
    , DiagnosisMaternalComplications
    , DiagnosisInfection
    , DiagnosisImminentDelivery
    , DiagnosisLaborAndDelivery
    , DiagnosisHeartburn
    , DiagnosisDeepVeinThrombosis
    , DiagnosisPelvicPainIntense
    , DiagnosisUrinaryTractInfection
    , DiagnosisPyelonephritis
    , DiagnosisCandidiasis
    , DiagnosisGonorrhea
    , DiagnosisTrichomonasOrBacterialVaginosis
    , DiagnosisTuberculosis
    , DiagnosisDiabetes
    , DiagnosisGestationalDiabetes
    , DiagnosisRhesusNegative
    , DiagnosisDepressionNotLikely
    , DiagnosisDepressionPossible
    , DiagnosisDepressionHighlyPossible
    , DiagnosisDepressionProbable
    , DiagnosisSuicideRisk
    , DiagnosisOther
    , DiagnosisPostpartumAbdominalPain
    , DiagnosisPostpartumUrinaryIncontinence
    , DiagnosisPostpartumHeadache
    , DiagnosisPostpartumFatigue
    , DiagnosisPostpartumFever
    , DiagnosisPostpartumPerinealPainOrDischarge
    , DiagnosisPostpartumInfection
    , DiagnosisPostpartumExcessiveBleeding
    , DiagnosisPostpartumEarlyMastitisOrEngorgment
    , DiagnosisPostpartumMastitis
    , NoPrenatalDiagnosis
    ]
