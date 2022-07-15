module Backend.PrenatalActivity.Model exposing (..)

{-| This module provides types relating to the UI for presenting
prenatal activities.
-}


type PrenatalActivity
    = DangerSigns
    | Examination
    | FamilyPlanning
    | History
    | PregnancyDating
    | PrenatalPhoto
    | Laboratory
    | HealthEducation
    | BirthPlan
    | NextSteps
    | PregnancyOutcome
    | MalariaPrevention
    | Medication
    | SymptomReview
    | PrenatalTreatmentReview
    | MaternalMentalHealth
    | PrenatalImmunisation


type PrenatalRecurrentActivity
    = LabResults
    | RecurrentNextSteps
    | RecurrentExamination


type RiskFactor
    = FactorNumberOfCSections Int
    | FactorCSectionInPreviousDelivery
    | FactorCSectionReason
    | FactorPreviousDeliveryPeriod
    | FactorSuccessiveAbortions
    | FactorSuccessivePrematureDeliveries
    | FactorStillbornPreviousDelivery
    | FactorBabyDiedOnDayOfBirthPreviousDelivery
    | FactorPartialPlacentaPreviousDelivery
    | FactorSevereHemorrhagingPreviousDelivery
    | FactorPreeclampsiaPreviousPregnancy
    | FactorConvulsionsPreviousDelivery
    | FactorConvulsionsAndUnconsciousPreviousDelivery
    | FactorIncompleteCervixPreviousPregnancy
    | FactorVerticalCSectionScar
    | FactorGestationalDiabetesPreviousPregnancy


type HighRiskFactor
    = HighRiskConvulsionsAndUnconsciousPreviousDelivery
    | HighRiskConvulsionsPreviousDelivery


type HighSeverityAlert
    = BodyTemperature
    | FetalHeartRate
    | FetalMovement
    | HeartRate
    | RespiratoryRate


type RecurringHighSeverityAlert
    = BloodPressure


type ObstetricalDiagnosis
    = DiagnosisRhNegative
    | DiagnosisModerateUnderweight
    | DiagnosisSevereUnderweight
    | DiagnosisOverweight
    | DiagnosisObese
    | DisgnosisPeripheralEdema
    | DiagnosisFetusBreech
    | DiagnosisFetusTransverse
    | DiagnosisBreastExamination
    | DiagnosisHypotension
    | DiagnosisPregnancyInducedHypertension
    | DiagnosisPreeclampsiaHighRisk


type MedicalDiagnosis
    = DiagnosisUterineMyoma
    | DiagnosisDiabetes
    | DiagnosisCardiacDisease
    | DiagnosisRenalDisease
    | DiagnosisHypertensionBeforePregnancy
    | DiagnosisTuberculosis
    | DiagnosisAsthma
    | DiagnosisBowedLegs
    | DiagnosisKnownHIV
    | DiagnosisMentalHealthHistory


type PregnancyTrimester
    = FirstTrimester
    | SecondTrimester
    | ThirdTrimester


allHighRiskFactors : List HighRiskFactor
allHighRiskFactors =
    [ HighRiskConvulsionsPreviousDelivery
    , HighRiskConvulsionsAndUnconsciousPreviousDelivery
    ]


allHighSeverityAlerts : List HighSeverityAlert
allHighSeverityAlerts =
    [ BodyTemperature, HeartRate, RespiratoryRate, FetalMovement, FetalHeartRate ]


allRecurringHighSeverityAlerts : List RecurringHighSeverityAlert
allRecurringHighSeverityAlerts =
    [ BloodPressure ]


allRiskFactors : List RiskFactor
allRiskFactors =
    [ FactorNumberOfCSections 0
    , FactorCSectionInPreviousDelivery
    , FactorCSectionReason
    , FactorPreviousDeliveryPeriod
    , FactorSuccessiveAbortions
    , FactorSuccessivePrematureDeliveries
    , FactorStillbornPreviousDelivery
    , FactorBabyDiedOnDayOfBirthPreviousDelivery
    , FactorPartialPlacentaPreviousDelivery
    , FactorSevereHemorrhagingPreviousDelivery
    , FactorPreeclampsiaPreviousPregnancy
    , FactorConvulsionsPreviousDelivery
    , FactorConvulsionsAndUnconsciousPreviousDelivery
    , FactorIncompleteCervixPreviousPregnancy
    , FactorVerticalCSectionScar
    , FactorGestationalDiabetesPreviousPregnancy
    ]


allObstetricalDiagnosis : List ObstetricalDiagnosis
allObstetricalDiagnosis =
    [ DiagnosisRhNegative
    , DiagnosisModerateUnderweight
    , DiagnosisSevereUnderweight
    , DiagnosisOverweight
    , DiagnosisObese
    , DisgnosisPeripheralEdema
    , DiagnosisFetusBreech
    , DiagnosisFetusTransverse
    , DiagnosisBreastExamination
    , DiagnosisHypotension
    , DiagnosisPregnancyInducedHypertension
    , DiagnosisPreeclampsiaHighRisk
    , DiagnosisGestationalDiabetes
    ]


allMedicalDiagnosis : List MedicalDiagnosis
allMedicalDiagnosis =
    [ DiagnosisUterineMyoma
    , DiagnosisDiabetes
    , DiagnosisCardiacDisease
    , DiagnosisRenalDisease
    , DiagnosisHypertensionBeforePregnancy
    , DiagnosisTuberculosis
    , DiagnosisAsthma
    , DiagnosisBowedLegs
    , DiagnosisKnownHIV
    , DiagnosisMentalHealthHistory
    , DiagnosisDiabetes
    ]


allTrimesters : List PregnancyTrimester
allTrimesters =
    [ FirstTrimester, SecondTrimester, ThirdTrimester ]
