module PrenatalActivity.Model exposing
    ( HighRiskFactor(..)
    , HighSeverityAlert(..)
    , MedicalDiagnosis(..)
    , ObstetricalDiagnosis(..)
    , PregnancyTrimester(..)
    , PrenatalActivity(..)
    , RecurringHighSeverityAlert(..)
    , RiskFactor(..)
    , allHighRiskFactors
    , allHighSeverityAlerts
    , allMedicalDiagnosis
    , allObstetricalDiagnosis
    , allRecurringHighSeverityAlerts
    , allRiskFactors
    , allTrimesters
    )

{-| This module provides types relating to the UI for presenting
prenatal activities.
-}


type PrenatalActivity
    = DangerSigns
    | Examination
    | FamilyPlanning
    | History
    | PatientProvisions
    | PregnancyDating
    | PrenatalPhoto


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
    = ConvulsionsAndUnconsciousPreviousDelivery
    | ConvulsionsPreviousDelivery


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
    | DiagnosisHIV
    | DiagnosisMentalHealthHistory


type PregnancyTrimester
    = FirstTrimester
    | SecondTrimester
    | ThirdTrimester


allHighRiskFactors : List HighRiskFactor
allHighRiskFactors =
    [ ConvulsionsPreviousDelivery, ConvulsionsAndUnconsciousPreviousDelivery ]


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
    , DiagnosisHIV
    , DiagnosisMentalHealthHistory
    ]


allTrimesters : List PregnancyTrimester
allTrimesters =
    [ FirstTrimester, SecondTrimester, ThirdTrimester ]
