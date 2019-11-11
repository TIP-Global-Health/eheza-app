module PrenatalActivity.Model exposing
    ( HighRiskFactor(..)
    , HighSeverityAlert(..)
    , MedicalDiagnosis(..)
    , ObstetricDiagnosis(..)
    , PrenatalActivity(..)
    , RiskFactor(..)
    , allHighRiskFactors
    , allHighSeverityAlerts
    , allMedicalDiagnosis
    , allObstetricDiagnosis
    , allRiskFactors
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


type HighRiskFactor
    = ConvulsionsAndUnconsciousPreviousDelivery
    | ConvulsionsPreviousDelivery


type HighSeverityAlert
    = BodyTemperature
    | BloodPressure
    | FetalHeartRate
    | FetalMovement
    | HeartRate
    | RespiratoryRate


type ObstetricDiagnosis
    = DiagnosisGestationalDiabetesPreviousPregnancy
    | DiagnosisRhNegative
    | DiagnosisModerateUnderweight
    | DiagnosisSevereUnderweight
    | DiagnosisOverweight
    | DiagnosisObese


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
    | DiagnosMentalHealthHistory


allHighRiskFactors : List HighRiskFactor
allHighRiskFactors =
    [ ConvulsionsPreviousDelivery, ConvulsionsAndUnconsciousPreviousDelivery ]


allHighSeverityAlerts : List HighSeverityAlert
allHighSeverityAlerts =
    [ BodyTemperature, BloodPressure, HeartRate, RespiratoryRate, FetalMovement, FetalHeartRate ]


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
    ]


allObstetricDiagnosis : List ObstetricDiagnosis
allObstetricDiagnosis =
    [ DiagnosisGestationalDiabetesPreviousPregnancy
    , DiagnosisRhNegative
    , DiagnosisModerateUnderweight
    , DiagnosisSevereUnderweight
    , DiagnosisOverweight
    , DiagnosisObese
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
    , DiagnosMentalHealthHistory
    ]
