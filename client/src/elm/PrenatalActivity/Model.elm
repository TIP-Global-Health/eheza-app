module PrenatalActivity.Model exposing
    ( HighRiskFactor(..)
    , HighSeverityAlert(..)
    , MedicalDiagnosis(..)
    , ObstetricalDiagnosis(..)
    , PregnancyTrimester(..)
    , PrenatalActivity(..)
    , RiskFactor(..)
    , allHighRiskFactors
    , allHighSeverityAlerts
    , allMedicalDiagnosis
    , allObstetricalDiagnosis
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



-- | PrenatalPicture


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
    | BloodPressure
    | FetalHeartRate
    | FetalMovement
    | HeartRate
    | RespiratoryRate


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
    | DiagnosisHypertension
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
    , DiagnosisHypertension
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
