module PrenatalActivity.Model exposing
    ( HighRiskFactor(..)
    , HighSeverityAlert(..)
    , PrenatalActivity(..)
    , RiskFactor(..)
    , allHighRiskFactors
    , allHighSeverityAlerts
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
    ]
