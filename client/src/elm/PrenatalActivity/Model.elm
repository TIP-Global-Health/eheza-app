module PrenatalActivity.Model exposing
    ( HighRiskFactor(..)
    , HighSeverityAlert(..)
    , PrenatalActivity(..)
    , allHighRiskFactors
    , allHighSeverityAlerts
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
