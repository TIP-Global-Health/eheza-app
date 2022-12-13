module Backend.NCDActivity.Utils exposing (..)

import Backend.NCDActivity.Model exposing (..)


activityToString : NCDActivity -> String
activityToString activity =
    case activity of
        DangerSigns ->
            "danger-signs"

        Examination ->
            "examination"

        FamilyPlanning ->
            "planning"

        Laboratory ->
            "laboratory"

        MedicalHistory ->
            "history"

        NextSteps ->
            "next-steps"

        SymptomReview ->
            "symptoms"

        OutsideCare ->
            "outside-care"


{-| The inverse of encodeActivityTypeAsString
-}
activityFromString : String -> Maybe NCDActivity
activityFromString s =
    case s of
        "danger-signs" ->
            Just DangerSigns

        "examination" ->
            Just Examination

        "planning" ->
            Just FamilyPlanning

        "laboratory" ->
            Just Laboratory

        "history" ->
            Just MedicalHistory

        "next-steps" ->
            Just NextSteps

        "symptoms" ->
            Just SymptomReview

        "outside-care" ->
            Just OutsideCare

        _ ->
            Nothing


recurrentActivityToString : NCDRecurrentActivity -> String
recurrentActivityToString activity =
    case activity of
        LabResults ->
            "laboratory"

        RecurrentNextSteps ->
            "next-steps"


recurrentActivityFromString : String -> Maybe NCDRecurrentActivity
recurrentActivityFromString s =
    case s of
        "laboratory" ->
            Just LabResults

        "next-steps" ->
            Just RecurrentNextSteps

        _ ->
            Nothing


getActivityIcon : NCDActivity -> String
getActivityIcon activity =
    activityToString activity


getRecurrentActivityIcon : NCDRecurrentActivity -> String
getRecurrentActivityIcon activity =
    recurrentActivityToString activity


getAllActivities : List NCDActivity
getAllActivities =
    [ DangerSigns
    , SymptomReview
    , MedicalHistory
    , OutsideCare
    , Examination
    , Laboratory
    , FamilyPlanning
    , NextSteps
    ]
