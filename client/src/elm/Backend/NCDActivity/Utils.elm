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

        _ ->
            Nothing


getActivityIcon : NCDActivity -> String
getActivityIcon activity =
    activityToString activity


getAllActivities : List NCDActivity
getAllActivities =
    [ DangerSigns
    , SymptomReview
    , MedicalHistory
    , Examination
    , Laboratory
    , FamilyPlanning
    , NextSteps
    ]
