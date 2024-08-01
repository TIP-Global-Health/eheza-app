module Backend.HIVActivity.Utils exposing (..)

import Backend.HIVActivity.Model exposing (..)


activityToString : HIVActivity -> String
activityToString activity =
    case activity of
        Diagnostics ->
            "diagnostics"

        Medication ->
            "medication"

        SymptomReview ->
            "symptoms"

        NextSteps ->
            "next-steps"


activityFromString : String -> Maybe HIVActivity
activityFromString s =
    case s of
        "diagnostics" ->
            Just Diagnostics

        "medication" ->
            Just Medication

        "symptoms" ->
            Just SymptomReview

        "next-steps" ->
            Just NextSteps

        _ ->
            Nothing


{-| Returns a string representing an icon for the activity, for use in a
"class" attribute.
-}
getActivityIcon : HIVActivity -> String
getActivityIcon activity =
    activityToString activity


allActivities : List HIVActivity
allActivities =
    [ Diagnostics, Medication, SymptomReview, NextSteps ]
