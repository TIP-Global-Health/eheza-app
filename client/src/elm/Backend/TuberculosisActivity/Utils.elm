module Backend.TuberculosisActivity.Utils exposing (..)

import Backend.TuberculosisActivity.Model exposing (..)
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust)
import Translate exposing (Language, translate)


activityToString : TuberculosisActivity -> String
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


activityFromString : String -> Maybe TuberculosisActivity
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
getActivityIcon : TuberculosisActivity -> String
getActivityIcon activity =
    activityToString activity


allActivities : List TuberculosisActivity
allActivities =
    [ Diagnostics, Medication, SymptomReview, NextSteps ]
