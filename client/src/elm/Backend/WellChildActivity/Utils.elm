module Backend.WellChildActivity.Utils exposing (decodeActivityFromString, encodeActivityAsString, getActivityIcon, getAllActivities)

import Backend.WellChildActivity.Model exposing (..)


encodeActivityAsString : WellChildActivity -> String
encodeActivityAsString activity =
    case activity of
        WellChildDangerSigns ->
            "well-child-danger-signs"

        WellChildNutritionAssessment ->
            "well-child-nutrition-assessment"

        WellChildECD ->
            "well-child-ecd"

        WellChildMedication ->
            "well-child-medication"

        WellChildPregnancySummary ->
            "well-child-pregnancy-summary"

        WellChildImmunisation ->
            "well-child-immunisation"

        WellChildNextSteps ->
            "well-child-next-steps"

        WellChildPhoto ->
            "well-child-photo"

        WellChildNCDA ->
            "well-child-ncda"

        WellChildHomeVisit ->
            "well-child-home-visit"


{-| The inverse of encodeActivityTypeAsString
-}
decodeActivityFromString : String -> Maybe WellChildActivity
decodeActivityFromString s =
    case s of
        "well-child-nutrition-assessment" ->
            Just WellChildNutritionAssessment

        "well-child-ecd" ->
            Just WellChildECD

        "well-child-danger-signs" ->
            Just WellChildDangerSigns

        "well-child-medication" ->
            Just WellChildMedication

        "well-child-pregnancy-summary" ->
            Just WellChildPregnancySummary

        "well-child-immunisation" ->
            Just WellChildImmunisation

        "well-child-next-steps" ->
            Just WellChildNextSteps

        "well-child-photo" ->
            Just WellChildPhoto

        "well-child-ncda" ->
            Just WellChildNCDA

        "well-child-home-visit" ->
            Just WellChildHomeVisit

        _ ->
            Nothing


{-| Returns a string representing an icon for the activity, for use in a
"class" attribute.
-}
getActivityIcon : WellChildActivity -> String
getActivityIcon activity =
    case activity of
        WellChildDangerSigns ->
            "danger-signs"

        WellChildNutritionAssessment ->
            "nutrition-assessment"

        WellChildECD ->
            "ecd"

        WellChildMedication ->
            "medication"

        WellChildPregnancySummary ->
            "history"

        WellChildImmunisation ->
            "immunisation"

        WellChildNextSteps ->
            "next-steps"

        WellChildPhoto ->
            "photo"

        WellChildNCDA ->
            "history"

        WellChildHomeVisit ->
            "home-visit"


getAllActivities : List WellChildActivity
getAllActivities =
    [ WellChildPregnancySummary
    , WellChildDangerSigns
    , WellChildNutritionAssessment
    , WellChildImmunisation
    , WellChildHomeVisit
    , WellChildECD
    , WellChildMedication
    , WellChildPhoto
    , WellChildNCDA
    , WellChildNextSteps
    ]
