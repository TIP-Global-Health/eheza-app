module Backend.WellChildActivity.Utils exposing (..)

import Backend.WellChildActivity.Model exposing (..)


encodeActivityAsString : WellChildActivity -> String
encodeActivityAsString activity =
    case activity of
        WellChildNutritionAssessment ->
            "well-child-nutrition-assessment"

        WellChildECD ->
            "well-child-ecd"


{-| The inverse of encodeActivityTypeAsString
-}
decodeActivityFromString : String -> Maybe WellChildActivity
decodeActivityFromString s =
    case s of
        "well-child-nutrition-assessment" ->
            Just WellChildNutritionAssessment

        "well-child-ecd" ->
            Just WellChildECD

        _ ->
            Nothing


{-| An activity type to use if we need to start somewhere.
-}
defaultActivity : WellChildActivity
defaultActivity =
    WellChildNutritionAssessment


{-| Returns a string representing an icon for the activity, for use in a
"class" attribute.
-}
getActivityIcon : WellChildActivity -> String
getActivityIcon activity =
    case activity of
        WellChildNutritionAssessment ->
            "nutrition-assessment"

        WellChildECD ->
            "ecd"


getAllActivities : List WellChildActivity
getAllActivities =
    [ WellChildNutritionAssessment, WellChildECD ]
