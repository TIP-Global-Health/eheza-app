module Backend.ChildScoreboardActivity.Utils exposing (activityFromString, activityToString, allActivities, getActivityIcon)

import Backend.ChildScoreboardActivity.Model exposing (..)


activityToString : ChildScoreboardActivity -> String
activityToString activity =
    case activity of
        ChildScoreboardNCDA ->
            "ncda"

        ChildScoreboardVaccinationHistory ->
            "vaccination-history"


{-| The inverse of encodeActivityTypeAsString
-}
activityFromString : String -> Maybe ChildScoreboardActivity
activityFromString s =
    case s of
        "ncda" ->
            Just ChildScoreboardNCDA

        "vaccination-history" ->
            Just ChildScoreboardVaccinationHistory

        _ ->
            Nothing


{-| Returns a string representing an icon for the activity, for use in a
"class" attribute.
-}
getActivityIcon : ChildScoreboardActivity -> String
getActivityIcon activity =
    case activity of
        ChildScoreboardNCDA ->
            "history"

        ChildScoreboardVaccinationHistory ->
            "immunisation"


allActivities : List ChildScoreboardActivity
allActivities =
    [ ChildScoreboardNCDA
    , ChildScoreboardVaccinationHistory
    ]
