module Backend.ChildScoreboardActivity.Utils exposing (..)

import Backend.ChildScoreboardActivity.Model exposing (..)


encodeActivityAsString : ChildScoreboardActivity -> String
encodeActivityAsString activity =
    case activity of
        ChildScoreboardNCDA ->
            "ncda"

        ChildScoreboardVaccinationHistory ->
            "vaccination-history"


{-| The inverse of encodeActivityTypeAsString
-}
decodeActivityFromString : String -> Maybe ChildScoreboardActivity
decodeActivityFromString s =
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
