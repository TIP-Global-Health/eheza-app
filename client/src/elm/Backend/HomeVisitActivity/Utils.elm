module Backend.HomeVisitActivity.Utils exposing (allActivities, decodeActivityFromString, encodeActivityAsString, getActivityIcon)

{-| Various utilities that deal with "activities". An activity represents the
need for a nurse to do something with respect to a person who is checked in.
-}

import Backend.HomeVisitActivity.Model exposing (HomeVisitActivity(..))


{-| Used for URL etc., not for display in the normal UI (since we'd use translate
for that).
-}
encodeActivityAsString : HomeVisitActivity -> String
encodeActivityAsString activity =
    case activity of
        Feeding ->
            "home-visit-feeding"

        Caring ->
            "home-visit-caring"

        Hygiene ->
            "home-visit-hygiene"

        FoodSecurity ->
            "home-visit-food-security"


{-| The inverse of encodeActivityTypeAsString
-}
decodeActivityFromString : String -> Maybe HomeVisitActivity
decodeActivityFromString s =
    case s of
        "home-visit-feeding" ->
            Just Feeding

        "home-visit-caring" ->
            Just Caring

        "home-visit-hygiene" ->
            Just Hygiene

        "home-visit-food-security" ->
            Just FoodSecurity

        _ ->
            Nothing


{-| Returns a string representing an icon for the activity, for use in a
"class" attribute.
-}
getActivityIcon : HomeVisitActivity -> String
getActivityIcon activity =
    case activity of
        Feeding ->
            "feeding"

        Caring ->
            "caring"

        Hygiene ->
            "hygiene"

        FoodSecurity ->
            "food-security"


allActivities : List HomeVisitActivity
allActivities =
    [ Feeding, Caring, Hygiene, FoodSecurity ]
