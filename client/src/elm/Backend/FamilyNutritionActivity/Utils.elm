module Backend.FamilyNutritionActivity.Utils exposing (allActivities, getActivityIcon)

{-| Various utilities that deal with "activities". An activity represents the
need for a nurse to do something with respect to a person who is checked in.

Just as a matter of terminology, we use "completed" to mean the obvious thing
-- that is, the action has been performed. The word "pending" is not precisely
the opposite of "completed", because the action is only "pending" if it is
expected (and not completed).

-}

import Backend.FamilyNutritionActivity.Model exposing (FamilyNutritionActivity(..))


{-| Returns a string representing an icon for the activity, for use in a
"class" attribute.
-}
getActivityIcon : FamilyNutritionActivity -> String
getActivityIcon activity =
    case activity of
        FamilyNutritionAheza ->
            "fbf"

        FamilyNutritionMuac ->
            "muac"

        FamilyNutritionPhoto ->
            "photo"


allActivities : List FamilyNutritionActivity
allActivities =
    [ FamilyNutritionAheza, FamilyNutritionMuac, FamilyNutritionPhoto ]
