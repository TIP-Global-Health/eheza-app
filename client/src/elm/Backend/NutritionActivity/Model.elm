module Backend.NutritionActivity.Model exposing (NutritionActivity(..))

{-| This module provides types relating to the UI for presenting
nutrition activities.
-}


type NutritionActivity
    = Muac
    | Height
    | Nutrition
    | Photo
    | Weight
    | NextSteps
      -- @todo: remove these 2
    | SendToHC
    | HealthEducation
