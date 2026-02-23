module Backend.FamilyNutritionActivity.Model exposing (FamilyActivity(..), FamilyNutritionActivity(..))

{-| This module provides types relating to the UI for presenting
family nutrition activities.
-}


type FamilyNutritionActivity
    = FamilyNutritionAheza
    | FamilyNutritionMuac


type FamilyActivity
    = FamilyActivityFbfChild
    | FamilyActivityFbfMother
    | FamilyActivityMuacChild
    | FamilyActivityMuacMother
