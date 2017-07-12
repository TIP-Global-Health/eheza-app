module Activity.Encoder exposing (encodeActivityType)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..), ChildNutritionSign(..), MotherActivityType(..))


encodeActivityType : ActivityType -> String
encodeActivityType activityType =
    case activityType of
        Child subType ->
            "child__" ++ encodeChildActivityType subType

        Mother subType ->
            "mother__" ++ encodeMotherActivityType subType


encodeChildActivityType : ChildActivityType -> String
encodeChildActivityType activityType =
    case activityType of
        ChildPicture ->
            "picture"

        Height ->
            "height"

        Muac ->
            "muac"

        NutritionSigns ->
            "nutrition_signs"

        ProgressReport ->
            "progress_report"

        Weight ->
            "weight"


encodeChildNutritionSign : ChildNutritionSign -> String
encodeChildNutritionSign nutritionSign =
    case nutritionSign of
        AbdominalDisortion ->
            "abdominal-disortion"

        Apathy ->
            "apathy"

        BrittleHair ->
            "brittle-hair"

        DrySkin ->
            "dry-skin"

        Edema ->
            "edema"

        None ->
            "none"

        PoorAppetite ->
            "poor-appetite"


encodeMotherActivityType : MotherActivityType -> String
encodeMotherActivityType activityType =
    case activityType of
        Aheza ->
            "aheza"

        Attendance ->
            "attendance"

        Education ->
            "education"

        FamilyPlanning ->
            "family_planning"

        Hiv ->
            "hiv"

        MotherPicture ->
            "picture"
