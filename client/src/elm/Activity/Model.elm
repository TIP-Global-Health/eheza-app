module Activity.Model exposing (..)


type ActivityType
    = Child ChildActivityType
    | Mother MotherActivityType


type ChildActivityType
    = ChildPicture
    | Height
    | Muac
    | NutritionSigns
    | ProgressReport
    | Weight


type MotherActivityType
    = FamilyPlanning


type FamilyPlanningSign
    = Condoms
    | IUD
    | Injection
    | Necklace
    | NoFamilyPlanning
    | Pill


type ChildNutritionSign
    = AbdominalDisortion
    | Apathy
    | BrittleHair
    | DrySkin
    | Edema
    | None
    | PoorAppetite


type alias ActivityIdentity =
    { name : String
    , icon : String
    , activityType : ActivityType
    }


type alias ActivityListItem =
    { activity : ActivityIdentity
    , totals : ( Int, Int )
    }
