module Activity.Model exposing (..)

import Date exposing (Date)


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


type alias ChildActivityDates =
    { childPicture : Maybe Date
    , height : Maybe Date
    , muac : Maybe Date
    , nutritionSigns : Maybe Date
    , progressReport : Maybe Date
    , weight : Maybe Date
    }


type alias MotherActivityDates =
    { familyPlanning : Maybe Date
    }


type alias ActivityIdentity =
    { name : String
    , icon : String
    , activityType : ActivityType
    }


type alias ActivityListItem =
    { activity : ActivityIdentity
    , totals : ( Int, Int )
    }


emptyChildActivityDates : ChildActivityDates
emptyChildActivityDates =
    { childPicture = Nothing
    , height = Nothing
    , muac = Nothing
    , nutritionSigns = Nothing
    , progressReport = Nothing
    , weight = Nothing
    }


emptyMotherActivityDates : MotherActivityDates
emptyMotherActivityDates =
    { familyPlanning = Nothing
    }
