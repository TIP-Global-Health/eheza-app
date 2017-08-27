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
    = Aheza
    | Attendance
    | Education
    | FamilyPlanning
    | Hiv
    | MotherPicture


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
    { aheza : Maybe Date
    , attendance : Maybe Date
    , education : Maybe Date
    , familyPlanning : Maybe Date
    , hiv : Maybe Date
    , motherPicture : Maybe Date
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
    { aheza = Nothing
    , attendance = Nothing
    , education = Nothing
    , familyPlanning = Nothing
    , hiv = Nothing
    , motherPicture = Nothing
    }
