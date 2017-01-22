module Activity.Model exposing (..)

import Date exposing (Date)


type ActivityType
    = Child ChildActivityType
    | Mother MotherActivityType


type ChildActivityType
    = ChildPicture
    | Height
    | Muac
    | ProgressReport
    | Weight


type MotherActivityType
    = Aheza
    | Attendance
    | Education
    | FamilyPlanning
    | Hiv
    | MotherPicture
    | NutritionSigns


type alias ChildActivityDates =
    { childPicture : Maybe Date
    , height : Maybe Date
    , muac : Maybe Date
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
    , nutritionSigns : Maybe Date
    }


type alias ActivityIdentity =
    { name : String
    , icon : String
    , activityType : ActivityType
    }


type alias ActivityListItem =
    { activity : ActivityIdentity
    , remaining : Int
    }


emptyChildActivityDates : ChildActivityDates
emptyChildActivityDates =
    { childPicture = Nothing
    , height = Nothing
    , muac = Nothing
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
    , nutritionSigns = Nothing
    }
