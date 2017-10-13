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


type alias ActivityIdentity =
    { name : String
    , icon : String
    , activityType : ActivityType
    }


type alias ActivityListItem =
    { activity : ActivityIdentity
    , totals : ( Int, Int )
    }
