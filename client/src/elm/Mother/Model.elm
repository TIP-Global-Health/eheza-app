module Mother.Model exposing (..)

import Activity.Model exposing (MotherActivityDates)


type alias ChildId =
    String


type alias MotherId =
    String


type alias Mother =
    { name : String
    , image : String
    , children : List ChildId
    , activityDates : MotherActivityDates
    }
