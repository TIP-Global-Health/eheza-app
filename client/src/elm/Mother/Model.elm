module Mother.Model exposing (..)

import Activity.Model exposing (MotherActivityDates)


type alias ChildId =
    Int


type alias MotherId =
    Int


type alias Mother =
    { name : String
    , image : String
    , children : List ChildId
    , activityDates : MotherActivityDates
    }
