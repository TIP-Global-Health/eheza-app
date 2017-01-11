module Child.Model exposing (..)

import Activity.Model exposing (ChildActivityDates)


type alias ChildId =
    String


type alias MotherId =
    String


type alias Child =
    { name : String
    , image : String
    , motherId : Maybe MotherId
    , childActivityDates : ChildActivityDates
    }
