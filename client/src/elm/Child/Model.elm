module Child.Model exposing (..)

import Activity.Model exposing (ChildActivityDates)
import EveryDictList exposing (EveryDictList)


type alias ChildId =
    Int


type alias ExaminationId =
    Int


type alias MotherId =
    Int


type alias Child =
    { name : String
    , image : String
    , motherId : Maybe MotherId
    , lastExamination : Maybe ExaminationId
    , activityDates : ChildActivityDates
    }
