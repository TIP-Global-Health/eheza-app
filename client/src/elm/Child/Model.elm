module Child.Model exposing (..)

import Activity.Model exposing (ChildActivityDates)
import EveryDictList exposing (EveryDictList)
import Examination.Model exposing (EveryDictListExaminations, ExaminationStorage)


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
    , examinations : EveryDictListExaminations
    , selectedExamination : Maybe ExaminationStorage
    , activityDates : ChildActivityDates
    }
