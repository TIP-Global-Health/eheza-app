module Child.Model exposing (..)

import Activity.Model exposing (ChildActivityDates)
import Examination.Model exposing (EveryDictListExaminationsChild, ExaminationId)


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
    , examinations : EveryDictListExaminationsChild
    , selectedExamination : Maybe ExaminationId
    , activityDates : ChildActivityDates
    }
