module Child.Model exposing (..)

import Activity.Model exposing (ChildActivityDates)
import Examination.Model exposing (EveryDictListExaminationsChild, ExaminationId)
import RemoteData exposing (WebData)


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
    , siblingId : Maybe ChildId
    , examinations : WebData EveryDictListExaminationsChild
    , selectedExamination : Maybe ExaminationId
    , activityDates : ChildActivityDates
    }
