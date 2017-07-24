module Mother.Model exposing (..)

import Activity.Model exposing (MotherActivityDates)
import Examination.Model exposing (EveryDictListExaminationsMother, ExaminationStorage)


type alias ChildId =
    Int


type alias MotherId =
    Int


type alias Mother =
    { name : String
    , image : String
    , children : List ChildId
    , examinations : EveryDictListExaminationsMother
    , selectedExamination : Maybe ExaminationStorage
    , activityDates : MotherActivityDates
    }
