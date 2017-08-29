module Mother.Model exposing (..)

import Activity.Model exposing (MotherActivityDates)
import Date exposing (Date)
import Examination.Model exposing (EveryDictListExaminationsMother, ExaminationId)
import RemoteData exposing (WebData)


type alias ChildId =
    Int


type alias MotherId =
    Int


type alias Mother =
    { name : String
    , image : String
    , children : List ChildId
    , examinations : WebData EveryDictListExaminationsMother
    , selectedExamination : Maybe ExaminationId
    , activityDates : MotherActivityDates
    , birthDate : Date
    }
