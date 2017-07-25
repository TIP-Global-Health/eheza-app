module Examination.Model exposing (..)

import Utils.EditableWebData exposing (EditableWebData)
import EveryDictList exposing (EveryDictList)


type ExaminationId
    = Examination Int


type alias FloatInput =
    Maybe Float


{-| @todo: Add `date` field to mark the date the examination
was completed

Maybe change name to `CompletedExaminationChild`?

-}
type alias ExaminationChild =
    { height : FloatInput
    , muac : FloatInput
    , photo : Maybe Int
    , weight : FloatInput
    }


emptyExaminationChild : ExaminationChild
emptyExaminationChild =
    { height = Nothing
    , muac = Nothing
    , photo = Nothing
    , weight = Nothing
    }


type alias ExaminationMother =
    {}


emptyExaminationMother : ExaminationMother
emptyExaminationMother =
    {}


type alias EveryDictListExaminationsChild =
    EveryDictList ExaminationId (EditableWebData ExaminationChild)


type alias EveryDictListExaminationsMother =
    EveryDictList ExaminationId (EditableWebData ExaminationMother)
