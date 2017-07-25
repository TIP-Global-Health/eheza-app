module Examination.Model exposing (..)

import Utils.EditableWebData exposing (EditableWebData)
import EveryDictList exposing (EveryDictList)


type ExaminationId
    = Examination Int


type alias FloatInput =
    Maybe Float


{-| Record holding a completed examination of a Child.
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


{-| Record holding a completed examination of a Mother.
-}
type alias ExaminationMother =
    {}


emptyExaminationMother : ExaminationMother
emptyExaminationMother =
    {}


type alias EveryDictListExaminationsChild =
    EveryDictList ExaminationId (EditableWebData ExaminationChild)


type alias EveryDictListExaminationsMother =
    EveryDictList ExaminationId (EditableWebData ExaminationMother)
