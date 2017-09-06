module Examination.Model exposing (..)

import Utils.EditableWebData exposing (EditableWebData)
import EveryDictList exposing (EveryDictList)


type ExaminationId
    = ExaminationId Int


type alias FloatInput =
    Maybe Float


{-| For some function signatures, we need a type that can either be a
`ExaminationChild` or `ExaminationMother` ... this corresponds to
how a `Participant` can be a `Child` or a `Mother`. There may actually be a
better way to model some of this, or perhaps not.
-}
type Examination
    = ChildExamination ExaminationChild
    | MotherExamination ExaminationMother


{-| A record which ties together various child measurements
that were taken together. Note that ultimately these are likely to need to get
the full `EditableWebData` treatment, since they are indeed editable, and
individually updated to the backend. And, they actually represent individual
nodes on the backend, so they'll need a `Storage` wrapper as well. Will
get to that shortly, no doubt -- for the moment, just hooking this into the
UI at all.
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
