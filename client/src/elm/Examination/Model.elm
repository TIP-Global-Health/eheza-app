module Examination.Model exposing (..)

import Utils.EditableWebData exposing (EditableWebData)
import EveryDictList exposing (EveryDictList)
import StorageKey exposing (StorageKey)


type ExaminationId
    = ExaminationId Int


{-| For some function signatures, we need a type that can either be a
`ExaminationChild` or `ExaminationMother` ... this corresponds to
how a `Participant` can be a `Child` or a `Mother`. There may actually be a
better way to model some of this, or perhaps not.
-}
type Examination
    = ChildExamination ExaminationChild
    | MotherExamination ExaminationMother


type HeightId
    = HeightId Int


type MuacId
    = MuacId Int


type WeightId
    = WeightId Int


{-| A record which ties together various child measurements that were taken
together.

This represents data from the backend, and should only be updated once the
backend has actually been updated. There are structures in `Measurement.Model`
that track in-progress edits on forms. (In theory, we could use something like
`EditableWebData` here instead -- one would have to think through the
advantages and disadvantages of doing so).

Each of the measurements currently reflect separate nodes on the backend,
so we need to track a node ID for each of them. We use the `StorageKey`
construct to track whether the value has been saved or not. And, the whole
thing is then wrapped in a `Maybe`, because the user might not have entered
anything at all yet.
-}
type alias ExaminationChild =
    { height : Maybe ( StorageKey HeightId, Float )
    , muac : Maybe ( StorageKey MuacId, Float )
    , photo : Maybe Int
    , weight : Maybe ( StorageKey WeightId, Float )
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
