module Examination.Model exposing (..)

import EveryDictList exposing (EveryDictList)
import RemoteData exposing (WebData)


type ExaminationId
    = Examination Int


type alias ExaminationStorage =
    RecordStorage ExaminationId


{-| @todo: Move to a more general place

@todo: `NewRecord` should be able to hold only `OriginalState` --
how can we enforce it?

-}
type RecordStorage recordId
    = NewRecord
    | ExistingRecord recordId


type RecordState record
    = OriginalState record (WebData ())
      -- The first record is the original record.
      -- The second record is the "dirty" record, the one that is
      -- being currently upated.
    | UpdatedState record record (WebData ())


{-| Get the original record out of a RecordState.
-}
getOriginalRecord : RecordState record -> record
getOriginalRecord recordState =
    case recordState of
        OriginalState originalRecord webData ->
            originalRecord

        UpdatedState originalRecord _ _ ->
            originalRecord


{-| Update a RecordState.
-}
updateRecord : RecordState record -> record -> RecordState record
updateRecord recordState newRecord =
    case recordState of
        OriginalState _ webData ->
            OriginalState newRecord webData

        UpdatedState originalRecord _ webData ->
            UpdatedState originalRecord newRecord webData


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
    EveryDictList ExaminationStorage (RecordState ExaminationChild)


type alias EveryDictListExaminationsMother =
    EveryDictList ExaminationStorage (RecordState ExaminationMother)
