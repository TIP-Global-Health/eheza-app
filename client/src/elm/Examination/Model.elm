module Examination.Model exposing (..)

import EveryDictList exposing (EveryDictList)
import RemoteData exposing (RemoteData(NotAsked), WebData)


-- @todo: move to EditableRecord manager package?


{-| @todo: Move to a more general place

`NewRecord` should be able to hold only `OriginalState`, and we cannot enfore
it on the type level, so it will be our resposnsibilty to make sure it does.

-}
type RecordStorage recordId
    = NewRecord
    | ExistingRecord recordId


type EditableRecord record
    = OriginalState record
      -- The first record is the original record.
      -- The second record is the "dirty" record, the one that is
      -- being currently upated.
    | UpdatedState record record (WebData ())


{-| Get the original record out of a EditableRecord.
-}
getOriginalRecord : EditableRecord record -> record
getOriginalRecord editableRecord =
    case editableRecord of
        OriginalState originalRecord ->
            originalRecord

        UpdatedState originalRecord _ _ ->
            originalRecord


{-| Create a new `EditableRecord` from a plain record.
-}
createEditableRecord : record -> EditableRecord record
createEditableRecord record =
    OriginalState record


{-| Update an `EditableRecord`.
-}
updateRecord : EditableRecord record -> record -> EditableRecord record
updateRecord editableRecord newRecord =
    case editableRecord of
        OriginalState _ ->
            OriginalState newRecord

        UpdatedState originalRecord _ webData ->
            UpdatedState originalRecord newRecord webData


revertDirtyRecord : EditableRecord record -> EditableRecord record
revertDirtyRecord editableRecord =
    case editableRecord of
        OriginalState _ ->
            editableRecord

        UpdatedState originalRecord _ webData ->
            UpdatedState originalRecord originalRecord NotAsked


isDirtyWith : EditableRecord record -> (record -> record -> Bool) -> Bool
isDirtyWith editableRecord compareFunc =
    case editableRecord of
        OriginalState _ ->
            False

        UpdatedState originalRecord updatedRecord _ ->
            compareFunc originalRecord originalRecord


isDirty : EditableRecord record -> Bool
isDirty editableRecord =
    isDirtyWith editableRecord (==)



-------


type ExaminationId
    = Examination Int


type alias ExaminationStorage =
    RecordStorage ExaminationId


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
    EveryDictList ExaminationStorage (EditableRecord ExaminationChild)


type alias EveryDictListExaminationsMother =
    EveryDictList ExaminationStorage (EditableRecord ExaminationMother)
