module Examination.Model exposing (..)

import Editable exposing (Editable(..))
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


type EditableWebData a
    = EditableWebData (Editable a) (WebData ())


create : a -> EditableWebData a
create record =
    EditableWebData (Editable.ReadOnly record) NotAsked


map : (a -> a) -> EditableWebData a -> EditableWebData a
map f (EditableWebData editable webData) =
    EditableWebData (Editable.map f editable) webData


{-| Updates an `Editable` and doesn't change a `ReadOnly`.

    Editable.ReadOnly "old"
        |> Editable.update "new"  --> ReadOnly "old"
    Editable.Editable "old" "old"
        |> Editable.update "new"  --> Editable "old" "new"

-}
update : a -> EditableWebData a -> EditableWebData a
update value =
    map (always value)


webDataUpdate : WebData () -> EditableWebData a -> EditableWebData a
webDataUpdate newWebData (EditableWebData editable webData) =
    EditableWebData editable newWebData


value : EditableWebData a -> Editable a
value (EditableWebData x _) =
    x


webDataValue : EditableWebData a -> WebData ()
webDataValue (EditableWebData _ x) =
    x


isDirtyWith : Editable record -> (record -> record -> Bool) -> Bool
isDirtyWith editableRecord compareFunc =
    case editableRecord of
        ReadOnly _ ->
            False

        Editable originalRecord updatedRecord ->
            compareFunc originalRecord originalRecord


isDirty : Editable record -> Bool
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
    EveryDictList ExaminationStorage (EditableWebData ExaminationChild)


type alias EveryDictListExaminationsMother =
    EveryDictList ExaminationStorage (EditableWebData ExaminationMother)
