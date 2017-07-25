module Utils.EditableWebData
    exposing
        ( EditableWebData(..)
        , create
        , map
        , mapEditable
        , update
        , webDataUpdate
        , value
        , webDataValue
        )

import Editable exposing (Editable(..))
import RemoteData exposing (RemoteData(..), WebData)


type EditableWebData a
    = EditableWebData (Editable a) (WebData ())


create : a -> EditableWebData a
create record =
    EditableWebData (Editable.ReadOnly record) NotAsked


map : (a -> a) -> EditableWebData a -> EditableWebData a
map f (EditableWebData editable webData) =
    EditableWebData (Editable.map f editable) webData


{-| Map Editable functions.
-}
mapEditable : (Editable a -> Editable a) -> EditableWebData a -> EditableWebData a
mapEditable f (EditableWebData editable webData) =
    EditableWebData (f editable) webData


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
