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


{-| A wrapper for `Editable`, that allows provides the means to track saving
back to the backend via `WebData`.
-}
type EditableWebData a
    = EditableWebData (Editable a) (WebData ())


{-| Creates a new `EditableWebData`.
-}
create : a -> EditableWebData a
create record =
    EditableWebData (Editable.ReadOnly record) NotAsked


{-| Pipes map to `Editable.map`
-}
map : (a -> a) -> EditableWebData a -> EditableWebData a
map f (EditableWebData editable webData) =
    EditableWebData (Editable.map f editable) webData


{-| Maps `Editable` functions. That is, unlike `map` it keeps the `Editable`
value wrapped.
-}
mapEditable : (Editable a -> Editable a) -> EditableWebData a -> EditableWebData a
mapEditable f (EditableWebData editable webData) =
    EditableWebData (f editable) webData


{-| Pipes update to `Editable.update`.
-}
update : a -> EditableWebData a -> EditableWebData a
update value =
    map (always value)


{-| Updates the `WebData` value.
-}
webDataUpdate : WebData () -> EditableWebData a -> EditableWebData a
webDataUpdate newWebData (EditableWebData editable webData) =
    EditableWebData editable newWebData


{-| Extracts the `Editable` value.
-}
value : EditableWebData a -> Editable a
value (EditableWebData x _) =
    x


{-| Extracts the `WebData` value.
-}
webDataValue : EditableWebData a -> WebData ()
webDataValue (EditableWebData _ x) =
    x
