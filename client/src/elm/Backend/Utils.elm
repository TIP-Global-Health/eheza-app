module Backend.Utils exposing (..)

import Backend.Session.Model exposing (EditableSession)
import Backend.Entities exposing (..)
import Backend.Model exposing (..)
import Maybe.Extra
import RemoteData


{-| Our editable session is inside a `RemoteData` and a `Maybe`, so it's
convenient to be able to unwrap it without too much verbosity.

Given the model, we apply your function to the editable session. If we don't
have an editable session (i.e. NotAsked or Nothing), we use the default instead
(your first parameter).

TODO: The fact we need this suggests that perhaps the types could be better
arranged. Or, perhaps this is the best we can do.

-}
withEditableSession : a -> (SessionId -> EditableSession -> a) -> ModelCached -> a
withEditableSession default func model =
    model.editableSession
        |> RemoteData.toMaybe
        |> Maybe.Extra.join
        |> Maybe.map (uncurry func)
        |> Maybe.withDefault default
