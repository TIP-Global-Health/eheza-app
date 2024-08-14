module Backend.Types exposing (BackendReturn)

{-| The return value of Backend update functions
-}

import App.Model
import Backend.Model exposing (ModelBackend)
import Error.Model exposing (Error)


type alias BackendReturn msg =
    { model : ModelBackend
    , cmd : Cmd msg
    , error : Maybe Error
    , appMsgs : List App.Model.Msg
    }
