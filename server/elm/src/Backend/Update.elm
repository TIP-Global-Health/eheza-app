module Backend.Update exposing (updateBackend)

import Backend.Model exposing (..)
import Backend.Types exposing (BackendReturn)
import Backend.Utils exposing (updateSubModel)
import Time


updateBackend : Time.Posix -> Msg -> ModelBackend -> BackendReturn Msg
updateBackend currentDate msg model =
    case msg of
        NoOp ->
            BackendReturn model Cmd.none Nothing []
