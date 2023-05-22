module Backend.Update exposing (updateBackend)

import Backend.Model exposing (..)
import Backend.Scoreboard.Update
import Backend.Types exposing (BackendReturn)
import Backend.Utils exposing (updateSubModel)


updateBackend : Msg -> ModelBackend -> BackendReturn Msg
updateBackend msg model =
    case msg of
        MsgScoreboard subMsg ->
            updateSubModel
                subMsg
                (\subMsg_ model_ -> Backend.Scoreboard.Update.update subMsg_ model_)
                (\subCmds -> MsgScoreboard subCmds)
                model
