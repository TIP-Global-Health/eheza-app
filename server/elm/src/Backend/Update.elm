module Backend.Update exposing (updateBackend)

import Backend.Model exposing (..)
import Backend.Scoreboard.Update
import Backend.Types exposing (BackendReturn)
import Backend.Utils exposing (updateSubModel)
import Gizra.NominalDate exposing (NominalDate)


updateBackend : NominalDate -> Msg -> ModelBackend -> BackendReturn Msg
updateBackend currentDate msg model =
    case msg of
        MsgScoreboard subMsg ->
            updateSubModel
                subMsg
                (\subMsg_ model_ -> Backend.Scoreboard.Update.update currentDate subMsg_ model_)
                (\subCmds -> MsgScoreboard subCmds)
                model
