module Backend.Update exposing (updateBackend)

import Backend.Completion.Update
import Backend.CompletionMenu.Update
import Backend.Model exposing (..)
import Backend.Reports.Update
import Backend.ReportsMenu.Update
import Backend.Scoreboard.Update
import Backend.ScoreboardMenu.Update
import Backend.Types exposing (BackendReturn)
import Backend.Utils exposing (updateSubModel)
import Gizra.NominalDate exposing (NominalDate)


updateBackend : NominalDate -> String -> Msg -> ModelBackend -> BackendReturn Msg
updateBackend currentDate backendUrl msg model =
    case msg of
        MsgScoreboardMenu subMsg ->
            updateSubModel
                subMsg
                (\subMsg_ model_ -> Backend.ScoreboardMenu.Update.update currentDate subMsg_ model_)
                (\subCmds -> MsgScoreboardMenu subCmds)
                model

        MsgScoreboard subMsg ->
            updateSubModel
                subMsg
                (\subMsg_ model_ -> Backend.Scoreboard.Update.update currentDate backendUrl subMsg_ model_)
                (\subCmds -> MsgScoreboard subCmds)
                model

        MsgReportsMenu subMsg ->
            updateSubModel
                subMsg
                (\subMsg_ model_ -> Backend.ReportsMenu.Update.update currentDate subMsg_ model_)
                (\subCmds -> MsgReportsMenu subCmds)
                model

        MsgReports subMsg ->
            updateSubModel
                subMsg
                (\subMsg_ model_ -> Backend.Reports.Update.update currentDate backendUrl subMsg_ model_)
                (\subCmds -> MsgReports subCmds)
                model

        MsgCompletionMenu subMsg ->
            updateSubModel
                subMsg
                (\subMsg_ model_ -> Backend.CompletionMenu.Update.update currentDate subMsg_ model_)
                (\subCmds -> MsgCompletionMenu subCmds)
                model

        MsgCompletion subMsg ->
            updateSubModel
                subMsg
                (\subMsg_ model_ -> Backend.Completion.Update.update currentDate backendUrl subMsg_ model_)
                (\subCmds -> MsgCompletion subCmds)
                model
