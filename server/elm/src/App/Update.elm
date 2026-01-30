module App.Update exposing
    ( init
    , subscriptions
    , update
    )

import App.Fetch exposing (fetch)
import App.Model exposing (Flags, Model, Msg(..), emptyModel)
import App.Types exposing (Page(..))
import App.Utils exposing (updateSubModel)
import Backend.Completion.Model
import Backend.CompletionMenu.Model
import Backend.Model
import Backend.Reports.Model
import Backend.ReportsMenu.Model
import Backend.Scoreboard.Model
import Backend.ScoreboardMenu.Model
import Backend.Update
import Gizra.NominalDate exposing (fromLocalDateTime)
import Pages.Completion.Update
import Pages.CompletionMenu.Update
import Pages.Reports.Update
import Pages.ReportsMenu.Update
import Pages.Scoreboard.Update
import Pages.ScoreboardMenu.Update
import Task
import Time


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        activePage =
            resolveActivePage flags.page

        model =
            { emptyModel | activePage = activePage, themePath = flags.themePath }

        modelWithAppData =
            case model.activePage of
                ScoreboardMenu ->
                    update
                        (Backend.ScoreboardMenu.Model.SetData flags.appData
                            |> Backend.Model.MsgScoreboardMenu
                            |> MsgBackend
                        )
                        model
                        |> Tuple.first

                Scoreboard ->
                    update
                        (Backend.Scoreboard.Model.SetData flags.appData
                            |> Backend.Model.MsgScoreboard
                            |> MsgBackend
                        )
                        model
                        |> Tuple.first

                ReportsMenu ->
                    update
                        (Backend.ReportsMenu.Model.SetData flags.appData
                            |> Backend.Model.MsgReportsMenu
                            |> MsgBackend
                        )
                        model
                        |> Tuple.first

                Reports ->
                    update
                        (Backend.Reports.Model.SetData flags.appData
                            |> Backend.Model.MsgReports
                            |> MsgBackend
                        )
                        model
                        |> Tuple.first

                CompletionMenu ->
                    update
                        (Backend.CompletionMenu.Model.SetData flags.appData
                            |> Backend.Model.MsgCompletionMenu
                            |> MsgBackend
                        )
                        model
                        |> Tuple.first

                Completion ->
                    update
                        (Backend.Completion.Model.SetData flags.appData
                            |> Backend.Model.MsgCompletion
                            |> MsgBackend
                        )
                        model
                        |> Tuple.first

                NotFound ->
                    model

        cmds =
            fetch modelWithAppData
                |> List.map (Task.succeed >> Task.perform identity)
                |> List.append [ Task.perform SetCurrentTime Time.now ]
                |> Cmd.batch
    in
    ( modelWithAppData
      -- Let the Fetcher act upon the active page.
    , cmds
    )


resolveActivePage : String -> Page
resolveActivePage page =
    case page of
        "ncda-menu" ->
            ScoreboardMenu

        "ncda-results" ->
            Scoreboard

        "reports-menu" ->
            ReportsMenu

        "reports-results" ->
            Reports

        "completion-menu" ->
            CompletionMenu

        "completion-results" ->
            Completion

        _ ->
            NotFound


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MsgBackend subMsg ->
            updateSubModel
                subMsg
                model.backend
                (\subMsg_ subModel -> Backend.Update.updateBackend (fromLocalDateTime model.currentTime) subMsg_ subModel)
                (\subModel model_ -> { model_ | backend = subModel })
                (\subCmds -> MsgBackend subCmds)
                model

        MsgScoreboardMenuPage subMsg ->
            updateSubModel
                subMsg
                model.scoreboardMenuPage
                (\subMsg_ subModel -> Pages.ScoreboardMenu.Update.update subMsg_ subModel)
                (\subModel model_ -> { model_ | scoreboardMenuPage = subModel })
                (\subCmds -> MsgScoreboardMenuPage subCmds)
                model

        MsgScoreboardPage subMsg ->
            updateSubModel
                subMsg
                model.scoreboardPage
                (\subMsg_ subModel ->
                    Pages.Scoreboard.Update.update
                        subMsg_
                        subModel
                )
                (\subModel model_ -> { model_ | scoreboardPage = subModel })
                (\subCmds -> MsgScoreboardPage subCmds)
                model

        MsgReportsMenuPage subMsg ->
            updateSubModel
                subMsg
                model.reportsMenuPage
                (\subMsg_ subModel -> Pages.ReportsMenu.Update.update subMsg_ subModel)
                (\subModel model_ -> { model_ | reportsMenuPage = subModel })
                (\subCmds -> MsgReportsMenuPage subCmds)
                model

        MsgReportsPage subMsg ->
            updateSubModel
                subMsg
                model.reportsPage
                (\subMsg_ subModel ->
                    Pages.Reports.Update.update
                        (fromLocalDateTime model.currentTime)
                        model.backend
                        subMsg_
                        subModel
                )
                (\subModel model_ -> { model_ | reportsPage = subModel })
                (\subCmds -> MsgReportsPage subCmds)
                model

        MsgCompletionMenuPage subMsg ->
            updateSubModel
                subMsg
                model.completionMenuPage
                (\subMsg_ subModel -> Pages.CompletionMenu.Update.update subMsg_ subModel)
                (\subModel model_ -> { model_ | completionMenuPage = subModel })
                (\subCmds -> MsgCompletionMenuPage subCmds)
                model

        MsgCompletionPage subMsg ->
            updateSubModel
                subMsg
                model.completionPage
                (\subMsg_ subModel ->
                    Pages.Completion.Update.update
                        subMsg_
                        subModel
                )
                (\subModel model_ -> { model_ | completionPage = subModel })
                (\subCmds -> MsgCompletionPage subCmds)
                model

        SetCurrentTime date ->
            ( { model | currentTime = date }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
