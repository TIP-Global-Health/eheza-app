module App.Update exposing
    ( init
    , subscriptions
    , update
    )

import App.Fetch exposing (fetch)
import App.Model exposing (..)
import App.Types exposing (Page(..))
import App.Utils exposing (updateSubModel)
import Backend.Menu.Model
import Backend.Model
import Backend.Scoreboard.Model
import Backend.Update
import Gizra.NominalDate exposing (fromLocalDateTime)
import Pages.Menu.Update
import Pages.Scoreboard.Update
import Task
import Time


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        activePage =
            resolveActivePage flags.page

        model =
            { emptyModel | activePage = activePage }

        modelWithAppData =
            case model.activePage of
                Menu ->
                    update
                        (Backend.Menu.Model.SetData flags.appData
                            |> Backend.Model.MsgMenu
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
        "menu" ->
            Menu

        "results" ->
            Scoreboard

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

        MsgMenuPage subMsg ->
            updateSubModel
                subMsg
                model.menuPage
                (\subMsg_ subModel -> Pages.Menu.Update.update subMsg_ subModel)
                (\subModel model_ -> { model_ | menuPage = subModel })
                (\subCmds -> MsgMenuPage subCmds)
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

        SetCurrentTime date ->
            ( { model | currentTime = date }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
