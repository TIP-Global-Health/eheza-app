module App.Update exposing
    ( init
    , subscriptions
    , update
    )

import App.Fetch exposing (fetch)
import App.Model exposing (..)
import App.Types exposing (Language(..), Page(..))
import App.Utils exposing (updateSubModel)
import Backend.Model
import Backend.Update
import Json.Decode exposing (Value, decodeValue)
import Pages.Menu.Model
import Pages.Menu.Update
import Pages.Scoreboard.Model
import Pages.Scoreboard.Update
import Pages.Scoreboard.Utils exposing (..)
import Task
import Time


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        activePage =
            resolveActivePage flags.page

        model =
            { emptyModel
                | activePage = activePage
                , backend = updateBackendWithAppData activePage flags.appData emptyModel.backend
            }

        cmds =
            fetch model
                |> List.map (Task.succeed >> Task.perform identity)
                |> List.append [ Task.perform SetCurrentTime Time.now ]
                |> Cmd.batch
    in
    ( model
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


updateBackendWithAppData : Page -> Value -> Backend.Model.ModelBackend -> Backend.Model.ModelBackend
updateBackendWithAppData activePage appData modelBackend =
    case activePage of
        Menu ->
            -- No data is passed for this page.
            modelBackend

        Scoreboard ->
            -- @todo
            modelBackend

        NotFound ->
            modelBackend


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        MsgBackend subMsg ->
            updateSubModel
                subMsg
                model.backend
                (\subMsg_ subModel -> Backend.Update.updateBackend model.currentTime subMsg_ subModel)
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
                        model.backend
                        subMsg_
                        subModel
                )
                (\subModel model_ -> { model_ | scoreboardPage = subModel })
                (\subCmds -> MsgScoreboardPage subCmds)
                model

        SetActivePage activePage ->
            ( { model | activePage = activePage }
            , Cmd.none
            )

        SetCurrentTime date ->
            ( { model | currentTime = date }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Time.every 60000 SetCurrentTime
    Sub.none
