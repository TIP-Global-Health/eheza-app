module Pages.NCD.Encounter.Update exposing (update)

import App.Model
import App.Ports
import Backend.Model
import Backend.NCDActivity.Model exposing (NCDActivity(..))
import Backend.NCDEncounter.Model
import Pages.NCD.Encounter.Model exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CloseEncounter id ->
            ( model
            , Cmd.none
            , [ Backend.NCDEncounter.Model.CloseNCDEncounter
                    |> Backend.Model.MsgNCDEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )

        SetEndEncounterDialogState isOpen ->
            ( { model | showEndEncounterDialog = isOpen }, Cmd.none, [] )
