module Pages.PrenatalEncounter.Update exposing (update)

import App.Model
import Backend.Model
import Backend.PrenatalEncounter.Model
import Pages.Page exposing (Page(..))
import Pages.PrenatalEncounter.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CloseEncounter id ->
            ( model
            , Cmd.none
            , [ Backend.PrenatalEncounter.Model.ClosePrenatalEncounter
                    |> Backend.Model.MsgPrenatalEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        CloseWarningPopup ->
            ( { model | warningPopupState = Nothing }, Cmd.none, [] )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetAlertsDialogState isOpen ->
            ( { model | showAlertsDialog = isOpen }, Cmd.none, [] )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )
