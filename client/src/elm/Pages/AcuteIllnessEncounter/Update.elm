module Pages.AcuteIllnessEncounter.Update exposing (update)

import App.Model
import Backend.AcuteIllnessEncounter.Model
import Backend.Model
import Pages.AcuteIllnessEncounter.Model exposing (..)
import Pages.Page exposing (Page(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CloseEncounter id ->
            ( { model | showEndEncounetrDialog = False }
            , Cmd.none
            , [ Backend.AcuteIllnessEncounter.Model.CloseAcuteIllnessEncounter
                    |> Backend.Model.MsgAcuteIllnessEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )

        SetAlertsDialogState isOpen ->
            ( { model | showAlertsDialog = isOpen }, Cmd.none, [] )

        SetEndEncounterDialogState isOpen ->
            ( { model | showEndEncounetrDialog = isOpen }, Cmd.none, [] )

        SetSelectedTab tab ->
            ( { model | selectedTab = tab }, Cmd.none, [] )
