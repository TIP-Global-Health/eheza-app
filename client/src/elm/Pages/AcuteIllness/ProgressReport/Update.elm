module Pages.AcuteIllness.ProgressReport.Update exposing (update)

import App.Model
import Backend.AcuteIllnessEncounter.Model
import Backend.Model
import Components.SendViaWhatsAppDialog.Model
import Components.SendViaWhatsAppDialog.Update
import Gizra.Update exposing (sequenceExtra)
import Pages.AcuteIllness.ProgressReport.Model exposing (..)
import Pages.Page exposing (Page(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CloseEncounter id ->
            ( { model | showEndEncounterDialog = False }
            , Cmd.none
            , [ Backend.AcuteIllnessEncounter.Model.CloseAcuteIllnessEncounter
                    |> Backend.Model.MsgAcuteIllnessEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )

        SetEndEncounterDialogState isOpen ->
            ( { model | showEndEncounterDialog = isOpen }, Cmd.none, [] )

        MsgSendViaWhatsAppDialog subMsg ->
            let
                ( dialogUpdated, extraMsgs, appMsgs ) =
                    Components.SendViaWhatsAppDialog.Update.update subMsg model.sendViaWhatsAppDialog
            in
            ( { model | sendViaWhatsAppDialog = dialogUpdated }, Cmd.none, appMsgs )
                |> sequenceExtra update extraMsgs
