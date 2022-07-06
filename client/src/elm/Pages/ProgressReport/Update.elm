module Pages.ProgressReport.Update exposing (update)

import App.Model
import Backend.Model
import Components.SendViaWhatsAppDialog.Update
import Pages.Page exposing (Page(..))
import Pages.ProgressReport.Model exposing (..)
import Pages.Session.Model


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )

        SetDiagnosisMode mode ->
            ( { model | diagnosisMode = mode }, Cmd.none, [] )

        MsgSendViaWhatsAppDialog subMsg ->
            let
                ( dialogUpdated, appMsgs ) =
                    Components.SendViaWhatsAppDialog.Update.update subMsg model.sendViaWhatsAppDialog
            in
            ( { model | sendViaWhatsAppDialog = dialogUpdated }
            , Cmd.none
            , appMsgs
            )
