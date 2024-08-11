module Pages.Tuberculosis.ProgressReport.Update exposing (update)

import App.Model
import Backend.Model
import Backend.TuberculosisEncounter.Model
import Components.ReportToWhatsAppDialog.Model
import Components.ReportToWhatsAppDialog.Update
import Gizra.Update exposing (sequenceExtra)
import Pages.Page exposing (Page(..))
import Pages.Tuberculosis.ProgressReport.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, [] )

        CloseEncounter id ->
            ( { model | showEndEncounterDialog = False }
            , Cmd.none
            , [ Backend.TuberculosisEncounter.Model.CloseTuberculosisEncounter
                    |> Backend.Model.MsgTuberculosisEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )

        SetViewMode mode ->
            ( { model | viewMode = mode }, Cmd.none, [] )

        SetEndEncounterDialogState isOpen ->
            ( { model | showEndEncounterDialog = isOpen }, Cmd.none, [] )

        MsgReportToWhatsAppDialog subMsg ->
            let
                ( dialogUpdated, cmd, ( extraMsgs, appMsgs ) ) =
                    Components.ReportToWhatsAppDialog.Update.update subMsg model.reportToWhatsAppDialog
            in
            ( { model | reportToWhatsAppDialog = dialogUpdated }, cmd, appMsgs )
                |> sequenceExtra update extraMsgs
