module Pages.ProgressReport.Update exposing (update)

import App.Model
import Components.ReportToWhatsAppDialog.Model
import Components.ReportToWhatsAppDialog.Update
import Gizra.Update exposing (sequenceExtra)
import Pages.ProgressReport.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, [] )

        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )

        SetDiagnosisMode mode ->
            ( { model | diagnosisMode = mode }, Cmd.none, [] )

        MsgReportToWhatsAppDialog subMsg ->
            let
                ( dialogUpdated, cmd, ( extraMsgs, appMsgs ) ) =
                    Components.ReportToWhatsAppDialog.Update.update subMsg model.reportToWhatsAppDialog
            in
            ( { model | reportToWhatsAppDialog = dialogUpdated }
            , cmd
            , appMsgs
            )
                |> sequenceExtra update extraMsgs

        SetReportComponents maybeComponents ->
            let
                updatedModel =
                    Maybe.map
                        (\components ->
                            case components of
                                Components.ReportToWhatsAppDialog.Model.WellChild wellChildComponents ->
                                    { model | components = Just wellChildComponents }

                                -- We should never get here.
                                _ ->
                                    model
                        )
                        maybeComponents
                        |> Maybe.withDefault { model | components = Nothing }
            in
            ( updatedModel, Cmd.none, [] )

        SetReportTab tab ->
            ( { model | reportTab = tab }, Cmd.none, [] )
