module Pages.PatientRecord.Update exposing (update)

import App.Model
import Backend.Entities exposing (..)
import Backend.PrenatalEncounter.Model exposing (PrenatalProgressReportInitiator(..))
import Components.SendViaWhatsAppDialog.Update
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PatientRecord.Model exposing (..)


update : NominalDate -> PersonId -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            , []
            )

        SetActivePage page ->
            ( { model | viewMode = ViewPatientRecord }
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetDiagnosisMode mode ->
            ( { model | diagnosisMode = mode }
            , Cmd.none
            , []
            )

        SetSPVReportTab tab ->
            ( { model | spvReportTab = tab }
            , Cmd.none
            , []
            )

        SetViewMode mode ->
            ( { model | viewMode = mode }
            , Cmd.none
            , []
            )

        SetFilter filter ->
            if filter == FilterDemographics then
                update currentDate id (SetActivePage <| UserPage <| DemographicsReportPage (InitiatorPatientRecord id) id) model

            else
                ( { model | filter = filter }
                , Cmd.none
                , []
                )

        MsgSendViaWhatsAppDialog subMsg ->
            let
                ( dialogUpdated, cmd, ( extraMsgs, appMsgs ) ) =
                    Components.SendViaWhatsAppDialog.Update.update subMsg model.reportToWhatsAppDialog
            in
            ( { model | reportToWhatsAppDialog = dialogUpdated }, cmd, appMsgs )
                |> sequenceExtra (update currentDate id) extraMsgs
