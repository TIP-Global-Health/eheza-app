module Pages.ChildScoreboard.ProgressReport.Update exposing (update)

import App.Model
import Backend.ChildScoreboardEncounter.Model
import Backend.IndividualEncounterParticipant.Model exposing (IndividualParticipantInitiator(..))
import Backend.Model
import Components.ReportToWhatsAppDialog.Model
import Components.ReportToWhatsAppDialog.Update
import Gizra.Update exposing (sequenceExtra)
import Pages.ChildScoreboard.ProgressReport.Model exposing (Model, Msg(..))
import Pages.Page exposing (Page(..), UserPage(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        NoOp ->
            ( model
            , Cmd.none
            , []
            )

        CloseEncounter id ->
            ( model
            , Cmd.none
            , [ Backend.ChildScoreboardEncounter.Model.CloseChildScoreboardEncounter
                    |> Backend.Model.MsgChildScoreboardEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        ShowAIEncounterPopup ->
            ( { model | showAIEncounterPopup = True }
            , Cmd.none
            , []
            )

        SetDiagnosisMode mode ->
            ( { model | diagnosisMode = mode }, Cmd.none, [] )

        MsgReportToWhatsAppDialog subMsg ->
            let
                ( dialogUpdated, cmd, ( extraMsgs, appMsgs ) ) =
                    Components.ReportToWhatsAppDialog.Update.update subMsg model.reportToWhatsAppDialog
            in
            ( { model | reportToWhatsAppDialog = dialogUpdated }, cmd, appMsgs )
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
