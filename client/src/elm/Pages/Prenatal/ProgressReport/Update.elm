module Pages.Prenatal.ProgressReport.Update exposing (update)

import App.Model
import Backend.Measurement.Model exposing (LabsResultsReviewState(..))
import Backend.Model
import Backend.PrenatalEncounter.Model
import Components.ReportToWhatsAppDialog.Model
import Components.ReportToWhatsAppDialog.Update
import Gizra.Update exposing (sequenceExtra)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.ProgressReport.Model exposing (..)
import Pages.Report.Model exposing (LabResultsMode(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        CloseEncounter id ->
            ( model
            , Cmd.none
            , [ Backend.PrenatalEncounter.Model.CloseEncounter
                    |> Backend.Model.MsgPrenatalEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )

        SetLabResultsMode mode ->
            let
                labResultsHistoryOrigin =
                    case mode of
                        Just (LabResultsHistory _) ->
                            model.labResultsMode

                        _ ->
                            Nothing
            in
            ( { model
                | labResultsMode = mode
                , labResultsHistoryOrigin = labResultsHistoryOrigin
              }
            , Cmd.none
            , []
            )

        SetEndEncounterDialogState value ->
            ( { model | showEndEncounterDialog = value }, Cmd.none, [] )

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
                                Components.ReportToWhatsAppDialog.Model.Antenatal antenatalComponents ->
                                    { model | components = Just antenatalComponents }

                                -- We should never get here.
                                _ ->
                                    model
                        )
                        maybeComponents
                        |> Maybe.withDefault { model | components = Nothing }
            in
            ( updatedModel, Cmd.none, [] )

        ReviewAndAcceptLabsResults personId encounterId labsResultsId value ->
            ( { model | labResultsMode = Nothing, labResultsHistoryOrigin = Nothing }
            , Cmd.none
            , [ Backend.PrenatalEncounter.Model.SaveLabsResults personId (Just labsResultsId) { value | reviewState = Just LabsResultsReviewCompleted }
                    |> Backend.Model.MsgPrenatalEncounter encounterId
                    |> App.Model.MsgIndexedDb
              ]
            )
                |> sequenceExtra update [ SetActivePage <| UserPage <| PrenatalRecurrentEncounterPage encounterId ]
