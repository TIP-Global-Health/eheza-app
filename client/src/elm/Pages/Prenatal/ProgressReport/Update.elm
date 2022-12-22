module Pages.Prenatal.ProgressReport.Update exposing (update)

import App.Model
import Backend.Model
import Backend.PrenatalEncounter.Model
import Components.SendViaWhatsAppDialog.Model
import Components.SendViaWhatsAppDialog.Update
import Gizra.Update exposing (sequenceExtra)
import Pages.Page exposing (Page(..))
import Pages.Prenatal.ProgressReport.Model exposing (..)


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
            ( { model | labResultsMode = mode }, Cmd.none, [] )

        SetEndEncounterDialogState value ->
            ( { model | showEndEncounterDialog = value }, Cmd.none, [] )

        MsgSendViaWhatsAppDialog subMsg ->
            let
                ( dialogUpdated, cmd, ( extraMsgs, appMsgs ) ) =
                    Components.SendViaWhatsAppDialog.Update.update subMsg model.sendViaWhatsAppDialog
            in
            ( { model | sendViaWhatsAppDialog = dialogUpdated }, cmd, appMsgs )
                |> sequenceExtra update extraMsgs

        SetReportComponents maybeComponents ->
            let
                updatedModel =
                    Maybe.map
                        (\components ->
                            case components of
                                Components.SendViaWhatsAppDialog.Model.Antenatal antenatalComponents ->
                                    { model | components = Just antenatalComponents }

                                -- We should never get here.
                                Components.SendViaWhatsAppDialog.Model.WellChild _ ->
                                    model
                        )
                        maybeComponents
                        |> Maybe.withDefault { model | components = Nothing }
            in
            ( updatedModel, Cmd.none, [] )
