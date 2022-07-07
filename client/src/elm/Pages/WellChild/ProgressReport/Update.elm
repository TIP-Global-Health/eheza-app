module Pages.WellChild.ProgressReport.Update exposing (update)

import App.Model
import Backend.Model
import Backend.WellChildEncounter.Model
import Components.SendViaWhatsAppDialog.Model
import Components.SendViaWhatsAppDialog.Update
import Pages.Page exposing (Page(..))
import Pages.WellChild.ProgressReport.Model exposing (..)


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, [] )

        CloseEncounter id ->
            ( { model | showEndEncounterDialog = False }
            , Cmd.none
            , [ Backend.WellChildEncounter.Model.CloseWellChildEncounter
                    |> Backend.Model.MsgWellChildEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )

        SetEndEncounterDialogState isOpen ->
            ( { model | showEndEncounterDialog = isOpen }, Cmd.none, [] )

        SetDiagnosisMode mode ->
            ( { model | diagnosisMode = mode }, Cmd.none, [] )

        MsgSendViaWhatsAppDialog subMsg ->
            let
                ( dialogUpdated, appMsgs ) =
                    Components.SendViaWhatsAppDialog.Update.update subMsg model.sendViaWhatsAppDialog
            in
            ( { model | sendViaWhatsAppDialog = dialogUpdated }, Cmd.none, appMsgs )

        SetReportComponents maybeComponents ->
            let
                updatedModel =
                    Maybe.map
                        (\components ->
                            case components of
                                Components.SendViaWhatsAppDialog.Model.WellChild wellChildComponents ->
                                    { model | components = Just wellChildComponents }

                                -- We should never get here.
                                Components.SendViaWhatsAppDialog.Model.Antnatal _ ->
                                    model
                        )
                        maybeComponents
                        |> Maybe.withDefault { model | components = Nothing }
            in
            ( updatedModel, Cmd.none, [] )
