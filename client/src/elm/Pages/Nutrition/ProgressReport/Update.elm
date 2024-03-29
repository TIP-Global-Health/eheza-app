module Pages.Nutrition.ProgressReport.Update exposing (update)

import App.Model
import Backend.Model
import Backend.NutritionEncounter.Model
import Components.ReportToWhatsAppDialog.Model
import Components.ReportToWhatsAppDialog.Update
import Gizra.Update exposing (sequenceExtra)
import Pages.Nutrition.ProgressReport.Model exposing (..)
import Pages.Page exposing (Page(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, [] )

        CloseEncounter id ->
            ( { model | showEndEncounterDialog = False }
            , Cmd.none
            , [ Backend.NutritionEncounter.Model.CloseNutritionEncounter
                    |> Backend.Model.MsgNutritionEncounter id
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
