module Pages.NCD.ProgressReport.Update exposing (update)

import App.Model
import Backend.Model
import Backend.NCDEncounter.Model
import Pages.NCD.ProgressReport.Model exposing (..)
import Pages.Page exposing (Page(..))
import Pages.Report.Model exposing (LabResultsMode(..))


update : Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none, [] )

        CloseEncounter id ->
            ( { model | showEndEncounterDialog = False }
            , Cmd.none
            , [ Backend.NCDEncounter.Model.CloseNCDEncounter
                    |> Backend.Model.MsgNCDEncounter id
                    |> App.Model.MsgIndexedDb
              , App.Model.SetActivePage PinCodePage
              ]
            )

        SetActivePage page ->
            ( model, Cmd.none, [ App.Model.SetActivePage page ] )

        SetDiagnosisMode mode ->
            ( { model | diagnosisMode = mode }, Cmd.none, [] )

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

        SetEndEncounterDialogState isOpen ->
            ( { model | showEndEncounterDialog = isOpen }, Cmd.none, [] )
