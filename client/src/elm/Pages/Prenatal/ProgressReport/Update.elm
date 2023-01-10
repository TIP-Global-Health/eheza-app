module Pages.Prenatal.ProgressReport.Update exposing (update)

import App.Model
import Backend.Model
import Backend.PrenatalEncounter.Model
import Pages.Page exposing (Page(..))
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
