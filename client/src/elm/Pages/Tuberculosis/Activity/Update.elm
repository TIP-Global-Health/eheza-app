module Pages.Tuberculosis.Activity.Update exposing (update)

import App.Model
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.TuberculosisEncounter.Model
import Date
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Maybe.Extra exposing (unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Tuberculosis.Activity.Model exposing (..)
import Pages.Tuberculosis.Activity.Utils exposing (toDiagnosticsValueWithDefault)
import RemoteData


update : NominalDate -> TuberculosisEncounterId -> ModelIndexedDb -> Msg -> Model -> ( Model, Cmd Msg, List App.Model.Msg )
update currentDate id db msg model =
    case msg of
        SetActivePage page ->
            ( model
            , Cmd.none
            , [ App.Model.SetActivePage page ]
            )

        SetDiagnosticsBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.diagnosticsData.form

                updatedData =
                    model.diagnosticsData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | diagnosticsData = updatedData }
            , Cmd.none
            , []
            )

        SaveDiagnostics personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.diagnosticsData.form
                        |> toDiagnosticsValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.TuberculosisEncounter.Model.SaveDiagnostics personId measurementId value
                                    |> Backend.Model.MsgTuberculosisEncounter id
                                    |> App.Model.MsgIndexedDb
                                , App.Model.SetActivePage <| UserPage <| TuberculosisEncounterPage id
                                ]
                            )
            in
            ( model
            , Cmd.none
            , appMsgs
            )
