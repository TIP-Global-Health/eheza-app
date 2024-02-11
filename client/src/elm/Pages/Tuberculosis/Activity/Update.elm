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
import Pages.Tuberculosis.Activity.Utils exposing (..)
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

        SetSymptomReviewBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.symptomReviewData.form

                updatedData =
                    model.symptomReviewData
                        |> (\data -> { data | form = updatedForm })
            in
            ( { model | symptomReviewData = updatedData }
            , Cmd.none
            , []
            )

        SaveSymptomReview personId saved ->
            let
                measurementId =
                    Maybe.map Tuple.first saved

                measurement =
                    getMeasurementValueFunc saved

                appMsgs =
                    model.symptomReviewData.form
                        |> toSymptomReviewValueWithDefault measurement
                        |> unwrap
                            []
                            (\value ->
                                [ Backend.TuberculosisEncounter.Model.SaveSymptomReview personId measurementId value
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

        SetActiveNextStepsTask task ->
            let
                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | activeTask = Just task })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetHealthEducationBoolInput formUpdateFunc value ->
            let
                updatedForm =
                    formUpdateFunc value model.nextStepsData.healthEducationForm

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | healthEducationForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )

        SetFollowUpOption option ->
            let
                form =
                    model.nextStepsData.followUpForm

                updatedForm =
                    { form | option = Just option }

                updatedData =
                    model.nextStepsData
                        |> (\data -> { data | followUpForm = updatedForm })
            in
            ( { model | nextStepsData = updatedData }
            , Cmd.none
            , []
            )
