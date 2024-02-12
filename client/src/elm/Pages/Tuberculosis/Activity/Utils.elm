module Pages.Tuberculosis.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.TuberculosisActivity.Model exposing (TuberculosisActivity(..))
import Backend.TuberculosisActivity.Utils exposing (allActivities)
import Date
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe.Extra exposing (andMap, isJust, or, unwrap)
import Pages.Tuberculosis.Activity.Model exposing (..)
import Pages.Tuberculosis.Encounter.Model exposing (AssembledData)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeToBoolTask
        , maybeValueConsideringIsDirtyField
        , taskCompleted
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewNumberInput
        , viewQuestionLabel
        )
import Translate
import Translate.Model exposing (Language(..))


expectActivity : NominalDate -> AssembledData -> TuberculosisActivity -> Bool
expectActivity currentDate assembled activity =
    case activity of
        Diagnostics ->
            True

        Medication ->
            True

        SymptomReview ->
            True

        NextSteps ->
            mandatoryActivitiesForNextStepsCompleted currentDate assembled
                && (resolveNextStepsTasks currentDate assembled
                        |> List.isEmpty
                        |> not
                   )


activityCompleted : NominalDate -> AssembledData -> TuberculosisActivity -> Bool
activityCompleted currentDate assembled activity =
    let
        notExpected activityToCheck =
            not <| expectActivity currentDate assembled activityToCheck
    in
    case activity of
        Diagnostics ->
            notExpected Diagnostics
                || isJust assembled.measurements.diagnostics

        Medication ->
            notExpected Medication
                || isJust assembled.measurements.medication

        SymptomReview ->
            notExpected SymptomReview
                || isJust assembled.measurements.symptomReview

        NextSteps ->
            resolveNextStepsTasks currentDate assembled
                |> List.all (nextStepsTaskCompleted assembled)


resolveNextStepsTasks : NominalDate -> AssembledData -> List NextStepsTask
resolveNextStepsTasks currentDate assembled =
    List.filter (expectNextStepsTask currentDate assembled)
        [ TaskReferral, TaskHealthEducation, TaskFollowUp ]


expectNextStepsTask : NominalDate -> AssembledData -> NextStepsTask -> Bool
expectNextStepsTask currentDate assembled task =
    case task of
        _ ->
            -- @todo:
            True


nextStepsTaskCompleted : AssembledData -> NextStepsTask -> Bool
nextStepsTaskCompleted assembled task =
    case task of
        TaskReferral ->
            isJust assembled.measurements.referral

        TaskHealthEducation ->
            isJust assembled.measurements.healthEducation

        TaskFollowUp ->
            isJust assembled.measurements.followUp


mandatoryActivitiesForNextStepsCompleted : NominalDate -> AssembledData -> Bool
mandatoryActivitiesForNextStepsCompleted currentDate assembled =
    -- todo:
    True


diagnosticsFormWithDefault : DiagnosticsForm -> Maybe TuberculosisDiagnosticsValue -> DiagnosticsForm
diagnosticsFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    ( diagnosed, isPulmonary ) =
                        case value of
                            TuberculosisPulmonary ->
                                ( Just True, Just True )

                            TuberculosisExtrapulmonary ->
                                ( Just True, Just False )

                            NoTuberculosis ->
                                ( Just False, Nothing )
                in
                { diagnosed = or form.diagnosed diagnosed
                , isPulmonary =
                    maybeValueConsideringIsDirtyField form.isPulmonaryDirty
                        form.isPulmonary
                        isPulmonary
                , isPulmonaryDirty = form.isPulmonaryDirty
                }
            )


toDiagnosticsValueWithDefault : Maybe TuberculosisDiagnosticsValue -> DiagnosticsForm -> Maybe TuberculosisDiagnosticsValue
toDiagnosticsValueWithDefault saved form =
    diagnosticsFormWithDefault form saved
        |> toDiagnosticsValue


toDiagnosticsValue : DiagnosticsForm -> Maybe TuberculosisDiagnosticsValue
toDiagnosticsValue form =
    Maybe.andThen
        (\diagnosed ->
            if not diagnosed then
                Just NoTuberculosis

            else
                Maybe.map
                    (\isPulmonary ->
                        if isPulmonary then
                            TuberculosisPulmonary

                        else
                            TuberculosisExtrapulmonary
                    )
                    form.isPulmonary
        )
        form.diagnosed
