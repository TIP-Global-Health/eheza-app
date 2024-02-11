module Pages.Tuberculosis.Activity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.TuberculosisActivity.Model exposing (TuberculosisActivity(..))
import Date
import EverySet
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Maybe.Extra exposing (isJust)
import Measurement.Utils exposing (followUpFormWithDefault, sendToHCFormWithDefault)
import Measurement.View
    exposing
        ( viewFollowUpForm
        , viewSendToHealthCenterForm
        )
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Tuberculosis.Activity.Model exposing (..)
import Pages.Tuberculosis.Activity.Utils exposing (..)
import Pages.Tuberculosis.Encounter.Model exposing (AssembledData)
import Pages.Tuberculosis.Encounter.Utils exposing (generateAssembledData)
import Pages.Utils
    exposing
        ( isTaskCompleted
        , resolveActiveTask
        , saveButton
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCustomBoolInput
        , viewPersonDetailsExtended
        , viewQuestionLabel
        , viewSaveAction
        )
import SyncManager.Model exposing (Site)
import Translate exposing (Language, translate)
import Utils.WebData exposing (viewWebData)


view :
    Language
    -> NominalDate
    -> TuberculosisEncounterId
    -> TuberculosisActivity
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate id activity db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id activity db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> TuberculosisEncounterId -> TuberculosisActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id activity db model assembled =
    div [ class "page-activity tuberculosis" ] <|
        [ viewHeader language id activity
        , viewContent language currentDate activity db model assembled
        ]


viewHeader : Language -> TuberculosisEncounterId -> TuberculosisActivity -> Html Msg
viewHeader language id activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.TuberculosisActivityTitle activity ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| TuberculosisEncounterPage id
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent : Language -> NominalDate -> TuberculosisActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate activity db model assembled =
    div [ class "ui unstackable items" ] <|
        ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
            :: viewActivity language currentDate activity assembled db model
        )


viewActivity : Language -> NominalDate -> TuberculosisActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate activity assembled db model =
    case activity of
        Diagnostics ->
            viewDiagnosticsContent language currentDate assembled model.diagnosticsData

        Medication ->
            -- @todo
            []

        SymptomReview ->
            viewSymptomReviewContent language currentDate assembled model.symptomReviewData

        NextSteps ->
            viewNextStepsContent language currentDate assembled db model.nextStepsData


viewDiagnosticsContent : Language -> NominalDate -> AssembledData -> DiagnosticsData -> List (Html Msg)
viewDiagnosticsContent language currentDate assembled data =
    let
        form =
            assembled.measurements.diagnostics
                |> getMeasurementValueFunc
                |> diagnosticsFormWithDefault data.form

        ( inputs, tasksCompleted, totalTasks ) =
            let
                ( derivedInputs, derivedTasksCompleted, derivedTotalTasks ) =
                    Maybe.map
                        (\diagnosed ->
                            if diagnosed then
                                ( [ viewQuestionLabel language Translate.TuberculosisLocationQuestion
                                  , viewCustomBoolInput language
                                        form.isPulmonary
                                        (SetDiagnosticsBoolInput
                                            (\value form_ ->
                                                { form_
                                                    | isPulmonary = Just value
                                                    , isPulmonaryDirty = True
                                                }
                                            )
                                        )
                                        "is-pulmonary"
                                        ( Translate.TuberculosisDiagnosis TuberculosisPulmonary
                                        , Translate.TuberculosisDiagnosis TuberculosisExtrapulmonary
                                        )
                                        "sixteen"
                                  ]
                                , taskCompleted form.isPulmonary
                                , 1
                                )

                            else
                                ( [], 0, 0 )
                        )
                        form.diagnosed
                        |> Maybe.withDefault ( [], 0, 0 )
            in
            ( [ viewQuestionLabel language Translate.TuberculosisDiagnosedQuestion
              , viewBoolInput
                    language
                    form.diagnosed
                    (SetDiagnosticsBoolInput
                        (\value form_ ->
                            { form_
                                | diagnosed = Just value
                                , isPulmonary = Nothing
                                , isPulmonaryDirty = True
                            }
                        )
                    )
                    "diagnosed"
                    Nothing
              ]
                ++ derivedInputs
            , taskCompleted form.diagnosed + derivedTasksCompleted
            , 1 + derivedTotalTasks
            )
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form danger-signs" ] inputs
            ]
        , div [ class "actions" ]
            [ saveButton language
                (tasksCompleted == totalTasks)
                (SaveDiagnostics assembled.participant.person assembled.measurements.diagnostics)
            ]
        ]
    ]


viewSymptomReviewContent : Language -> NominalDate -> AssembledData -> SymptomReviewData -> List (Html Msg)
viewSymptomReviewContent language currentDate assembled data =
    let
        form =
            assembled.measurements.symptomReview
                |> getMeasurementValueFunc
                |> symptomReviewFormWithDefault data.form

        ( inputs, tasksCompleted, totalTasks ) =
            ( [ viewQuestionLabel language <| Translate.TuberculosisSymptomQuestion SymptomNightSweats
              , viewBoolInput
                    language
                    form.nightSweats
                    (SetSymptomReviewBoolInput
                        (\value form_ ->
                            { form_ | nightSweats = Just value }
                        )
                    )
                    "night-sweats"
                    Nothing
              , viewQuestionLabel language <| Translate.TuberculosisSymptomQuestion SymptomBloodInSputum
              , viewBoolInput
                    language
                    form.bloodInSputum
                    (SetSymptomReviewBoolInput
                        (\value form_ ->
                            { form_ | bloodInSputum = Just value }
                        )
                    )
                    "blood-in-Sputum"
                    Nothing
              , viewQuestionLabel language <| Translate.TuberculosisSymptomQuestion SymptomWeightLoss
              , viewBoolInput
                    language
                    form.weightLoss
                    (SetSymptomReviewBoolInput
                        (\value form_ ->
                            { form_ | weightLoss = Just value }
                        )
                    )
                    "weight-loss"
                    Nothing
              , viewQuestionLabel language <| Translate.TuberculosisSymptomQuestion SymptomSevereFatigue
              , viewBoolInput
                    language
                    form.severeFatigue
                    (SetSymptomReviewBoolInput
                        (\value form_ ->
                            { form_ | severeFatigue = Just value }
                        )
                    )
                    "severe-fatigue"
                    Nothing
              ]
            , taskCompleted form.nightSweats
                + taskCompleted form.bloodInSputum
                + taskCompleted form.weightLoss
                + taskCompleted form.severeFatigue
            , 4
            )
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form danger-signs" ] inputs
            ]
        , div [ class "actions" ]
            [ saveButton language
                (tasksCompleted == totalTasks)
                (SaveSymptomReview assembled.participant.person assembled.measurements.symptomReview)
            ]
        ]
    ]


viewNextStepsContent : Language -> NominalDate -> AssembledData -> ModelIndexedDb -> NextStepsData -> List (Html Msg)
viewNextStepsContent language currentDate assembled db data =
    let
        measurements =
            assembled.measurements

        tasks =
            List.filter (expectNextStepsTask currentDate assembled) nextStepsTasks

        activeTask =
            resolveActiveTask tasks data.activeTask

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        TaskHealthEducation ->
                            ( "next-steps-health-education"
                            , isJust measurements.healthEducation
                            )

                        TaskFollowUp ->
                            ( "next-steps-follow-up"
                            , isJust measurements.followUp
                            )

                        TaskReferral ->
                            ( "next-steps-send-to-hc"
                            , isJust measurements.referral
                            )

                isActive =
                    activeTask == Just task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveNextStepsTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.TuberculosisNextStepsTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            List.map (\task -> ( task, nextStepsTasksCompletedFromTotal measurements data task )) tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just TaskHealthEducation ->
                    getMeasurementValueFunc measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> viewHealthEducationForm language
                            currentDate
                            assembled
                        |> List.singleton

                Just TaskFollowUp ->
                    getMeasurementValueFunc measurements.followUp
                        |> followUpFormWithDefault data.followUpForm
                        |> viewFollowUpForm language
                            currentDate
                            [ OneDay, OneWeek ]
                            SetFollowUpOption
                        |> List.singleton

                Just TaskReferral ->
                    getMeasurementValueFunc measurements.referral
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> viewSendToHealthCenterForm language
                            currentDate
                            SetReferToHealthCenter
                            SetReasonForNonReferral
                            SetHandReferralForm
                            Nothing
                        |> List.singleton

                Nothing ->
                    []

        nextTask =
            List.filter
                (\task ->
                    (Just task /= activeTask)
                        && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
                )
                tasks
                |> List.head

        actions =
            activeTask
                |> Maybe.map
                    (\task ->
                        let
                            personId =
                                assembled.participant.person

                            saveMsg =
                                case task of
                                    TaskHealthEducation ->
                                        SaveHealthEducation personId measurements.healthEducation nextTask

                                    TaskFollowUp ->
                                        SaveFollowUp personId measurements.followUp nextTask

                                    TaskReferral ->
                                        SaveReferral personId measurements.referral nextTask

                            disabled =
                                tasksCompleted /= totalTasks
                        in
                        viewSaveAction language saveMsg disabled
                    )
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui five column grid" ] <|
            List.map viewTask tasks
        ]
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            (viewForm ++ [ actions ])
        ]
    ]


viewHealthEducationForm : Language -> NominalDate -> AssembledData -> HealthEducationForm -> Html Msg
viewHealthEducationForm language currentDate assembled form =
    div [ class "ui form health-education" ]
        [ viewQuestionLabel language <| Translate.TuberculosisHealthEducationQuestion EducationFollowUpTesting
        , viewBoolInput
            language
            form.followUpTesting
            (SetHealthEducationBoolInput (\value form_ -> { form_ | followUpTesting = Just value }))
            "followup-testing"
            Nothing
        ]
