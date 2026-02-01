module Pages.Tuberculosis.Activity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.TuberculosisActivity.Model exposing (TuberculosisActivity(..))
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Measurement.Model exposing (OngoingTreatmentReviewForm)
import Measurement.Utils
    exposing
        ( followUpFormWithDefault
        , ongoingTreatmentReviewFormWithDefault
        , sendToHCFormWithDefault
        , treatmentReviewInputsAndTasks
        )
import Measurement.View
    exposing
        ( viewFollowUpForm
        , viewSendToHealthCenterForm
        )
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Tuberculosis.Activity.Model exposing (DOTForm, DiagnosticsData, HealthEducationForm, MedicationData, MedicationTask(..), Model, Msg(..), NextStepsData, NextStepsTask(..), PrescribedMedicationForm, SymptomReviewData)
import Pages.Tuberculosis.Activity.Utils exposing (diagnosticsFormWithDefault, dotFormWithDefault, dotInputsAndTasks, expectMedicationTask, expectNextStepsTask, healthEducationFormInputsAndTasks, healthEducationFormWithDefault, medicationTaskCompleted, medicationTasks, medicationTasksCompletedFromTotal, nextStepsTaskCompleted, nextStepsTasks, nextStepsTasksCompletedFromTotal, prescribedMedicationFormWithDefault, prescribedMedicationsInputsAndTasks, symptomReviewFormWithDefault)
import Pages.Tuberculosis.Encounter.Model exposing (AssembledData)
import Pages.Tuberculosis.Encounter.Utils exposing (generateAssembledData)
import Pages.Utils
    exposing
        ( resolveActiveTask
        , resolveNextTask
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewConfirmationDialog
        , viewCustomBoolInput
        , viewPersonDetailsExtended
        , viewQuestionLabel
        , viewSaveAction
        , viewTasksCount
        )
import Translate exposing (Language, translate)
import Utils.Html exposing (viewModal)
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
    viewWebData language (viewHeaderAndContent language currentDate id activity model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> TuberculosisEncounterId -> TuberculosisActivity -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id activity model assembled =
    div [ class "page-activity tuberculosis" ]
        [ viewHeader language id activity
        , viewContent language currentDate activity model assembled
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


viewContent : Language -> NominalDate -> TuberculosisActivity -> Model -> AssembledData -> Html Msg
viewContent language currentDate activity model assembled =
    div [ class "ui unstackable items" ]
        ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
            :: viewActivity language currentDate activity assembled model
        )


viewActivity : Language -> NominalDate -> TuberculosisActivity -> AssembledData -> Model -> List (Html Msg)
viewActivity language currentDate activity assembled model =
    case activity of
        Diagnostics ->
            viewDiagnosticsContent language assembled model.diagnosticsData

        Medication ->
            viewMedicationContent language assembled model.medicationData

        SymptomReview ->
            viewSymptomReviewContent language assembled model.symptomReviewData

        NextSteps ->
            viewNextStepsContent language currentDate assembled model.nextStepsData


viewDiagnosticsContent : Language -> AssembledData -> DiagnosticsData -> List (Html Msg)
viewDiagnosticsContent language assembled data =
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
                                        False
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

        endEncounterDialog =
            if data.showEndEncounterDialog then
                Just <|
                    viewConfirmationDialog language
                        Translate.EndEncounterQuestion
                        Translate.EndEncounterNoTuberculosisDiagnosisPhrase
                        saveDiagnosticsMsg
                        (SetEndEncounterDialogState False)

            else
                Nothing

        saveAction =
            if form.diagnosed == Just False then
                SetEndEncounterDialogState True

            else
                saveDiagnosticsMsg

        saveDiagnosticsMsg =
            SaveDiagnostics assembled.participant.person assembled.encounter.participant assembled.measurements.diagnostics
    in
    [ viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form danger-signs" ] inputs
            ]
        , viewSaveAction language saveAction (tasksCompleted /= totalTasks)
        ]
    , viewModal endEncounterDialog
    ]


viewMedicationContent : Language -> AssembledData -> MedicationData -> List (Html Msg)
viewMedicationContent language assembled data =
    let
        measurements =
            assembled.measurements

        tasks =
            List.filter (expectMedicationTask assembled) medicationTasks

        activeTask =
            resolveActiveTask tasks data.activeTask

        viewTask task =
            let
                isCompleted =
                    medicationTaskCompleted assembled task

                iconClass =
                    case task of
                        TaskPrescribedMedication ->
                            "medication"

                        TaskDOT ->
                            "dot"

                        TaskTreatmentReview ->
                            "treatment-review"

                isActive =
                    activeTask == Just task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveMedicationTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.TuberculosisMedicationTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            List.map (\task -> ( task, medicationTasksCompletedFromTotal language assembled data task )) tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just TaskPrescribedMedication ->
                    getMeasurementValueFunc measurements.medication
                        |> prescribedMedicationFormWithDefault data.prescribedMedicationForm
                        |> viewPrescribedMedicationForm language assembled
                        |> List.singleton

                Just TaskDOT ->
                    getMeasurementValueFunc measurements.dot
                        |> dotFormWithDefault data.dotForm
                        |> viewDOTForm language assembled
                        |> List.singleton

                Just TaskTreatmentReview ->
                    getMeasurementValueFunc measurements.treatmentReview
                        |> ongoingTreatmentReviewFormWithDefault data.treatmentReviewForm
                        |> viewTreatmentReviewForm language
                        |> List.singleton

                Nothing ->
                    []

        actions =
            Maybe.map
                (\task ->
                    let
                        personId =
                            assembled.participant.person

                        nextTask =
                            resolveNextTask task tasksCompletedFromTotalDict tasksAfterSave

                        tasksAfterSave =
                            case task of
                                TaskPrescribedMedication ->
                                    -- DOT and Treatment Review review appear only after
                                    -- Prescribed Medication task is saved.
                                    [ TaskPrescribedMedication, TaskDOT, TaskTreatmentReview ]

                                _ ->
                                    tasks

                        saveMsg =
                            case task of
                                TaskPrescribedMedication ->
                                    SavePrescribedMedication personId measurements.medication nextTask

                                TaskDOT ->
                                    SaveDOT personId measurements.dot nextTask

                                TaskTreatmentReview ->
                                    SaveTreatmentReview personId measurements.treatmentReview nextTask

                        disabled =
                            tasksCompleted /= totalTasks
                    in
                    viewSaveAction language saveMsg disabled
                )
                activeTask
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui five column grid" ] <|
            List.map viewTask tasks
        ]
    , viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] (viewForm ++ [ actions ])
        ]
    ]


viewPrescribedMedicationForm : Language -> AssembledData -> PrescribedMedicationForm -> Html Msg
viewPrescribedMedicationForm language assembled form =
    prescribedMedicationsInputsAndTasks language assembled form
        |> Tuple.first
        |> div [ class "ui form prescribed-medication" ]


viewDOTForm : Language -> AssembledData -> DOTForm -> Html Msg
viewDOTForm language assembled form =
    let
        ( inputs, _ ) =
            dotInputsAndTasks language assembled form
    in
    div [ class "ui form dot" ]
        inputs


viewTreatmentReviewForm : Language -> OngoingTreatmentReviewForm -> Html Msg
viewTreatmentReviewForm language form =
    let
        ( inputs, _ ) =
            treatmentReviewInputsAndTasks language
                SetTreatmentReviewBoolInput
                SetReasonForNotTaking
                SetTotalMissedDoses
                SetAdverseEvent
                form
    in
    div [ class "ui form treatment-review" ]
        inputs


viewSymptomReviewContent : Language -> AssembledData -> SymptomReviewData -> List (Html Msg)
viewSymptomReviewContent language assembled data =
    let
        form =
            assembled.measurements.symptomReview
                |> getMeasurementValueFunc
                |> symptomReviewFormWithDefault data.form

        ( inputs, tasksCompleted, totalTasks ) =
            ( [ viewQuestionLabel language <| Translate.TuberculosisSymptomQuestion TuberculosisSymptomNightSweats
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
              , viewQuestionLabel language <| Translate.TuberculosisSymptomQuestion TuberculosisSymptomBloodInSputum
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
              , viewQuestionLabel language <| Translate.TuberculosisSymptomQuestion TuberculosisSymptomWeightLoss
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
              , viewQuestionLabel language <| Translate.TuberculosisSymptomQuestion TuberculosisSymptomSevereFatigue
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
    [ viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form symptom-review" ] inputs
            ]
        , viewSaveAction language
            (SaveSymptomReview assembled.participant.person assembled.measurements.symptomReview)
            (tasksCompleted /= totalTasks)
        ]
    ]


viewNextStepsContent : Language -> NominalDate -> AssembledData -> NextStepsData -> List (Html Msg)
viewNextStepsContent language currentDate assembled data =
    let
        measurements =
            assembled.measurements

        tasks =
            List.filter (expectNextStepsTask assembled) nextStepsTasks

        activeTask =
            resolveActiveTask tasks data.activeTask

        viewTask task =
            let
                isCompleted =
                    nextStepsTaskCompleted assembled task

                iconClass =
                    case task of
                        TaskHealthEducation ->
                            "next-steps-health-education"

                        TaskFollowUp ->
                            "next-steps-follow-up"

                        TaskReferral ->
                            "next-steps-send-to-hc"

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
                        |> List.singleton

                Just TaskFollowUp ->
                    getMeasurementValueFunc measurements.followUp
                        |> followUpFormWithDefault data.followUpForm
                        |> viewFollowUpForm language
                            [ OneDay, OneWeek, FollowUpNotNeeded ]
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

        actions =
            Maybe.map
                (\task ->
                    let
                        personId =
                            assembled.participant.person

                        nextTask =
                            resolveNextTask task tasksCompletedFromTotalDict tasks

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
                activeTask
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui five column grid" ] <|
            List.map viewTask tasks
        ]
    , viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] (viewForm ++ [ actions ])
        ]
    ]


viewHealthEducationForm : Language -> HealthEducationForm -> Html Msg
viewHealthEducationForm language form =
    let
        ( inputs, _ ) =
            healthEducationFormInputsAndTasks language form
    in
    div [ class "ui form health-education" ]
        inputs
