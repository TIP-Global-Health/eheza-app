module Pages.HIV.Activity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.HIVActivity.Model exposing (HIVActivity(..))
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, testResultToString)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getNCDEncountersForParticipant, getPrenatalEncountersForParticipant)
import Backend.Utils exposing (resolveIndividualParticipantsForPerson)
import Date exposing (Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Measurement.Model exposing (OngoingTreatmentReviewForm)
import Measurement.Utils
    exposing
        ( followUpFormWithDefault
        , ongoingTreatmentReviewFormWithDefault
        , sendToHCFormWithDefault
        , treatmentReviewCustomReasonsForNotTakingInputsAndTasks
        , viewSelectInput
        )
import Measurement.View
    exposing
        ( viewFollowUpForm
        , viewSendToHealthCenterForm
        )
import Pages.HIV.Activity.Model exposing (..)
import Pages.HIV.Activity.Utils exposing (..)
import Pages.HIV.Encounter.Model exposing (AssembledData)
import Pages.HIV.Encounter.Utils exposing (generateAssembledData)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils
    exposing
        ( resolveActiveTask
        , resolveNextTask
        , saveButton
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewEndEncounterDialog
        , viewLabel
        , viewPersonDetailsExtended
        , viewQuestionLabel
        , viewSaveAction
        )
import RemoteData
import Translate exposing (Language, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view :
    Language
    -> NominalDate
    -> HIVEncounterId
    -> HIVActivity
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate id activity db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id activity db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> HIVEncounterId -> HIVActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id activity db model assembled =
    div [ class "page-activity hiv" ] <|
        [ viewHeader language id activity
        , viewContent language currentDate activity db model assembled
        ]


viewHeader : Language -> HIVEncounterId -> HIVActivity -> Html Msg
viewHeader language id activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.HIVActivityTitle activity ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| HIVEncounterPage id
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent : Language -> NominalDate -> HIVActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate activity db model assembled =
    div [ class "ui unstackable items" ] <|
        ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
            :: viewActivity language currentDate activity assembled db model
        )


viewActivity : Language -> NominalDate -> HIVActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate activity assembled db model =
    case activity of
        Diagnostics ->
            viewDiagnosticsContent language currentDate assembled db model.diagnosticsData

        Medication ->
            viewMedicationContent language currentDate assembled model.medicationData

        SymptomReview ->
            viewSymptomReviewContent language currentDate assembled model.symptomReviewData

        NextSteps ->
            viewNextStepsContent language currentDate assembled model.nextStepsData


viewDiagnosticsContent : Language -> NominalDate -> AssembledData -> ModelIndexedDb -> DiagnosticsData -> List (Html Msg)
viewDiagnosticsContent language currentDate assembled db data =
    let
        form =
            assembled.measurements.diagnostics
                |> getMeasurementValueFunc
                |> diagnosticsFormWithDefault data.form

        personId =
            assembled.participant.person

        ncdParticipantsIds =
            resolveIndividualParticipantsForPerson personId NCDEncounter db

        prenatalParticipantsIds =
            resolveIndividualParticipantsForPerson personId AntenatalEncounter db

        ncdEncountersIds =
            List.concatMap (getNCDEncountersForParticipant db >> List.map Tuple.first) ncdParticipantsIds

        prenatalEncountersIds =
            List.concatMap (getPrenatalEncountersForParticipant db >> List.map Tuple.first) prenatalParticipantsIds

        resolvePositiveHIVResultDates getMeasurementsFunc =
            List.filterMap
                (\encounterId ->
                    getMeasurementsFunc db
                        |> Dict.get encounterId
                        |> Maybe.andThen RemoteData.toMaybe
                        |> Maybe.andThen .hivTest
                        |> getMeasurementValueFunc
                        |> Maybe.andThen
                            (\value ->
                                if value.testResult == Just TestPositive then
                                    value.executionDate

                                else
                                    Nothing
                            )
                )

        positiveHIVResultDatesFromNCD =
            resolvePositiveHIVResultDates .ncdMeasurements ncdEncountersIds

        positiveHIVResultDatesFromPrenatal =
            resolvePositiveHIVResultDates .prenatalMeasurements prenatalEncountersIds

        mPositiveHIVResultDate =
            positiveHIVResultDatesFromNCD
                ++ positiveHIVResultDatesFromPrenatal
                |> List.sortWith Date.compare
                |> List.head

        ( inputs, tasksCompleted, totalTasks ) =
            Maybe.map (resolveInputsAndTasksForExistingPositiveHIVResult language currentDate form) mPositiveHIVResultDate
                |> Maybe.withDefault (resolveInputsAndTasksForNonExistingPositiveHIVResult language currentDate form)

        endEncounterDialog =
            if data.showEndEncounterDialog then
                let
                    revertAnswerFunc =
                        if form.runHIVTest == Just False then
                            -- Case where patient does not want to run HIV test.
                            \form_ ->
                                { form_
                                    | runHIVTest = Nothing
                                    , runHIVTestDirty = True
                                }

                        else
                            -- Case where patient has run HIV test and did not
                            -- get positive diagnosis.
                            \form_ ->
                                { form_
                                    | testResult = Nothing
                                    , testResultDirty = True
                                }
                in
                Just <|
                    viewEndEncounterDialog language
                        Translate.EndEncounterQuestion
                        Translate.EndEncounterNoHIVDiagnosisPhrase
                        saveDiagnosticsMsg
                        (SetEndEncounterDialogState False (Just revertAnswerFunc))

            else
                Nothing

        saveAction =
            if endEncounterDialogRequired then
                SetEndEncounterDialogState True Nothing

            else
                saveDiagnosticsMsg

        -- Double check that patient does not have (or unable to resolve)
        -- HIV diagnosis before closing the encounter.
        endEncounterDialogRequired =
            (form.runHIVTest == Just False)
                || (form.testResult == Just TestNegative)
                || (form.testResult == Just TestIndeterminate)

        saveDiagnosticsMsg =
            SaveDiagnostics assembled.participant.person
                assembled.encounter.participant
                (isJust mPositiveHIVResultDate)
                assembled.measurements.diagnostics
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form danger-signs" ] inputs
            ]
        , div [ class "actions" ]
            [ saveButton language
                (tasksCompleted == totalTasks)
                saveAction
            ]
        ]
    , viewModal endEncounterDialog
    ]


resolveInputsAndTasksForExistingPositiveHIVResult : Language -> NominalDate -> DiagnosticsForm -> NominalDate -> ( List (Html Msg), Int, Int )
resolveInputsAndTasksForExistingPositiveHIVResult language currentDate form positiveHIVResultDate =
    let
        ( derivedInputs, derivedTasksCompleted, derivedTotalTasks ) =
            Maybe.map
                (\resultDateCorrect ->
                    if not resultDateCorrect then
                        resolveInputsAndTasksForPositiveHIVDate language currentDate form

                    else
                        ( [], 0, 0 )
                )
                form.resultDateCorrect
                |> Maybe.withDefault ( [], 0, 0 )
    in
    ( [ viewQuestionLabel language <| Translate.HIVPositiveDateCorrectQuestion positiveHIVResultDate
      , viewBoolInput
            language
            form.resultDateCorrect
            (ConfirmPositiveResultDate positiveHIVResultDate)
            "result-date-correct"
            Nothing
      ]
        ++ derivedInputs
    , taskCompleted form.resultDateCorrect + derivedTasksCompleted
    , 1 + derivedTotalTasks
    )


resolveInputsAndTasksForNonExistingPositiveHIVResult : Language -> NominalDate -> DiagnosticsForm -> ( List (Html Msg), Int, Int )
resolveInputsAndTasksForNonExistingPositiveHIVResult language currentDate form =
    let
        ( derivedInputs, derivedTasksCompleted, derivedTotalTasks ) =
            Maybe.map
                (\resultPositive ->
                    if resultPositive then
                        resolveInputsAndTasksForPositiveHIVDate language currentDate form

                    else
                        resolveInputsAndTasksForSuggestedHIVTest language currentDate form
                )
                form.resultPositive
                |> Maybe.withDefault ( [], 0, 0 )
    in
    ( [ viewQuestionLabel language Translate.HIVPositiveDiagnosedQuestion
      , viewBoolInput
            language
            form.resultPositive
            (SetDiagnosticsBoolInput
                (\value form_ ->
                    { form_
                        | resultPositive = Just value
                        , positiveResultDate = Nothing
                        , positiveResultDateDirty = True
                        , positiveResultDateEstimated = Nothing
                        , positiveResultDateEstimatedDirty = True
                        , runHIVTest = Nothing
                        , runHIVTestDirty = True
                        , testResult = Nothing
                        , testResultDirty = True
                    }
                )
            )
            "result-positive"
            Nothing
      ]
        ++ derivedInputs
    , taskCompleted form.resultPositive + derivedTasksCompleted
    , 1 + derivedTotalTasks
    )


resolveInputsAndTasksForPositiveHIVDate : Language -> NominalDate -> DiagnosticsForm -> ( List (Html Msg), Int, Int )
resolveInputsAndTasksForPositiveHIVDate language currentDate form =
    let
        dateForView =
            Maybe.map formatDDMMYYYY form.positiveResultDate
                |> Maybe.withDefault ""

        estimatedChecked =
            Maybe.withDefault False form.positiveResultDateEstimated

        dateSelectorConfig =
            { select = SetPositiveResultDate
            , close = SetDateSelectorState Nothing
            , dateFrom = Date.add Years -120 currentDate
            , dateTo = currentDate
            , dateDefault = Nothing
            }
    in
    ( [ div [ class "ui grid" ]
            [ div [ class "twelve wide column required" ]
                [ viewQuestionLabel language Translate.HIVPositiveTestDateQuestion
                , div
                    [ class "form-input date"
                    , onClick <| SetDateSelectorState (Just dateSelectorConfig)
                    ]
                    [ text dateForView ]
                ]
            , div
                [ class "three wide column" ]
                [ viewLabel language Translate.Estimated
                , input
                    [ type_ "checkbox"
                    , onClick
                        (SetDiagnosticsBoolInput
                            (\value form_ ->
                                { form_
                                    | positiveResultDateEstimated = Just value
                                    , positiveResultDateEstimatedDirty = True
                                }
                            )
                            (not estimatedChecked)
                        )
                    , checked estimatedChecked
                    , classList
                        [ ( "checkbox", True )
                        , ( "checked", estimatedChecked )
                        ]
                    ]
                    []
                ]
            , viewModal <| viewCalendarPopup language form.dateSelectorPopupState form.positiveResultDate
            ]
      ]
    , taskCompleted form.positiveResultDate
    , 1
    )


resolveInputsAndTasksForSuggestedHIVTest : Language -> NominalDate -> DiagnosticsForm -> ( List (Html Msg), Int, Int )
resolveInputsAndTasksForSuggestedHIVTest language currentDate form =
    let
        ( derivedInputs, derivedTasksCompleted, derivedTotalTasks ) =
            Maybe.map
                (\runHIVTest ->
                    if runHIVTest then
                        ( viewSelectInput language
                            Translate.Result
                            form.testResult
                            Translate.TestResult
                            testResultToString
                            [ TestPositive, TestNegative, TestIndeterminate ]
                            SetHIVTestResult
                        , taskCompleted form.testResult
                        , 1
                        )

                    else
                        ( [], 0, 0 )
                )
                form.runHIVTest
                |> Maybe.withDefault ( [], 0, 0 )
    in
    ( [ viewQuestionLabel language Translate.HIVSuggestTakingTestQuestion
      , viewBoolInput
            language
            form.runHIVTest
            (SetDiagnosticsBoolInput
                (\value form_ ->
                    { form_
                        | runHIVTest = Just value
                        , runHIVTestDirty = True
                        , testResult = Nothing
                        , testResultDirty = True
                        , positiveResultDate = Nothing
                        , positiveResultDateDirty = True
                        , positiveResultDateEstimated = Nothing
                        , positiveResultDateEstimatedDirty = True
                    }
                )
            )
            "run-hiv-test"
            Nothing
      ]
        ++ derivedInputs
    , taskCompleted form.runHIVTest + derivedTasksCompleted
    , 1 + derivedTotalTasks
    )


viewMedicationContent : Language -> NominalDate -> AssembledData -> MedicationData -> List (Html Msg)
viewMedicationContent language currentDate assembled data =
    let
        measurements =
            assembled.measurements

        tasks =
            List.filter (expectMedicationTask currentDate assembled) medicationTasks

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
                    , text <| translate language (Translate.HIVMedicationTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            List.map (\task -> ( task, medicationTasksCompletedFromTotal language currentDate assembled data task )) tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just TaskPrescribedMedication ->
                    getMeasurementValueFunc measurements.medication
                        |> prescribedMedicationFormWithDefault data.prescribedMedicationForm
                        |> viewPrescribedMedicationForm language currentDate assembled
                        |> List.singleton

                Just TaskTreatmentReview ->
                    getMeasurementValueFunc measurements.treatmentReview
                        |> ongoingTreatmentReviewFormWithDefault data.treatmentReviewForm
                        |> viewTreatmentReviewForm language currentDate
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
                                    [ TaskPrescribedMedication, TaskTreatmentReview ]

                                _ ->
                                    tasks

                        saveMsg =
                            case task of
                                TaskPrescribedMedication ->
                                    SavePrescribedMedication personId measurements.medication nextTask

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
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            (viewForm ++ [ actions ])
        ]
    ]


viewPrescribedMedicationForm : Language -> NominalDate -> AssembledData -> PrescribedMedicationForm -> Html Msg
viewPrescribedMedicationForm language currentDate assembled form =
    prescribedMedicationsInputsAndTasks language currentDate assembled form
        |> Tuple.first
        |> div [ class "ui form prescribed-medication" ]


viewTreatmentReviewForm : Language -> NominalDate -> OngoingTreatmentReviewForm -> Html Msg
viewTreatmentReviewForm language currentDate form =
    let
        ( inputs, _ ) =
            treatmentReviewCustomReasonsForNotTakingInputsAndTasks language
                currentDate
                ( [ NotTakingAdverseEvent, NotTakingNoMoney, NotTakingTreatmentNotStarted ]
                , [ NotTakingMemoryProblems, NotTakingOther ]
                )
                SetTreatmentReviewBoolInput
                SetReasonForNotTaking
                SetTotalMissedDoses
                SetAdverseEvent
                form
    in
    div [ class "ui form treatment-review" ]
        inputs


viewSymptomReviewContent : Language -> NominalDate -> AssembledData -> SymptomReviewData -> List (Html Msg)
viewSymptomReviewContent language currentDate assembled data =
    let
        form =
            assembled.measurements.symptomReview
                |> getMeasurementValueFunc
                |> symptomReviewFormWithDefault data.form

        ( inputs, tasksCompleted, totalTasks ) =
            ( [ viewQuestionLabel language Translate.HIVSymptomReviewQuestion
              , viewCheckBoxMultipleSelectInput language
                    [ HIVSymptomFever
                    , HIVSymptomFatigue
                    , HIVSymptomSwollenLymphNodes
                    , HIVSymptomSoreThroat
                    , HIVSymptomRash
                    , HIVSymptomMuscleJointPain
                    , HIVSymptomHeadache
                    , HIVSymptomSevereAbdominalPain
                    ]
                    [ HIVSymptomNightSweats
                    , HIVSymptomDiarrhea
                    , HIVSymptomWeightLoss
                    , HIVSymptomCoughingUpBlood
                    , HIVSymptomHairLoss
                    , HIVSymptomMouthUlcers
                    , HIVSymptomDifficultyBreathing
                    , HIVSymptomVomiting
                    ]
                    (Maybe.withDefault [] form.symptoms)
                    (Just NoHIVSymptoms)
                    SetSymptom
                    Translate.HIVSymptom
              ]
            , taskCompleted form.symptoms
            , 1
            )
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form symptom-review" ] inputs
            ]
        , div [ class "actions" ]
            [ saveButton language
                (tasksCompleted == totalTasks)
                (SaveSymptomReview assembled.participant.person assembled.measurements.symptomReview)
            ]
        ]
    ]


viewNextStepsContent : Language -> NominalDate -> AssembledData -> NextStepsData -> List (Html Msg)
viewNextStepsContent language currentDate assembled data =
    let
        measurements =
            assembled.measurements

        tasks =
            List.filter (expectNextStepsTask currentDate assembled) nextStepsTasks

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
                    , text <| translate language (Translate.HIVNextStepsTask task)
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
                            [ OneDay, OneWeek, OneMonth, FollowUpNotNeeded ]
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
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            (viewForm ++ [ actions ])
        ]
    ]


viewHealthEducationForm : Language -> NominalDate -> AssembledData -> HealthEducationForm -> Html Msg
viewHealthEducationForm language currentDate assembled form =
    div [ class "ui form health-education" ]
        [ viewQuestionLabel language <| Translate.HIVHealthEducationQuestion EducationPositiveResult
        , viewBoolInput
            language
            form.positiveResult
            (SetHealthEducationBoolInput (\value form_ -> { form_ | positiveResult = Just value }))
            "positive-result"
            Nothing
        , viewQuestionLabel language <| Translate.HIVHealthEducationQuestion EducationSaferSexPractices
        , viewBoolInput
            language
            form.saferSexPractices
            (SetHealthEducationBoolInput (\value form_ -> { form_ | saferSexPractices = Just value }))
            "safer-sex-practices"
            Nothing
        , viewQuestionLabel language <| Translate.HIVHealthEducationQuestion EducationEncouragedPartnerTesting
        , viewBoolInput
            language
            form.encouragedPartnerTesting
            (SetHealthEducationBoolInput (\value form_ -> { form_ | encouragedPartnerTesting = Just value }))
            "encouraged-partner-testing"
            Nothing
        , viewQuestionLabel language <| Translate.HIVHealthEducationQuestion EducationFamilyPlanningOptions
        , viewBoolInput
            language
            form.familyPlanningOptions
            (SetHealthEducationBoolInput (\value form_ -> { form_ | familyPlanningOptions = Just value }))
            "family-planning-options"
            Nothing
        ]
