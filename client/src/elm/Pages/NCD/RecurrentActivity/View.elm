module Pages.NCD.RecurrentActivity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model exposing (NCDRecurrentActivity(..))
import Backend.NCDActivity.Utils exposing (getActivityIcon)
import Backend.NCDEncounter.Model exposing (NCDEncounter)
import Backend.Person.Model exposing (Person)
import Date exposing (Unit(..))
import EverySet
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Model exposing (InvokationModule(..), LaboratoryTask(..))
import Measurement.Utils
    exposing
        ( creatinineResultFormAndTasks
        , creatinineResultFormWithDefault
        , laboratoryTaskIconClass
        , liverFunctionResultFormAndTasks
        , liverFunctionResultFormWithDefault
        , randomBloodSugarResultFormAndTasks
        , randomBloodSugarResultFormWithDefault
        , urineDipstickResultFormAndTasks
        , urineDipstickResultFormWithDefault
        )
import Measurement.View exposing (viewSendToHospitalForm)
import Pages.NCD.Model exposing (..)
import Pages.NCD.RecurrentActivity.Model exposing (..)
import Pages.NCD.RecurrentActivity.Types exposing (..)
import Pages.NCD.RecurrentActivity.Utils exposing (..)
import Pages.NCD.Utils exposing (..)
import Pages.NCD.View exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils
    exposing
        ( emptySelectOption
        , isTaskCompleted
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectCustomInput
        , viewCheckBoxSelectInput
        , viewConditionalAlert
        , viewCustomLabel
        , viewInstructionsLabel
        , viewLabel
        , viewMeasurementInput
        , viewPersonDetailsExtended
        , viewQuestionLabel
        , viewSaveAction
        )
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> NCDEncounterId -> NCDRecurrentActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id activity db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> NCDEncounterId -> NCDRecurrentActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id activity db model assembled =
    div [ class "page-activity ncd" ] <|
        [ viewHeader language (NCDRecurrentEncounterPage id) (Translate.NCDRecurrentActivitiesTitle activity) assembled
        , viewContent language currentDate activity db model assembled
        ]


viewHeader : Language -> UserPage -> TranslationId -> AssembledData -> Html Msg
viewHeader language goBackPage labelTransId assembled =
    div
        [ class "ui basic segment head" ]
        [ h1 [ class "ui header" ]
            [ text <| translate language labelTransId ]
        , span
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage goBackPage
            ]
            [ span [ class "icon-back" ] [] ]
        ]


viewContent : Language -> NominalDate -> NCDRecurrentActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate activity db model assembled =
    ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
        :: viewActivity language currentDate activity assembled db model
    )
        |> div [ class "ui unstackable items" ]


viewActivity : Language -> NominalDate -> NCDRecurrentActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate activity assembled db model =
    case activity of
        LabResults ->
            viewLabResultsContent language currentDate assembled model

        RecurrentNextSteps ->
            viewNextStepsContent language currentDate assembled model.nextStepsData


viewLabResultsContent : Language -> NominalDate -> AssembledData -> Model -> List (Html Msg)
viewLabResultsContent language currentDate assembled model =
    let
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            resolveLaboratoryResultTask currentDate assembled

        activeTask =
            Maybe.Extra.or model.labResultsData.activeTask (List.head tasks)

        viewTask task =
            let
                iconClass =
                    laboratoryTaskIconClass task

                isActive =
                    activeTask == Just task

                isCompleted =
                    laboratoryResultTaskCompleted currentDate assembled task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveLabResultsTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.LaboratoryTask task)
                    ]
                ]

        formHtmlAndTasks =
            List.map
                (\task ->
                    ( task
                    , case task of
                        TaskRandomBloodSugarTest ->
                            measurements.randomBloodSugarTest
                                |> getMeasurementValueFunc
                                |> randomBloodSugarResultFormWithDefault model.labResultsData.randomBloodSugarTestForm
                                |> randomBloodSugarResultFormAndTasks language currentDate SetRandomBloodSugar

                        TaskUrineDipstickTest ->
                            measurements.urineDipstickTest
                                |> getMeasurementValueFunc
                                |> urineDipstickResultFormWithDefault model.labResultsData.urineDipstickTestForm
                                |> urineDipstickResultFormAndTasks language
                                    currentDate
                                    SetProtein
                                    SetPH
                                    SetGlucose
                                    SetLeukocytes
                                    SetNitrite
                                    SetUrobilinogen
                                    SetHaemoglobin
                                    SetKetone
                                    SetBilirubin

                        TaskCreatinineTest ->
                            measurements.creatinineTest
                                |> getMeasurementValueFunc
                                |> creatinineResultFormWithDefault model.labResultsData.creatinineTestForm
                                |> creatinineResultFormAndTasks language
                                    currentDate
                                    SetCreatinineResult
                                    SetUreaResult
                                    SetNitorogenResult

                        TaskLiverFunctionTest ->
                            measurements.liverFunctionTest
                                |> getMeasurementValueFunc
                                |> liverFunctionResultFormWithDefault model.labResultsData.liverFunctionTestForm
                                |> liverFunctionResultFormAndTasks language currentDate SetAltResult SetAstResult

                        -- Others are not in use for NCD.
                        _ ->
                            ( emptyNode, 0, 0 )
                    )
                )
                tasks
                |> Dict.fromList

        tasksCompletedFromTotalDict =
            Dict.map (\_ ( _, completed, total ) -> ( completed, total ))
                formHtmlAndTasks

        ( viewForm, tasksCompleted, totalTasks ) =
            Maybe.andThen
                (\task -> Dict.get task formHtmlAndTasks)
                activeTask
                |> Maybe.withDefault ( emptyNode, 0, 0 )

        nextTask =
            List.filter
                (\task ->
                    (Just task /= activeTask)
                        && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
                )
                tasks
                |> List.head

        actions =
            Maybe.andThen
                (\task ->
                    let
                        saveMsg =
                            case task of
                                TaskRandomBloodSugarTest ->
                                    SaveRandomBloodSugarResult personId measurements.randomBloodSugarTest nextTask |> Just

                                TaskUrineDipstickTest ->
                                    SaveUrineDipstickResult personId measurements.urineDipstickTest nextTask |> Just

                                TaskCreatinineTest ->
                                    SaveCreatinineResult personId measurements.creatinineTest nextTask |> Just

                                TaskLiverFunctionTest ->
                                    SaveLiverFunctionResult personId measurements.liverFunctionTest nextTask |> Just

                                -- Others are not in use for NCD.
                                _ ->
                                    Nothing
                    in
                    Maybe.map
                        (\msg ->
                            viewSaveAction language msg (tasksCompleted /= totalTasks)
                        )
                        saveMsg
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
            [ viewForm
            , actions
            ]
        ]
    ]


viewNextStepsContent : Language -> NominalDate -> AssembledData -> NextStepsData -> List (Html Msg)
viewNextStepsContent language currentDate assembled data =
    let
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            resolveNextStepsTasks currentDate assembled

        activeTask =
            Maybe.map
                (\task ->
                    if List.member task tasks then
                        Just task

                    else
                        List.head tasks
                )
                data.activeTask
                |> Maybe.withDefault (List.head tasks)

        viewTask task =
            let
                iconClass =
                    case task of
                        TaskMedicationDistribution ->
                            "next-steps-treatment"

                        TaskReferral ->
                            "next-steps-referral"

                isActive =
                    activeTask == Just task

                isCompleted =
                    nextStepsTaskCompleted assembled task

                attributes =
                    classList
                        [ ( "link-section", True )
                        , ( "active", isActive )
                        , ( "completed", not isActive && isCompleted )
                        ]
                        :: navigationAction

                navigationAction =
                    if isActive then
                        []

                    else
                        [ onClick <| SetActiveNextStepsTask task ]
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.NCDRecurrentNextStepsTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, nextStepsTasksCompletedFromTotal language currentDate assembled data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just TaskMedicationDistribution ->
                    getMeasurementValueFunc measurements.medicationDistribution
                        |> medicationDistributionFormWithDefault data.medicationDistributionForm
                        |> viewMedicationDistributionForm language
                            currentDate
                            NCDEncounterPhaseRecurrent
                            SetRecommendedTreatmentSign
                            SetMedicationDistributionBoolInput
                            assembled

                Just TaskReferral ->
                    getMeasurementValueFunc measurements.referral
                        |> referralFormWithDefault data.referralForm
                        |> viewReferralForm language
                            currentDate
                            NCDEncounterPhaseRecurrent
                            SetReferralBoolInput
                            SetFacilityNonReferralReason
                            assembled

                Nothing ->
                    emptyNode

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
                            saveMsg =
                                case task of
                                    TaskMedicationDistribution ->
                                        SaveMedicationDistribution personId measurements.medicationDistribution nextTask

                                    TaskReferral ->
                                        SaveReferral personId measurements.referral nextTask
                        in
                        viewSaveAction language saveMsg (tasksCompleted /= totalTasks)
                    )
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui four column grid" ] <|
            List.map viewTask tasks
        ]
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewForm, actions ]
        ]
    ]
