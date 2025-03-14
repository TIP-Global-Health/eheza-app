module Pages.NCD.RecurrentActivity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Utils exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NCDActivity.Model exposing (NCDRecurrentActivity(..))
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Measurement.Model exposing (ContentAndTasksLaboratoryResultConfig, LaboratoryTask(..))
import Measurement.Utils
    exposing
        ( creatinineResultFormAndTasks
        , creatinineResultFormWithDefault
        , emptyContentAndTasksLaboratoryResultConfig
        , laboratoryTaskIconClass
        , lipidPanelResultFormAndTasks
        , lipidPanelResultFormWithDefault
        , liverFunctionResultFormAndTasks
        , liverFunctionResultFormWithDefault
        , randomBloodSugarResultFormAndTasks
        , randomBloodSugarResultFormWithDefault
        , urineDipstickResultFormAndTasks
        , urineDipstickResultFormWithDefault
        )
import Pages.NCD.Model exposing (..)
import Pages.NCD.RecurrentActivity.Model exposing (..)
import Pages.NCD.RecurrentActivity.Types exposing (..)
import Pages.NCD.RecurrentActivity.Utils exposing (..)
import Pages.NCD.Utils exposing (..)
import Pages.NCD.View exposing (..)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils
    exposing
        ( resolveActiveTask
        , resolveNextTask
        , tasksBarId
        , viewPersonDetailsExtended
        , viewSaveAction
        , viewTasksCount
        )
import Translate exposing (Language, TranslationId, translate)
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
        isLabTech =
            -- For now, NCD doesn not support Lab tech feature.
            False

        measurements =
            assembled.measurements

        tasks =
            resolveLaboratoryResultTasks currentDate assembled

        activeTask =
            resolveActiveTask tasks model.labResultsData.activeTask

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
                                |> randomBloodSugarResultFormAndTasks language
                                    currentDate
                                    isLabTech
                                    contentAndTasksLaboratorResultsConfig
                                    SetRandomBloodSugar

                        TaskUrineDipstickTest ->
                            measurements.urineDipstickTest
                                |> getMeasurementValueFunc
                                |> urineDipstickResultFormWithDefault model.labResultsData.urineDipstickTestForm
                                |> urineDipstickResultFormAndTasks language
                                    currentDate
                                    isLabTech
                                    contentAndTasksLaboratorResultsConfig
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
                                    SetBUNResult

                        TaskLiverFunctionTest ->
                            measurements.liverFunctionTest
                                |> getMeasurementValueFunc
                                |> liverFunctionResultFormWithDefault model.labResultsData.liverFunctionTestForm
                                |> liverFunctionResultFormAndTasks language
                                    currentDate
                                    SetAltResult
                                    SetAstResult

                        TaskLipidPanelTest ->
                            measurements.lipidPanelTest
                                |> getMeasurementValueFunc
                                |> lipidPanelResultFormWithDefault model.labResultsData.lipidPanelTestForm
                                |> lipidPanelResultFormAndTasks language
                                    currentDate
                                    SetUnitOfMeasurement
                                    SetTotalCholesterolResult
                                    SetLDLCholesterolResult
                                    SetHDLCholesterolResult
                                    SetTriglyceridesResult

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

        actions =
            Maybe.andThen
                (\task ->
                    let
                        personId =
                            assembled.participant.person

                        nextTask =
                            resolveNextTask task tasksCompletedFromTotalDict tasks

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

                                TaskLipidPanelTest ->
                                    SaveLipidPanelResult personId measurements.lipidPanelTest nextTask |> Just

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
    , viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            [ viewForm
            , actions
            ]
        ]
    ]


contentAndTasksLaboratorResultsConfig : ContentAndTasksLaboratoryResultConfig Msg NCDEncounterId
contentAndTasksLaboratorResultsConfig =
    emptyContentAndTasksLaboratoryResultConfig NoOp


viewNextStepsContent : Language -> NominalDate -> AssembledData -> NextStepsData -> List (Html Msg)
viewNextStepsContent language currentDate assembled data =
    let
        measurements =
            assembled.measurements

        tasks =
            resolveNextStepsTasks currentDate assembled

        activeTask =
            resolveActiveTask tasks data.activeTask

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
                            SetRecommendedTreatmentSignSingle
                            SetRecommendedTreatmentSignMultiple
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
                                TaskMedicationDistribution ->
                                    SaveMedicationDistribution personId measurements.medicationDistribution nextTask

                                TaskReferral ->
                                    SaveReferral personId measurements.referral nextTask
                    in
                    viewSaveAction language saveMsg (tasksCompleted /= totalTasks)
                )
                activeTask
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui four column grid" ] <|
            List.map viewTask tasks
        ]
    , viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewForm, actions ]
        ]
    ]
