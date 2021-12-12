module Pages.PrenatalLabResults.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (bloodGroupToString, getMeasurementValueFunc, prenatalTestResultToString, rhesusToString)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalActivity.Model exposing (PrenatalActivity(..))
import Backend.PrenatalActivity.Utils exposing (getActivityIcon)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
import Date exposing (Unit(..))
import EverySet
import Gizra.Html exposing (divKeyed, emptyNode, keyed, keyedDivKeyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalActivity.Model exposing (LaboratoryTask(..))
import Pages.PrenatalActivity.Utils exposing (laboratoryTaskIconClass)
import Pages.PrenatalEncounter.Model exposing (AssembledData)
import Pages.PrenatalEncounter.Utils exposing (..)
import Pages.PrenatalEncounter.View exposing (viewMotherAndMeasurements)
import Pages.PrenatalLabResults.Model exposing (..)
import Pages.PrenatalLabResults.Utils exposing (..)
import Pages.Utils
    exposing
        ( emptySelectOption
        , isTaskCompleted
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewConditionalAlert
        , viewCustomLabel
        , viewLabel
        , viewQuestionLabel
        , viewSaveAction
        )
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> PrenatalEncounterId -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id db model assembled =
    div [ class "page-activity prenatal" ] <|
        [ viewHeader language id assembled
        , viewContent language currentDate db model assembled
        ]


viewHeader : Language -> PrenatalEncounterId -> AssembledData -> Html Msg
viewHeader language id assembled =
    let
        ( label, icon ) =
            ( Translate.PrenatalActivitiesTitle Laboratory, getActivityIcon Laboratory )
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language label ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage GlobalCaseManagementPage
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate db model assembled =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate False assembled Nothing
            ++ viewLabResults language currentDate assembled model


viewLabResults : Language -> NominalDate -> AssembledData -> Model -> List (Html Msg)
viewLabResults language currentDate assembled data =
    let
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            List.filter (expectLaboratoryResultTask currentDate assembled) laboratoryResultTasks

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

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
                                [ onClick <| SetActiveTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.PrenatalLaboratoryTask task)
                    ]
                ]

        formHtmlAndTasks =
            List.map
                (\task ->
                    ( task
                    , case task of
                        TaskHIVTest ->
                            ( emptyNode, 0, 0 )

                        TaskSyphilisTest ->
                            measurements.syphilisTest
                                |> getMeasurementValueFunc
                                |> prenatalTestResultFormWithDefault data.syphilisTestForm
                                |> prenatalTestResultFormAndTasks language currentDate TaskSyphilisTest

                        TaskHepatitisBTest ->
                            measurements.hepatitisBTest
                                |> getMeasurementValueFunc
                                |> prenatalTestResultFormWithDefault data.hepatitisBTestForm
                                |> prenatalTestResultFormAndTasks language currentDate TaskHepatitisBTest

                        TaskMalariaTest ->
                            ( emptyNode, 0, 0 )

                        TaskBloodGpRsTest ->
                            measurements.bloodGpRsTest
                                |> getMeasurementValueFunc
                                |> prenatalBloodGpRsResultFormWithDefault data.bloodGpRsTestForm
                                |> prenatalBloodGpRsResultFormAndTasks language currentDate

                        -- TaskUrineDipstickTest ->
                        --     measurements.urineDipstickTest
                        --         |> getMeasurementValueFunc
                        --         |> prenatalUrineDipstickFormWithDefault data.urineDipstickTestForm
                        --         |> viewPrenatalUrineDipstickForm language currentDate
                        --
                        -- TaskHemoglobinTest ->
                        --     measurements.hemoglobinTest
                        --         |> getMeasurementValueFunc
                        --         |> prenatalNonRDTFormWithDefault data.hemoglobinTestForm
                        --         |> viewPrenatalNonRDTForm language currentDate TaskHemoglobinTest
                        --
                        -- TaskRandomBloodSugarTest ->
                        --     measurements.randomBloodSugarTest
                        --         |> getMeasurementValueFunc
                        --         |> prenatalNonRDTFormWithDefault data.randomBloodSugarTestForm
                        --         |> viewPrenatalNonRDTForm language currentDate TaskRandomBloodSugarTest
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

        -- actions =
        --     Maybe.map
        --         (\task ->
        --             let
        --                 saveMsg =
        --                     case task of
        --                         TaskHIVTest ->
        --                             SaveHIVTest personId measurements.hivTest nextTask
        --
        --                         TaskSyphilisTest ->
        --                             SaveSyphilisTest personId measurements.syphilisTest nextTask
        --
        --                         TaskHepatitisBTest ->
        --                             SaveHepatitisBTest personId measurements.hepatitisBTest nextTask
        --
        --                         TaskMalariaTest ->
        --                             SaveMalariaTest personId measurements.malariaTest nextTask
        --
        --                         TaskBloodGpRsTest ->
        --                             SaveBloodGpRsTest personId measurements.bloodGpRsTest nextTask
        --
        --                         TaskUrineDipstickTest ->
        --                             SaveUrineDipstickTest personId measurements.urineDipstickTest nextTask
        --
        --                         TaskHemoglobinTest ->
        --                             SaveHemoglobinTest personId measurements.hemoglobinTest nextTask
        --
        --                         TaskRandomBloodSugarTest ->
        --                             SaveRandomBloodSugarTest personId measurements.randomBloodSugarTest nextTask
        --             in
        --             viewSaveAction language saveMsg (tasksCompleted /= totalTasks)
        --         )
        --         activeTask
        --         |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui five column grid" ] <|
            List.map viewTask tasks
        ]
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            [ viewForm

            -- , actions
            ]
        ]
    ]


prenatalTestResultFormAndTasks : Language -> NominalDate -> LaboratoryTask -> PrenatalTestResultForm -> ( Html Msg, Int, Int )
prenatalTestResultFormAndTasks language currentDate task form =
    let
        setTestResultMsg =
            case task of
                TaskHepatitisBTest ->
                    Just SetHepatitisBTestResult

                TaskSyphilisTest ->
                    Just SetSyphilisTestResult

                _ ->
                    Nothing

        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            Maybe.map
                (\setResultMsg ->
                    let
                        emptyOption =
                            if isNothing form.testResult then
                                emptySelectOption True

                            else
                                emptyNode
                    in
                    ( [ viewLabel language <| Translate.PrenatalLaboratoryTaskResult task
                      , emptyOption
                            :: List.map
                                (\result ->
                                    option
                                        [ value (prenatalTestResultToString result)
                                        , selected (form.testResult == Just result)
                                        ]
                                        [ text <| translate language <| Translate.PrenatalTestResult result ]
                                )
                                [ PrenatalTestPositive, PrenatalTestNegative, PrenatalTestIndeterminate ]
                            |> select
                                [ onInput setResultMsg
                                , class "form-input select"
                                ]
                      ]
                    , taskCompleted form.testResult
                    , 1
                    )
                )
                setTestResultMsg
                |> Maybe.withDefault ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory prenatal-test-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate task
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


prenatalBloodGpRsResultFormAndTasks : Language -> NominalDate -> PrenatalBloodGpRsResultForm -> ( Html Msg, Int, Int )
prenatalBloodGpRsResultFormAndTasks language currentDate form =
    let
        ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
            let
                emptyOptionForSelect value =
                    if isNothing value then
                        emptySelectOption True

                    else
                        emptyNode
            in
            ( [ viewLabel language Translate.PrenatalLaboratoryBloodGroupResult
              , emptyOptionForSelect form.bloodGroup
                    :: List.map
                        (\bloodGroup ->
                            option
                                [ value (bloodGroupToString bloodGroup)
                                , selected (form.bloodGroup == Just bloodGroup)
                                ]
                                [ text <| translate language <| Translate.BloodGroup bloodGroup ]
                        )
                        [ BloodGroupA, BloodGroupB, BloodGroupAB, BloodGroupO ]
                    |> select
                        [ onInput SetBloodGroup
                        , class "form-input select"
                        ]
              , viewLabel language Translate.PrenatalLaboratoryRhesusResult
              , emptyOptionForSelect form.rhesus
                    :: List.map
                        (\rhesus ->
                            option
                                [ value (rhesusToString rhesus)
                                , selected (form.rhesus == Just rhesus)
                                ]
                                [ text <| translate language <| Translate.Rhesus rhesus ]
                        )
                        [ RhesusPositive, RhesusNegative ]
                    |> select
                        [ onInput SetRhesus
                        , class "form-input select"
                        ]
              ]
            , taskCompleted form.bloodGroup + taskCompleted form.rhesus
            , 2
            )
    in
    ( div [ class "ui form laboratory hemoglobin-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskBloodGpRsTest
            ++ testResultSection
    , testResultTasksCompleted
    , testResultTasksTotal
    )


prenatalUrineDipstickResultFormAndTasks : Language -> NominalDate -> PrenatalUrineDipstickResultForm -> ( Html Msg, Int, Int )
prenatalUrineDipstickResultFormAndTasks language currentDate form =
    ( div [ class "ui form laboratory hemoglobin-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskUrineDipstickTest
    , 0
    , 0
    )


prenatalHemoglobinResultFormAndTasks : Language -> NominalDate -> PrenatalHemoglobinResultForm -> ( Html Msg, Int, Int )
prenatalHemoglobinResultFormAndTasks language currentDate form =
    ( div [ class "ui form laboratory hemoglobin-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskHemoglobinTest
    , 0
    , 0
    )


prenatalRandomBloodSugarResultFormAndTasks : Language -> NominalDate -> PrenatalRandomBloodSugarResultForm -> ( Html Msg, Int, Int )
prenatalRandomBloodSugarResultFormAndTasks language currentDate form =
    ( div [ class "ui form laboratory random-blood-sugar-result" ] <|
        resultFormHeaderSection language currentDate form.executionDate TaskRandomBloodSugarTest
    , 0
    , 0
    )


resultFormHeaderSection : Language -> NominalDate -> Maybe NominalDate -> LaboratoryTask -> List (Html Msg)
resultFormHeaderSection language currentDate executionDate task =
    let
        executionDateSection =
            Maybe.map
                (\date ->
                    [ viewLabel language <| Translate.PrenatalLaboratoryTaskDate task
                    , p [ class "test-date" ] [ text <| formatDDMMYYYY date ]
                    ]
                )
                executionDate
                |> Maybe.withDefault []
    in
    viewCustomLabel language (Translate.PrenatalLaboratoryTaskLabel task) "" "label header"
        :: executionDateSection
