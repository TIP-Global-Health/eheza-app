module Pages.PrenatalActivity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Encoder exposing (socialHistoryHivTestingResultToString)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
import Date exposing (Unit(..))
import DateSelector.SelectorDropdown
import EverySet
import Gizra.Html exposing (divKeyed, emptyNode, keyed, keyedDivKeyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Decoder exposing (decodeDropZoneFile)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalActivity.Model exposing (..)
import Pages.PrenatalActivity.Utils exposing (..)
import Pages.PrenatalEncounter.Model exposing (AssembledData)
import Pages.PrenatalEncounter.Utils exposing (..)
import Pages.PrenatalEncounter.View exposing (viewMotherAndMeasurements)
import Pages.Utils
    exposing
        ( taskCompleted
        , taskListCompleted
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewLabel
        , viewMeasurementInput
        , viewPhotoThumbFromPhotoUrl
        , viewQuestionLabel
        )
import PrenatalActivity.Model exposing (PrenatalActivity(..))
import RemoteData exposing (RemoteData(..), WebData)
import Round
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> PrenatalEncounterId -> PrenatalActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
    let
        data =
            generateAssembledData id db

        content =
            viewWebData language (viewContent language currentDate activity model) identity data
    in
    div [ class "page-prenatal-activity" ] <|
        [ viewHeader language id activity
        , content
        ]


viewContent : Language -> NominalDate -> PrenatalActivity -> Model -> AssembledData -> Html Msg
viewContent language currentDate activity model data =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate data model.showAlertsDialog SetAlertsDialogState
            ++ viewActivity language currentDate activity data model


viewHeader : Language -> PrenatalEncounterId -> PrenatalActivity -> Html Msg
viewHeader language id activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.PrenatalActivitiesTitle activity ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage id
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewActivity : Language -> NominalDate -> PrenatalActivity -> AssembledData -> Model -> List (Html Msg)
viewActivity language currentDate activity data model =
    case activity of
        PregnancyDating ->
            viewPregnancyDatingContent language currentDate data model.pregnancyDatingData

        History ->
            viewHistoryContent language currentDate data model.historyData

        Examination ->
            viewExaminationContent language currentDate data model.examinationData

        FamilyPlanning ->
            viewFamilyPlanningContent language currentDate data model.familyPlanningData

        PatientProvisions ->
            viewPatientProvisionsContent language currentDate data model.patientProvisionsData

        DangerSigns ->
            viewDangerSignsContent language currentDate data model.dangerSignsData

        PrenatalPhoto ->
            viewPrenatalPhotoContent language currentDate data model.prenatalPhotoData


viewPregnancyDatingContent : Language -> NominalDate -> AssembledData -> PregnancyDatingData -> List (Html Msg)
viewPregnancyDatingContent language currentDate assembled data =
    let
        form =
            assembled.measurements.lastMenstrualPeriod
                |> Maybe.map (Tuple.second >> .value)
                |> lastMenstrualPeriodFormWithDefault data.form

        lmpRangeInput =
            option
                [ value ""
                , selected (form.lmpRange == Nothing)
                ]
                [ text "" ]
                :: ([ OneMonth, ThreeMonth, SixMonth ]
                        |> List.map
                            (\range ->
                                option
                                    [ value (encodeLmpRange range)
                                    , selected (form.lmpRange == Just range)
                                    ]
                                    [ text <| translate language <| Translate.LmpRange range ]
                            )
                   )
                |> select [ onInput SetLmpRange, class "form-input range" ]

        today =
            currentDate

        lmpDateInput =
            if isJust form.lmpRange then
                DateSelector.SelectorDropdown.view
                    ToggleDateSelector
                    SetLmpDate
                    form.isDateSelectorOpen
                    (Date.add Days -280 today)
                    today
                    form.lmpDate

            else
                emptyNode

        ( edd, ega ) =
            generateEDDandEGA language currentDate ( "", "" ) form.lmpDate

        totalTasks =
            2

        tasksCompleted =
            taskCompleted form.lmpDate + taskCompleted form.lmpDateConfident
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "form pregnancy-dating" ]
                [ viewQuestionLabel language Translate.LmpRangeHeader
                , lmpRangeInput
                , viewLabel language Translate.LmpDateHeader
                , div [ class "form-input date" ]
                    [ lmpDateInput ]
                , viewQuestionLabel language Translate.LmpDateConfidentHeader
                , viewBoolInput language form.lmpDateConfident SetLmpDateConfident "is-confident" Nothing
                , div [ class "separator" ] []
                , div [ class "results" ]
                    [ div [ class "edd-result" ]
                        [ viewLabel language Translate.EddHeader
                        , div [ class "value" ] [ text edd ]
                        ]
                    , div [ class "ega-result" ]
                        [ viewLabel language Translate.EgaHeader
                        , div [ class "value" ] [ text ega ]
                        ]
                    ]
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SavePregnancyDating assembled.encounter.participant assembled.participant.person assembled.measurements.lastMenstrualPeriod
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewHistoryContent : Language -> NominalDate -> AssembledData -> HistoryData -> List (Html Msg)
viewHistoryContent language currentDate assembled data_ =
    let
        firstEnconter =
            isFirstPrenatalEncounter assembled

        ( tasks, data ) =
            if firstEnconter then
                ( [ Obstetric, Medical, Social ], data_ )

            else
                ( [ Social ], { data_ | activeTask = Social } )

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        Obstetric ->
                            ( "obstetric", isJust assembled.measurements.obstetricHistory && isJust assembled.measurements.obstetricHistoryStep2 )

                        Medical ->
                            ( "medical", isJust assembled.measurements.medicalHistory )

                        Social ->
                            ( "social", isJust assembled.measurements.socialHistory )

                isActive =
                    task == data.activeTask

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveHistoryTask task ]
                           )
            in
            div [ class "column" ]
                [ a attributes
                    [ span [ class <| "icon-history-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.HistoryTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, historyTasksCompletedFromTotal assembled data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Dict.get data.activeTask tasksCompletedFromTotalDict
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case data.activeTask of
                Obstetric ->
                    case data.obstetricHistoryStep of
                        ObstetricHistoryFirstStep ->
                            let
                                formStep1_ =
                                    assembled.measurements.obstetricHistory
                                        |> Maybe.map (Tuple.second >> .value)
                                        |> obstetricHistoryFormWithDefault data.obstetricFormFirstStep
                            in
                            viewObstetricFormFirstStep language currentDate assembled formStep1_

                        ObstetricHistorySecondStep ->
                            let
                                formStep2_ =
                                    assembled.measurements.obstetricHistoryStep2
                                        |> Maybe.map (Tuple.second >> .value)
                                        |> obstetricHistoryStep2FormWithDefault data.obstetricFormSecondStep
                            in
                            viewObstetricFormSecondStep language currentDate assembled formStep2_

                Medical ->
                    let
                        medicalForm =
                            assembled.measurements.medicalHistory
                                |> Maybe.map (Tuple.second >> .value)
                                |> medicalHistoryFormWithDefault data.medicalForm
                    in
                    viewMedicalForm language currentDate assembled medicalForm

                Social ->
                    let
                        socialForm =
                            assembled.measurements.socialHistory
                                |> Maybe.map (Tuple.second >> .value)
                                |> socialHistoryFormWithDefault data.socialForm

                        showCounselingQuestion =
                            assembled.previousMeasurementsWithDates
                                |> List.filter
                                    (\( _, measurements ) ->
                                        measurements.socialHistory
                                            |> Maybe.map (Tuple.second >> .value >> .socialHistory >> EverySet.member PartnerHivCounseling)
                                            |> Maybe.withDefault False
                                    )
                                |> List.isEmpty

                        showTestingQuestions =
                            assembled.previousMeasurementsWithDates
                                |> List.filter
                                    (\( _, measurements ) ->
                                        measurements.socialHistory
                                            |> Maybe.map
                                                (\socialHistory ->
                                                    let
                                                        value =
                                                            Tuple.second socialHistory |> .value
                                                    in
                                                    (value.hivTestingResult == ResultHivPositive)
                                                        || (value.hivTestingResult == ResultHivNegative)
                                                )
                                            |> Maybe.withDefault False
                                    )
                                |> List.isEmpty
                    in
                    viewSocialForm language currentDate showCounselingQuestion showTestingQuestions socialForm

        getNextTask currentTask =
            if not firstEnconter then
                Nothing

            else
                case currentTask of
                    Obstetric ->
                        case data.obstetricHistoryStep of
                            ObstetricHistoryFirstStep ->
                                Nothing

                            ObstetricHistorySecondStep ->
                                [ Medical, Social ]
                                    |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                                    |> List.head

                    Medical ->
                        [ Social, Obstetric ]
                            |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                            |> List.head

                    Social ->
                        [ Obstetric, Medical ]
                            |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                            |> List.head

        actions =
            let
                nextTask =
                    getNextTask data.activeTask

                ( buttons, stepIndicationClass ) =
                    case data.activeTask of
                        Obstetric ->
                            case data.obstetricHistoryStep of
                                ObstetricHistoryFirstStep ->
                                    ( [ button
                                            [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                                            , onClick <| SaveOBHistoryStep1 assembled.participant.person assembled.measurements.obstetricHistory
                                            ]
                                            [ text <| translate language Translate.SaveAndNext ]
                                      ]
                                    , "first"
                                    )

                                ObstetricHistorySecondStep ->
                                    ( [ button
                                            [ class "ui fluid primary button"
                                            , onClick BackToOBHistoryStep1
                                            ]
                                            [ text <| ("< " ++ translate language Translate.Back) ]
                                      , button
                                            [ classList
                                                [ ( "ui fluid primary button", True )
                                                , ( "disabled", tasksCompleted /= totalTasks )
                                                , ( "active", tasksCompleted == totalTasks )
                                                ]
                                            , onClick <|
                                                SaveOBHistoryStep2
                                                    assembled.participant.person
                                                    assembled.measurements.obstetricHistoryStep2
                                                    nextTask
                                            ]
                                            [ text <| translate language Translate.Save ]
                                      ]
                                    , "second"
                                    )

                        Medical ->
                            ( [ button
                                    [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                                    , onClick <|
                                        SaveMedicalHistory
                                            assembled.participant.person
                                            assembled.measurements.medicalHistory
                                            nextTask
                                    ]
                                    [ text <| translate language Translate.Save ]
                              ]
                            , ""
                            )

                        Social ->
                            ( [ button
                                    [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                                    , onClick <|
                                        SaveSocialHistory
                                            assembled.participant.person
                                            assembled.measurements.socialHistory
                                            nextTask
                                    ]
                                    [ text <| translate language Translate.Save ]
                              ]
                            , ""
                            )
            in
            div [ class <| "actions history obstetric " ++ stepIndicationClass ]
                buttons
    in
    [ div [ class "ui task segment blue", id tasksBarId ]
        [ div [ class "ui five column grid" ] <|
            List.map viewTask <|
                tasks
        ]
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewForm
            , actions
            ]
        ]
    ]


viewExaminationContent : Language -> NominalDate -> AssembledData -> ExaminationData -> List (Html Msg)
viewExaminationContent language currentDate assembled data =
    let
        tasks =
            [ Vitals, NutritionAssessment, CorePhysicalExam, ObstetricalExam, BreastExam ]

        firstEnconter =
            isFirstPrenatalEncounter assembled

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, examinationTasksCompletedFromTotal assembled data firstEnconter task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Dict.get data.activeTask tasksCompletedFromTotalDict
                |> Maybe.withDefault ( 0, 0 )

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        Vitals ->
                            ( "vitals", isJust assembled.measurements.vitals )

                        NutritionAssessment ->
                            ( "nutrition-assessment", isJust assembled.measurements.nutrition )

                        CorePhysicalExam ->
                            ( "core-physical-exam", isJust assembled.measurements.corePhysicalExam )

                        ObstetricalExam ->
                            ( "obstetrical-exam", isJust assembled.measurements.obstetricalExam )

                        BreastExam ->
                            ( "breast-exam", isJust assembled.measurements.breastExam )

                isActive =
                    task == data.activeTask

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveExaminationTask task ]
                           )
            in
            div [ class <| "column " ++ iconClass ]
                [ a attributes
                    [ span [ class <| "icon-examination-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.ExaminationTask task)
                    ]
                ]

        viewForm =
            case data.activeTask of
                Vitals ->
                    let
                        form =
                            assembled.measurements.vitals
                                |> Maybe.map (Tuple.second >> .value)
                                |> vitalsFormWithDefault data.vitalsForm
                    in
                    viewVitalsForm language currentDate assembled form

                NutritionAssessment ->
                    let
                        hideHeightInput =
                            not firstEnconter

                        form_ =
                            assembled.measurements.nutrition
                                |> Maybe.map (Tuple.second >> .value)
                                |> prenatalNutritionFormWithDefault data.nutritionAssessmentForm

                        form =
                            if hideHeightInput then
                                assembled.previousMeasurementsWithDates
                                    |> List.head
                                    |> Maybe.andThen (Tuple.second >> getMotherHeightMeasurement)
                                    |> Maybe.map (\(HeightInCm height) -> { form_ | height = Just height })
                                    |> Maybe.withDefault form_

                            else
                                form_
                    in
                    viewNutritionAssessmentForm language currentDate assembled form hideHeightInput

                CorePhysicalExam ->
                    let
                        form =
                            assembled.measurements.corePhysicalExam
                                |> Maybe.map (Tuple.second >> .value)
                                |> corePhysicalExamFormWithDefault data.corePhysicalExamForm
                    in
                    viewCorePhysicalExamForm language currentDate assembled form

                ObstetricalExam ->
                    let
                        form =
                            assembled.measurements.obstetricalExam
                                |> Maybe.map (Tuple.second >> .value)
                                |> obstetricalExamFormWithDefault data.obstetricalExamForm
                    in
                    viewObstetricalExamForm language currentDate assembled form

                BreastExam ->
                    let
                        form =
                            assembled.measurements.breastExam
                                |> Maybe.map (Tuple.second >> .value)
                                |> breastExamFormWithDefault data.breastExamForm
                    in
                    viewBreastExamForm language currentDate assembled form

        getNextTask currentTask =
            case currentTask of
                Vitals ->
                    [ NutritionAssessment, CorePhysicalExam, ObstetricalExam, BreastExam ]
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

                NutritionAssessment ->
                    [ CorePhysicalExam, ObstetricalExam, BreastExam, Vitals ]
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

                CorePhysicalExam ->
                    [ ObstetricalExam, BreastExam, Vitals, NutritionAssessment ]
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

                ObstetricalExam ->
                    [ BreastExam, Vitals, NutritionAssessment, CorePhysicalExam ]
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

                BreastExam ->
                    [ Vitals, NutritionAssessment, CorePhysicalExam, ObstetricalExam ]
                        |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                        |> List.head

        actions =
            let
                nextTask =
                    getNextTask data.activeTask

                saveAction =
                    case data.activeTask of
                        Vitals ->
                            SaveVitals
                                assembled.participant.person
                                assembled.measurements.vitals
                                nextTask

                        NutritionAssessment ->
                            let
                                passHeight =
                                    isFirstPrenatalEncounter assembled |> not

                                maybeHeight =
                                    if passHeight then
                                        assembled.previousMeasurementsWithDates
                                            |> List.head
                                            |> Maybe.andThen (Tuple.second >> getMotherHeightMeasurement)
                                            |> Maybe.map (\(HeightInCm height) -> height)

                                    else
                                        Nothing
                            in
                            SaveNutritionAssessment
                                assembled.participant.person
                                assembled.measurements.nutrition
                                maybeHeight
                                nextTask

                        CorePhysicalExam ->
                            SaveCorePhysicalExam
                                assembled.participant.person
                                assembled.measurements.corePhysicalExam
                                nextTask

                        ObstetricalExam ->
                            SaveObstetricalExam
                                assembled.participant.person
                                assembled.measurements.obstetricalExam
                                nextTask

                        BreastExam ->
                            SaveBreastExam
                                assembled.participant.person
                                assembled.measurements.breastExam
                                nextTask
            in
            div [ class "actions examination" ]
                [ button
                    [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                    , onClick saveAction
                    ]
                    [ text <| translate language Translate.Save ]
                ]
    in
    [ div [ class "ui task segment blue" ]
        [ div [ class "ui five column grid" ] <|
            List.map viewTask <|
                tasks
        ]
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewForm
            , actions
            ]
        ]
    ]


viewFamilyPlanningContent : Language -> NominalDate -> AssembledData -> FamilyPlanningData -> List (Html Msg)
viewFamilyPlanningContent language currentDate assembled data =
    let
        form =
            assembled.measurements.familyPlanning
                |> Maybe.map (Tuple.second >> .value)
                |> familyPlanningFormWithDefault data.form

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.signs
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form family-planning" ]
                [ viewQuestionLabel language Translate.FamilyPlanningInFutureQuestion
                , viewCheckBoxMultipleSelectInput language
                    [ AutoObservation, Condoms, CycleBeads, CycleCounting, Hysterectomy, Implants, Injectables ]
                    [ IUD, LactationAmenorrhea, OralContraceptives, Spermicide, TubalLigatures, Vasectomy ]
                    (form.signs |> Maybe.withDefault [])
                    (Just NoFamilyPlanning)
                    SetFamilyPlanningSign
                    Translate.FamilyPlanningSignLabel
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SaveFamilyPlanning assembled.participant.person assembled.measurements.familyPlanning
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewPatientProvisionsContent : Language -> NominalDate -> AssembledData -> PatientProvisionsData -> List (Html Msg)
viewPatientProvisionsContent language currentDate assembled data =
    let
        showResourcesTask =
            shouldShowPatientProvisionsResourcesTask assembled

        -- We show the question starting EGA week 20, and
        -- as long as all preivious answers were 'No'.
        showDewormingPillQuestion =
            assembled.globalLmpDate
                |> Maybe.map
                    (\lmpDate ->
                        let
                            currentWeek =
                                diffDays lmpDate currentDate // 7
                        in
                        if currentWeek < 20 then
                            False

                        else
                            assembled.previousMeasurementsWithDates
                                |> List.filter
                                    (\( _, measurements ) ->
                                        measurements.medication
                                            |> Maybe.map (Tuple.second >> .value >> EverySet.member DewormingPill)
                                            |> Maybe.withDefault False
                                    )
                                |> List.isEmpty
                    )
                |> Maybe.withDefault False

        tasks =
            if showResourcesTask then
                [ Medication, Resources ]

            else
                [ Medication ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, patientProvisionsTasksCompletedFromTotal assembled data showDewormingPillQuestion task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Dict.get data.activeTask tasksCompletedFromTotalDict
                |> Maybe.withDefault ( 0, 0 )

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        Medication ->
                            ( "medication", isJust assembled.measurements.medication )

                        Resources ->
                            ( "resources", isJust assembled.measurements.resource )

                isActive =
                    task == data.activeTask

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActivePatientProvisionsTask task ]
                           )
            in
            div [ class "column" ]
                [ a attributes
                    [ span [ class <| "icon-patient-provisions-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.PatientProvisionsTask task)
                    ]
                ]

        viewForm =
            case data.activeTask of
                Medication ->
                    let
                        form =
                            assembled.measurements.medication
                                |> Maybe.map (Tuple.second >> .value)
                                |> medicationFormWithDefault data.medicationForm

                        questions =
                            if showDewormingPillQuestion then
                                [ form.receivedIronFolicAcid, form.receivedDewormingPill ]

                            else
                                [ form.receivedIronFolicAcid ]
                    in
                    viewMedicationForm language currentDate assembled showDewormingPillQuestion form

                Resources ->
                    let
                        form =
                            assembled.measurements.resource
                                |> Maybe.map (Tuple.second >> .value)
                                |> resourceFormWithDefault data.resourcesForm
                    in
                    viewResourcesForm language currentDate assembled form

        getNextTask currentTask =
            if not showResourcesTask then
                Nothing

            else
                case currentTask of
                    Medication ->
                        [ Resources ]
                            |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                            |> List.head

                    Resources ->
                        [ Medication ]
                            |> List.filter (isTaskCompleted tasksCompletedFromTotalDict >> not)
                            |> List.head

        actions =
            let
                nextTask =
                    getNextTask data.activeTask

                saveAction =
                    case data.activeTask of
                        Medication ->
                            SaveMedication
                                assembled.participant.person
                                assembled.measurements.medication
                                nextTask

                        Resources ->
                            SaveResources
                                assembled.participant.person
                                assembled.measurements.resource
                                nextTask
            in
            div [ class "actions examination" ]
                [ button
                    [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                    , onClick saveAction
                    ]
                    [ text <| translate language Translate.Save ]
                ]
    in
    [ div [ class "ui task segment blue" ]
        [ div [ class "ui five column grid" ] <|
            List.map viewTask tasks
        ]
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewForm
            , actions
            ]
        ]
    ]


viewDangerSignsContent : Language -> NominalDate -> AssembledData -> DangerSignsData -> List (Html Msg)
viewDangerSignsContent language currentDate assembled data =
    let
        form =
            assembled.measurements.dangerSigns
                |> Maybe.map (Tuple.second >> .value)
                |> dangerSignsFormWithDefault data.form

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.signs
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form danger-signs" ]
                [ viewLabel language Translate.SelectDangerSigns
                , viewCheckBoxMultipleSelectInput language
                    [ VaginalBleeding, HeadacheBlurredVision, Convulsions, AbdominalPain ]
                    [ DifficultyBreathing, Fever, ExtremeWeakness ]
                    (form.signs |> Maybe.withDefault [])
                    (Just NoDangerSign)
                    SetDangerSign
                    Translate.DangerSign
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SaveDangerSigns assembled.participant.person assembled.measurements.dangerSigns
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewPrenatalPhotoContent : Language -> NominalDate -> AssembledData -> PrenatalPhotoData -> List (Html Msg)
viewPrenatalPhotoContent language currentDate assembled data =
    let
        photoId =
            Maybe.map Tuple.first assembled.measurements.prenatalPhoto

        -- If we have a photo that we've just taken, but not saved, that is in
        -- `data.url`. We show that if we have it. Otherwise, we'll show the saved
        -- measurement, if we have that.
        ( displayPhoto, saveMsg, isDisabled ) =
            case data.url of
                Just url ->
                    ( Just url
                    , [ onClick <| SavePrenatalPhoto assembled.participant.person photoId url ]
                    , False
                    )

                Nothing ->
                    ( Maybe.map (Tuple.second >> .value)
                        assembled.measurements.prenatalPhoto
                    , []
                    , True
                    )

        totalTasks =
            1

        tasksCompleted =
            if isJust displayPhoto then
                1

            else
                0
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , divKeyed [ class "ui full segment photo" ]
        [ keyedDivKeyed "content"
            [ class "content" ]
            [ p [] [ text <| translate language Translate.PrenatalPhotoHelper ]
                |> keyed "help"
            , keyedDivKeyed "grid"
                [ class "ui grid" ]
                [ Maybe.map viewPhotoThumbFromPhotoUrl displayPhoto
                    |> showMaybe
                    |> List.singleton
                    |> div [ class "eight wide column" ]
                    |> keyed "thumbnail"
                , div
                    [ id "dropzone"
                    , class "eight wide column dropzone"
                    , on "dropzonecomplete" (Json.Decode.map DropZoneComplete decodeDropZoneFile)
                    ]
                    [ div
                        [ class "dz-message"
                        , attribute "data-dz-message" ""
                        ]
                        [ span
                            []
                            [ text <| translate language Translate.DropzoneDefaultMessage ]
                        ]
                    ]
                    |> keyed "dropzone"
                ]
            ]
        , keyed "button" <|
            div [ class "actions" ]
                [ button
                    ([ classList
                        [ ( "ui fluid primary button", True )
                        , ( "disabled", isDisabled )
                        ]
                     ]
                        ++ saveMsg
                    )
                    [ text <| translate language Translate.Save ]
                ]
        ]
    ]



-- Forms


viewObstetricFormFirstStep : Language -> NominalDate -> AssembledData -> ObstetricFormFirstStep -> Html Msg
viewObstetricFormFirstStep language currentDate assembled form =
    let
        gravida =
            Maybe.map generateGravida (toObstetricHistoryValue form)
                |> Maybe.withDefault ""

        para =
            Maybe.map generatePara (toObstetricHistoryValue form)
                |> Maybe.withDefault ""

        termPregnancyUpdateFunc value form_ =
            { form_ | termPregnancy = value, termPregnancyDirty = True }

        preTermPregnancyUpdateFunc value form_ =
            { form_ | preTermPregnancy = value, preTermPregnancyDirty = True }

        stillbirthsAtTermUpdateFunc value form_ =
            { form_ | stillbirthsAtTerm = value, stillbirthsAtTermDirty = True }

        stillbirthsPreTermUpdateFunc value form_ =
            { form_ | stillbirthsPreTerm = value, stillbirthsPreTermDirty = True }

        abortionsUpdateFunc value form_ =
            { form_ | abortions = value, abortionsDirty = True }

        liveChildrenUpdateFunc value form_ =
            { form_ | liveChildren = value, liveChildrenDirty = True }
    in
    div [ class "form history obstetric first" ]
        [ viewLabel language Translate.CurrentlyPregnant
        , viewBoolInput language
            form.currentlyPregnant
            SetCurrentlyPregnant
            "currently-pregnant"
            Nothing
        , viewNumberInput language
            form.termPregnancy
            (SetOBIntInput termPregnancyUpdateFunc)
            "term-pregnancy"
            Translate.TermPregnancy
            Nothing
        , viewNumberInput language
            form.preTermPregnancy
            (SetOBIntInput preTermPregnancyUpdateFunc)
            "preterm-pregnancy"
            Translate.PreTermPregnancy
            Nothing
        , viewNumberInput language
            form.stillbirthsAtTerm
            (SetOBIntInput stillbirthsAtTermUpdateFunc)
            "stillbirths-at-term"
            Translate.NumberOfStillbirthsAtTerm
            Nothing
        , viewNumberInput language
            form.stillbirthsPreTerm
            (SetOBIntInput stillbirthsPreTermUpdateFunc)
            "stillbirths-pre-term"
            Translate.NumberOfStillbirthsPreTerm
            Nothing
        , viewNumberInput language
            form.abortions
            (SetOBIntInput abortionsUpdateFunc)
            "abortions"
            Translate.NumberOfAbortions
            Nothing
        , viewNumberInput language
            form.liveChildren
            (SetOBIntInput liveChildrenUpdateFunc)
            "live-children"
            Translate.NumberOfLiveChildren
            Nothing
        , div [ class "separator" ] []
        , div [ class "results" ]
            [ div [ class "gravida-result" ]
                [ span [ class "label" ] [ text <| (translate language Translate.Gravida ++ ":") ]
                , span [] [ text gravida ]
                ]
            , div [ class "para-result" ]
                [ span [ class "label" ] [ text <| (translate language Translate.Para ++ ":") ]
                , span [] [ text para ]
                ]
            ]
        ]


viewObstetricFormSecondStep : Language -> NominalDate -> AssembledData -> ObstetricFormSecondStep -> Html Msg
viewObstetricFormSecondStep language currentDate assembled form =
    let
        cSectionInPreviousDeliveryUpdateFunc value form_ =
            { form_ | cSectionInPreviousDelivery = Just value }

        successiveAbortionsUpdateFunc value form_ =
            { form_ | successiveAbortions = Just value }

        successivePrematureDeliveriesUpdateFunc value form_ =
            { form_ | successivePrematureDeliveries = Just value }

        stillbornPreviousDeliveryUpdateFunc value form_ =
            { form_ | stillbornPreviousDelivery = Just value }

        babyDiedOnDayOfBirthPreviousDeliveryUpdateFunc value form_ =
            { form_ | babyDiedOnDayOfBirthPreviousDelivery = Just value }

        partialPlacentaPreviousDeliveryUpdateFunc value form_ =
            { form_ | partialPlacentaPreviousDelivery = Just value }

        severeHemorrhagingPreviousDeliveryUpdateFunc value form_ =
            { form_ | severeHemorrhagingPreviousDelivery = Just value }

        preeclampsiaPreviousPregnancyUpdateFunc value form_ =
            { form_ | preeclampsiaPreviousPregnancy = Just value }

        convulsionsPreviousDeliveryUpdateFunc value form_ =
            { form_ | convulsionsPreviousDelivery = Just value }

        convulsionsAndUnconsciousPreviousDeliveryUpdateFunc value form_ =
            { form_ | convulsionsAndUnconsciousPreviousDelivery = Just value }

        gestationalDiabetesPreviousPregnancyUpdateFunc value form_ =
            { form_ | gestationalDiabetesPreviousPregnancy = Just value }

        incompleteCervixPreviousPregnancyUpdateFunc value form_ =
            { form_ | incompleteCervixPreviousPregnancy = Just value }

        rhNegativeUpdateFunc value form_ =
            { form_ | rhNegative = Just value }
    in
    div [ class "form history obstetric second" ]
        [ viewNumberInput
            language
            form.cSections
            SetNumberOfCSections
            "c-sections"
            Translate.NumberOfCSections
            (Just ( [ [ (<) 0 ] ], [] ))
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.CSectionInPreviousDelivery ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.cSectionInPreviousDelivery False ]
            ]
        , viewBoolInput
            language
            form.cSectionInPreviousDelivery
            (SetOBBoolInput cSectionInPreviousDeliveryUpdateFunc)
            "c-section-previous-delivery"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.CSectionReason ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.cSectionReason |> Maybe.map List.singleton |> Maybe.withDefault [])
                    [ None ]
                ]
            ]
        , viewCheckBoxSelectInput language
            [ Breech, Emergency, Other ]
            [ FailureToProgress, None ]
            form.cSectionReason
            SetCSectionReason
            Translate.CSectionReasons
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewCustomLabel language Translate.PreviousDelivery ":" "label c-section-previous-delivery" ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.previousDeliveryPeriod |> Maybe.map List.singleton |> Maybe.withDefault [])
                    [ Neither ]
                ]
            ]
        , viewCheckBoxSelectInput language
            [ LessThan18Month, MoreThan5Years ]
            [ Neither ]
            form.previousDeliveryPeriod
            SetPreviousDeliveryPeriod
            Translate.PreviousDeliveryPeriods
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewCustomLabel language Translate.SuccessiveAbortions "?" "label successive-abortions" ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.successiveAbortions False ]
            ]
        , viewBoolInput
            language
            form.successiveAbortions
            (SetOBBoolInput successiveAbortionsUpdateFunc)
            "successive-abortions"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.SuccessivePrematureDeliveries ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.successivePrematureDeliveries False ]
            ]
        , viewBoolInput
            language
            form.successivePrematureDeliveries
            (SetOBBoolInput successivePrematureDeliveriesUpdateFunc)
            "successive-primature-deliveries"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.StillbornPreviousDelivery ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.stillbornPreviousDelivery False ]
            ]
        , viewBoolInput
            language
            form.stillbornPreviousDelivery
            (SetOBBoolInput stillbornPreviousDeliveryUpdateFunc)
            "stillborn-previous-delivery"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.BabyDiedOnDayOfBirthPreviousDelivery ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.babyDiedOnDayOfBirthPreviousDelivery False ]
            ]
        , viewBoolInput
            language
            form.babyDiedOnDayOfBirthPreviousDelivery
            (SetOBBoolInput babyDiedOnDayOfBirthPreviousDeliveryUpdateFunc)
            "baby-died-on-day-off-birth-previous-delivery"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.PartialPlacentaPreviousDelivery ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.partialPlacentaPreviousDelivery False ]
            ]
        , viewBoolInput
            language
            form.partialPlacentaPreviousDelivery
            (SetOBBoolInput partialPlacentaPreviousDeliveryUpdateFunc)
            "partial-placenta-previous-delivery"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.SevereHemorrhagingPreviousDelivery ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.severeHemorrhagingPreviousDelivery False ]
            ]
        , viewBoolInput
            language
            form.severeHemorrhagingPreviousDelivery
            (SetOBBoolInput severeHemorrhagingPreviousDeliveryUpdateFunc)
            "severe-hemorrhaging-previous-delivery"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.PreeclampsiaPreviousPregnancy ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.preeclampsiaPreviousPregnancy False ]
            ]
        , viewBoolInput
            language
            form.preeclampsiaPreviousPregnancy
            (SetOBBoolInput preeclampsiaPreviousPregnancyUpdateFunc)
            "preeclampsia-previous-pregnancy"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.ConvulsionsPreviousDelivery ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.convulsionsPreviousDelivery False ]
            ]
        , viewBoolInput
            language
            form.convulsionsPreviousDelivery
            (SetOBBoolInput convulsionsPreviousDeliveryUpdateFunc)
            "convulsions-previous-pelivery"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.ConvulsionsAndUnconsciousPreviousDelivery ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.convulsionsAndUnconsciousPreviousDelivery False ]
            ]
        , viewBoolInput
            language
            form.convulsionsAndUnconsciousPreviousDelivery
            (SetOBBoolInput convulsionsAndUnconsciousPreviousDeliveryUpdateFunc)
            "convulsions-and-unconscious-previous-delivery"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.GestationalDiabetesPreviousPregnancy ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.gestationalDiabetesPreviousPregnancy False ]
            ]
        , viewBoolInput
            language
            form.gestationalDiabetesPreviousPregnancy
            (SetOBBoolInput gestationalDiabetesPreviousPregnancyUpdateFunc)
            "gestatipnal-diabetes-previous-pregnancy"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.IncompleteCervixPreviousPregnancy ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.incompleteCervixPreviousPregnancy False ]
            ]
        , viewBoolInput
            language
            form.incompleteCervixPreviousPregnancy
            (SetOBBoolInput incompleteCervixPreviousPregnancyUpdateFunc)
            "incomplete-cervix-previous-pregnancy"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.RhNegative ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.rhNegative False ]
            ]
        , viewBoolInput
            language
            form.rhNegative
            (SetOBBoolInput rhNegativeUpdateFunc)
            "rh-negative"
            Nothing
        ]


viewMedicalForm : Language -> NominalDate -> AssembledData -> MedicalHistoryForm -> Html Msg
viewMedicalForm language currentDate assembled form =
    let
        uterineMyomaUpdateFunc value form_ =
            { form_ | uterineMyoma = Just value }

        diabetesUpdateFunc value form_ =
            { form_ | diabetes = Just value }

        cardiacDiseaseUpdateFunc value form_ =
            { form_ | cardiacDisease = Just value }

        renalDiseaseUpdateFunc value form_ =
            { form_ | renalDisease = Just value }

        hypertensionBeforePregnancyUpdateFunc value form_ =
            { form_ | hypertensionBeforePregnancy = Just value }

        tuberculosisPastUpdateFunc value form_ =
            { form_ | tuberculosisPast = Just value }

        tuberculosisPresentUpdateFunc value form_ =
            { form_ | tuberculosisPresent = Just value }

        asthmaUpdateFunc value form_ =
            { form_ | asthma = Just value }

        bowedLegsUpdateFunc value form_ =
            { form_ | bowedLegs = Just value }

        hivUpdateFunc value form_ =
            { form_ | hiv = Just value }

        mentalHealthHistoryUpdateFunc value form_ =
            { form_ | mentalHealthHistory = Just value }
    in
    div [ class "form history medical" ]
        [ viewCustomLabel language Translate.MedicalFormHelper ":" "label helper"
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.UterineMyoma ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.uterineMyoma False ]
            ]
        , viewBoolInput
            language
            form.uterineMyoma
            (SetMedicalBoolInput uterineMyomaUpdateFunc)
            "uterine-myoma"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.Diabetes ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.diabetes False ]
            ]
        , viewBoolInput
            language
            form.diabetes
            (SetMedicalBoolInput diabetesUpdateFunc)
            "diabetes"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.CardiacDisease ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.cardiacDisease False ]
            ]
        , viewBoolInput
            language
            form.cardiacDisease
            (SetMedicalBoolInput cardiacDiseaseUpdateFunc)
            "cardiac-disease"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.RenalDisease ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.renalDisease False ]
            ]
        , viewBoolInput
            language
            form.renalDisease
            (SetMedicalBoolInput renalDiseaseUpdateFunc)
            "renal-disease"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.HypertensionBeforePregnancy ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.hypertensionBeforePregnancy False ]
            ]
        , viewBoolInput
            language
            form.hypertensionBeforePregnancy
            (SetMedicalBoolInput hypertensionBeforePregnancyUpdateFunc)
            "hypertension-before-pregnancy"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.TuberculosisPast ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.tuberculosisPast False ]
            ]
        , viewBoolInput
            language
            form.tuberculosisPast
            (SetMedicalBoolInput tuberculosisPastUpdateFunc)
            "tuberculosis-past"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.TuberculosisPresent ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.tuberculosisPresent False ]
            ]
        , viewBoolInput
            language
            form.tuberculosisPresent
            (SetMedicalBoolInput tuberculosisPresentUpdateFunc)
            "tuberculosis-present"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.Asthma ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.asthma False ]
            ]
        , viewBoolInput
            language
            form.asthma
            (SetMedicalBoolInput asthmaUpdateFunc)
            "asthma"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.BowedLegs ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.bowedLegs False ]
            ]
        , viewBoolInput
            language
            form.bowedLegs
            (SetMedicalBoolInput bowedLegsUpdateFunc)
            "bowed-legs"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.HIV ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.hiv False ]
            ]
        , viewBoolInput
            language
            form.hiv
            (SetMedicalBoolInput hivUpdateFunc)
            "hiv"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.MentalHealthHistory ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.mentalHealthHistory False ]
            ]
        , viewBoolInput
            language
            form.mentalHealthHistory
            (SetMedicalBoolInput mentalHealthHistoryUpdateFunc)
            "mental-health-history"
            Nothing
        ]


viewSocialForm : Language -> NominalDate -> Bool -> Bool -> SocialHistoryForm -> Html Msg
viewSocialForm language currentDate showCounselingQuestion showTestingQuestions form =
    let
        accompaniedByPartnerUpdateFunc value form_ =
            { form_ | accompaniedByPartner = Just value }

        accompaniedQuestion =
            [ div [ class "ui grid" ]
                [ div [ class "twelve wide column" ]
                    [ viewQuestionLabel language Translate.AccompaniedByPartner ]
                , div [ class "four wide column" ]
                    [ viewRedAlertForBool form.accompaniedByPartner True ]
                ]
            , viewBoolInput
                language
                form.accompaniedByPartner
                (SetSocialBoolInput accompaniedByPartnerUpdateFunc)
                "accompanied-by-partner"
                Nothing
            ]

        counselingQuestion =
            if showCounselingQuestion then
                let
                    partnerReceivedCounselingUpdateFunc value form_ =
                        { form_ | partnerReceivedCounseling = Just value }
                in
                [ div [ class "ui grid" ]
                    [ div [ class "twelve wide column" ]
                        [ viewQuestionLabel language Translate.PartnerReceivedHivCounseling ]
                    , div [ class "four wide column" ]
                        [ viewRedAlertForBool form.partnerReceivedCounseling True ]
                    ]
                , viewBoolInput
                    language
                    form.partnerReceivedCounseling
                    (SetSocialBoolInput partnerReceivedCounselingUpdateFunc)
                    "partner-received-counseling"
                    Nothing
                ]

            else
                []

        testingReceivedQuestion =
            if showTestingQuestions then
                let
                    partnerReceivedTestingUpdateFunc value form_ =
                        { form_ | partnerReceivedTesting = Just value }
                in
                [ div [ class "ui grid" ]
                    [ div [ class "twelve wide column" ]
                        [ viewQuestionLabel language Translate.PartnerReceivedHivTesting ]
                    , div [ class "four wide column" ]
                        [ viewRedAlertForBool form.partnerReceivedTesting True ]
                    ]
                , viewBoolInput
                    language
                    form.partnerReceivedTesting
                    (SetSocialBoolInput partnerReceivedTestingUpdateFunc)
                    "partner-received-testing"
                    Nothing
                ]

            else
                []

        testingResultQuestion =
            if showTestingQuestions && form.partnerReceivedTesting == Just True then
                [ div [ class "ui grid" ]
                    [ div [ class "twelve wide column" ]
                        [ viewQuestionLabel language Translate.PartnerHivTestResult ]
                    , div [ class "four wide column" ]
                        [ viewRedAlertForSelect
                            (form.partnerTestingResult |> Maybe.map List.singleton |> Maybe.withDefault [])
                            [ NoHivTesting, ResultHivNegative, ResultHivIndeterminate ]
                        , viewYellowAlertForSelect
                            (form.partnerTestingResult |> Maybe.map List.singleton |> Maybe.withDefault [])
                            [ NoHivTesting, ResultHivNegative, ResultHivPositive ]
                        ]
                    ]
                , option
                    [ value ""
                    , selected (form.partnerTestingResult == Nothing)
                    ]
                    [ text "" ]
                    :: ([ ResultHivNegative, ResultHivPositive, ResultHivIndeterminate ]
                            |> List.map
                                (\result ->
                                    option
                                        [ value (socialHistoryHivTestingResultToString result)
                                        , selected (form.partnerTestingResult == Just result)
                                        ]
                                        [ text <| translate language <| Translate.SocialHistoryHivTestingResult result ]
                                )
                       )
                    |> select [ onInput SetSocialHivTestingResult, class "form-input hiv-test-result" ]
                ]

            else
                []
    in
    (accompaniedQuestion ++ counselingQuestion ++ testingReceivedQuestion ++ testingResultQuestion)
        |> div [ class "form history social" ]


viewVitalsForm : Language -> NominalDate -> AssembledData -> VitalsForm -> Html Msg
viewVitalsForm language currentDate assembled form =
    let
        sysBloodPressureUpdateFunc value form_ =
            { form_ | sysBloodPressure = value }

        diaBloodPressureUpdateFunc value form_ =
            { form_ | diaBloodPressure = value }

        heartRateUpdateFunc value form_ =
            { form_ | heartRate = value }

        respiratoryRateUpdateFunc value form_ =
            { form_ | respiratoryRate = value }

        bodyTemperatureUpdateFunc value form_ =
            { form_ | bodyTemperature = value }

        sysBloodPressurePreviousValue =
            resolvePreviousValue assembled .vitals .sys

        diaBloodPressurePreviousValue =
            resolvePreviousValue assembled .vitals .dia

        heartRatePreviousValue =
            resolvePreviousValue assembled .vitals .heartRate
                |> Maybe.map toFloat

        respiratoryRatePreviousValue =
            resolvePreviousValue assembled .vitals .respiratoryRate
                |> Maybe.map toFloat

        bodyTemperaturePreviousValue =
            resolvePreviousValue assembled .vitals .bodyTemperature
    in
    div [ class "ui form examination vitals" ]
        [ div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.BloodPressure ]
            , viewWarning language Nothing
            ]
        , div [ class "ui grid systolic" ]
            [ div [ class "twelve wide column" ]
                [ div [ class "title sys" ] [ text <| translate language Translate.BloodPressureSysLabel ] ]
            , div [ class "four wide column" ]
                [ viewConditionalAlert form.sysBloodPressure
                    [ [ (<) 140 ] ]
                    []
                ]
            ]
        , viewMeasurementInput
            language
            form.sysBloodPressure
            (SetVitalsFloatMeasurement sysBloodPressureUpdateFunc)
            "sys-blood-pressure"
            Translate.MMHGUnit
        , viewPreviousMeasurement language sysBloodPressurePreviousValue Translate.MMHGUnit
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ div [ class "title dia" ] [ text <| translate language Translate.BloodPressureDiaLabel ] ]
            , div [ class "four wide column" ]
                [ viewConditionalAlert form.diaBloodPressure
                    [ [ (<) 90 ] ]
                    []
                ]
            ]
        , viewMeasurementInput
            language
            form.diaBloodPressure
            (SetVitalsFloatMeasurement diaBloodPressureUpdateFunc)
            "dia-blood-pressure"
            Translate.MMHGUnit
        , viewPreviousMeasurement language diaBloodPressurePreviousValue Translate.MMHGUnit
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.HeartRate ]
            , div [ class "four wide column" ]
                [ viewConditionalAlert form.heartRate
                    [ [ (>) 40 ], [ (<=) 120 ] ]
                    [ [ (<=) 40, (>=) 50 ], [ (<) 100, (>) 120 ] ]
                ]
            ]
        , viewMeasurementInput
            language
            (Maybe.map toFloat form.heartRate)
            (SetVitalsIntMeasurement heartRateUpdateFunc)
            "heart-rate"
            Translate.BpmUnit
        , viewPreviousMeasurement language heartRatePreviousValue Translate.BpmUnit
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.RespiratoryRate ]
            , div [ class "four wide column" ]
                [ viewConditionalAlert form.respiratoryRate
                    [ [ (>) 12 ], [ (<) 30 ] ]
                    [ [ (<=) 21, (>=) 30 ] ]
                ]
            ]
        , viewMeasurementInput
            language
            (Maybe.map toFloat form.respiratoryRate)
            (SetVitalsIntMeasurement respiratoryRateUpdateFunc)
            "respiratory-rate"
            Translate.BpmUnit
        , viewPreviousMeasurement language respiratoryRatePreviousValue Translate.BpmUnit
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.BodyTemperature ]
            , div [ class "four wide column" ]
                [ viewConditionalAlert form.bodyTemperature
                    [ [ (>) 35 ], [ (<) 37.5 ] ]
                    []
                ]
            ]
        , viewMeasurementInput
            language
            form.bodyTemperature
            (SetVitalsFloatMeasurement bodyTemperatureUpdateFunc)
            "body-temperature"
            Translate.Celsius
        , viewPreviousMeasurement language bodyTemperaturePreviousValue Translate.Celsius
        ]


viewNutritionAssessmentForm : Language -> NominalDate -> AssembledData -> NutritionAssessmentForm -> Bool -> Html Msg
viewNutritionAssessmentForm language currentDate assembled form hideHeightInput =
    let
        heightUpdateFunc value form_ =
            { form_ | height = value }

        weightUpdateFunc value form_ =
            { form_ | weight = value }

        bmiUpdateFunc value form_ =
            form_

        muacUpdateFunc value form_ =
            { form_ | muac = value }

        heightPreviousValue =
            resolvePreviousValue assembled .nutrition .height
                |> Maybe.map (\(HeightInCm cm) -> cm)

        weightPreviousValue =
            resolvePreviousValue assembled .nutrition .weight
                |> Maybe.map (\(WeightInKg kg) -> kg)

        bmiPreviousValue =
            calculateBmi heightPreviousValue weightPreviousValue
                |> Maybe.map (Round.roundNum 1)

        muacPreviousValue =
            resolvePreviousValue assembled .nutrition .muac
                |> Maybe.map (\(MuacInCm cm) -> cm)

        calculatedBmi =
            calculateBmi form.height form.weight
                |> Maybe.map (Round.roundNum 1)

        heightSection =
            if not hideHeightInput then
                [ div [ class "ui grid" ]
                    [ div [ class "eleven wide column" ]
                        [ viewLabel language Translate.Height ]
                    , viewWarning language Nothing
                    ]
                , viewMeasurementInput
                    language
                    form.height
                    (SetNutritionAssessmentMeasurement heightUpdateFunc)
                    "height"
                    Translate.CentimeterShorthand
                , viewPreviousMeasurement language Nothing Translate.EmptyString
                , div [ class "separator" ] []
                ]

            else
                []
    in
    div [ class "ui form examination nutrition-assessment" ] <|
        heightSection
            ++ [ div [ class "ui grid" ]
                    [ div [ class "eleven wide column" ]
                        [ viewLabel language Translate.Weight ]
                    , viewWarning language Nothing
                    ]
               , viewMeasurementInput
                    language
                    form.weight
                    (SetNutritionAssessmentMeasurement weightUpdateFunc)
                    "weight"
                    Translate.KilogramShorthand
               , viewPreviousMeasurement language weightPreviousValue Translate.KilogramShorthand
               , div [ class "separator" ] []
               , div [ class "ui grid" ]
                    [ div [ class "twelve wide column" ]
                        [ viewLabel language Translate.BMI ]
                    , div [ class "four wide column" ]
                        [ viewConditionalAlert calculatedBmi
                            [ [ (<) 30 ], [ (>) 18.5 ] ]
                            [ [ (>=) 30, (<=) 25 ] ]
                        ]
                    ]
               , div [ class "title bmi" ] [ text <| translate language Translate.BMIHelper ]
               , viewMeasurementInput
                    language
                    calculatedBmi
                    (SetNutritionAssessmentMeasurement bmiUpdateFunc)
                    "bmi disabled"
                    Translate.EmptyString
               , viewPreviousMeasurement language bmiPreviousValue Translate.EmptyString
               , div [ class "separator" ] []
               , div [ class "ui grid" ]
                    [ div [ class "twelve wide column" ]
                        [ viewLabel language Translate.MUAC ]
                    , div [ class "four wide column" ]
                        [ viewConditionalAlert form.muac
                            [ [ (>) 18.5 ] ]
                            [ [ (<=) 18.5, (>) 22 ] ]
                        ]
                    ]
               , viewMeasurementInput
                    language
                    form.muac
                    (SetNutritionAssessmentMeasurement muacUpdateFunc)
                    "muac"
                    Translate.CentimeterShorthand
               , viewPreviousMeasurement language muacPreviousValue Translate.CentimeterShorthand
               ]


viewCorePhysicalExamForm : Language -> NominalDate -> AssembledData -> CorePhysicalExamForm -> Html Msg
viewCorePhysicalExamForm language currentDate assembled form =
    let
        brittleHairUpdateFunc value form_ =
            { form_ | brittleHair = Just value }

        paleConjuctivaUpdateFunc value form_ =
            { form_ | paleConjuctiva = Just value }

        heartMurmurUpdateFunc value form_ =
            { form_ | heartMurmur = Just value }
    in
    div [ class "ui form examination core-physical-exam" ]
        [ div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.HeadHair ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.brittleHair False ]
            ]
        , viewBoolInput
            language
            form.brittleHair
            (SetCorePhysicalExamBoolInput brittleHairUpdateFunc)
            "head-hair"
            (Just ( Translate.BrittleHair, Translate.Normal ))
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.Eyes ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.paleConjuctiva False ]
            ]
        , viewBoolInput
            language
            form.paleConjuctiva
            (SetCorePhysicalExamBoolInput paleConjuctivaUpdateFunc)
            "eyes"
            (Just ( Translate.PaleConjuctiva, Translate.Normal ))
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.Neck ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.neck |> Maybe.withDefault [])
                    [ NormalNeck ]
                ]
            ]
        , viewCheckBoxMultipleSelectInput language
            [ EnlargedThyroid, EnlargedLymphNodes ]
            [ NormalNeck ]
            (form.neck |> Maybe.withDefault [])
            Nothing
            SetCorePhysicalExamNeck
            Translate.NeckCPESign
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.Heart ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.heart |> Maybe.map List.singleton |> Maybe.withDefault [])
                    [ NormalRateAndRhythm ]
                ]
            ]
        , viewCheckBoxSelectInput language
            [ IrregularRhythm, SinusTachycardia ]
            [ NormalRateAndRhythm ]
            form.heart
            SetCorePhysicalExamHeart
            Translate.HeartCPESign
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.HeartMurmur ]
            , div [ class "four wide column" ]
                [ viewRedAlertForBool form.heartMurmur False ]
            ]
        , viewBoolInput
            language
            form.heartMurmur
            (SetCorePhysicalExamBoolInput heartMurmurUpdateFunc)
            "heart-murmur"
            Nothing
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.Lungs ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.lungs |> Maybe.withDefault [])
                    [ NormalLungs ]
                ]
            ]
        , viewCheckBoxMultipleSelectInput language
            [ Wheezes, Crackles ]
            [ NormalLungs ]
            (form.lungs |> Maybe.withDefault [])
            Nothing
            SetCorePhysicalExamLungs
            Translate.LungsCPESign
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.Abdomen ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.abdomen |> Maybe.withDefault [])
                    [ NormalAbdomen ]
                ]
            ]
        , viewCheckBoxMultipleSelectInput language
            [ Hepatomegaly, Splenomegaly, TPRightUpper, TPLeftUpper ]
            [ NormalAbdomen, Hernia, TPRightLower, TPLeftLower ]
            (form.abdomen |> Maybe.withDefault [])
            Nothing
            SetCorePhysicalExamAbdomen
            Translate.AbdomenCPESign
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.Extremities ]
            ]
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ div [ class "title hands" ] [ text <| (translate language Translate.Hands ++ ":") ] ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.hands |> Maybe.withDefault [])
                    [ NormalHands ]
                ]
            ]
        , viewCheckBoxMultipleSelectInput language
            [ PallorHands, EdemaHands ]
            [ NormalHands ]
            (form.hands |> Maybe.withDefault [])
            Nothing
            SetCorePhysicalExamHands
            Translate.HandsCPESign
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ div [ class "title legs" ] [ text <| (translate language Translate.Legs ++ ":") ] ]
            , div [ class "four wide column" ]
                [ viewRedAlertForSelect
                    (form.legs |> Maybe.withDefault [])
                    [ NormalLegs ]
                ]
            ]
        , viewCheckBoxMultipleSelectInput language
            [ PallorLegs, EdemaLegs ]
            [ NormalLegs ]
            (form.legs |> Maybe.withDefault [])
            Nothing
            SetCorePhysicalExamLegs
            Translate.LegsCPESign
        ]


viewObstetricalExamForm : Language -> NominalDate -> AssembledData -> ObstetricalExamForm -> Html Msg
viewObstetricalExamForm language currentDate assembled form =
    let
        alerts =
            assembled.measurements.lastMenstrualPeriod
                |> Maybe.map
                    (\lastMenstrualPeriod ->
                        let
                            lmpDate =
                                Tuple.second lastMenstrualPeriod |> .value |> .date

                            egaInWeeks =
                                diffDays lmpDate currentDate // 7 |> toFloat

                            fundalHeightAlert =
                                viewConditionalAlert form.fundalHeight
                                    [ [ (>) (egaInWeeks - 4) ], [ (<=) (egaInWeeks + 4) ] ]
                                    [ [ (<=) (egaInWeeks - 4), (>) (egaInWeeks - 2) ], [ (<) (egaInWeeks + 2), (>=) (egaInWeeks + 4) ] ]

                            fetalPresentationAlert =
                                if egaInWeeks > 36 then
                                    viewConditionalAlert form.fetalPresentation
                                        [ [ (==) Cephalic ], [ (==) Twins ] ]
                                        []

                                else if egaInWeeks > 31 then
                                    viewConditionalAlert form.fetalPresentation
                                        []
                                        [ [ (==) Cephalic ], [ (==) Twins ] ]

                                else
                                    emptyNode

                            fetalMovementAlert =
                                if egaInWeeks > 19 then
                                    viewRedAlertForBool form.fetalMovement True

                                else
                                    emptyNode

                            fetalHeartRateAlert =
                                if egaInWeeks > 19 then
                                    viewConditionalAlert form.fetalHeartRate
                                        [ [ (>) 120 ], [ (<) 160 ] ]
                                        []

                                else
                                    emptyNode
                        in
                        { fundalHeight = fundalHeightAlert
                        , fetalPresentation = fetalPresentationAlert
                        , fetalMovement = fetalMovementAlert
                        , fetalHeartRate = fetalHeartRateAlert
                        }
                    )
                |> Maybe.withDefault
                    { fundalHeight = emptyNode
                    , fetalPresentation = emptyNode
                    , fetalMovement = emptyNode
                    , fetalHeartRate = emptyNode
                    }

        fundalHeightUpdateFunc value form_ =
            { form_ | fundalHeight = value }

        fetalHeartRateUpdateFunc value form_ =
            { form_ | fetalHeartRate = value }

        fetalMovementUpdateFunc value form_ =
            { form_ | fetalMovement = Just value }

        fetalHeartRatePreviousValue =
            resolvePreviousValue assembled .obstetricalExam .fetalHeartRate
                |> Maybe.map toFloat

        fundalHeightPreviousValue =
            resolvePreviousValue assembled .obstetricalExam .fundalHeight
                |> Maybe.map (\(HeightInCm cm) -> cm)
    in
    div [ class "ui form examination obstetrical-exam" ]
        [ div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.FundalHeight ]
            , div [ class "four wide column" ]
                [ alerts.fundalHeight ]
            ]
        , viewMeasurementInput
            language
            form.fundalHeight
            (SetObstetricalExamFloatMeasurement fundalHeightUpdateFunc)
            "fundal-height"
            Translate.CentimeterShorthand
        , viewPreviousMeasurement language fundalHeightPreviousValue Translate.CentimeterShorthand
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.FetalPresentationLabel ]
            , div [ class "four wide column" ]
                [ alerts.fetalPresentation ]
            ]
        , viewCheckBoxSelectInput language
            [ Transverse, Cephalic, Unknown ]
            [ FetalBreech, Twins ]
            form.fetalPresentation
            SetObstetricalExamFetalPresentation
            Translate.FetalPresentation
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.FetalMovement ]
            , div [ class "four wide column" ]
                [ alerts.fetalMovement ]
            ]
        , viewBoolInput
            language
            form.fetalMovement
            (SetObstetricalExamBoolInput fetalMovementUpdateFunc)
            "fetal-movement"
            Nothing
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.FetalHeartRate ]
            , div [ class "four wide column" ]
                [ alerts.fetalHeartRate ]
            ]
        , viewMeasurementInput
            language
            (Maybe.map toFloat form.fetalHeartRate)
            (SetObstetricalExamIntMeasurement fetalHeartRateUpdateFunc)
            "fetal-heart-rate"
            Translate.BpmUnit
        , viewPreviousMeasurement language fetalHeartRatePreviousValue Translate.BpmUnit
        , div [ class "separator" ] []
        , viewLabel language Translate.PreviousCSectionScar
        , viewCheckBoxSelectInput language
            [ Vertical, Horizontal ]
            [ NoScar ]
            form.cSectionScar
            SetObstetricalExamCSectionScar
            Translate.CSectionScar
        ]


viewBreastExamForm : Language -> NominalDate -> AssembledData -> BreastExamForm -> Html Msg
viewBreastExamForm language currentDate assembled form =
    let
        selfGuidanceUpdateFunc value form_ =
            { form_ | selfGuidance = Just value }
    in
    div [ class "ui form examination breast-exam" ]
        [ div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.BreastExam ]
            , div [ class "four wide column" ]
                [ viewYellowAlertForSelect
                    (form.breast |> Maybe.withDefault [])
                    [ NormalBreast ]
                ]
            ]
        , viewCheckBoxMultipleSelectInput language
            [ Mass, Discharge ]
            [ Infection, NormalBreast ]
            (form.breast |> Maybe.withDefault [])
            Nothing
            SetBreastExamBreast
            Translate.BreastExamSign
        , div [ class "separator double" ] []
        , viewCustomLabel language Translate.BreastExamQuestion "?" "label self-guidance"
        , viewBoolInput
            language
            form.selfGuidance
            (SetBreastExamBoolInput selfGuidanceUpdateFunc)
            "self-guidance"
            Nothing
        ]


viewMedicationForm : Language -> NominalDate -> AssembledData -> Bool -> MedicationForm -> Html Msg
viewMedicationForm language currentDate assembled showDewormingPillQuestion form =
    let
        receivedIronFolicAcidUpdateFunc value form_ =
            { form_ | receivedIronFolicAcid = Just value }

        receivedIronFolicAcidQuestion =
            [ viewQuestionLabel language Translate.ReceivedIronFolicAcid
            , viewBoolInput
                language
                form.receivedIronFolicAcid
                (SetMedicationBoolInput receivedIronFolicAcidUpdateFunc)
                "iron-folic-acid"
                Nothing
            ]

        receivedDewormingPillQuestion =
            if showDewormingPillQuestion then
                let
                    receivedDewormingPillUpdateFunc value form_ =
                        { form_ | receivedDewormingPill = Just value }
                in
                [ viewQuestionLabel language Translate.ReceivedDewormingPill
                , viewBoolInput
                    language
                    form.receivedDewormingPill
                    (SetMedicationBoolInput receivedDewormingPillUpdateFunc)
                    "deworming-pill"
                    Nothing
                ]

            else
                []

        questions =
            receivedIronFolicAcidQuestion ++ receivedDewormingPillQuestion
    in
    div [ class "ui form patient-provisions medication" ]
        questions


viewResourcesForm : Language -> NominalDate -> AssembledData -> ResourcesForm -> Html Msg
viewResourcesForm language currentDate assembled form =
    let
        receivedMosquitoNetUpdateFunc value form_ =
            { form_ | receivedMosquitoNet = Just value }
    in
    div [ class "ui form patient-provisions resources" ]
        [ viewQuestionLabel language Translate.ReceivedMosquitoNet
        , viewBoolInput
            language
            form.receivedMosquitoNet
            (SetResourcesBoolInput receivedMosquitoNetUpdateFunc)
            "mosquito-net"
            Nothing
        ]



-- Components


viewPreviousMeasurement : Language -> Maybe Float -> TranslationId -> Html any
viewPreviousMeasurement language maybePreviousValue unitTranslationId =
    let
        message =
            maybePreviousValue
                |> unwrap
                    (translate language Translate.PreviousMeasurementNotFound)
                    (\previousValue ->
                        (previousValue
                            |> Translate.PreviousFloatMeasurement
                            |> translate language
                        )
                            ++ " "
                            ++ translate language unitTranslationId
                    )
    in
    div [ class "previous-value" ] [ text message ]


viewRedAlertForSelect : List a -> List a -> Html any
viewRedAlertForSelect actual normal =
    viewAlertForSelect "red" actual normal


viewYellowAlertForSelect : List a -> List a -> Html any
viewYellowAlertForSelect actual normal =
    viewAlertForSelect "yellow" actual normal


viewAlertForSelect : String -> List a -> List a -> Html any
viewAlertForSelect color actual normal =
    if
        List.isEmpty actual
            || List.all
                (\item ->
                    List.member item normal
                )
                actual
    then
        emptyNode

    else
        div [ class <| "alert " ++ color ]
            [ viewAlert color ]


viewRedAlertForBool : Maybe Bool -> Bool -> Html any
viewRedAlertForBool actual normal =
    viewRedAlertForSelect
        (actual |> Maybe.map List.singleton |> Maybe.withDefault [])
        [ normal ]


viewNumberInput :
    Language
    -> Maybe a
    -> (String -> msg)
    -> String
    -> TranslationId
    -> Maybe ( List (List (a -> Bool)), List (List (a -> Bool)) )
    -> Html msg
viewNumberInput language maybeCurrentValue setMsg inputClass labelTranslationId maybeAlertConditions =
    let
        currentValue =
            maybeCurrentValue
                |> unwrap
                    ""
                    Debug.toString

        ( labelWidth, inputWidth, alert ) =
            maybeAlertConditions
                |> Maybe.map
                    (\( red, yellow ) ->
                        ( "eight"
                        , "four"
                        , div [ class "four wide column" ]
                            [ viewConditionalAlert maybeCurrentValue red yellow ]
                        )
                    )
                |> Maybe.withDefault ( "ten", "six", emptyNode )
    in
    div [ class <| "form-input number " ++ inputClass ]
        [ div [ class "ui grid" ]
            [ div [ class <| labelWidth ++ " wide column" ]
                [ viewLabel language labelTranslationId ]
            , div [ class <| inputWidth ++ " wide column" ]
                [ input
                    [ type_ "number"
                    , Html.Attributes.min "0"
                    , Html.Attributes.max "99"
                    , onInput setMsg
                    , value currentValue
                    ]
                    []
                ]
            , alert
            ]
        ]


{-| The idea here is that we get lists for red alert conditions, and yellow
alert conditions. If any of red conditions matches, we present red alert.
If any of yellow conditions matches, we present yellow alert.
Otherwise, no alret is needed.

Note that conditions are list of lists, so all conditions in inner list
need to match, for a condition in outer list to match.
We need this for range conditions. For example, number between 5 and 8.

-}
viewConditionalAlert : Maybe a -> List (List (a -> Bool)) -> List (List (a -> Bool)) -> Html any
viewConditionalAlert maybeActual redConditions yellowConditions =
    maybeActual
        |> Maybe.map
            (\actual ->
                if
                    List.any
                        (\conditions ->
                            List.all
                                (\condition ->
                                    condition actual
                                )
                                conditions
                        )
                        redConditions
                then
                    viewAlert "red"

                else if
                    List.any
                        (\conditions ->
                            List.all (\condition -> condition actual) conditions
                        )
                        yellowConditions
                then
                    viewAlert "yellow"

                else
                    emptyNode
            )
        |> Maybe.withDefault emptyNode


viewWarning : Language -> Maybe String -> Html any
viewWarning language maybeMessage =
    maybeMessage
        |> unwrap
            emptyNode
            (\message ->
                div [ class "five wide column" ]
                    [ text message ]
            )


viewAlert : String -> Html any
viewAlert color =
    let
        icon =
            "assets/images/alert-" ++ color ++ ".png"
    in
    img [ src icon ] []



-- Helper functions


resolvePreviousValue : AssembledData -> (PrenatalMeasurements -> Maybe ( id, PrenatalMeasurement a )) -> (a -> b) -> Maybe b
resolvePreviousValue assembled measurementFunc valueFunc =
    assembled.previousMeasurementsWithDates
        |> List.filterMap
            (\( _, measurements ) ->
                measurementFunc measurements
                    |> Maybe.map (Tuple.second >> .value >> valueFunc)
            )
        |> List.reverse
        |> List.head
