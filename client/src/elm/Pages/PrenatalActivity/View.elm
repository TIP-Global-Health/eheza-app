module Pages.PrenatalActivity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Encoder exposing (pregnancyTestResultAsString, socialHistoryHivTestingResultToString)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, heightValueFunc, muacIndication, muacValueFunc, prenatalTestResultToString, weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalActivity.Model exposing (PrenatalActivity(..))
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterType(..))
import Date exposing (Unit(..))
import DateSelector.SelectorDropdown
import EverySet
import Gizra.Html exposing (divKeyed, emptyNode, keyed, keyedDivKeyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Decoder exposing (decodeDropZoneFile)
import Measurement.Model exposing (InvokationModule(..), SendToHCForm, VitalsForm, VitalsFormMode(..))
import Measurement.Utils exposing (sendToHCFormWithDefault, vitalsFormWithDefault)
import Measurement.View exposing (viewActionTakenLabel, viewSendToHealthCenterForm)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalActivity.Model exposing (..)
import Pages.PrenatalActivity.Utils exposing (..)
import Pages.PrenatalEncounter.Model exposing (AssembledData)
import Pages.PrenatalEncounter.Utils exposing (..)
import Pages.PrenatalEncounter.View exposing (generateActivityData, viewMotherAndMeasurements)
import Pages.Utils
    exposing
        ( isTaskCompleted
        , taskAllCompleted
        , taskCompleted
        , tasksBarId
        , viewAlert
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewConditionalAlert
        , viewCustomLabel
        , viewLabel
        , viewMeasurementInput
        , viewPhotoThumbFromPhotoUrl
        , viewPreviousMeasurement
        , viewQuestionLabel
        , viewRedAlertForBool
        , viewRedAlertForSelect
        , viewSaveAction
        , viewYellowAlertForSelect
        )
import RemoteData exposing (RemoteData(..), WebData)
import Round
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)


view : Language -> NominalDate -> PrenatalEncounterId -> Bool -> PrenatalActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id isChw activity db model =
    let
        assembled =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate id isChw activity db model) identity assembled


viewHeaderAndContent : Language -> NominalDate -> PrenatalEncounterId -> Bool -> PrenatalActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate id isChw activity db model assembled =
    div [ class "page-activity prenatal" ] <|
        [ viewHeader language id activity assembled
        , viewContent language currentDate isChw activity db model assembled
        , viewModal <|
            warningPopup language currentDate model.warningPopupState
        ]


viewHeader : Language -> PrenatalEncounterId -> PrenatalActivity -> AssembledData -> Html Msg
viewHeader language id activity assembled =
    let
        ( label_, icon ) =
            generateActivityData activity assembled

        label =
            if icon == "appointment-confirmation" then
                Translate.ScheduleFollowUp

            else
                label_
    in
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language label ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage id
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> Bool -> PrenatalActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate isChw activity db model assembled =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate isChw assembled (Just ( model.showAlertsDialog, SetAlertsDialogState ))
            ++ viewActivity language currentDate activity assembled db model


warningPopup : Language -> NominalDate -> Maybe String -> Maybe (Html Msg)
warningPopup language currentDate dangerSigns =
    dangerSigns
        |> Maybe.map
            (\signs ->
                div [ class "ui active modal diagnosis-popup" ]
                    [ div [ class "content" ] <|
                        [ div [ class "popup-heading-wrapper" ]
                            [ img [ src "assets/images/exclamation-red.png" ] []
                            , div [ class "popup-heading" ] [ text <| translate language Translate.Warning ++ "!" ]
                            ]
                        , div [ class "popup-title" ]
                            [ p [] [ text <| translate language Translate.DangerSignsLabel ++ ": " ++ signs ]
                            , p [] [ text <| translate language Translate.DangerSignsHelper ]
                            ]
                        ]
                    , div
                        [ class "actions" ]
                        [ button
                            [ class "ui primary fluid button"
                            , onClick <| SetWarningPopupState Nothing
                            ]
                            [ text <| translate language Translate.Continue ]
                        ]
                    ]
            )


viewActivity : Language -> NominalDate -> PrenatalActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate activity assembled db model =
    case activity of
        PregnancyDating ->
            viewPregnancyDatingContent language currentDate assembled model.pregnancyDatingData

        History ->
            viewHistoryContent language currentDate assembled model.historyData

        Examination ->
            viewExaminationContent language currentDate assembled model.examinationData

        FamilyPlanning ->
            viewFamilyPlanningContent language currentDate assembled model.familyPlanningData

        DangerSigns ->
            viewDangerSignsContent language currentDate assembled model.dangerSignsData

        PrenatalPhoto ->
            viewPrenatalPhotoContent language currentDate assembled model.prenatalPhotoData

        Laboratory ->
            viewLaboratoryContent language currentDate assembled db model.laboratoryData

        Backend.PrenatalActivity.Model.HealthEducation ->
            viewHealthEducationContent language currentDate assembled model.healthEducationData

        BirthPlan ->
            viewBirthPlanContent language currentDate assembled model.birthPlanData

        NextSteps ->
            viewNextStepsContent language currentDate assembled model.nextStepsData

        Backend.PrenatalActivity.Model.MalariaPrevention ->
            viewMalariaPreventionContent language currentDate assembled model.malariaPreventionData

        Backend.PrenatalActivity.Model.Medication ->
            viewMedicationContent language currentDate assembled model.medicationData

        PregnancyOutcome ->
            -- When selected, we redirect to Pregannacy Outcome page.
            []


viewPregnancyDatingContent : Language -> NominalDate -> AssembledData -> PregnancyDatingData -> List (Html Msg)
viewPregnancyDatingContent language currentDate assembled data =
    let
        form =
            assembled.measurements.lastMenstrualPeriod
                |> getMeasurementValueFunc
                |> lastMenstrualPeriodFormWithDefault data.form

        chwLmpConfirmationSection dateToConfirm =
            [ viewCustomLabel language Translate.LmpDateConfirmationLabel "." "label"
            , viewLabel language Translate.LmpLabel
            , p [ class "chw-lmp" ] [ text <| formatDDMMYYYY dateToConfirm ]
            , viewQuestionLabel language Translate.LmpDateConfirmationQuestion
            , viewBoolInput language form.chwLmpConfirmation (SetConfirmLmpDate dateToConfirm) "confirm-lmp" Nothing
            ]

        chwLmpConfirmationTasksCompleted =
            taskCompleted form.chwLmpConfirmation

        newLmpInputSection =
            [ viewQuestionLabel language Translate.LmpRangeHeader
            , lmpRangeInput
            , viewLabel language Translate.LmpDateHeader
            , div [ class "form-input date" ]
                [ lmpDateInput ]
            , viewQuestionLabel language Translate.LmpDateConfidentHeader
            , viewBoolInput language form.lmpDateConfident SetLmpDateConfident "is-confident" Nothing
            ]

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
                |> select [ onInput SetLmpRange, class "form-input select" ]

        lmpDateInput =
            if isJust form.lmpRange then
                DateSelector.SelectorDropdown.view
                    ToggleDateSelector
                    SetLmpDate
                    form.isDateSelectorOpen
                    (Date.add Days -280 currentDate)
                    currentDate
                    form.lmpDate

            else
                emptyNode

        newLmpInputTasksCompleted =
            taskCompleted form.lmpDate + taskCompleted form.lmpDateConfident

        ( inputs, tasksCompleted, totalTasks ) =
            if assembled.encounter.encounterType == NurseEncounter then
                let
                    lmpDateTakenByChw =
                        List.head assembled.chwPreviousMeasurementsWithDates
                            |> Maybe.andThen
                                (\( _, _, measurements ) ->
                                    getLmpDate measurements
                                )
                in
                Maybe.map
                    (\lmpDateByChw ->
                        if form.chwLmpConfirmation == Just False then
                            ( chwLmpConfirmationSection lmpDateByChw ++ newLmpInputSection
                            , chwLmpConfirmationTasksCompleted + newLmpInputTasksCompleted
                            , 3
                            )

                        else
                            ( chwLmpConfirmationSection lmpDateByChw
                            , chwLmpConfirmationTasksCompleted
                            , 1
                            )
                    )
                    lmpDateTakenByChw
                    |> Maybe.withDefault
                        ( newLmpInputSection
                        , newLmpInputTasksCompleted
                        , 2
                        )

            else
                ( newLmpInputSection
                , newLmpInputTasksCompleted
                , 2
                )

        ( edd, ega ) =
            generateEDDandEGA language currentDate ( "", "" ) form.lmpDate
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "form pregnancy-dating" ] <|
                inputs
                    ++ [ div [ class "separator" ] []
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
            isFirstEncounter assembled

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
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
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
                                        |> getMeasurementValueFunc
                                        |> obstetricHistoryFormWithDefault data.obstetricFormFirstStep
                            in
                            viewObstetricFormFirstStep language currentDate assembled formStep1_

                        ObstetricHistorySecondStep ->
                            let
                                formStep2_ =
                                    assembled.measurements.obstetricHistoryStep2
                                        |> getMeasurementValueFunc
                                        |> obstetricHistoryStep2FormWithDefault data.obstetricFormSecondStep
                            in
                            viewObstetricFormSecondStep language currentDate assembled formStep2_

                Medical ->
                    let
                        medicalForm =
                            assembled.measurements.medicalHistory
                                |> getMeasurementValueFunc
                                |> medicalHistoryFormWithDefault data.medicalForm
                    in
                    viewMedicalForm language currentDate assembled medicalForm

                Social ->
                    let
                        socialForm =
                            assembled.measurements.socialHistory
                                |> getMeasurementValueFunc
                                |> socialHistoryFormWithDefault data.socialForm

                        showCounselingQuestion =
                            assembled.nursePreviousMeasurementsWithDates
                                |> List.filter
                                    (\( _, measurements ) ->
                                        measurements.socialHistory
                                            |> Maybe.map (Tuple.second >> .value >> .socialHistory >> EverySet.member PartnerHivCounseling)
                                            |> Maybe.withDefault False
                                    )
                                |> List.isEmpty

                        showTestingQuestions =
                            assembled.nursePreviousMeasurementsWithDates
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
            isFirstEncounter assembled

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
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.ExaminationTask task)
                    ]
                ]

        viewForm =
            case data.activeTask of
                Vitals ->
                    let
                        form =
                            assembled.measurements.vitals
                                |> getMeasurementValueFunc
                                |> vitalsFormWithDefault data.vitalsForm
                    in
                    viewVitalsForm language currentDate assembled form

                NutritionAssessment ->
                    let
                        hideHeightInput =
                            not firstEnconter

                        form_ =
                            assembled.measurements.nutrition
                                |> getMeasurementValueFunc
                                |> prenatalNutritionFormWithDefault data.nutritionAssessmentForm

                        form =
                            if hideHeightInput then
                                assembled.nursePreviousMeasurementsWithDates
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
                                |> getMeasurementValueFunc
                                |> corePhysicalExamFormWithDefault data.corePhysicalExamForm
                    in
                    viewCorePhysicalExamForm language currentDate assembled form

                ObstetricalExam ->
                    let
                        form =
                            assembled.measurements.obstetricalExam
                                |> getMeasurementValueFunc
                                |> obstetricalExamFormWithDefault data.obstetricalExamForm
                    in
                    viewObstetricalExamForm language currentDate assembled form

                BreastExam ->
                    let
                        form =
                            assembled.measurements.breastExam
                                |> getMeasurementValueFunc
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
                                    isFirstEncounter assembled |> not

                                maybeHeight =
                                    if passHeight then
                                        assembled.nursePreviousMeasurementsWithDates
                                            |> List.head
                                            |> Maybe.andThen (Tuple.second >> getMotherHeightMeasurement)
                                            |> Maybe.map heightValueFunc

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
                |> getMeasurementValueFunc
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


viewMedicationContent : Language -> NominalDate -> AssembledData -> MedicationData -> List (Html Msg)
viewMedicationContent language currentDate assembled data =
    let
        form =
            assembled.measurements.medication
                |> getMeasurementValueFunc
                |> medicationFormWithDefault data.form

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
                            assembled.nursePreviousMeasurementsWithDates
                                |> List.filter
                                    (\( _, measurements ) ->
                                        measurements.medication
                                            |> Maybe.map (Tuple.second >> .value >> EverySet.member DewormingPill)
                                            |> Maybe.withDefault False
                                    )
                                |> List.isEmpty
                    )
                |> Maybe.withDefault False

        ( tasksCompleted, totalTasks ) =
            let
                tasks =
                    if showDewormingPillQuestion then
                        [ form.receivedIronFolicAcid, form.receivedDewormingPill ]

                    else
                        [ form.receivedIronFolicAcid ]
            in
            ( List.map taskCompleted tasks
                |> List.sum
            , List.length tasks
            )

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
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form medication" ]
                questions
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SaveMedication assembled.participant.person assembled.measurements.medication
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewMalariaPreventionContent : Language -> NominalDate -> AssembledData -> MalariaPreventionData -> List (Html Msg)
viewMalariaPreventionContent language currentDate assembled data =
    let
        form =
            assembled.measurements.malariaPrevention
                |> getMeasurementValueFunc
                |> malariaPreventionFormWithDefault data.form

        tasksCompleted =
            taskCompleted form.receivedMosquitoNet

        totalTasks =
            1

        receivedMosquitoNetUpdateFunc value form_ =
            { form_ | receivedMosquitoNet = Just value }
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form malaria-prevention" ]
                [ viewQuestionLabel language Translate.ReceivedMosquitoNet
                , viewBoolInput
                    language
                    form.receivedMosquitoNet
                    (SetMalariaPreventionBoolInput receivedMosquitoNetUpdateFunc)
                    "mosquito-net"
                    Nothing
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SaveMalariaPrevention assembled.participant.person assembled.measurements.malariaPrevention
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewDangerSignsContent : Language -> NominalDate -> AssembledData -> DangerSignsData -> List (Html Msg)
viewDangerSignsContent language currentDate assembled data =
    let
        form =
            assembled.measurements.dangerSigns
                |> getMeasurementValueFunc
                |> dangerSignsFormWithDefault data.form

        ( inputs, tasksCompleted, totalTasks ) =
            case assembled.encounter.encounterType of
                ChwPostpartumEncounter ->
                    ( [ viewLabel language Translate.SelectPostpartumMotherDangerSigns
                      , viewCheckBoxMultipleSelectInput language
                            [ PostpartumMotheUterineBleeding, PostpartumMotherFever, PostpartumMotherMigraine ]
                            [ PostpartumMotherParalysis, PostpartumMotherAcuteAbdominalPain, PostpartumMotherLabouredBreathing ]
                            (form.postpartumMother |> Maybe.withDefault [])
                            (Just NoPostpartumMotherDangerSigns)
                            SetPostpartumMotherDangerSign
                            Translate.PostpartumMotherDangerSign
                      , viewLabel language Translate.SelectPostpartumChildDangerSigns
                      , viewCheckBoxMultipleSelectInput language
                            [ PostpartumChildInabilityToSuckle, PostpartumChildParalysis, PostpartumChildLabouredBreathing ]
                            [ PostpartumChildAbnormalTemperature, PostpartumChildInactiveNoMovement, PostpartumChildBodyTurnedYellow ]
                            (form.postpartumChild |> Maybe.withDefault [])
                            (Just NoPostpartumChildDangerSigns)
                            SetPostpartumChildDangerSign
                            Translate.PostpartumChildDangerSign
                      ]
                    , taskCompleted form.postpartumMother + taskCompleted form.postpartumChild
                    , 2
                    )

                _ ->
                    ( [ viewLabel language Translate.SelectDangerSigns
                      , viewCheckBoxMultipleSelectInput language
                            [ VaginalBleeding, HeadacheBlurredVision, Convulsions, AbdominalPain ]
                            [ DifficultyBreathing, Fever, ExtremeWeakness ]
                            (form.signs |> Maybe.withDefault [])
                            (Just NoDangerSign)
                            SetDangerSign
                            Translate.DangerSign
                      ]
                    , taskCompleted form.signs
                    , 1
                    )
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form danger-signs" ] inputs
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
                    ( getMeasurementValueFunc
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


viewBirthPlanContent : Language -> NominalDate -> AssembledData -> BirthPlanData -> List (Html Msg)
viewBirthPlanContent language currentDate assembled data =
    let
        totalTasks =
            6

        tasksCompleted =
            taskCompleted form.haveInsurance
                + taskCompleted form.boughtClothes
                + taskCompleted form.caregiverAccompany
                + taskCompleted form.savedMoney
                + taskCompleted form.haveTransportation
                + taskCompleted form.familyPlanning

        form =
            assembled.measurements.birthPlan
                |> getMeasurementValueFunc
                |> birthPlanFormWithDefault data.form

        healthInsuranceFunc value form_ =
            { form_ | haveInsurance = Just value }

        boughtClothesFunc value form_ =
            { form_ | boughtClothes = Just value }

        caregiverAccompanyFunc value form_ =
            { form_ | caregiverAccompany = Just value }

        savedMoneyFunc value form_ =
            { form_ | savedMoney = Just value }

        transportationFunc value form_ =
            { form_ | haveTransportation = Just value }
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form birth-plan" ]
                [ viewQuestionLabel language Translate.HealthInsuranceQuestion
                , viewBoolInput
                    language
                    form.haveInsurance
                    (SetBirthPlanBoolInput healthInsuranceFunc)
                    "insurance"
                    Nothing
                , viewQuestionLabel language Translate.BoughtClothesQuestion
                , viewBoolInput
                    language
                    form.boughtClothes
                    (SetBirthPlanBoolInput boughtClothesFunc)
                    "clothes"
                    Nothing
                , viewQuestionLabel language Translate.CaregiverAccompanyQuestion
                , viewBoolInput
                    language
                    form.caregiverAccompany
                    (SetBirthPlanBoolInput caregiverAccompanyFunc)
                    "caregiver-accompany"
                    Nothing
                , viewQuestionLabel language Translate.SavedMoneyQuestion
                , viewBoolInput
                    language
                    form.savedMoney
                    (SetBirthPlanBoolInput savedMoneyFunc)
                    "saved-money"
                    Nothing
                , viewQuestionLabel language Translate.FamilyPlanningInFutureQuestion
                , viewCheckBoxMultipleSelectInput language
                    [ AutoObservation, Condoms, CycleBeads, CycleCounting, Hysterectomy, Implants, Injectables ]
                    [ IUD, LactationAmenorrhea, OralContraceptives, Spermicide, TubalLigatures, Vasectomy ]
                    (form.familyPlanning |> Maybe.withDefault [])
                    (Just NoFamilyPlanning)
                    SetBirthPlanFamilyPlanning
                    Translate.FamilyPlanningSignLabel
                , viewQuestionLabel language Translate.TransportationPlanQuestion
                , viewBoolInput
                    language
                    form.haveTransportation
                    (SetBirthPlanBoolInput transportationFunc)
                    "saved-money"
                    Nothing
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SaveBirthPlan assembled.participant.person assembled.measurements.birthPlan
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewLaboratoryContent : Language -> NominalDate -> AssembledData -> ModelIndexedDb -> LaboratoryData -> List (Html Msg)
viewLaboratoryContent language currentDate assembled db data =
    if assembled.encounter.encounterType == NurseEncounter then
        viewLaboratoryContentForNurse language currentDate assembled db data

    else
        viewLaboratoryContentForChw language currentDate assembled data


viewLaboratoryContentForNurse : Language -> NominalDate -> AssembledData -> ModelIndexedDb -> LaboratoryData -> List (Html Msg)
viewLaboratoryContentForNurse language currentDate assembled db data =
    let
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            List.filter (expectLaboratoryTask currentDate assembled db) laboratoryTasks

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        TaskHIVTest ->
                            ( "laboratory-hiv"
                            , isJust measurements.hivTest
                            )

                        TaskSyphilisTest ->
                            ( "laboratory-syphilis"
                            , isJust measurements.syphilisTest
                            )

                        TaskHepatitisBTest ->
                            ( "laboratory-hepatitis-b"
                            , isJust measurements.hepatitisBTest
                            )

                        TaskMalariaTest ->
                            ( "laboratory-malaria-testing"
                            , isJust measurements.malariaTest
                            )

                        TaskBloodGpRsTest ->
                            ( "laboratory-blood-group"
                            , isJust measurements.bloodGpRsTest
                            )

                        TaskUrineDipstickTest ->
                            ( "laboratory-urine-dipstick"
                            , isJust measurements.urineDipstickTest
                            )

                        TaskHemoglobinTest ->
                            ( "laboratory-hemoglobin"
                            , isJust measurements.hemoglobinTest
                            )

                        TaskRandomBloodSugarTest ->
                            ( "laboratory-blood-sugar"
                            , isJust measurements.randomBloodSugarTest
                            )

                isActive =
                    activeTask == Just task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveLaboratoryTask task ]
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
                            measurements.hivTest
                                |> getMeasurementValueFunc
                                |> prenatalRDTFormWithDefault data.hivTestForm
                                |> viewPrenatalRDTForm language currentDate TaskHIVTest

                        TaskSyphilisTest ->
                            measurements.syphilisTest
                                |> getMeasurementValueFunc
                                |> prenatalNonRDTFormWithDefault data.syphilisTestForm
                                |> viewPrenatalNonRDTForm language currentDate TaskSyphilisTest

                        TaskHepatitisBTest ->
                            measurements.hepatitisBTest
                                |> getMeasurementValueFunc
                                |> prenatalNonRDTFormWithDefault data.hepatitisBTestForm
                                |> viewPrenatalNonRDTForm language currentDate TaskHepatitisBTest

                        TaskMalariaTest ->
                            measurements.malariaTest
                                |> getMeasurementValueFunc
                                |> prenatalRDTFormWithDefault data.malariaTestForm
                                |> viewPrenatalRDTForm language currentDate TaskMalariaTest

                        TaskBloodGpRsTest ->
                            measurements.bloodGpRsTest
                                |> getMeasurementValueFunc
                                |> prenatalNonRDTFormWithDefault data.bloodGpRsTestForm
                                |> viewPrenatalNonRDTForm language currentDate TaskBloodGpRsTest

                        TaskUrineDipstickTest ->
                            measurements.urineDipstickTest
                                |> getMeasurementValueFunc
                                |> prenatalUrineDipstickFormWithDefault data.urineDipstickTestForm
                                |> viewPrenatalUrineDipstickForm language currentDate

                        TaskHemoglobinTest ->
                            measurements.hemoglobinTest
                                |> getMeasurementValueFunc
                                |> prenatalNonRDTFormWithDefault data.hemoglobinTestForm
                                |> viewPrenatalNonRDTForm language currentDate TaskHemoglobinTest

                        TaskRandomBloodSugarTest ->
                            measurements.randomBloodSugarTest
                                |> getMeasurementValueFunc
                                |> prenatalNonRDTFormWithDefault data.randomBloodSugarTestForm
                                |> viewPrenatalNonRDTForm language currentDate TaskRandomBloodSugarTest
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
            Maybe.map
                (\task ->
                    let
                        saveMsg =
                            case task of
                                TaskHIVTest ->
                                    SaveHIVTest personId measurements.hivTest nextTask

                                TaskSyphilisTest ->
                                    SaveSyphilisTest personId measurements.syphilisTest nextTask

                                TaskHepatitisBTest ->
                                    SaveHepatitisBTest personId measurements.hepatitisBTest nextTask

                                TaskMalariaTest ->
                                    SaveMalariaTest personId measurements.malariaTest nextTask

                                TaskBloodGpRsTest ->
                                    SaveBloodGpRsTest personId measurements.bloodGpRsTest nextTask

                                TaskUrineDipstickTest ->
                                    SaveUrineDipstickTest personId measurements.urineDipstickTest nextTask

                                TaskHemoglobinTest ->
                                    SaveHemoglobinTest personId measurements.hemoglobinTest nextTask

                                TaskRandomBloodSugarTest ->
                                    SaveRandomBloodSugarTest personId measurements.randomBloodSugarTest nextTask
                    in
                    viewSaveAction language saveMsg (tasksCompleted /= totalTasks)
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
            [ viewForm, actions ]
        ]
    ]


viewLaboratoryContentForChw : Language -> NominalDate -> AssembledData -> LaboratoryData -> List (Html Msg)
viewLaboratoryContentForChw language currentDate assembled data =
    let
        form =
            assembled.measurements.pregnancyTest
                |> getMeasurementValueFunc
                |> pregnancyTestFormWithDefault data.pregnancyTestForm

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.pregnancyTestResult

        emptyOption =
            if isNothing form.pregnancyTestResult then
                option
                    [ value ""
                    , selected (form.pregnancyTestResult == Nothing)
                    ]
                    [ text "" ]

            else
                emptyNode

        resultInput =
            emptyOption
                :: ([ PregnancyTestPositive, PregnancyTestNegative, PregnancyTestIndeterminate, PregnancyTestUnableToConduct ]
                        |> List.map
                            (\result ->
                                option
                                    [ value (pregnancyTestResultAsString result)
                                    , selected (form.pregnancyTestResult == Just result)
                                    ]
                                    [ text <| translate language <| Translate.PregnancyTestResult result ]
                            )
                   )
                |> select [ onInput SetPregnancyTestResult, class "form-input select" ]
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form laboratory pregnancy-testing" ] <|
                [ viewLabel language Translate.PregnancyUrineTest
                , resultInput
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SavePregnancyTest assembled.participant.person assembled.measurements.pregnancyTest
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewHealthEducationContent : Language -> NominalDate -> AssembledData -> HealthEducationData -> List (Html Msg)
viewHealthEducationContent language currentDate assembled data =
    let
        totalTasks =
            List.length tasks

        tasksCompleted =
            List.map taskCompleted tasks
                |> List.sum

        ( inputs, tasks ) =
            healthEducationFormInputsAndTasks language assembled data.form
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form health-education" ]
                inputs
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SaveHealthEducation assembled.participant.person assembled.measurements.healthEducation
                ]
                [ text <| translate language Translate.Save ]
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
            Maybe.Extra.or data.activeTask (List.head tasks)

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        NextStepsAppointmentConfirmation ->
                            ( "next-steps-send-to-hc"
                            , isJust measurements.appointmentConfirmation
                            )

                        NextStepsFollowUp ->
                            ( "next-steps-follow-up"
                            , isJust measurements.followUp
                            )

                        NextStepsSendToHC ->
                            ( "next-steps-send-to-hc"
                            , isJust measurements.sendToHC
                            )

                        NextStepsHealthEducation ->
                            ( "next-steps-health-education"
                            , isJust measurements.healthEducation
                            )

                        NextStepsNewbornEnrolment ->
                            ( "next-steps-newborn-enrolment"
                            , isJust assembled.participant.newborn
                            )

                isActive =
                    activeTask == Just task

                navigationAction =
                    if isActive then
                        []

                    else
                        [ onClick <| SetActiveNextStepsTask task ]

                attributes =
                    classList
                        [ ( "link-section", True )
                        , ( "active", isActive )
                        , ( "completed", not isActive && isCompleted )
                        ]
                        :: navigationAction
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.PrenatalNextStepsTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, nextStepsTasksCompletedFromTotal language assembled data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just NextStepsAppointmentConfirmation ->
                    measurements.appointmentConfirmation
                        |> getMeasurementValueFunc
                        |> appointmentConfirmationFormWithDefault data.appointmentConfirmationForm
                        |> viewAppointmentConfirmationForm language currentDate assembled

                Just NextStepsFollowUp ->
                    measurements.followUp
                        |> getMeasurementValueFunc
                        |> followUpFormWithDefault data.followUpForm
                        |> viewFollowUpForm language currentDate assembled

                Just NextStepsSendToHC ->
                    measurements.sendToHC
                        |> getMeasurementValueFunc
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> viewSendToHealthCenterForm language
                            currentDate
                            SetReferToHealthCenter
                            SetReasonForNotSendingToHC
                            SetHandReferralForm
                            (Just SetAccompanyToHC)

                Just NextStepsHealthEducation ->
                    measurements.healthEducation
                        |> getMeasurementValueFunc
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> viewHealthEducationForm language currentDate assembled

                Just NextStepsNewbornEnrolment ->
                    viewNewbornEnrolmentForm language currentDate assembled

                Nothing ->
                    emptyNode

        nextTask =
            tasks
                |> List.filter
                    (\task ->
                        (Just task /= activeTask)
                            && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
                    )
                |> List.head

        actions =
            activeTask
                |> Maybe.map
                    (\task ->
                        let
                            saveMsg =
                                case task of
                                    NextStepsAppointmentConfirmation ->
                                        SaveAppointmentConfirmation personId measurements.appointmentConfirmation nextTask

                                    NextStepsFollowUp ->
                                        let
                                            assesment =
                                                generatePrenatalAssesment assembled
                                        in
                                        SaveFollowUp personId assesment measurements.followUp nextTask

                                    NextStepsSendToHC ->
                                        SaveSendToHC personId measurements.sendToHC nextTask

                                    NextStepsHealthEducation ->
                                        SaveHealthEducationSubActivity personId measurements.healthEducation nextTask

                                    NextStepsNewbornEnrolment ->
                                        SaveNewbornEnrollment nextTask
                        in
                        div [ class "actions next-steps" ]
                            [ button
                                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                                , onClick saveMsg
                                ]
                                [ text <| translate language Translate.Save ]
                            ]
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
            [ viewForm
            , actions
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
        [ viewQuestionLabel language Translate.CurrentlyPregnant
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
        formConfig =
            { setIntInputMsg = SetVitalsIntInput
            , setFloatInputMsg = SetVitalsFloatInput
            , sysBloodPressurePreviousValue = resolvePreviousValue assembled .vitals .sys
            , diaBloodPressurePreviousValue = resolvePreviousValue assembled .vitals .dia
            , heartRatePreviousValue =
                resolvePreviousValue assembled .vitals .heartRate
                    |> Maybe.map toFloat
            , respiratoryRatePreviousValue =
                resolvePreviousValue assembled .vitals .respiratoryRate
                    |> Maybe.map toFloat
            , bodyTemperaturePreviousValue = resolvePreviousValue assembled .vitals .bodyTemperature
            , birthDate = assembled.person.birthDate
            , formClass = "examination vitals"
            , mode = VitalsFormFull
            , invokationModule = InvokationModulePrenatal
            }
    in
    Measurement.View.viewVitalsForm language currentDate formConfig form


viewNutritionAssessmentForm : Language -> NominalDate -> AssembledData -> NutritionAssessmentForm -> Bool -> Html Msg
viewNutritionAssessmentForm language currentDate assembled form hideHeightInput =
    let
        heightUpdateFunc value form_ =
            { form_ | height = value, heightDirty = True }

        weightUpdateFunc value form_ =
            { form_ | weight = value, weightDirty = True }

        bmiUpdateFunc value form_ =
            form_

        muacUpdateFunc value form_ =
            { form_ | muac = value, muacDirty = True }

        heightPreviousValue =
            resolvePreviousValue assembled .nutrition .height
                |> Maybe.map heightValueFunc

        weightPreviousValue =
            resolvePreviousValue assembled .nutrition .weight
                |> Maybe.map weightValueFunc

        bmiPreviousValue =
            calculateBmi heightPreviousValue weightPreviousValue
                |> Maybe.map (Round.roundNum 1)

        muacPreviousValue =
            resolvePreviousValue assembled .nutrition .muac
                |> Maybe.map muacValueFunc

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
            { form_ | fundalHeight = value, fundalHeightDirty = True }

        fetalHeartRateUpdateFunc value form_ =
            { form_ | fetalHeartRate = value, fetalHeartRateDirty = True }

        fetalMovementUpdateFunc value form_ =
            { form_ | fetalMovement = Just value }

        fetalHeartRatePreviousValue =
            resolvePreviousValue assembled .obstetricalExam .fetalHeartRate
                |> Maybe.map toFloat

        fundalHeightPreviousValue =
            resolvePreviousValue assembled .obstetricalExam .fundalHeight
                |> Maybe.map heightValueFunc
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
            Translate.BpmUnitLabel
        , viewPreviousMeasurement language fetalHeartRatePreviousValue Translate.BpmUnitLabel
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


viewHealthEducationForm : Language -> NominalDate -> AssembledData -> HealthEducationForm -> Html Msg
viewHealthEducationForm language currentDate assembled form =
    let
        ( inputs, _ ) =
            healthEducationFormInputsAndTasks language assembled form
    in
    div [ class "ui form health-education" ]
        inputs


viewFollowUpForm : Language -> NominalDate -> AssembledData -> FollowUpForm -> Html Msg
viewFollowUpForm language assembled currentDate form =
    div [ class "ui form follow-up" ]
        [ viewLabel language Translate.FollowUpWithMotherLabel
        , viewCheckBoxSelectInput language
            [ ThreeDays, OneMonths, TwoMonths, ThreeMonths ]
            []
            form.option
            SetFollowUpOption
            Translate.FollowUpOption
        ]


viewAppointmentConfirmationForm : Language -> NominalDate -> AssembledData -> AppointmentConfirmationForm -> Html Msg
viewAppointmentConfirmationForm language currentDate assembled form =
    let
        appointmentDateInput =
            DateSelector.SelectorDropdown.view
                AppointmentToggleDateSelector
                SetAppointmentConfirmation
                form.isDateSelectorOpen
                currentDate
                (Date.add Months 9 currentDate)
                form.appointmentDate
    in
    div [ class "form appointment-confirmation" ]
        [ viewLabel language Translate.AppointmentConfirmationInstrunction
        , div [ class "form-input date" ]
            [ appointmentDateInput ]
        ]


viewNewbornEnrolmentForm : Language -> NominalDate -> AssembledData -> Html Msg
viewNewbornEnrolmentForm language currentDate assembled =
    div [ class "form newborn-enrolment" ]
        [ text <| translate language <| Translate.EnrolNewbornHelper <| isJust assembled.participant.newborn
        , button
            [ classList [ ( "ui fluid primary button", True ), ( "disabled", isJust assembled.participant.newborn ) ]
            , onClick <|
                SetActivePage <|
                    UserPage <|
                        CreatePersonPage (Just assembled.participant.person) <|
                            Backend.Person.Model.PrenatalNextStepsActivityOrigin assembled.id
            ]
            [ text <| translate language Translate.EnrolNewborn ]
        ]


viewPrenatalRDTForm : Language -> NominalDate -> LaboratoryTask -> PrenatalLabsRDTForm -> ( Html Msg, Int, Int )
viewPrenatalRDTForm language currentDate task form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryTestInitial language currentDate task form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            if form.testPerformed == Just True then
                let
                    ( performedTestSection, performedTestTasksCompleted, performedTestTasksTotal ) =
                        contentAndTasksForPerformedLaboratoryTest language currentDate task form

                    setTestResultMsg =
                        case task of
                            TaskHIVTest ->
                                Just SetHIVTestResult

                            TaskMalariaTest ->
                                Just SetMalariaTestResult

                            _ ->
                                Nothing

                    ( testResultSection, testResultTasksCompleted, testResultTasksTotal ) =
                        if isNothing form.executionDate then
                            ( [], 0, 0 )

                        else
                            Maybe.map
                                (\setResultMsg ->
                                    let
                                        emptyOption =
                                            if isNothing form.testResult then
                                                option
                                                    [ value ""
                                                    , selected True
                                                    ]
                                                    [ text "" ]

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
                ( performedTestSection ++ testResultSection
                , performedTestTasksCompleted + testResultTasksCompleted
                , performedTestTasksTotal + testResultTasksTotal
                )

            else
                ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory rdt" ] <|
        [ viewCustomLabel language (Translate.PrenatalLaboratoryTaskLabel task) "" "label header"
        ]
            ++ initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


viewPrenatalUrineDipstickForm : Language -> NominalDate -> PrenatalUrineDipstickForm -> ( Html Msg, Int, Int )
viewPrenatalUrineDipstickForm language currentDate form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryTestInitial language currentDate TaskUrineDipstickTest form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            if form.testPerformed == Just True then
                let
                    ( performedTestSection, performedTestTasksCompleted, performedTestTasksTotal ) =
                        contentAndTasksForPerformedLaboratoryTest language currentDate TaskUrineDipstickTest form

                    ( testVariantSection, testVariantTasksCompleted, testVariantTasksTotal ) =
                        ( [ viewQuestionLabel language Translate.TestVariantQuestion
                          , viewCheckBoxSelectInput language
                                [ VariantShortTest ]
                                [ VariantLongTest ]
                                form.testVariant
                                SetUrineDipstickTestVariant
                                Translate.PrenatalUrineDipstickTestVariant
                          ]
                        , taskCompleted form.testVariant
                        , 1
                        )

                    testResultSection =
                        if isNothing form.executionDate then
                            []

                        else
                            [ viewCustomLabel language Translate.PrenatalLaboratoryTaskResultsHelper "." "label header" ]
                in
                ( testVariantSection ++ performedTestSection ++ testResultSection
                , performedTestTasksCompleted + testVariantTasksCompleted
                , performedTestTasksTotal + testVariantTasksTotal
                )

            else
                ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory urine-dipstick" ] <|
        [ viewCustomLabel language (Translate.PrenatalLaboratoryTaskLabel TaskUrineDipstickTest) "" "label"
        ]
            ++ initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


viewPrenatalNonRDTForm : Language -> NominalDate -> LaboratoryTask -> PrenatalLabsNonRDTForm -> ( Html Msg, Int, Int )
viewPrenatalNonRDTForm language currentDate task form =
    let
        ( initialSection, initialTasksCompleted, initialTasksTotal ) =
            contentAndTasksLaboratoryTestInitial language currentDate task form

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            if form.testPerformed == Just True then
                let
                    ( performedTestSection, performedTestTasksCompleted, performedTestTasksTotal ) =
                        contentAndTasksForPerformedLaboratoryTest language currentDate task form

                    testResultSection =
                        if isNothing form.executionDate then
                            []

                        else
                            [ viewCustomLabel language Translate.PrenatalLaboratoryTaskResultsHelper "." "label" ]
                in
                ( performedTestSection ++ testResultSection
                , performedTestTasksCompleted
                , performedTestTasksTotal
                )

            else
                ( [], 0, 0 )
    in
    ( div [ class "ui form laboratory non-rdt" ] <|
        [ viewCustomLabel language (Translate.PrenatalLaboratoryTaskLabel task) "" "label header"
        ]
            ++ initialSection
            ++ derivedSection
    , initialTasksCompleted + derivedTasksCompleted
    , initialTasksTotal + derivedTasksTotal
    )


contentAndTasksLaboratoryTestInitial :
    Language
    -> NominalDate
    -> LaboratoryTask
    ->
        { f
            | testPerformed : Maybe Bool
            , executionNote : Maybe PrenatalTestExecutionNote
        }
    -> ( List (Html Msg), Int, Int )
contentAndTasksLaboratoryTestInitial language currentDate task form =
    let
        boolInputUpdateFunc =
            \value form_ ->
                { form_
                    | testPerformed = Just value
                    , testPerformedToday = Nothing
                    , executionNote = Nothing
                    , executionDate = Nothing
                }

        msgs =
            case task of
                TaskHIVTest ->
                    { setBoolInputMsg = SetHIVTestFormBoolInput boolInputUpdateFunc
                    , setExecutionNoteMsg = SetHIVTestExecutionNote
                    }

                TaskSyphilisTest ->
                    { setBoolInputMsg = SetSyphilisTestFormBoolInput boolInputUpdateFunc
                    , setExecutionNoteMsg = SetSyphilisTestExecutionNote
                    }

                TaskHepatitisBTest ->
                    { setBoolInputMsg = SetHepatitisBTestFormBoolInput boolInputUpdateFunc
                    , setExecutionNoteMsg = SetHepatitisBTestExecutionNote
                    }

                TaskMalariaTest ->
                    { setBoolInputMsg = SetMalariaTestFormBoolInput boolInputUpdateFunc
                    , setExecutionNoteMsg = SetMalariaTestExecutionNote
                    }

                TaskBloodGpRsTest ->
                    { setBoolInputMsg = SetBloodGpRsTestFormBoolInput boolInputUpdateFunc
                    , setExecutionNoteMsg = SetBloodGpRsTestExecutionNote
                    }

                TaskUrineDipstickTest ->
                    { setBoolInputMsg = SetUrineDipstickTestFormBoolInput boolInputUpdateFunc
                    , setExecutionNoteMsg = SetUrineDipstickTestExecutionNote
                    }

                TaskHemoglobinTest ->
                    { setBoolInputMsg = SetHemoglobinTestFormBoolInput boolInputUpdateFunc
                    , setExecutionNoteMsg = SetHemoglobinTestExecutionNote
                    }

                TaskRandomBloodSugarTest ->
                    { setBoolInputMsg = SetRandomBloodSugarTestFormBoolInput boolInputUpdateFunc
                    , setExecutionNoteMsg = SetRandomBloodSugarTestExecutionNote
                    }

        ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
            Maybe.map
                (\testPerformed ->
                    if testPerformed then
                        ( [], 0, 0 )

                    else
                        ( [ div [ class "why-not" ]
                                [ viewQuestionLabel language Translate.WhyNot
                                , viewCheckBoxSelectInput language
                                    [ TestNoteLackOfReagents
                                    , TestNoteLackOfOtherSupplies
                                    , TestNoteBrokenEquipment
                                    ]
                                    [ TestNoteNoEquipment
                                    , TestNoteNotIndicated
                                    ]
                                    form.executionNote
                                    msgs.setExecutionNoteMsg
                                    Translate.PrenatalTestExecutionNote
                                ]
                          ]
                        , taskCompleted form.executionNote
                        , 1
                        )
                )
                form.testPerformed
                |> Maybe.withDefault ( [], 0, 0 )
    in
    ( [ viewQuestionLabel language Translate.TestPerformedQuestion
      , viewBoolInput
            language
            form.testPerformed
            msgs.setBoolInputMsg
            "test-performed"
            Nothing
      ]
        ++ derivedSection
    , taskCompleted form.testPerformed + derivedTasksCompleted
    , 1 + derivedTasksTotal
    )


contentAndTasksForPerformedLaboratoryTest :
    Language
    -> NominalDate
    -> LaboratoryTask
    ->
        { f
            | testPerformed : Maybe Bool
            , testPerformedToday : Maybe Bool
            , executionNote : Maybe PrenatalTestExecutionNote
            , executionDate : Maybe NominalDate
            , isDateSelectorOpen : Bool
        }
    -> ( List (Html Msg), Int, Int )
contentAndTasksForPerformedLaboratoryTest language currentDate task form =
    if form.testPerformed /= Just True then
        ( [], 0, 0 )

    else
        let
            boolInputUpdateFunc =
                \value form_ ->
                    let
                        ( executionNote, executionDate ) =
                            if value == True then
                                ( Just TestNoteRunToday, Just currentDate )

                            else
                                ( Just TestNoteRunPreviously, Nothing )
                    in
                    { form_
                        | testPerformedToday = Just value
                        , executionNote = executionNote
                        , executionDate = executionDate
                    }

            msgs =
                case task of
                    TaskHIVTest ->
                        { setBoolInputMsg = SetHIVTestFormBoolInput boolInputUpdateFunc
                        , setExecutionDateMsg = SetHIVTestExecutionDate
                        , toggleDateSelectorMsg = ToggleHIVTestDateSelector
                        }

                    TaskSyphilisTest ->
                        { setBoolInputMsg = SetSyphilisTestFormBoolInput boolInputUpdateFunc
                        , setExecutionDateMsg = SetSyphilisTestExecutionDate
                        , toggleDateSelectorMsg = ToggleSyphilisTestDateSelector
                        }

                    TaskHepatitisBTest ->
                        { setBoolInputMsg = SetHepatitisBTestFormBoolInput boolInputUpdateFunc
                        , setExecutionDateMsg = SetHepatitisBTestExecutionDate
                        , toggleDateSelectorMsg = ToggleHepatitisBTestDateSelector
                        }

                    TaskMalariaTest ->
                        { setBoolInputMsg = SetMalariaTestFormBoolInput boolInputUpdateFunc
                        , setExecutionDateMsg = SetMalariaTestExecutionDate
                        , toggleDateSelectorMsg = ToggleMalariaTestDateSelector
                        }

                    TaskBloodGpRsTest ->
                        { setBoolInputMsg = SetBloodGpRsTestFormBoolInput boolInputUpdateFunc
                        , setExecutionDateMsg = SetBloodGpRsTestExecutionDate
                        , toggleDateSelectorMsg = ToggleBloodGpRsTestDateSelector
                        }

                    TaskUrineDipstickTest ->
                        { setBoolInputMsg = SetUrineDipstickTestFormBoolInput boolInputUpdateFunc
                        , setExecutionDateMsg = SetUrineDipstickTestExecutionDate
                        , toggleDateSelectorMsg = ToggleUrineDipstickTestDateSelector
                        }

                    TaskHemoglobinTest ->
                        { setBoolInputMsg = SetHemoglobinTestFormBoolInput boolInputUpdateFunc
                        , setExecutionDateMsg = SetHemoglobinTestExecutionDate
                        , toggleDateSelectorMsg = ToggleHemoglobinTestDateSelector
                        }

                    TaskRandomBloodSugarTest ->
                        { setBoolInputMsg = SetRandomBloodSugarTestFormBoolInput boolInputUpdateFunc
                        , setExecutionDateMsg = SetRandomBloodSugarTestExecutionDate
                        , toggleDateSelectorMsg = ToggleRandomBloodSugarTestDateSelector
                        }

            ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
                Maybe.map
                    (\testPerformedToday ->
                        let
                            ( executionDateContent, executionDateTasksCompleted, executionDateTasksTotal ) =
                                if testPerformedToday then
                                    ( p [ class "test-date" ] [ text <| formatDDMMYYYY currentDate ], 0, 0 )

                                else
                                    ( div [ class "form-input date" ]
                                        [ DateSelector.SelectorDropdown.view
                                            msgs.toggleDateSelectorMsg
                                            msgs.setExecutionDateMsg
                                            form.isDateSelectorOpen
                                            (Date.add Days -30 currentDate)
                                            currentDate
                                            form.executionDate
                                        ]
                                    , taskCompleted form.executionDate
                                    , 1
                                    )
                        in
                        ( [ viewLabel language <| Translate.PrenatalLaboratoryTaskDate task
                          , executionDateContent
                          ]
                        , executionDateTasksCompleted
                        , executionDateTasksTotal
                        )
                    )
                    form.testPerformedToday
                    |> Maybe.withDefault ( [], 0, 0 )
        in
        ( [ viewQuestionLabel language Translate.TestPerformedTodayQuestion
          , viewBoolInput
                language
                form.testPerformedToday
                msgs.setBoolInputMsg
                "test-performed-today"
                Nothing
          ]
            ++ derivedSection
        , taskCompleted form.testPerformedToday + derivedTasksCompleted
        , 1 + derivedTasksTotal
        )



-- HELPER FUNCITONS


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


viewWarning : Language -> Maybe String -> Html any
viewWarning language maybeMessage =
    maybeMessage
        |> unwrap
            emptyNode
            (\message ->
                div [ class "five wide column" ]
                    [ text message ]
            )
