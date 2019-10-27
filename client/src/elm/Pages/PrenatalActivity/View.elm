module Pages.PrenatalActivity.View exposing (view)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
import Backend.PrenatalParticipant.Model exposing (PrenatalParticipant)
import Date.Extra as Date exposing (Interval(Day, Month))
import DateSelector.SelectorDropdown
import EveryDict
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY, fromLocalDateTime, toLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalActivity.Model exposing (..)
import Pages.PrenatalActivity.Utils
    exposing
        ( breastExamFormWithDefault
        , calculateBmi
        , corePhysicalExamFormWithDefault
        , dangerSignsFormWithDefault
        , familyPlanningFormWithDefault
        , lastMenstrualPeriodFormWithDefault
        , medicalHistoryFormWithDefault
        , medicationFormWithDefault
        , obstetricHistoryFormWithDefault
        , obstetricHistoryStep2FormWithDefault
        , obstetricalExamFormWithDefault
        , prenatalNutritionFormWithDefault
        , resourceFormWithDefault
        , socialHistoryFormWithDefault
        , vitalsFormWithDefault
        )
import Pages.PrenatalEncounter.Utils exposing (..)
import Pages.PrenatalEncounter.View exposing (viewMotherAndMeasurements)
import PrenatalActivity.Model exposing (PrenatalActivity(..))
import RemoteData exposing (RemoteData(..), WebData)
import Round
import Translate exposing (Language, TranslationId, translate)
import Utils.WebData exposing (viewWebData)


type alias AssembledData =
    { id : PrenatalEncounterId
    , activity : PrenatalActivity
    , encounter : PrenatalEncounter
    , participant : PrenatalParticipant
    , measurements : PrenatalMeasurements
    , person : Person
    }


view : Language -> NominalDate -> PrenatalEncounterId -> PrenatalActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
    let
        encounter =
            EveryDict.get id db.prenatalEncounters
                |> Maybe.withDefault NotAsked

        measurements =
            EveryDict.get id db.prenatalMeasurements
                |> Maybe.withDefault NotAsked

        participant =
            encounter
                |> RemoteData.andThen
                    (\encounter ->
                        EveryDict.get encounter.participant db.prenatalParticipants
                            |> Maybe.withDefault NotAsked
                    )

        person =
            participant
                |> RemoteData.andThen
                    (\participant ->
                        EveryDict.get participant.person db.people
                            |> Maybe.withDefault NotAsked
                    )

        data =
            RemoteData.map AssembledData (Success id)
                |> RemoteData.andMap (Success activity)
                |> RemoteData.andMap encounter
                |> RemoteData.andMap participant
                |> RemoteData.andMap measurements
                |> RemoteData.andMap person

        content =
            viewWebData language (viewContent language currentDate model) identity data
    in
    div [ class "page-prenatal-activity" ] <|
        [ viewHeader language id activity
        , content
        ]


viewContent : Language -> NominalDate -> Model -> AssembledData -> Html Msg
viewContent language currentDate model data =
    div [ class "ui unstackable items" ] <|
        viewMotherAndMeasurements language currentDate data.person data.measurements model.showAlertsDialog SetAlertsDialogState
            ++ viewActivity language currentDate data model


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


viewActivity : Language -> NominalDate -> AssembledData -> Model -> List (Html Msg)
viewActivity language currentDate data model =
    case data.activity of
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
            toLocalDateTime currentDate 0 0 0 0

        lmpDateInput =
            form.lmpRange
                |> unwrap
                    []
                    (\range ->
                        let
                            daysBack =
                                case range of
                                    OneMonth ->
                                        -31

                                    ThreeMonth ->
                                        -92

                                    SixMonth ->
                                        -184
                        in
                        [ DateSelector.SelectorDropdown.view
                            ToggleDateSelector
                            SetLmpDate
                            form.isDateSelectorOpen
                            (Date.add Day daysBack today)
                            today
                            form.lmpDate
                        ]
                    )
                |> div [ class "form-input date" ]

        ( edd, ega ) =
            Maybe.map fromLocalDateTime form.lmpDate
                |> generateEDDandEGA language currentDate ( "", "" )

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
                , lmpDateInput
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
                , onClick <| SavePregnancyDating assembled.id assembled.participant.person assembled.measurements.lastMenstrualPeriod
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewHistoryContent : Language -> NominalDate -> AssembledData -> HistoryData -> List (Html Msg)
viewHistoryContent language currentDate assembled data =
    let
        tasks =
            [ Obstetric, Medical, Social ]

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

        ( viewForm, tasksCompleted, totalTasks ) =
            case data.activeTask of
                Obstetric ->
                    case data.obstetricHistoryStep of
                        ObstetricHistoryFirstStep ->
                            let
                                formStep1_ =
                                    assembled.measurements.obstetricHistory
                                        |> Maybe.map (Tuple.second >> .value)
                                        |> obstetricHistoryFormWithDefault data.obstetricFormFirstStep

                                intInputs =
                                    [ formStep1_.termPregnancy
                                    , formStep1_.preTermPregnancy
                                    , formStep1_.stillbirthsAtTerm
                                    , formStep1_.stillbirthsPreTerm
                                    , formStep1_.abortions
                                    , formStep1_.liveChildren
                                    ]
                            in
                            ( viewObstetricFormFirstStep language currentDate assembled formStep1_
                            , (intInputs
                                |> List.map taskCompleted
                                |> List.sum
                              )
                                + taskCompleted formStep1_.currentlyPregnant
                            , 7
                            )

                        ObstetricHistorySecondStep ->
                            let
                                formStep2_ =
                                    assembled.measurements.obstetricHistoryStep2
                                        |> Maybe.map (Tuple.second >> .value)
                                        |> obstetricHistoryStep2FormWithDefault data.obstetricFormSecondStep

                                boolInputs =
                                    [ formStep2_.cSectionInPreviousDelivery
                                    , formStep2_.successiveAbortions
                                    , formStep2_.successivePrematureDeliveries
                                    , formStep2_.stillbornPreviousDelivery
                                    , formStep2_.babyDiedOnDayOfBirthPreviousDelivery
                                    , formStep2_.partialPlacentaPreviousDelivery
                                    , formStep2_.severeHemorrhagingPreviousDelivery
                                    , formStep2_.preeclampsiaPreviousPregnancy
                                    , formStep2_.convulsionsPreviousDelivery
                                    , formStep2_.convulsionsAndUnconsciousPreviousDelivery
                                    , formStep2_.gestationalDiabetesPreviousPregnancy
                                    , formStep2_.incompleteCervixPreviousPregnancy
                                    , formStep2_.rhNegative
                                    ]
                            in
                            ( viewObstetricFormSecondStep language currentDate assembled formStep2_
                            , (boolInputs
                                |> List.map taskCompleted
                                |> List.sum
                              )
                                + taskCompleted formStep2_.cSections
                                + taskCompleted formStep2_.cSectionReason
                                + taskCompleted formStep2_.previousDeliveryPeriod
                            , 16
                            )

                Medical ->
                    let
                        medicalForm =
                            assembled.measurements.medicalHistory
                                |> Maybe.map (Tuple.second >> .value)
                                |> medicalHistoryFormWithDefault data.medicalForm

                        boolInputs =
                            [ medicalForm.uterineMyoma
                            , medicalForm.diabetes
                            , medicalForm.cardiacDisease
                            , medicalForm.renalDisease
                            , medicalForm.hypertensionBeforePregnancy
                            , medicalForm.tuberculosisPast
                            , medicalForm.tuberculosisPresent
                            , medicalForm.asthma
                            , medicalForm.bowedLegs
                            , medicalForm.hiv
                            ]
                    in
                    ( viewMedicalForm language currentDate assembled medicalForm
                    , boolInputs
                        |> List.map taskCompleted
                        |> List.sum
                    , 10
                    )

                Social ->
                    let
                        socialForm =
                            assembled.measurements.socialHistory
                                |> Maybe.map (Tuple.second >> .value)
                                |> socialHistoryFormWithDefault data.socialForm

                        boolInputs =
                            [ socialForm.accompaniedByPartner
                            , socialForm.partnerReceivedCounseling
                            , socialForm.mentalHealthHistory
                            ]
                    in
                    ( viewSocialForm language currentDate assembled socialForm
                    , boolInputs
                        |> List.map taskCompleted
                        |> List.sum
                    , 3
                    )

        actions =
            let
                ( buttons, stepIndicationClass ) =
                    case data.activeTask of
                        Obstetric ->
                            case data.obstetricHistoryStep of
                                ObstetricHistoryFirstStep ->
                                    ( [ button
                                            [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                                            , onClick <| SaveOBHistoryStep1 assembled.id assembled.participant.person assembled.measurements.obstetricHistory
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
                                            , onClick <| SaveOBHistoryStep2 assembled.id assembled.participant.person assembled.measurements.obstetricHistoryStep2
                                            ]
                                            [ text <| translate language Translate.Save ]
                                      ]
                                    , "second"
                                    )

                        Medical ->
                            ( [ button
                                    [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                                    , onClick <| SaveMedicalHistory assembled.id assembled.participant.person assembled.measurements.medicalHistory
                                    ]
                                    [ text <| translate language Translate.Save ]
                              ]
                            , ""
                            )

                        Social ->
                            ( [ button
                                    [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                                    , onClick <| SaveSocialHistory assembled.id assembled.participant.person assembled.measurements.socialHistory
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

        ( viewForm, tasksCompleted, totalTasks ) =
            case data.activeTask of
                Vitals ->
                    let
                        form =
                            assembled.measurements.vitals
                                |> Maybe.map (Tuple.second >> .value)
                                |> vitalsFormWithDefault data.vitalsForm
                    in
                    ( viewVitalsForm language currentDate assembled form
                    , taskListCompleted [ form.sysBloodPressure, form.diaBloodPressure ]
                        + ([ Maybe.map (always ()) form.heartRate
                           , Maybe.map (always ()) form.respiratoryRate
                           , Maybe.map (always ()) form.bodyTemperature
                           ]
                            |> List.map taskCompleted
                            |> List.sum
                          )
                    , 4
                    )

                NutritionAssessment ->
                    let
                        form =
                            assembled.measurements.nutrition
                                |> Maybe.map (Tuple.second >> .value)
                                |> prenatalNutritionFormWithDefault data.nutritionAssessmentForm
                    in
                    ( viewNutritionAssessmentForm language currentDate assembled form
                    , ([ form.height, form.weight, form.muac ]
                        |> List.map taskCompleted
                        |> List.sum
                      )
                        -- This is for BMI task, which is considered as completed
                        -- when both height and weight are set.
                        + taskListCompleted [ form.height, form.weight ]
                    , 4
                    )

                CorePhysicalExam ->
                    let
                        form =
                            assembled.measurements.corePhysicalExam
                                |> Maybe.map (Tuple.second >> .value)
                                |> corePhysicalExamFormWithDefault data.corePhysicalExamForm

                        extremitiesTaskCompleted =
                            if isJust form.hands && isJust form.legs then
                                1

                            else
                                0
                    in
                    ( viewCorePhysicalExamForm language currentDate assembled form
                    , extremitiesTaskCompleted
                        + taskCompleted form.neck
                        + taskCompleted form.lungs
                        + taskCompleted form.abdomen
                        + taskCompleted form.heart
                        + ([ form.brittleHair
                           , form.paleConjuctiva
                           ]
                            |> List.map taskCompleted
                            |> List.sum
                          )
                    , 7
                    )

                ObstetricalExam ->
                    let
                        form =
                            assembled.measurements.obstetricalExam
                                |> Maybe.map (Tuple.second >> .value)
                                |> obstetricalExamFormWithDefault data.obstetricalExamForm
                    in
                    ( viewObstetricalExamForm language currentDate assembled form
                    , taskCompleted form.fetalPresentation
                        + taskCompleted form.fetalMovement
                        + taskCompleted form.cSectionScar
                        + ([ Maybe.map (always ()) form.fundalHeight, Maybe.map (always ()) form.fetalHeartRate ]
                            |> List.map taskCompleted
                            |> List.sum
                          )
                    , 5
                    )

                BreastExam ->
                    let
                        form =
                            assembled.measurements.breastExam
                                |> Maybe.map (Tuple.second >> .value)
                                |> breastExamFormWithDefault data.breastExamForm
                    in
                    ( viewBreastExamForm language currentDate assembled form
                    , taskCompleted form.breast + taskCompleted form.selfGuidance
                    , 2
                    )

        actions =
            let
                saveAction =
                    case data.activeTask of
                        Vitals ->
                            SaveVitals assembled.id assembled.participant.person assembled.measurements.vitals

                        NutritionAssessment ->
                            SaveNutritionAssessment assembled.id assembled.participant.person assembled.measurements.nutrition

                        CorePhysicalExam ->
                            SaveCorePhysicalExam assembled.id assembled.participant.person assembled.measurements.corePhysicalExam

                        ObstetricalExam ->
                            SaveObstetricalExam assembled.id assembled.participant.person assembled.measurements.obstetricalExam

                        BreastExam ->
                            SaveBreastExam assembled.id assembled.participant.person assembled.measurements.breastExam
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
                , onClick <| SaveFamilyPlanning assembled.id assembled.participant.person assembled.measurements.familyPlanning
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewPatientProvisionsContent : Language -> NominalDate -> AssembledData -> PatientProvisionsData -> List (Html Msg)
viewPatientProvisionsContent language currentDate assembled data =
    let
        tasks =
            [ Medication, Resources ]

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

        ( viewForm, tasksCompleted, totalTasks ) =
            case data.activeTask of
                Medication ->
                    let
                        form =
                            assembled.measurements.medication
                                |> Maybe.map (Tuple.second >> .value)
                                |> medicationFormWithDefault data.medicationForm
                    in
                    ( viewMedicationForm language currentDate assembled form
                    , [ form.receivedIronFolicAcid, form.receivedDewormingPill ]
                        |> List.map taskCompleted
                        |> List.sum
                    , 2
                    )

                Resources ->
                    let
                        form =
                            assembled.measurements.resource
                                |> Maybe.map (Tuple.second >> .value)
                                |> resourceFormWithDefault data.resourcesForm
                    in
                    ( viewResourcesForm language currentDate assembled form
                    , taskCompleted form.receivedMosquitoNet
                    , 1
                    )

        actions =
            let
                saveAction =
                    case data.activeTask of
                        Medication ->
                            SaveMedication assembled.id assembled.participant.person assembled.measurements.medication

                        Resources ->
                            SaveResources assembled.id assembled.participant.person assembled.measurements.resource
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
                , onClick <| SaveDangerSigns assembled.id assembled.participant.person assembled.measurements.dangerSigns
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]



-- Forms


viewObstetricFormFirstStep : Language -> NominalDate -> AssembledData -> ObstetricFormFirstStep -> Html Msg
viewObstetricFormFirstStep language currentDate assembled form =
    let
        gravida =
            Maybe.map3 generateGravida form.termPregnancy form.preTermPregnancy form.currentlyPregnant
                |> Maybe.withDefault ""

        para =
            Maybe.map4 generatePara form.termPregnancy form.preTermPregnancy form.abortions form.liveChildren
                |> Maybe.withDefault ""

        termPregnancyUpdateFunc value form_ =
            { form_ | termPregnancy = value }

        preTermPregnancyUpdateFunc value form_ =
            { form_ | preTermPregnancy = value }

        stillbirthsAtTermUpdateFunc value form_ =
            { form_ | stillbirthsAtTerm = value }

        stillbirthsPreTermUpdateFunc value form_ =
            { form_ | stillbirthsPreTerm = value }

        abortionsUpdateFunc value form_ =
            { form_ | abortions = value }

        liveChildrenUpdateFunc value form_ =
            { form_ | liveChildren = value }
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
                [ viewRedAlertForSelect
                    (form.cSectionInPreviousDelivery |> Maybe.map List.singleton |> Maybe.withDefault [])
                    False
                ]
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
                    None
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
                    Neither
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
                [ viewRedAlertForSelect
                    (form.successiveAbortions |> Maybe.map List.singleton |> Maybe.withDefault [])
                    False
                ]
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
                [ viewRedAlertForSelect
                    (form.successivePrematureDeliveries |> Maybe.map List.singleton |> Maybe.withDefault [])
                    False
                ]
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
                [ viewRedAlertForSelect
                    (form.stillbornPreviousDelivery |> Maybe.map List.singleton |> Maybe.withDefault [])
                    False
                ]
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
                [ viewRedAlertForSelect
                    (form.babyDiedOnDayOfBirthPreviousDelivery |> Maybe.map List.singleton |> Maybe.withDefault [])
                    False
                ]
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
                [ viewRedAlertForSelect
                    (form.partialPlacentaPreviousDelivery |> Maybe.map List.singleton |> Maybe.withDefault [])
                    False
                ]
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
                [ viewRedAlertForSelect
                    (form.severeHemorrhagingPreviousDelivery |> Maybe.map List.singleton |> Maybe.withDefault [])
                    False
                ]
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
                [ viewRedAlertForSelect
                    (form.preeclampsiaPreviousPregnancy |> Maybe.map List.singleton |> Maybe.withDefault [])
                    False
                ]
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
                [ viewRedAlertForSelect
                    (form.convulsionsPreviousDelivery |> Maybe.map List.singleton |> Maybe.withDefault [])
                    False
                ]
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
                [ viewRedAlertForSelect
                    (form.convulsionsAndUnconsciousPreviousDelivery |> Maybe.map List.singleton |> Maybe.withDefault [])
                    False
                ]
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
                [ viewRedAlertForSelect
                    (form.gestationalDiabetesPreviousPregnancy |> Maybe.map List.singleton |> Maybe.withDefault [])
                    False
                ]
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
                [ viewRedAlertForSelect
                    (form.incompleteCervixPreviousPregnancy |> Maybe.map List.singleton |> Maybe.withDefault [])
                    False
                ]
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
                [ viewRedAlertForSelect
                    (form.rhNegative |> Maybe.map List.singleton |> Maybe.withDefault [])
                    False
                ]
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
    in
    div [ class "form history medical" ]
        [ viewCustomLabel language Translate.MedicalFormHelper ":" "label helper"
        , viewLabel language Translate.UterineMyoma
        , viewBoolInput
            language
            form.uterineMyoma
            (SetMedicalBoolInput uterineMyomaUpdateFunc)
            "uterine-myoma"
            Nothing
        , viewLabel language Translate.Diabetes
        , viewBoolInput
            language
            form.diabetes
            (SetMedicalBoolInput diabetesUpdateFunc)
            "diabetes"
            Nothing
        , viewLabel language Translate.CardiacDisease
        , viewBoolInput
            language
            form.cardiacDisease
            (SetMedicalBoolInput cardiacDiseaseUpdateFunc)
            "cardiac-disease"
            Nothing
        , viewLabel language Translate.RenalDisease
        , viewBoolInput
            language
            form.renalDisease
            (SetMedicalBoolInput renalDiseaseUpdateFunc)
            "renal-disease"
            Nothing
        , viewLabel language Translate.HypertensionBeforePregnancy
        , viewBoolInput
            language
            form.hypertensionBeforePregnancy
            (SetMedicalBoolInput hypertensionBeforePregnancyUpdateFunc)
            "hypertension-before-pregnancy"
            Nothing
        , viewLabel language Translate.TuberculosisPast
        , viewBoolInput
            language
            form.tuberculosisPast
            (SetMedicalBoolInput tuberculosisPastUpdateFunc)
            "tuberculosis-past"
            Nothing
        , viewLabel language Translate.TuberculosisPresent
        , viewBoolInput
            language
            form.tuberculosisPresent
            (SetMedicalBoolInput tuberculosisPresentUpdateFunc)
            "tuberculosis-present"
            Nothing
        , viewLabel language Translate.Asthma
        , viewBoolInput
            language
            form.asthma
            (SetMedicalBoolInput asthmaUpdateFunc)
            "asthma"
            Nothing
        , viewLabel language Translate.BowedLegs
        , viewBoolInput
            language
            form.bowedLegs
            (SetMedicalBoolInput bowedLegsUpdateFunc)
            "bowed-legs"
            Nothing
        , viewLabel language Translate.HIV
        , viewBoolInput
            language
            form.hiv
            (SetMedicalBoolInput hivUpdateFunc)
            "hiv"
            Nothing
        ]


viewSocialForm : Language -> NominalDate -> AssembledData -> SocialHistoryForm -> Html Msg
viewSocialForm language currentDate assembled form =
    let
        accompaniedByPartnerUpdateFunc value form_ =
            { form_ | accompaniedByPartner = Just value }

        partnerReceivedCounselingUpdateFunc value form_ =
            { form_ | partnerReceivedCounseling = Just value }

        mentalHealthHistoryUpdateFunc value form_ =
            { form_ | mentalHealthHistory = Just value }
    in
    div [ class "form history social" ]
        [ viewQuestionLabel language Translate.AccompaniedByPartner
        , viewBoolInput
            language
            form.accompaniedByPartner
            (SetSocialBoolInput accompaniedByPartnerUpdateFunc)
            "accompanied-by-partner"
            Nothing
        , viewQuestionLabel language Translate.PartnerReceivedCounseling
        , viewBoolInput
            language
            form.partnerReceivedCounseling
            (SetSocialBoolInput partnerReceivedCounselingUpdateFunc)
            "partner-received-counseling"
            Nothing
        , viewLabel language Translate.MentalHealthHistory
        , viewBoolInput
            language
            form.mentalHealthHistory
            (SetSocialBoolInput mentalHealthHistoryUpdateFunc)
            "mental-health-history"
            Nothing
        ]


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
            Nothing

        diaBloodPressurePreviousValue =
            Nothing

        heartRatePreviousValue =
            Nothing

        respiratoryRatePreviousValue =
            Nothing

        bodyTemperaturePreviousValue =
            Nothing
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
                    [ [ (<) 140, (>=) 180 ] ]
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
                    [ [ (<) 90, (>=) 100 ] ]
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


viewNutritionAssessmentForm : Language -> NominalDate -> AssembledData -> NutritionAssessmentForm -> Html Msg
viewNutritionAssessmentForm language currentDate assembled form =
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
            Nothing

        weightPreviousValue =
            Nothing

        bmiPreviousValue =
            Nothing

        muacPreviousValue =
            Nothing

        calculatedBmi =
            calculateBmi form.height form.weight
    in
    div [ class "ui form examination nutrition-assessment" ]
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
        , viewPreviousMeasurement language heightPreviousValue Translate.CentimeterShorthand
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
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
        , viewMeasurementInputAndRound
            language
            calculatedBmi
            (SetNutritionAssessmentMeasurement bmiUpdateFunc)
            "bmi"
            Translate.EmptyString
            (Just 1)
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
                [ viewRedAlertForSelect
                    (form.brittleHair |> Maybe.map List.singleton |> Maybe.withDefault [])
                    False
                ]
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
                [ viewRedAlertForSelect
                    (form.paleConjuctiva |> Maybe.map List.singleton |> Maybe.withDefault [])
                    False
                ]
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
                    NormalNeck
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
                    NormalRateAndRhythm
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
                [ viewRedAlertForSelect
                    (form.heartMurmur |> Maybe.map List.singleton |> Maybe.withDefault [])
                    False
                ]
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
                    NormalLungs
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
                    NormalAbdomen
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
                    NormalHands
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
                    NormalLegs
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
        fundalHeightUpdateFunc value form_ =
            { form_ | fundalHeight = value }

        fetalHeartRateUpdateFunc value form_ =
            { form_ | fetalHeartRate = value }

        fetalMovementUpdateFunc value form_ =
            { form_ | fetalMovement = Just value }

        fetalHeartRatePreviousValue =
            Nothing

        fundalHeightPreviousValue =
            Nothing
    in
    div [ class "ui form examination obstetrical-exam" ]
        [ div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.FundalHeight ]
            , viewWarning language Nothing
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
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.FetalPresentationLabel ]
            , viewWarning language Nothing
            ]
        , viewCheckBoxMultipleSelectInput language
            [ Transverse, Cephalic ]
            [ Breach, Twins ]
            (form.fetalPresentation |> Maybe.withDefault [])
            Nothing
            SetObstetricalExamFetalPresentation
            Translate.FetalPresentation
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.FetalMovement ]
            , viewWarning language Nothing
            ]
        , viewBoolInput
            language
            form.fetalMovement
            (SetObstetricalExamBoolInput fetalMovementUpdateFunc)
            "fetal-movement"
            Nothing
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.FetalHeartRate ]
            , viewWarning language Nothing
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
                    NormalBreast
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


viewMedicationForm : Language -> NominalDate -> AssembledData -> MedicationForm -> Html Msg
viewMedicationForm language currentDate assembled form =
    let
        receivedIronFolicAcidUpdateFunc value form_ =
            { form_ | receivedIronFolicAcid = Just value }

        receivedDewormingPillUpdateFunc value form_ =
            { form_ | receivedDewormingPill = Just value }
    in
    div [ class "ui form patient-provisions medication" ]
        [ viewQuestionLabel language Translate.ReceivedIronFolicAcid
        , viewBoolInput
            language
            form.receivedIronFolicAcid
            (SetMedicationBoolInput receivedIronFolicAcidUpdateFunc)
            "iron-folic-acid"
            Nothing
        , viewQuestionLabel language Translate.ReceivedDewormingPill
        , viewBoolInput
            language
            form.receivedDewormingPill
            (SetMedicationBoolInput receivedDewormingPillUpdateFunc)
            "deworming-pill"
            Nothing
        ]


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



-- Inputs


viewBoolInput :
    Language
    -> Maybe Bool
    -> (Bool -> Msg)
    -> String
    -> Maybe ( TranslationId, TranslationId )
    -> Html Msg
viewBoolInput language currentValue setMsg inputClass optionsTranslationIds =
    let
        ( yesTransId, noTransId ) =
            optionsTranslationIds |> Maybe.withDefault ( Translate.Yes, Translate.No )

        inputWidth =
            if isJust optionsTranslationIds then
                "eight"

            else
                "four"

        viewInput value currentValue setMsg =
            let
                isChecked =
                    currentValue == Just value
            in
            input
                [ type_ "radio"
                , checked isChecked
                , classList [ ( "checked", isChecked ) ]
                , onCheck (always (setMsg value))
                ]
                []
    in
    div [ class <| "form-input yes-no " ++ inputClass ]
        [ div [ class "ui grid" ]
            [ div [ class <| inputWidth ++ " wide column" ]
                [ viewInput True currentValue setMsg
                , label [ onClick <| setMsg True ]
                    [ text <| translate language yesTransId ]
                ]
            , div [ class <| inputWidth ++ " wide column" ]
                [ viewInput False currentValue setMsg
                , label [ onClick <| setMsg False ]
                    [ text <| translate language noTransId ]
                ]
            ]
        ]


viewNumberInput :
    Language
    -> Maybe a
    -> (String -> Msg)
    -> String
    -> TranslationId
    -> Maybe ( List (List (a -> Bool)), List (List (a -> Bool)) )
    -> Html Msg
viewNumberInput language maybeCurrentValue setMsg inputClass labelTranslationId maybeAlertConditions =
    let
        currentValue =
            maybeCurrentValue
                |> unwrap
                    ""
                    toString

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


viewCheckBoxSelectInput : Language -> List a -> List a -> Maybe a -> (a -> Msg) -> (a -> TranslationId) -> Html Msg
viewCheckBoxSelectInput language leftOptions rightOptions currentValue setMsg translateFunc =
    let
        checkedOptions =
            currentValue |> Maybe.map List.singleton |> Maybe.withDefault []
    in
    viewCheckBoxMultipleSelectInput language leftOptions rightOptions checkedOptions Nothing setMsg translateFunc


viewCheckBoxMultipleSelectInput : Language -> List a -> List a -> List a -> Maybe a -> (a -> Msg) -> (a -> TranslationId) -> Html Msg
viewCheckBoxMultipleSelectInput language leftOptions rightOptions checkedOptions noneOption setMsg translateFunc =
    let
        noneSection =
            noneOption
                |> unwrap
                    []
                    (\option ->
                        [ div [ class "ui divider" ] []
                        , viewCheckBoxSelectInputItem language checkedOptions setMsg translateFunc option
                        ]
                    )
    in
    div [ class "checkbox-select-input" ] <|
        div [ class "ui grid" ]
            [ leftOptions
                |> List.map (viewCheckBoxSelectInputItem language checkedOptions setMsg translateFunc)
                |> div [ class "eight wide column" ]
            , rightOptions
                |> List.map (viewCheckBoxSelectInputItem language checkedOptions setMsg translateFunc)
                |> div [ class "eight wide column" ]
            ]
            :: noneSection


viewCheckBoxSelectInputItem : Language -> List a -> (a -> Msg) -> (a -> TranslationId) -> a -> Html Msg
viewCheckBoxSelectInputItem language checkedOptions setMsg translateFunc option =
    let
        isChecked =
            List.member option checkedOptions
    in
    div
        [ class "ui checkbox activity"
        , onClick <| setMsg option
        ]
        [ input
            [ type_ "checkbox"
            , checked isChecked
            , classList [ ( "checked", isChecked ) ]
            ]
            []
        , label []
            [ text <| translate language (translateFunc option) ]
        ]


viewMeasurementInput : Language -> Maybe Float -> (String -> Msg) -> String -> TranslationId -> Html Msg
viewMeasurementInput language maybeCurrentValue setMsg inputClass unitTranslationId =
    viewMeasurementInputAndRound language maybeCurrentValue setMsg inputClass unitTranslationId Nothing


viewMeasurementInputAndRound : Language -> Maybe Float -> (String -> Msg) -> String -> TranslationId -> Maybe Int -> Html Msg
viewMeasurementInputAndRound language maybeCurrentValue setMsg inputClass unitTranslationId maybePrecision =
    let
        currentValue =
            maybeCurrentValue
                |> unwrap
                    ""
                    (\currentValue ->
                        maybePrecision
                            |> unwrap
                                (toString currentValue)
                                (\precision -> Round.round precision currentValue)
                    )

        inputAttrs =
            [ type_ "number"
            , Html.Attributes.min "0"
            , onInput setMsg
            , value currentValue
            ]
    in
    div [ class <| "form-input measurement " ++ inputClass ]
        [ input inputAttrs []
        , div [ class "unit" ]
            [ text <| translate language unitTranslationId ]
        ]



-- Components


viewLabel : Language -> TranslationId -> Html any
viewLabel language translationId =
    viewCustomLabel language translationId ":" "label"


viewQuestionLabel : Language -> TranslationId -> Html any
viewQuestionLabel language translationId =
    viewCustomLabel language translationId "?" "label"


viewCustomLabel : Language -> TranslationId -> String -> String -> Html any
viewCustomLabel language translationId suffix class_ =
    div [ class class_ ] [ text <| (translate language translationId ++ suffix) ]


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


viewRedAlertForSelect : List a -> a -> Html any
viewRedAlertForSelect actual expected =
    viewAlertForSelect "red" actual expected


viewYellowAlertForSelect : List a -> a -> Html any
viewYellowAlertForSelect actual expected =
    viewAlertForSelect "yellow" actual expected


viewAlertForSelect : String -> List a -> a -> Html any
viewAlertForSelect color actual expected =
    if List.isEmpty actual || actual == [ expected ] then
        emptyNode

    else
        div [ class <| "alert " ++ color ]
            [ viewAlert color ]


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
                                    let
                                        _ =
                                            Debug.log " " condition
                                    in
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


taskCompleted : Maybe a -> Int
taskCompleted maybe =
    if isJust maybe then
        1

    else
        0


taskListCompleted : List (Maybe a) -> Int
taskListCompleted list =
    if List.all isJust list then
        1

    else
        0
