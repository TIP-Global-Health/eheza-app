module Pages.PrenatalActivity.View exposing (view)

import AllDict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (FamilyPlanningSign(..))
import Backend.Model exposing (ModelIndexedDb)
import Date.Extra as Date exposing (Interval(Day, Month))
import DateSelector.SelectorDropdown
import EveryDict exposing (EveryDict)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, showMaybe)
import Gizra.NominalDate exposing (NominalDate, diffDays, formatMMDDYYYY, fromLocalDateTime, toLocalDateTime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust, unwrap)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalActivity.Model exposing (..)
import Pages.PrenatalEncounter.View exposing (viewMotherAndMeasurements)
import PrenatalActivity.Model exposing (PrenatalActivity(..))
import RemoteData exposing (RemoteData(..), WebData)
import Round
import Translate exposing (Language, TranslationId, translate)


view : Language -> NominalDate -> PersonId -> PrenatalActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate id activity db model =
    let
        content =
            AllDict.get id db.people
                |> unwrap
                    []
                    (RemoteData.toMaybe
                        >> unwrap
                            []
                            (\mother ->
                                [ div [ class "ui unstackable items" ] <|
                                    viewMotherAndMeasurements language currentDate mother
                                        ++ viewContent language currentDate id activity model
                                ]
                            )
                    )
    in
    div [ class "page-prenatal-activity" ] <|
        viewHeader language id activity
            :: content


viewHeader : Language -> PersonId -> PrenatalActivity -> Html Msg
viewHeader language motherId activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.PrenatalActivitiesTitle activity ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage motherId
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> PersonId -> PrenatalActivity -> Model -> List (Html Msg)
viewContent language currentDate motherId activity model =
    case activity of
        PregnancyDating ->
            viewPregnancyDatingContent language currentDate motherId model.pregnancyDatingData

        History ->
            viewHistoryContent language currentDate motherId model.historyData

        Examination ->
            viewExaminationContent language currentDate motherId model.examinationData

        FamilyPlanning ->
            viewFamilyPlanningContent language currentDate motherId model.familyPlanningData

        PatientProvisions ->
            viewPatientProvisionsContent language currentDate motherId model.patientProvisionsData

        DangerSigns ->
            viewDangerSignsContent language currentDate motherId model.dangerSignsData


viewPregnancyDatingContent : Language -> NominalDate -> PersonId -> PregnancyDatingData -> List (Html Msg)
viewPregnancyDatingContent language currentDate motherId data =
    let
        form =
            data.form

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

        ( eddResult, egaResult ) =
            form.lmpDate
                |> unwrap
                    ( emptyNode, emptyNode )
                    (\date ->
                        let
                            eddDate =
                                Date.add Month 9 date
                                    |> fromLocalDateTime

                            lmpDate =
                                fromLocalDateTime date

                            diffInDays =
                                diffDays lmpDate currentDate

                            diffInWeeks =
                                diffInDays // 7

                            egaWeeks =
                                translate language <| Translate.WeekSinglePlural diffInWeeks

                            egaDays =
                                translate language <| Translate.DaySinglePlural (diffInDays - 7 * diffInWeeks)
                        in
                        ( div [ class "value" ] [ text <| formatMMDDYYYY eddDate ]
                        , div [ class "value" ] [ text <| egaWeeks ++ ", " ++ egaDays ]
                        )
                    )

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
                        , eddResult
                        ]
                    , div [ class "ega-result" ]
                        [ viewLabel language Translate.EgaHeader
                        , egaResult
                        ]
                    ]
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage motherId
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewHistoryContent : Language -> NominalDate -> PersonId -> HistoryData -> List (Html Msg)
viewHistoryContent language currentDate motherId data =
    let
        tasks =
            [ Obstetric, Medical, Social ]

        viewTask task =
            let
                iconClass =
                    case task of
                        Obstetric ->
                            "obstetric"

                        Medical ->
                            "medical"

                        Social ->
                            "social"

                isActive =
                    task == data.activeTask

                isCompleted =
                    List.member task data.completedTasks

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
                    case data.obstetricForm of
                        FirstStep form ->
                            let
                                intInputs =
                                    [ form.termPregnancy
                                    , form.preTermPregnancy
                                    , form.stillbirthsAtTerm
                                    , form.stillbirthsPreTerm
                                    , form.abortions
                                    , form.liveChildren
                                    ]
                            in
                            ( viewObstetricFormFirstStep language currentDate motherId form
                            , (intInputs
                                |> List.map taskCompleted
                                |> List.sum
                              )
                                + taskCompleted form.currentlyPregnant
                            , 7
                            )

                        SecondStep form ->
                            let
                                boolInputs =
                                    [ form.cSectionInPreviousDelivery
                                    , form.successiveAbortions
                                    , form.successivePrimatureDeliveries
                                    , form.stillbornPreviousDelivery
                                    , form.babyDiedOnDayOfBirthPreviousDelivery
                                    , form.partialPlacentaPreviousDelivery
                                    , form.severeHemorrhagingPreviousDelivery
                                    , form.preeclampsiaPreviousPregnancy
                                    , form.convulsionsPreviousDelivery
                                    , form.convulsionsAndUnconciousPreviousDelivery
                                    , form.gestatipnalDiabetesPreviousPregnancy
                                    , form.incompleteCervixPreviousPregnancy
                                    , form.rhNegative
                                    ]
                            in
                            ( viewObstetricFormSecondStep language currentDate motherId form
                            , (boolInputs
                                |> List.map taskCompleted
                                |> List.sum
                              )
                                + taskCompleted form.cSections
                                + taskCompleted form.reasonForCSection
                                + taskCompleted form.previousDeliveryPeriod
                            , 16
                            )

                Medical ->
                    let
                        boolInputs =
                            [ data.medicalForm.uterineMyoma
                            , data.medicalForm.diabates
                            , data.medicalForm.cardiacDisease
                            , data.medicalForm.renalDisease
                            , data.medicalForm.hypertensionBeforePregnancy
                            , data.medicalForm.tuberculosisPast
                            , data.medicalForm.tuberculosisPresent
                            , data.medicalForm.asthma
                            , data.medicalForm.bowedLegs
                            , data.medicalForm.hiv
                            ]
                    in
                    ( viewMedicalForm language currentDate motherId data.medicalForm
                    , boolInputs
                        |> List.map taskCompleted
                        |> List.sum
                    , 10
                    )

                Social ->
                    let
                        boolInputs =
                            [ data.socialForm.accompaniedByPartner
                            , data.socialForm.partnerReceivedCounseling
                            , data.socialForm.mentalHealthHistory
                            ]
                    in
                    ( viewSocialForm language currentDate motherId data.socialForm
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
                            case data.obstetricForm of
                                FirstStep _ ->
                                    ( [ button
                                            [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                                            , onClick SetOBFirstStepCompleted
                                            ]
                                            [ text <| translate language Translate.SaveAndNext ]
                                      ]
                                    , "first"
                                    )

                                SecondStep _ ->
                                    ( [ button [ class "ui fluid primary button" ]
                                            [ text <| ("< " ++ translate language Translate.Back) ]
                                      , button
                                            [ classList
                                                [ ( "ui fluid primary button", True )
                                                , ( "disabled", tasksCompleted /= totalTasks )
                                                , ( "active", tasksCompleted == totalTasks )
                                                ]
                                            , onClick SetHistoryTaskCompleted
                                            ]
                                            [ text <| translate language Translate.Save ]
                                      ]
                                    , "second"
                                    )

                        Medical ->
                            ( [ button
                                    [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                                    , onClick SetHistoryTaskCompleted
                                    ]
                                    [ text <| translate language Translate.Save ]
                              ]
                            , ""
                            )

                        Social ->
                            ( [ button
                                    [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                                    , onClick SetHistoryTaskCompleted
                                    ]
                                    [ text <| translate language Translate.Save ]
                              ]
                            , ""
                            )
            in
            div [ class <| "actions history obstetric " ++ stepIndicationClass ]
                buttons
    in
    [ div [ class "ui task segment" ]
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


viewExaminationContent : Language -> NominalDate -> PersonId -> ExaminationData -> List (Html Msg)
viewExaminationContent language currentDate motherId data =
    let
        tasks =
            [ Vitals, NutritionAssessment, CorePhysicalExam, ObstetricalExam, BreastExam ]

        viewTask task =
            let
                iconClass =
                    case task of
                        Vitals ->
                            "vitals"

                        NutritionAssessment ->
                            "nutrition-assessment"

                        CorePhysicalExam ->
                            "core-physical-exam"

                        ObstetricalExam ->
                            "obstetrical-exam"

                        BreastExam ->
                            "breast-exam"

                isActive =
                    task == data.activeTask

                isCompleted =
                    List.member task data.completedTasks

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
                            data.vitalsForm
                    in
                    ( viewVitalsForm language currentDate motherId form
                    , taskListCompleted [ form.sysBloodPressure, form.diaBloodPressure ]
                        + ([ form.heartRate, form.respiratoryRate, form.bodyTemperature ]
                            |> List.map
                                taskCompleted
                            |> List.sum
                          )
                    , 4
                    )

                NutritionAssessment ->
                    let
                        form =
                            data.nutritionAssessmentForm
                    in
                    ( viewNutritionAssessmentForm language currentDate motherId form
                    , [ form.height, form.weight, form.bmi, form.muac ]
                        |> List.map taskCompleted
                        |> List.sum
                    , 4
                    )

                CorePhysicalExam ->
                    let
                        form =
                            data.corePhysicalExamForm

                        extremitiesTaskCompleted =
                            if isJust form.hands && isJust form.legs then
                                1

                            else
                                0
                    in
                    ( viewCorePhysicalExamForm language currentDate motherId form
                    , extremitiesTaskCompleted
                        + taskCompleted form.neck
                        + taskCompleted form.lungs
                        + taskCompleted form.abdomen
                        + ([ form.brittleHair
                           , form.paleConjuctiva
                           , form.abnormalHeart
                           ]
                            |> List.map taskCompleted
                            |> List.sum
                          )
                    , 7
                    )

                ObstetricalExam ->
                    let
                        form =
                            data.obstetricalExamForm
                    in
                    ( viewObstetricalExamForm language currentDate motherId data.obstetricalExamForm
                    , taskCompleted form.fetalPresentation
                        + ([ form.fundalHeight, form.fetalHeartRate ]
                            |> List.map taskCompleted
                            |> List.sum
                          )
                        + ([ form.fetalMovement, form.cSectionScar ]
                            |> List.map taskCompleted
                            |> List.sum
                          )
                    , 5
                    )

                BreastExam ->
                    let
                        form =
                            data.breastExamForm
                    in
                    ( viewBreastExamForm language currentDate motherId form
                    , taskCompleted form.breast + taskCompleted form.selfGuidance
                    , 2
                    )

        actions =
            div [ class "actions examination" ]
                [ button
                    [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                    , onClick SetExaminationTaskCompleted
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


viewFamilyPlanningContent : Language -> NominalDate -> PersonId -> FamilyPlanningData -> List (Html Msg)
viewFamilyPlanningContent language currentDate motherId data =
    let
        form =
            data.form

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
                    [ Pill, Condoms, IUD ]
                    [ Injection, Necklace, Implant ]
                    (form.signs |> Maybe.withDefault [])
                    (Just NoFamilyPlanning)
                    SetFamilyPlanningSign
                    Translate.FamilyPlanningSignLabel
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage motherId
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewPatientProvisionsContent : Language -> NominalDate -> PersonId -> PatientProvisionsData -> List (Html Msg)
viewPatientProvisionsContent language currentDate motherId data =
    let
        tasks =
            [ Medication, Resources ]

        viewTask task =
            let
                iconClass =
                    case task of
                        Medication ->
                            "medication"

                        Resources ->
                            "resources"

                isActive =
                    task == data.activeTask

                isCompleted =
                    List.member task data.completedTasks

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
                            data.medicationForm
                    in
                    ( viewMedicationForm language currentDate motherId form
                    , [ form.recievedIronFolicAcid, form.recievedDewormingPill ]
                        |> List.map taskCompleted
                        |> List.sum
                    , 2
                    )

                Resources ->
                    let
                        form =
                            data.resourcesForm
                    in
                    ( viewResourcesForm language currentDate motherId form
                    , taskCompleted form.recievedMosquitoNet
                    , 1
                    )

        actions =
            div [ class "actions examination" ]
                [ button
                    [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                    , onClick SetPatientProvisionsTaskCompleted
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


viewDangerSignsContent : Language -> NominalDate -> PersonId -> DangerSignsData -> List (Html Msg)
viewDangerSignsContent language currentDate motherId data =
    let
        form =
            data.form

        totalTasks =
            1

        tasksCompleted =
            taskCompleted form.signs
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form danger-signs" ]
                [ viewQuestionLabel language Translate.SelectDangerSigns
                , viewCheckBoxMultipleSelectInput language
                    [ VaginalBleeding, HeadacheBlurredVision, Convulsions, AbdominalPain ]
                    [ DificultyBreathing, Fever, ExtremeWeakness ]
                    (form.signs |> Maybe.withDefault [])
                    (Just NoDangerSign)
                    SetDangerSign
                    Translate.DangerSign
                ]
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SetActivePage <| UserPage <| PrenatalEncounterPage motherId
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]



-- Forms


viewObstetricFormFirstStep : Language -> NominalDate -> PersonId -> ObstetricFormFirstStep -> Html Msg
viewObstetricFormFirstStep language currentDate motherId form =
    let
        gravidaResult =
            span [] [ text "02" ]

        paraResult =
            span [] [ text "0102" ]

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
        , viewNumberInput language
            form.preTermPregnancy
            (SetOBIntInput preTermPregnancyUpdateFunc)
            "preterm-pregnancy"
            Translate.PreTermPregnancy
        , viewNumberInput language
            form.stillbirthsAtTerm
            (SetOBIntInput stillbirthsAtTermUpdateFunc)
            "stillbirths-at-term"
            Translate.NumberOfStillbirthsAtTerm
        , viewNumberInput language
            form.stillbirthsPreTerm
            (SetOBIntInput stillbirthsPreTermUpdateFunc)
            "stillbirths-pre-term"
            Translate.NumberOfStillbirthsPreTerm
        , viewNumberInput language
            form.abortions
            (SetOBIntInput abortionsUpdateFunc)
            "abortions"
            Translate.NumberOfAbortions
        , viewNumberInput language
            form.liveChildren
            (SetOBIntInput liveChildrenUpdateFunc)
            "live-children"
            Translate.NumberOfLiveChildren
        , div [ class "separator" ] []
        , div [ class "results" ]
            [ div [ class "gravida-result" ]
                [ span [ class "label" ] [ text <| (translate language Translate.Gravida ++ ":") ]
                , gravidaResult
                ]
            , div [ class "para-result" ]
                [ span [ class "label" ] [ text <| (translate language Translate.Para ++ ":") ]
                , paraResult
                ]
            ]
        ]


viewObstetricFormSecondStep : Language -> NominalDate -> PersonId -> ObstetricFormSecondStep -> Html Msg
viewObstetricFormSecondStep language currentDate motherId form =
    let
        cSectionInPreviousDeliveryUpdateFunc value form_ =
            { form_ | cSectionInPreviousDelivery = Just value }

        successiveAbortionsUpdateFunc value form_ =
            { form_ | successiveAbortions = Just value }

        successivePrimatureDeliveriesUpdateFunc value form_ =
            { form_ | successivePrimatureDeliveries = Just value }

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

        convulsionsAndUnconciousPreviousDeliveryUpdateFunc value form_ =
            { form_ | convulsionsAndUnconciousPreviousDelivery = Just value }

        gestatipnalDiabetesPreviousPregnancyUpdateFunc value form_ =
            { form_ | gestatipnalDiabetesPreviousPregnancy = Just value }

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
        , viewLabel language Translate.CSectionInPreviousDelivery
        , viewBoolInput
            language
            form.cSectionInPreviousDelivery
            (SetOBBoolInput cSectionInPreviousDeliveryUpdateFunc)
            "c-section-previous-delivery"
            Nothing
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.CSectionReason ]
            , viewWarning language (Just "Warning!!")
            ]
        , viewCheckBoxSelectInput language
            [ Breech, Emergency, Other ]
            [ FailureToProgress, None ]
            form.reasonForCSection
            SetCSectionReason
            Translate.CSectionReasons
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewCustomLabel language Translate.PreviousDelivery ":" "label c-section-previous-delivery" ]
            , viewWarning language Nothing
            ]
        , viewCheckBoxSelectInput language
            [ LessThan18Month, MoreThan5Years ]
            [ Neither ]
            form.previousDeliveryPeriod
            SetPreviousDeliveryPeriod
            Translate.PreviousDeliveryPeriods
        , viewCustomLabel language Translate.SuccessiveAbortions "?" "label successive-abortions"
        , viewBoolInput
            language
            form.successiveAbortions
            (SetOBBoolInput successiveAbortionsUpdateFunc)
            "successive-abortions"
            Nothing
        , viewLabel language Translate.SuccessivePrimatureDeliveries
        , viewBoolInput
            language
            form.successivePrimatureDeliveries
            (SetOBBoolInput successivePrimatureDeliveriesUpdateFunc)
            "successive-primature-deliveries"
            Nothing
        , viewLabel language Translate.StillbornPreviousDelivery
        , viewBoolInput
            language
            form.stillbornPreviousDelivery
            (SetOBBoolInput stillbornPreviousDeliveryUpdateFunc)
            "stillborn-previous-delivery"
            Nothing
        , viewLabel language Translate.BabyDiedOnDayOfBirthPreviousDelivery
        , viewBoolInput
            language
            form.babyDiedOnDayOfBirthPreviousDelivery
            (SetOBBoolInput babyDiedOnDayOfBirthPreviousDeliveryUpdateFunc)
            "baby-died-on-day-off-birth-previous-delivery"
            Nothing
        , viewLabel language Translate.PartialPlacentaPreviousDelivery
        , viewBoolInput
            language
            form.partialPlacentaPreviousDelivery
            (SetOBBoolInput partialPlacentaPreviousDeliveryUpdateFunc)
            "partial-placenta-previous-delivery"
            Nothing
        , viewLabel language Translate.SevereHemorrhagingPreviousDelivery
        , viewBoolInput
            language
            form.severeHemorrhagingPreviousDelivery
            (SetOBBoolInput severeHemorrhagingPreviousDeliveryUpdateFunc)
            "severe-hemorrhaging-previous-delivery"
            Nothing
        , viewLabel language Translate.PreeclampsiaPreviousPregnancy
        , viewBoolInput
            language
            form.preeclampsiaPreviousPregnancy
            (SetOBBoolInput preeclampsiaPreviousPregnancyUpdateFunc)
            "preeclampsia-previous-pregnancy"
            Nothing
        , viewLabel language Translate.ConvulsionsPreviousDelivery
        , viewBoolInput
            language
            form.convulsionsPreviousDelivery
            (SetOBBoolInput convulsionsPreviousDeliveryUpdateFunc)
            "convulsions-previous-pelivery"
            Nothing
        , viewLabel language Translate.ConvulsionsAndUnconciousPreviousDelivery
        , viewBoolInput
            language
            form.convulsionsAndUnconciousPreviousDelivery
            (SetOBBoolInput convulsionsAndUnconciousPreviousDeliveryUpdateFunc)
            "convulsions-and-unconcious-previous-delivery"
            Nothing
        , viewLabel language Translate.GestatipnalDiabetesPreviousPregnancy
        , viewBoolInput
            language
            form.gestatipnalDiabetesPreviousPregnancy
            (SetOBBoolInput gestatipnalDiabetesPreviousPregnancyUpdateFunc)
            "gestatipnal-diabetes-previous-pregnancy"
            Nothing
        , viewLabel language Translate.IncompleteCervixPreviousPregnancy
        , viewBoolInput
            language
            form.incompleteCervixPreviousPregnancy
            (SetOBBoolInput incompleteCervixPreviousPregnancyUpdateFunc)
            "incomplete-cervix-previous-pregnancy"
            Nothing
        , viewLabel language Translate.RhNegative
        , viewBoolInput
            language
            form.rhNegative
            (SetOBBoolInput rhNegativeUpdateFunc)
            "rh-negative"
            Nothing
        ]


viewMedicalForm : Language -> NominalDate -> PersonId -> MedicalHistoryForm -> Html Msg
viewMedicalForm language currentDate motherId form =
    let
        uterineMyomaUpdateFunc value form_ =
            { form_ | uterineMyoma = Just value }

        diabatesUpdateFunc value form_ =
            { form_ | diabates = Just value }

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
        [ viewCustomLabel language Translate.MedicalFormHelper "?" "label helper"
        , viewLabel language Translate.UterineMyoma
        , viewBoolInput
            language
            form.uterineMyoma
            (SetMedicalBoolInput uterineMyomaUpdateFunc)
            "uterine-myoma"
            Nothing
        , viewLabel language Translate.Diabates
        , viewBoolInput
            language
            form.diabates
            (SetMedicalBoolInput diabatesUpdateFunc)
            "diabates"
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


viewSocialForm : Language -> NominalDate -> PersonId -> SocialHistoryForm -> Html Msg
viewSocialForm language currentDate motherId form =
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


viewVitalsForm : Language -> NominalDate -> PersonId -> VitalsForm -> Html Msg
viewVitalsForm language currentDate motherId form =
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
            Just 110

        heartRatePreviousValue =
            Nothing

        respiratoryRatePreviousValue =
            Just 21

        bodyTemperaturePreviousValue =
            Just 36.9
    in
    div [ class "ui form examination vitals" ]
        [ div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.BloodPressure ]
            , viewWarning language (Just "Warning!!")
            ]
        , div [ class "title sys" ] [ text <| translate language Translate.BloodPressureSysLabel ]
        , viewMeasurementInput
            language
            form.sysBloodPressure
            (SetVitalsMeasurement sysBloodPressureUpdateFunc)
            "sys-blood-pressure"
            Translate.MMHGUnit
        , viewPreviousMeasurement language sysBloodPressurePreviousValue Translate.MMHGUnit
        , div [ class "title dia" ] [ text <| translate language Translate.BloodPressureDiaLabel ]
        , viewMeasurementInput
            language
            form.diaBloodPressure
            (SetVitalsMeasurement diaBloodPressureUpdateFunc)
            "dia-blood-pressure"
            Translate.MMHGUnit
        , viewPreviousMeasurement language diaBloodPressurePreviousValue Translate.MMHGUnit
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.HeartRate ]
            , viewWarning language Nothing
            ]
        , viewMeasurementInput
            language
            form.heartRate
            (SetVitalsMeasurement heartRateUpdateFunc)
            "heart-rate"
            Translate.BpmUnit
        , viewPreviousMeasurement language heartRatePreviousValue Translate.BpmUnit
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.RespiratoryRate ]
            , viewWarning language Nothing
            ]
        , viewMeasurementInput
            language
            form.respiratoryRate
            (SetVitalsMeasurement respiratoryRateUpdateFunc)
            "respiratory-rate"
            Translate.BpmUnit
        , viewPreviousMeasurement language respiratoryRatePreviousValue Translate.BpmUnit
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.BodyTemperature ]
            , viewWarning language Nothing
            ]
        , viewMeasurementInput
            language
            form.bodyTemperature
            (SetVitalsMeasurement bodyTemperatureUpdateFunc)
            "body-temperature"
            Translate.Celcius
        , viewPreviousMeasurement language bodyTemperaturePreviousValue Translate.Celcius
        ]


viewNutritionAssessmentForm : Language -> NominalDate -> PersonId -> NutritionAssessmentForm -> Html Msg
viewNutritionAssessmentForm language currentDate motherId form =
    let
        heightUpdateFunc value form_ =
            { form_ | height = value }

        weightUpdateFunc value form_ =
            { form_ | weight = value }

        bmiUpdateFunc value form_ =
            { form_ | bmi = value }

        muacUpdateFunc value form_ =
            { form_ | muac = value }

        heightPreviousValue =
            Nothing

        weightPreviousValue =
            Just 76

        bmiPreviousValue =
            Nothing

        muacPreviousValue =
            Just 18
    in
    div [ class "ui form examination nutrition-assessment" ]
        [ div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.Height ]
            , viewWarning language (Just "Warning!!")
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
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.BMI ]
            , viewWarning language Nothing
            ]
        , div [ class "title bmi" ] [ text <| translate language Translate.BMIHelper ]
        , viewMeasurementInputAndRound
            language
            form.bmi
            (SetNutritionAssessmentMeasurement bmiUpdateFunc)
            "bmi"
            Translate.EmptyString
            (Just 1)
        , viewPreviousMeasurement language bmiPreviousValue Translate.EmptyString
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.MUAC ]
            , viewWarning language (Just "Warning!!")
            ]
        , viewMeasurementInput
            language
            form.muac
            (SetNutritionAssessmentMeasurement muacUpdateFunc)
            "muac"
            Translate.CentimeterShorthand
        , viewPreviousMeasurement language muacPreviousValue Translate.CentimeterShorthand
        ]


viewCorePhysicalExamForm : Language -> NominalDate -> PersonId -> CorePhysicalExamForm -> Html Msg
viewCorePhysicalExamForm language currentDate motherId form =
    let
        brittleHairUpdateFunc value form_ =
            { form_ | brittleHair = Just value }

        paleConjuctivaUpdateFunc value form_ =
            { form_ | paleConjuctiva = Just value }

        abnormalHeartUpdateFunc value form_ =
            { form_ | abnormalHeart = Just value }
    in
    div [ class "ui form examination core-physical-exam" ]
        [ div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.HeadHair ]
            , viewWarning language Nothing
            ]
        , viewBoolInput
            language
            form.brittleHair
            (SetCorePhysicalExamBoolInput brittleHairUpdateFunc)
            "head-hair"
            (Just ( Translate.BrittleHair, Translate.Normal ))
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.Eyes ]
            , viewWarning language Nothing
            ]
        , viewBoolInput
            language
            form.paleConjuctiva
            (SetCorePhysicalExamBoolInput paleConjuctivaUpdateFunc)
            "eyes"
            (Just ( Translate.PaleConjuctiva, Translate.Normal ))
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.Neck ]
            , viewWarning language Nothing
            ]
        , viewCheckBoxSelectInput language
            [ EnlargedThyroid, EnlargedLymphNodes ]
            [ NormalNeck ]
            form.neck
            SetCorePhysicalExamNeck
            Translate.NeckCPEOption
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.Heart ]
            , viewWarning language Nothing
            ]
        , viewBoolInput
            language
            form.abnormalHeart
            (SetCorePhysicalExamBoolInput abnormalHeartUpdateFunc)
            "abnormal-heart"
            (Just ( Translate.Abnormal, Translate.Normal ))
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.Lungs ]
            , viewWarning language Nothing
            ]
        , viewCheckBoxSelectInput language
            [ Wheezes, Crackles ]
            [ NormalLungs ]
            form.lungs
            SetCorePhysicalExamLungs
            Translate.LungsCPEOption
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.Abdomen ]
            , viewWarning language Nothing
            ]
        , viewCheckBoxSelectInput language
            [ Heptomegaly, Splenomegaly, TPRightUpper, TPLeftUpper ]
            [ NormalAbdomen, Hernia, TPRightLower, TPLeftLower ]
            form.abdomen
            SetCorePhysicalExamAbdomen
            Translate.AbdomenCPEOption
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.Extremities ]
            , viewWarning language (Just "Attention!")
            ]
        , div [ class "title hands" ] [ text <| (translate language Translate.Hands ++ ":") ]
        , viewCheckBoxSelectInput language
            [ PallorHands, EdemaHands ]
            [ NormalHands ]
            form.hands
            SetCorePhysicalExamHands
            Translate.HandsCPEOption
        , div [ class "title legs" ] [ text <| (translate language Translate.Legs ++ ":") ]
        , viewCheckBoxSelectInput language
            [ PallorLegs, EdemaLegs ]
            [ NormalLegs ]
            form.legs
            SetCorePhysicalExamLegs
            Translate.LegsCPEOption
        ]


viewObstetricalExamForm : Language -> NominalDate -> PersonId -> ObstetricalExamForm -> Html Msg
viewObstetricalExamForm language currentDate motherId form =
    let
        fundalHeightUpdateFunc value form_ =
            { form_ | fundalHeight = value }

        fetalHeartRateUpdateFunc value form_ =
            { form_ | fetalHeartRate = value }

        fetalMovementUpdateFunc value form_ =
            { form_ | fetalMovement = Just value }

        cSectionScarUpdateFunc value form_ =
            { form_ | cSectionScar = Just value }

        fetalHeartRatePreviousValue =
            Just 170

        fundalHeightPreviousValue =
            Nothing
    in
    div [ class "ui form examination obstetrical-exam" ]
        [ div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.FundalHeight ]
            , viewWarning language (Just "Attention!")
            ]
        , viewMeasurementInput
            language
            form.fundalHeight
            (SetObstetricalExamMeasurement fundalHeightUpdateFunc)
            "fundal-height"
            Translate.CentimeterShorthand
        , viewPreviousMeasurement language fundalHeightPreviousValue Translate.CentimeterShorthand
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.FetalPresentationLabel ]
            , viewWarning language Nothing
            ]
        , viewCheckBoxSelectInput language
            [ Transverse, Cephalic ]
            [ Breach ]
            form.fetalPresentation
            SetObstetricalExamFetalPresentation
            Translate.FetalPresentation
        , div [ class "separator" ] []
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.FetalMovement ]
            , viewWarning language (Just "Attention!")
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
            form.fetalHeartRate
            (SetObstetricalExamMeasurement fetalHeartRateUpdateFunc)
            "fetal-heart-rate"
            Translate.BpmUnit
        , viewPreviousMeasurement language fetalHeartRatePreviousValue Translate.BpmUnit
        , div [ class "separator" ] []
        , viewLabel language Translate.PreviousCSectionScar
        , viewBoolInput
            language
            form.cSectionScar
            (SetObstetricalExamBoolInput cSectionScarUpdateFunc)
            "c-section-scar"
            Nothing
        ]


viewBreastExamForm : Language -> NominalDate -> PersonId -> BreastExamForm -> Html Msg
viewBreastExamForm language currentDate motherId form =
    let
        selfGuidanceUpdateFunc value form_ =
            { form_ | selfGuidance = Just value }
    in
    div [ class "ui form examination breast-exam" ]
        [ div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewLabel language Translate.BreastExam ]
            , viewWarning language Nothing
            ]
        , viewCheckBoxSelectInput language
            [ Mass, Discharge ]
            [ Infection, NormalBreast ]
            form.breast
            SetBreastExamBreast
            Translate.BreastBEOption
        , div [ class "separator double" ] []
        , viewCustomLabel language Translate.BreastExamQuestion "?" "label self-guidance"
        , viewBoolInput
            language
            form.selfGuidance
            (SetBreastExamBoolInput selfGuidanceUpdateFunc)
            "self-guidance"
            Nothing
        ]


viewMedicationForm : Language -> NominalDate -> PersonId -> MedicationForm -> Html Msg
viewMedicationForm language currentDate motherId form =
    let
        recievedIronFolicAcidUpdateFunc value form_ =
            { form_ | recievedIronFolicAcid = Just value }

        recievedDewormingPillUpdateFunc value form_ =
            { form_ | recievedDewormingPill = Just value }
    in
    div [ class "ui form patient-provisions medication" ]
        [ viewQuestionLabel language Translate.RecievedIronFolicAcid
        , viewBoolInput
            language
            form.recievedIronFolicAcid
            (SetMedicationBoolInput recievedIronFolicAcidUpdateFunc)
            "iron-folic-acid"
            Nothing
        , viewQuestionLabel language Translate.RecievedDewormingPill
        , viewBoolInput
            language
            form.recievedDewormingPill
            (SetMedicationBoolInput recievedDewormingPillUpdateFunc)
            "deworming-pill"
            Nothing
        ]


viewResourcesForm : Language -> NominalDate -> PersonId -> ResourcesForm -> Html Msg
viewResourcesForm language currentDate motherId form =
    let
        recievedMosquitoNetUpdateFunc value form_ =
            { form_ | recievedMosquitoNet = Just value }
    in
    div [ class "ui form patient-provisions resources" ]
        [ viewQuestionLabel language Translate.RecievedMosquitoNet
        , viewBoolInput
            language
            form.recievedMosquitoNet
            (SetResourcesBoolInput recievedMosquitoNetUpdateFunc)
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
    -> Maybe Int
    ->
        (String
         -> Msg
        )
    -> String
    -> TranslationId
    -> Html Msg
viewNumberInput language maybeCurrentValue setMsg inputClass labelTranslationId =
    let
        currentValue =
            maybeCurrentValue
                |> unwrap
                    ""
                    toString
    in
    div [ class <| "form-input number " ++ inputClass ]
        [ div [ class "ui grid" ]
            [ div [ class "ten wide column" ]
                [ viewLabel language labelTranslationId ]
            , div [ class "six wide column" ]
                [ input
                    [ type_ "number"
                    , Html.Attributes.min "0"
                    , Html.Attributes.max "99"
                    , onInput setMsg
                    , value currentValue
                    ]
                    []
                ]
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


viewWarning : Language -> Maybe String -> Html any
viewWarning language maybeMessage =
    maybeMessage
        |> unwrap
            emptyNode
            (\message ->
                div [ class "five wide column" ]
                    [ text message ]
            )



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
