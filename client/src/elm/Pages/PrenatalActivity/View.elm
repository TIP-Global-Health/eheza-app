module Pages.PrenatalActivity.View exposing (view)

import AllDict
import Backend.Entities exposing (..)
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

        _ ->
            []


viewPregnancyDatingContent : Language -> NominalDate -> PersonId -> PregnancyDatingData -> List (Html Msg)
viewPregnancyDatingContent language currentDate motherId pregnancyDatingData =
    let
        form =
            pregnancyDatingData.form

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
                [ div [ class "label" ] [ text <| translate language Translate.LmpRangeHeader ]
                , lmpRangeInput
                , div [ class "label" ] [ text <| translate language Translate.LmpDateHeader ]
                , lmpDateInput
                , div [ class "label" ] [ text <| translate language Translate.LmpDateConfidentHeader ]
                , viewBoolInput language form.lmpDateConfident SetLmpDateConfident "is-confident" Nothing
                , div [ class "separator" ] []
                , div [ class "results" ]
                    [ div [ class "edd-result" ]
                        [ div [ class "label" ] [ text <| translate language Translate.EddHeader ]
                        , eddResult
                        ]
                    , div [ class "ega-result" ]
                        [ div [ class "label" ] [ text <| translate language Translate.EgaHeader ]
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
            div [ class "column" ]
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
                    ( viewNutritionAssessmentForm language currentDate motherId data.nutritionAssessmentForm
                    , 0
                    , 0
                    )

                CorePhysicalExam ->
                    ( viewCorePhysicalExamForm language currentDate motherId data.corePhysicalExamForm
                    , 0
                    , 0
                    )

                ObstetricalExam ->
                    ( viewObstetricalExamForm language currentDate motherId data.obstetricExamForm
                    , 0
                    , 0
                    )

                BreastExam ->
                    ( viewBreastExamForm language currentDate motherId data.breastExamForm
                    , 0
                    , 0
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
        [ viewBoolInput language form.currentlyPregnant SetCurrentlyPregnant "currently-pregnant" (Just Translate.CurrentlyPregnant)
        , viewNumberInput language form.termPregnancy (SetOBIntInput termPregnancyUpdateFunc) "term-pregnancy" Translate.TermPregnancy
        , viewNumberInput language form.preTermPregnancy (SetOBIntInput preTermPregnancyUpdateFunc) "preterm-pregnancy" Translate.PreTermPregnancy
        , viewNumberInput language form.stillbirthsAtTerm (SetOBIntInput stillbirthsAtTermUpdateFunc) "stillbirths-at-term" Translate.NumberOfStillbirthsAtTerm
        , viewNumberInput language form.stillbirthsPreTerm (SetOBIntInput stillbirthsPreTermUpdateFunc) "stillbirths-pre-term" Translate.NumberOfStillbirthsPreTerm
        , viewNumberInput language form.abortions (SetOBIntInput abortionsUpdateFunc) "abortions" Translate.NumberOfAbortions
        , viewNumberInput language form.liveChildren (SetOBIntInput liveChildrenUpdateFunc) "live-children" Translate.NumberOfLiveChildren
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
        , div [ class "label normal" ] [ text <| (translate language Translate.CSectionInPreviousDelivery ++ ":") ]
        , viewBoolInput
            language
            form.cSectionInPreviousDelivery
            (SetOBBoolInput cSectionInPreviousDeliveryUpdateFunc)
            "c-section-previous-delivery"
            Nothing
        , viewCheckBoxSelectInput language
            [ Breech, Emergency, Other ]
            [ FailureToProgress, None ]
            form.reasonForCSection
            Translate.CSectionReason
            SetCSectionReason
            Translate.CSectionReasons
        , viewCheckBoxSelectInput language
            [ LessThan18Month, MoreThan5Years ]
            [ Neither ]
            form.previousDeliveryPeriod
            Translate.PreviousDelivery
            SetPreviousDeliveryPeriod
            Translate.PreviousDeliveryPeriods
        , div [ class "label normal" ] [ text <| (translate language Translate.SuccessiveAbortions ++ ":") ]
        , viewBoolInput
            language
            form.successiveAbortions
            (SetOBBoolInput successiveAbortionsUpdateFunc)
            "successive-abortions"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.SuccessivePrimatureDeliveries ++ ":") ]
        , viewBoolInput
            language
            form.successivePrimatureDeliveries
            (SetOBBoolInput successivePrimatureDeliveriesUpdateFunc)
            "successive-primature-deliveries"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.StillbornPreviousDelivery ++ ":") ]
        , viewBoolInput
            language
            form.stillbornPreviousDelivery
            (SetOBBoolInput stillbornPreviousDeliveryUpdateFunc)
            "stillborn-previous-delivery"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.BabyDiedOnDayOfBirthPreviousDelivery ++ ":") ]
        , viewBoolInput
            language
            form.babyDiedOnDayOfBirthPreviousDelivery
            (SetOBBoolInput babyDiedOnDayOfBirthPreviousDeliveryUpdateFunc)
            "baby-died-on-day-off-birth-previous-delivery"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.PartialPlacentaPreviousDelivery ++ ":") ]
        , viewBoolInput
            language
            form.partialPlacentaPreviousDelivery
            (SetOBBoolInput partialPlacentaPreviousDeliveryUpdateFunc)
            "partial-placenta-previous-delivery"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.SevereHemorrhagingPreviousDelivery ++ ":") ]
        , viewBoolInput
            language
            form.severeHemorrhagingPreviousDelivery
            (SetOBBoolInput severeHemorrhagingPreviousDeliveryUpdateFunc)
            "severe-hemorrhaging-previous-delivery"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.PreeclampsiaPreviousPregnancy ++ ":") ]
        , viewBoolInput
            language
            form.preeclampsiaPreviousPregnancy
            (SetOBBoolInput preeclampsiaPreviousPregnancyUpdateFunc)
            "preeclampsia-previous-pregnancy"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.ConvulsionsPreviousDelivery ++ ":") ]
        , viewBoolInput
            language
            form.convulsionsPreviousDelivery
            (SetOBBoolInput convulsionsPreviousDeliveryUpdateFunc)
            "convulsions-previous-pelivery"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.ConvulsionsAndUnconciousPreviousDelivery ++ ":") ]
        , viewBoolInput
            language
            form.convulsionsAndUnconciousPreviousDelivery
            (SetOBBoolInput convulsionsAndUnconciousPreviousDeliveryUpdateFunc)
            "convulsions-and-unconcious-previous-delivery"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.GestatipnalDiabetesPreviousPregnancy ++ ":") ]
        , viewBoolInput
            language
            form.gestatipnalDiabetesPreviousPregnancy
            (SetOBBoolInput gestatipnalDiabetesPreviousPregnancyUpdateFunc)
            "gestatipnal-diabetes-previous-pregnancy"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.IncompleteCervixPreviousPregnancy ++ ":") ]
        , viewBoolInput
            language
            form.incompleteCervixPreviousPregnancy
            (SetOBBoolInput incompleteCervixPreviousPregnancyUpdateFunc)
            "incomplete-cervix-previous-pregnancy"
            Nothing
        , div [ class "label normal" ] [ text <| (translate language Translate.RhNegative ++ ":") ]
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
        [ div [ class "label helper" ] [ text <| (translate language Translate.MedicalFormHelper ++ ":") ]
        , div [ class "label" ] [ text <| (translate language Translate.UterineMyoma ++ ":") ]
        , viewBoolInput
            language
            form.uterineMyoma
            (SetMedicalBoolInput uterineMyomaUpdateFunc)
            "uterine-myoma"
            Nothing
        , div [ class "label" ] [ text <| (translate language Translate.Diabates ++ ":") ]
        , viewBoolInput
            language
            form.diabates
            (SetMedicalBoolInput diabatesUpdateFunc)
            "diabates"
            Nothing
        , div [ class "label" ] [ text <| (translate language Translate.CardiacDisease ++ ":") ]
        , viewBoolInput
            language
            form.cardiacDisease
            (SetMedicalBoolInput cardiacDiseaseUpdateFunc)
            "cardiac-disease"
            Nothing
        , div [ class "label" ] [ text <| (translate language Translate.RenalDisease ++ ":") ]
        , viewBoolInput
            language
            form.renalDisease
            (SetMedicalBoolInput renalDiseaseUpdateFunc)
            "renal-disease"
            Nothing
        , div [ class "label" ] [ text <| (translate language Translate.HypertensionBeforePregnancy ++ ":") ]
        , viewBoolInput
            language
            form.hypertensionBeforePregnancy
            (SetMedicalBoolInput hypertensionBeforePregnancyUpdateFunc)
            "hypertension-before-pregnancy"
            Nothing
        , div [ class "label" ] [ text <| (translate language Translate.TuberculosisPast ++ ":") ]
        , viewBoolInput
            language
            form.tuberculosisPast
            (SetMedicalBoolInput tuberculosisPastUpdateFunc)
            "tuberculosis-past"
            Nothing
        , div [ class "label" ] [ text <| (translate language Translate.TuberculosisPresent ++ ":") ]
        , viewBoolInput
            language
            form.tuberculosisPresent
            (SetMedicalBoolInput tuberculosisPresentUpdateFunc)
            "tuberculosis-present"
            Nothing
        , div [ class "label" ] [ text <| (translate language Translate.Asthma ++ ":") ]
        , viewBoolInput
            language
            form.asthma
            (SetMedicalBoolInput asthmaUpdateFunc)
            "asthma"
            Nothing
        , div [ class "label" ] [ text <| (translate language Translate.BowedLegs ++ ":") ]
        , viewBoolInput
            language
            form.bowedLegs
            (SetMedicalBoolInput bowedLegsUpdateFunc)
            "bowed-legs"
            Nothing
        , div [ class "label" ] [ text <| (translate language Translate.HIV ++ ":") ]
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
        [ div [ class "label" ] [ text <| (translate language Translate.AccompaniedByPartner ++ "?") ]
        , viewBoolInput
            language
            form.accompaniedByPartner
            (SetSocialBoolInput accompaniedByPartnerUpdateFunc)
            "accompanied-by-partner"
            Nothing
        , div [ class "label" ] [ text <| (translate language Translate.PartnerReceivedCounseling ++ "?") ]
        , viewBoolInput
            language
            form.partnerReceivedCounseling
            (SetSocialBoolInput partnerReceivedCounselingUpdateFunc)
            "partner-received-counseling"
            Nothing
        , div [ class "label" ] [ text <| (translate language Translate.MentalHealthHistory ++ ":") ]
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
        [ div [ class "label" ] [ text <| (translate language Translate.BloodPressure ++ ":") ]
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ div [ class "title" ] [ text <| translate language Translate.BloodPressureSysLabel ]
                , viewMeasurementInput
                    language
                    form.sysBloodPressure
                    (SetVitalsMeasurement sysBloodPressureUpdateFunc)
                    "sys-blood-pressure"
                    Translate.MMHGUnit
                , div [ class "title" ] [ text <| translate language Translate.BloodPressureDiaLabel ]
                , viewPreviousMeasurement language sysBloodPressurePreviousValue Translate.MMHGUnit
                , viewMeasurementInput
                    language
                    form.diaBloodPressure
                    (SetVitalsMeasurement diaBloodPressureUpdateFunc)
                    "dia-blood-pressure"
                    Translate.MMHGUnit
                , viewPreviousMeasurement language diaBloodPressurePreviousValue Translate.MMHGUnit
                ]
            , div [ class "five wide column" ]
                [ text "Warning" ]
            ]
        , div [ class "separator" ] []
        , div [ class "label" ] [ text <| (translate language Translate.HeartRate ++ ":") ]
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewMeasurementInput
                    language
                    form.heartRate
                    (SetVitalsMeasurement heartRateUpdateFunc)
                    "heart-rate"
                    Translate.BpmUnit
                , viewPreviousMeasurement language heartRatePreviousValue Translate.BpmUnit
                ]
            , div [ class "five wide column" ]
                [ text "Warning" ]
            ]
        , div [ class "separator" ] []
        , div [ class "label" ] [ text <| (translate language Translate.RespiratoryRate ++ ":") ]
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewMeasurementInput
                    language
                    form.respiratoryRate
                    (SetVitalsMeasurement respiratoryRateUpdateFunc)
                    "respiratory-rate"
                    Translate.BpmUnit
                , viewPreviousMeasurement language respiratoryRatePreviousValue Translate.BpmUnit
                ]
            , div [ class "five wide column" ]
                [ text "Warning" ]
            ]
        , div [ class "separator" ] []
        , div [ class "label" ] [ text <| (translate language Translate.BodyTemperature ++ ":") ]
        , div [ class "ui grid" ]
            [ div [ class "eleven wide column" ]
                [ viewMeasurementInput
                    language
                    form.bodyTemperature
                    (SetVitalsMeasurement bodyTemperatureUpdateFunc)
                    "body-temperature"
                    Translate.Celcius
                , viewPreviousMeasurement language bodyTemperaturePreviousValue Translate.Celcius
                ]
            , div [ class "five wide column" ]
                [ text "Warning" ]
            ]
        ]


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


viewNutritionAssessmentForm : Language -> NominalDate -> PersonId -> NutritionAssessmentForm -> Html Msg
viewNutritionAssessmentForm language currentDate motherId form =
    div [ class "form examination nutrition-assessment" ]
        [ div [ class "label" ] [ text <| (translate language Translate.BloodPressure ++ ":") ]
        ]


viewCorePhysicalExamForm : Language -> NominalDate -> PersonId -> CorePhysicalExamForm -> Html Msg
viewCorePhysicalExamForm language currentDate motherId form =
    div [ class "form examination core-physical-exam" ]
        [ div [ class "label" ] [ text <| (translate language Translate.BloodPressure ++ ":") ]
        ]


viewObstetricalExamForm : Language -> NominalDate -> PersonId -> ObstetricalExamForm -> Html Msg
viewObstetricalExamForm language currentDate motherId form =
    div [ class "form examination obstetrical-exam" ]
        [ div [ class "label" ] [ text <| (translate language Translate.BloodPressure ++ ":") ]
        ]


viewBreastExamForm : Language -> NominalDate -> PersonId -> BreastExamForm -> Html Msg
viewBreastExamForm language currentDate motherId form =
    div [ class "form examination breast-exam" ]
        [ div [ class "label" ] [ text <| (translate language Translate.BloodPressure ++ ":") ]
        ]


viewBoolInput : Language -> Maybe Bool -> (Bool -> Msg) -> String -> Maybe TranslationId -> Html Msg
viewBoolInput language currentValue setMsg inputClass labelTranslateId =
    let
        inputLabel =
            labelTranslateId
                |> unwrap
                    emptyNode
                    (\translationId ->
                        div [ class "label" ] [ text <| (translate language translationId ++ ":") ]
                    )

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
    div [ class <| "form-input " ++ inputClass ]
        [ inputLabel
        , viewInput True currentValue setMsg
        , label [ class "yes" ] [ text <| translate language Translate.Yes ]
        , viewInput False currentValue setMsg
        , label [ class "no" ] [ text <| translate language Translate.No ]
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
    div [ class <| "form-input " ++ inputClass ]
        [ div [ class "label" ] [ text <| (translate language labelTranslationId ++ ":") ]
        , input
            [ type_ "number"
            , Html.Attributes.min "0"
            , Html.Attributes.max "99"
            , onInput setMsg
            , value currentValue
            ]
            []
        ]


viewCheckBoxSelectInput : Language -> List a -> List a -> Maybe a -> TranslationId -> (a -> Msg) -> (a -> TranslationId) -> Html Msg
viewCheckBoxSelectInput language leftOptions rightOptions currentValue labelTranslateId setMsg translateFunc =
    div [ class "ui form" ]
        [ p [] [ text <| (translate language labelTranslateId ++ ":") ]
        , div [ class "ui grid" ]
            [ leftOptions
                |> List.map (viewCheckBoxSelectInputItem language currentValue setMsg translateFunc)
                |> div [ class "eight wide column" ]
            , rightOptions
                |> List.map (viewCheckBoxSelectInputItem language currentValue setMsg translateFunc)
                |> div [ class "eight wide column" ]
            ]
        ]


viewCheckBoxSelectInputItem : Language -> Maybe a -> (a -> Msg) -> (a -> TranslationId) -> a -> Html Msg
viewCheckBoxSelectInputItem language currentValue setMsg translateFunc option =
    let
        isChecked =
            currentValue == Just option
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
    let
        currentValue =
            maybeCurrentValue
                |> unwrap
                    ""
                    toString

        inputAttrs =
            [ type_ "number"
            , Html.Attributes.min "0"
            , onInput setMsg
            , value currentValue
            ]
    in
    div [ class "ui right labeled input" ]
        [ input inputAttrs []
        , div
            [ class "ui basic label" ]
            [ text <| translate language unitTranslationId ]
        ]


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
