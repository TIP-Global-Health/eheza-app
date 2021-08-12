module Pages.WellChildActivity.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, headCircumferenceIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (nutritionAssessmentForBackend, resolvePreviousValuesSetForChild)
import Backend.Person.Model exposing (Person)
import Backend.Person.Utils exposing (ageInMonths)
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Backend.WellChildEncounter.Model exposing (WellChildEncounter)
import Date exposing (Unit(..))
import DateSelector.SelectorDropdown
import EverySet
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatDDMMyyyy)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import List.Extra
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Utils exposing (..)
import Measurement.View
    exposing
        ( renderDatePart
        , viewBasicVitalsForm
        , viewColorAlertIndication
        , viewContributingFactorsForm
        , viewFollowUpForm
        , viewHealthEducationForm
        , viewMeasurementFloatDiff
        , viewReferToProgramForm
        , viewSendToHCForm
        , zScoreForHeightOrLength
        )
import Pages.AcuteIllnessActivity.View exposing (viewAdministeredMedicationCustomLabel, viewAdministeredMedicationQuestion)
import Pages.NutritionActivity.View exposing (viewHeightForm, viewMuacForm, viewNutritionForm, viewPhotoForm, viewWeightForm, warningPopup)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.PrenatalEncounter.View exposing (viewPersonDetails)
import Pages.Utils
    exposing
        ( isTaskCompleted
        , taskCompleted
        , taskCompletedWithException
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewLabel
        , viewMeasurementInput
        , viewPreviousMeasurement
        , viewQuestionLabel
        )
import Pages.WellChildActivity.Model exposing (..)
import Pages.WellChildActivity.Utils exposing (..)
import Pages.WellChildEncounter.Model exposing (AssembledData, VaccinationProgressDict)
import Pages.WellChildEncounter.Utils exposing (generateAssembledData)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.NominalDate exposing (diffDays)
import Utils.WebData exposing (viewWebData)
import ZScore.Model exposing (Centimetres(..), Kilograms(..), ZScore)
import ZScore.Utils exposing (viewZScore, zScoreHeadCircumferenceForAge)


view : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> WellChildActivity -> ModelIndexedDb -> Model -> Html Msg
view language currentDate zscores id isChw activity db model =
    let
        data =
            generateAssembledData id db
    in
    viewWebData language (viewHeaderAndContent language currentDate zscores id isChw activity db model) identity data


viewHeaderAndContent : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> WellChildActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewHeaderAndContent language currentDate zscores id isChw activity db model assembled =
    let
        header =
            viewHeader language id activity

        content =
            viewContent language currentDate zscores id isChw activity db model assembled
    in
    div [ class "page-activity well-child" ]
        [ header
        , content
        , viewModal <|
            viewWarningPopup language currentDate model.warningPopupState
        ]


viewHeader : Language -> WellChildEncounterId -> WellChildActivity -> Html Msg
viewHeader language id activity =
    div
        [ class "ui basic segment head" ]
        [ h1
            [ class "ui header" ]
            [ text <| translate language <| Translate.WellChildActivityTitle activity ]
        , a
            [ class "link-back"
            , onClick <| SetActivePage <| UserPage <| WellChildEncounterPage id
            ]
            [ span [ class "icon-back" ] []
            , span [] []
            ]
        ]


viewContent : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> WellChildActivity -> ModelIndexedDb -> Model -> AssembledData -> Html Msg
viewContent language currentDate zscores id isChw activity db model assembled =
    ((viewPersonDetails language currentDate assembled.person Nothing |> div [ class "item" ])
        :: viewActivity language currentDate zscores id isChw activity assembled db model
    )
        |> div [ class "ui unstackable items" ]


viewWarningPopup : Language -> NominalDate -> Maybe WarningPopupType -> Maybe (Html Msg)
viewWarningPopup language currentDate warningPopupState =
    warningPopupState
        |> Maybe.andThen
            (\popupType ->
                case popupType of
                    PopupNutritionAssessment assessment ->
                        warningPopup language
                            currentDate
                            (SetWarningPopupState Nothing)
                            assessment

                    PopupMacrocephaly personId saved nextTask_ ->
                        headCircumferencePopup language ( personId, saved, nextTask_ ) Translate.WellChildMacrocephalyWarning

                    PopupMicrocephaly personId saved nextTask_ ->
                        headCircumferencePopup language ( personId, saved, nextTask_ ) Translate.WellChildMicrocephalyWarning

                    PopupVaccinationHistory vaccinationHistory ->
                        vaccinationHistoryPopup language currentDate vaccinationHistory
            )


headCircumferencePopup :
    Language
    -> ( PersonId, Maybe ( WellChildHeadCircumferenceId, WellChildHeadCircumference ), Maybe NutritionAssessmentTask )
    -> TranslationId
    -> Maybe (Html Msg)
headCircumferencePopup language ( personId, saved, nextTask_ ) message =
    Just <|
        div [ class "ui active modal danger-signs-popup" ]
            [ div [ class "content" ]
                [ div [ class "popup-heading-wrapper" ]
                    [ img [ src "assets/images/exclamation-red.png" ] []
                    , div [ class "popup-heading warning" ] [ text <| translate language Translate.Warning ++ "!" ]
                    ]
                , div [ class "popup-action" ] [ text <| translate language message ]
                ]
            , div [ class "actions" ]
                [ button
                    [ class "ui fluid button"
                    , onClick <| CloseHeadCircumferencePopup personId saved nextTask_
                    ]
                    [ text <| translate language Translate.Continue ]
                ]
            ]


vaccinationHistoryPopup : Language -> NominalDate -> VaccinationProgressDict -> Maybe (Html Msg)
vaccinationHistoryPopup language currentDate vaccinationHistory =
    let
        entries =
            Dict.toList vaccinationHistory
                |> List.map viewVaccinationEntry

        viewVaccinationEntry ( vaccineType, doses ) =
            if Dict.isEmpty doses then
                emptyNode

            else
                div [ class "entry" ]
                    [ div [ class "name" ] [ text <| translate language <| Translate.VaccineType vaccineType ]
                    , Dict.values doses
                        |> List.sortWith Date.compare
                        |> List.map (formatDDMMyyyy >> text >> List.singleton >> p [])
                        |> div [ class "dates" ]
                    ]
    in
    Just <|
        div [ class "ui active modal vaccination-history-popup" ] <|
            [ div [ class "header" ]
                [ text <| translate language Translate.ImmunisationHistory ]
            , div [ class "content" ]
                [ div [ class "caption" ]
                    [ div [ class "name" ] [ text <| translate language Translate.Immunisation ]
                    , div [ class "dates" ] [ text <| translate language Translate.DateReceived ]
                    ]
                , div [ class "entries" ] entries
                ]
            , div [ class "actions" ]
                [ button
                    [ class "ui primary fluid button"
                    , onClick <| SetWarningPopupState Nothing
                    ]
                    [ text <| translate language Translate.Close ]
                ]
            ]


viewActivity : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> WellChildActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate zscores id isChw activity assembled db model =
    case activity of
        WellChildPregnancySummary ->
            viewPregnancySummaryForm language currentDate assembled model.pregnancySummaryForm

        WellChildVaccinationHistory ->
            viewVaccinationHistoryForm language currentDate isChw assembled model.vaccinationHistoryForm

        WellChildDangerSigns ->
            viewDangerSignsContent language currentDate assembled model.dangerSignsData

        WellChildNutritionAssessment ->
            viewNutritionAssessmenContent language currentDate zscores id isChw assembled db model.nutritionAssessmentData

        WellChildImmunisation ->
            viewImmunisationForm language currentDate isChw assembled model.immunisationForm

        WellChildECD ->
            viewECDForm language currentDate assembled model.ecdForm

        WellChildMedication ->
            viewMedicationContent language currentDate isChw assembled model.medicationData

        WellChildNextSteps ->
            viewNextStepsContent language currentDate zscores id isChw assembled db model.nextStepsData


viewPregnancySummaryForm : Language -> NominalDate -> AssembledData -> PregnancySummaryForm -> List (Html Msg)
viewPregnancySummaryForm language currentDate assembled form_ =
    let
        form =
            assembled.measurements.pregnancySummary
                |> getMeasurementValueFunc
                |> pregnancySummaryFormWithDefault form_

        ( deliveryComplicationsCompleted, deliveryComplicationsActive ) =
            if form.deliveryComplicationsPresent == Just True then
                ( taskCompleted form.deliveryComplications, 1 )

            else
                ( 0, 0 )

        ( tasksCompleted, totalTasks ) =
            ( taskCompleted form.expectedDateConcluded
                + taskCompleted form.dateConcluded
                + taskCompleted form.apgarsOneMinute
                + taskCompleted form.apgarsFiveMinutes
                + taskCompleted form.deliveryComplicationsPresent
                + deliveryComplicationsCompleted
            , 5 + deliveryComplicationsActive
            )

        expectedDateConcludedInput =
            DateSelector.SelectorDropdown.view
                ToggleExpectedDateConcluded
                SetExpectedDateConcluded
                form.isExpectedDateConcludedSelectorOpen
                (Date.add Months -3 currentDate)
                (Date.add Months 4 currentDate)
                form.expectedDateConcluded

        dateConcludedInput =
            DateSelector.SelectorDropdown.view
                ToggleDateConcluded
                SetDateConcluded
                form.isDateConcludedSelectorOpen
                (Date.add Months -2 currentDate)
                currentDate
                form.dateConcluded

        viewDatesDiff =
            Maybe.map2
                (\expected actual ->
                    let
                        diffMonths =
                            Date.diff Months expected actual

                        expectedAdjustedMonths =
                            Date.add Months diffMonths expected

                        diffWeeks =
                            Date.diff Weeks expectedAdjustedMonths actual

                        diffDays =
                            Date.diff Days (Date.add Weeks diffWeeks expectedAdjustedMonths) actual

                        parts =
                            [ ( diffMonths, Translate.MonthSinglePlural ), ( diffWeeks, Translate.WeekSinglePlural ), ( diffDays, Translate.DaySinglePlural ) ]
                                |> List.filter (Tuple.first >> (/=) 0)

                        viewPart ( value, transId ) =
                            translate language <| transId (abs value)

                        viewDiff =
                            case parts of
                                [ single ] ->
                                    viewPart single

                                [ first, second ] ->
                                    viewPart first ++ " " ++ translate language Translate.And ++ " " ++ viewPart second

                                [ first, second, third ] ->
                                    viewPart first ++ ", " ++ viewPart second ++ " " ++ translate language Translate.And ++ " " ++ viewPart third

                                _ ->
                                    viewPart ( 0, Translate.DaySinglePlural )
                    in
                    [ viewLabel language Translate.DifferenceBetweenDates
                    , div [ class "form-input" ] [ text viewDiff ]
                    ]
                )
                form.expectedDateConcluded
                form.dateConcluded
                |> Maybe.withDefault []

        apgarsOptions fromValue =
            option
                [ value ""
                , selected (fromValue == Nothing)
                ]
                [ text "" ]
                :: (List.repeat 11 ""
                        |> List.indexedMap
                            (\index _ ->
                                option
                                    [ value <| String.fromInt index
                                    , selected <| fromValue == Just index
                                    ]
                                    [ text <| String.fromInt index ]
                            )
                   )

        apgarsOneMinuteInput =
            apgarsOptions form.apgarsOneMinute
                |> select
                    [ class "form-input apgars"
                    , onInput SetApgarsOneMinute
                    ]

        apgarsFiveMinutesInput =
            apgarsOptions form.apgarsFiveMinutes
                |> select
                    [ class "form-input apgars"
                    , onInput SetApgarsFiveMinutes
                    ]

        deliveryComplicationsPresentInput =
            viewBoolInput
                language
                form.deliveryComplicationsPresent
                SetDeliveryComplicationsPresent
                ""
                Nothing

        deliveryComplicationsSection =
            if form.deliveryComplicationsPresent == Just True then
                [ viewLabel language Translate.DeliveryComplicationsSelectionLabel
                , viewCheckBoxMultipleSelectInput language
                    [ ComplicationGestationalDiabetes, ComplicationEmergencyCSection, ComplicationPreclampsia ]
                    [ ComplicationMaternalHemmorhage, ComplicationHiv, ComplicationMaternalDeath ]
                    (form.deliveryComplications |> Maybe.withDefault [])
                    Nothing
                    SetDeliveryComplication
                    Translate.DeliveryComplication
                ]

            else
                []

        disabled =
            tasksCompleted /= totalTasks
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form pregnancy-summary" ] <|
                [ viewQuestionLabel language Translate.DateConcludedEstimatedQuestion
                , div [ class "form-input date" ]
                    [ expectedDateConcludedInput ]
                , viewQuestionLabel language Translate.DateConcludedActualQuestion
                , div [ class "form-input date" ]
                    [ dateConcludedInput ]
                ]
                    ++ viewDatesDiff
                    ++ [ viewQuestionLabel language Translate.ChildOneMinuteApgarsQuestion
                       , apgarsOneMinuteInput
                       , viewQuestionLabel language Translate.ChildFiveMinutesApgarsQuestion
                       , apgarsFiveMinutesInput
                       , viewQuestionLabel language Translate.DeliveryComplicationsPresentQuestion
                       , deliveryComplicationsPresentInput
                       ]
                    ++ deliveryComplicationsSection
            ]
        , viewAction language (SavePregnancySummary assembled.participant.person assembled.measurements.pregnancySummary) disabled
        ]
    ]


viewDangerSignsContent :
    Language
    -> NominalDate
    -> AssembledData
    -> DangerSignsData
    -> List (Html Msg)
viewDangerSignsContent language currentDate assembled data =
    let
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            [ TaskSymptomsReview, TaskVitals ]

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        TaskSymptomsReview ->
                            ( "symptoms"
                            , isJust measurements.symptomsReview
                            )

                        TaskVitals ->
                            ( "vitals"
                            , isJust measurements.vitals
                            )

                isActive =
                    activeTask == Just task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveDangerSignsTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.WellChildDangerSignsTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map (\task -> ( task, dangerSignsTasksCompletedFromTotal measurements data task ))
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just TaskSymptomsReview ->
                    measurements.symptomsReview
                        |> getMeasurementValueFunc
                        |> symptomsReviewFormWithDefault data.symptomsReviewForm
                        |> viewSymptomsReviewForm language currentDate assembled.person

                Just TaskVitals ->
                    let
                        previousRespiratoryRate =
                            resolvePreviousValue assembled .vitals .respiratoryRate
                                |> Maybe.map toFloat

                        previousBodyTemperature =
                            resolvePreviousValue assembled .vitals .bodyTemperature
                    in
                    measurements.vitals
                        |> getMeasurementValueFunc
                        |> basicVitalsFormWithDefault data.vitalsForm
                        |> viewBasicVitalsForm language
                            currentDate
                            assembled.person
                            previousRespiratoryRate
                            previousBodyTemperature
                            SetVitalsResporatoryRate
                            SetVitalsBodyTemperature

                Nothing ->
                    []

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
                                    TaskSymptomsReview ->
                                        SaveSymptomsReview personId measurements.symptomsReview nextTask

                                    TaskVitals ->
                                        SaveVitals personId measurements.vitals nextTask

                            disabled =
                                tasksCompleted /= totalTasks
                        in
                        viewAction language saveMsg disabled
                    )
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue" ]
        [ div [ class "ui four column grid" ] <|
            List.map viewTask tasks
        ]
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            (viewForm ++ [ actions ])
        ]
    ]


viewSymptomsReviewForm : Language -> NominalDate -> Person -> SymptomsReviewForm -> List (Html Msg)
viewSymptomsReviewForm language currentDate person form =
    [ div [ class "ui form symptoms-review" ]
        [ viewLabel language Translate.SelectAllSigns
        , viewCheckBoxMultipleSelectInput language
            [ SymptomBreathingProblems
            , SymptomConvulsions
            , SymptomLethargyOrUnresponsiveness
            , SymptomDiarrhea
            , SymptomVomiting
            , SymptomUmbilicalCordRedness
            , SymptomStiffNeckOrBulgingFontanelle
            , SymptomSevereEdema
            , SymptomPalmoplantarPallor
            , SymptomHistoryOfFever
            , SymptomBabyTiresQuicklyWhenFeeding
            , SymptomCoughingOrTearingWhileFeeding
            , SymptomRigidMusclesOrJawClenchingPreventingFeeding
            , ExcessiveSweatingWhenFeeding
            ]
            []
            (form.symptoms |> Maybe.withDefault [])
            (Just NoWellChildSymptoms)
            SetSymptom
            Translate.WellChildSymptom
        ]
    ]


viewNutritionAssessmenContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> WellChildEncounterId
    -> Bool
    -> AssembledData
    -> ModelIndexedDb
    -> NutritionAssessmentData
    -> List (Html Msg)
viewNutritionAssessmenContent language currentDate zscores id isChw assembled db data =
    let
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        ( mandatory, optional ) =
            partitionNutritionAssessmentTasks isChw

        tasks =
            (mandatory ++ optional)
                |> List.filter (expectNutritionAssessmentTask currentDate isChw assembled db)

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        TaskHeight ->
                            ( "height"
                            , isJust measurements.height
                            )

                        TaskHeadCircumference ->
                            ( "head-circumference"
                            , isJust measurements.headCircumference
                            )

                        TaskMuac ->
                            ( "muac"
                            , isJust measurements.muac
                            )

                        TaskNutrition ->
                            ( "nutrition"
                            , isJust measurements.nutrition
                            )

                        TaskPhoto ->
                            ( "photo"
                            , isJust measurements.photo
                            )

                        TaskWeight ->
                            ( "weight"
                            , isJust measurements.weight
                            )

                isActive =
                    activeTask == Just task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveNutritionAssessmentTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.NutritionAssessmentTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map (\task -> ( task, nutritionAssessmentTasksCompletedFromTotal measurements data task ))
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        previousValuesSet =
            resolvePreviousValuesSetForChild assembled.participant.person db

        headCircumferenceForm =
            measurements.headCircumference
                |> getMeasurementValueFunc
                |> headCircumferenceFormWithDefault data.headCircumferenceForm

        headCircumferenceZScore =
            if headCircumferenceForm.measurementNotTaken == Just True then
                Nothing

            else
                let
                    maybeAgeInDays =
                        Maybe.map
                            (\birthDate -> diffDays birthDate currentDate)
                            person.birthDate
                in
                headCircumferenceForm.headCircumference
                    |> Maybe.andThen
                        (\headCircumference ->
                            Maybe.andThen
                                (\ageInDays ->
                                    zScoreHeadCircumferenceForAge zscores ageInDays person.gender (Centimetres headCircumference)
                                )
                                maybeAgeInDays
                        )

        viewForm =
            case activeTask of
                Just TaskHeight ->
                    measurements.height
                        |> getMeasurementValueFunc
                        |> heightFormWithDefault data.heightForm
                        |> viewHeightForm language currentDate zscores assembled.person previousValuesSet.height SetHeight

                Just TaskHeadCircumference ->
                    viewHeadCircumferenceForm language currentDate assembled.person headCircumferenceZScore previousValuesSet.headCircumference headCircumferenceForm

                Just TaskMuac ->
                    measurements.muac
                        |> getMeasurementValueFunc
                        |> muacFormWithDefault data.muacForm
                        |> viewMuacForm language currentDate assembled.person previousValuesSet.muac SetMuac

                Just TaskNutrition ->
                    measurements.nutrition
                        |> getMeasurementValueFunc
                        |> nutritionFormWithDefault data.nutritionForm
                        |> viewNutritionForm language currentDate SetNutritionSign

                Just TaskPhoto ->
                    let
                        displayPhoto =
                            case data.photoForm.url of
                                Just url ->
                                    Just url

                                Nothing ->
                                    getMeasurementValueFunc assembled.measurements.photo
                    in
                    viewPhotoForm language currentDate displayPhoto DropZoneComplete

                Just TaskWeight ->
                    let
                        heightValue =
                            assembled.measurements.height
                                |> getMeasurementValueFunc
                    in
                    measurements.weight
                        |> getMeasurementValueFunc
                        |> weightFormWithDefault data.weightForm
                        |> viewWeightForm language currentDate zscores assembled.person heightValue previousValuesSet.weight SetWeight

                Nothing ->
                    []

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
                                    TaskHeight ->
                                        SaveHeight personId measurements.height nextTask

                                    TaskHeadCircumference ->
                                        let
                                            saveHeadCircumferenceMsg =
                                                SaveHeadCircumference personId measurements.headCircumference nextTask
                                        in
                                        headCircumferenceZScore
                                            |> Maybe.map
                                                (\zscore ->
                                                    if zscore > 3 then
                                                        PopupMacrocephaly personId measurements.headCircumference nextTask
                                                            |> Just
                                                            |> SetWarningPopupState

                                                    else if zscore < -3 then
                                                        PopupMicrocephaly personId measurements.headCircumference nextTask
                                                            |> Just
                                                            |> SetWarningPopupState

                                                    else
                                                        saveHeadCircumferenceMsg
                                                )
                                            |> Maybe.withDefault saveHeadCircumferenceMsg

                                    TaskMuac ->
                                        SaveMuac personId measurements.muac nextTask

                                    TaskNutrition ->
                                        SaveNutrition personId measurements.nutrition nextTask

                                    TaskPhoto ->
                                        let
                                            photoId =
                                                Maybe.map Tuple.first measurements.photo
                                        in
                                        data.photoForm.url
                                            |> Maybe.map (\url -> SavePhoto personId photoId url nextTask)
                                            |> Maybe.withDefault NoOp

                                    TaskWeight ->
                                        SaveWeight personId measurements.weight nextTask

                            disabled =
                                case task of
                                    TaskPhoto ->
                                        isNothing data.photoForm.url

                                    _ ->
                                        tasksCompleted /= totalTasks
                        in
                        viewAction language saveMsg disabled
                    )
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


viewHeadCircumferenceForm :
    Language
    -> NominalDate
    -> Person
    -> Maybe Float
    -> Maybe Float
    -> HeadCircumferenceForm
    -> List (Html Msg)
viewHeadCircumferenceForm language currentDate person zscore previousValue form =
    let
        maybeAgeInDays =
            Maybe.map
                (\birthDate -> diffDays birthDate currentDate)
                person.birthDate

        zScoreText =
            Maybe.map viewZScore zscore
                |> Maybe.withDefault (translate language Translate.NotAvailable)

        inputSection =
            if measurementNotTakenChecked then
                []

            else
                [ div [ class "ui grid" ]
                    [ div [ class "eleven wide column" ]
                        [ viewMeasurementInput
                            language
                            form.headCircumference
                            SetHeadCircumference
                            "head-circumference"
                            Translate.CentimeterShorthand
                        ]
                    , div
                        [ class "five wide column" ]
                        [ showMaybe <|
                            Maybe.map (HeadCircumferenceInCm >> headCircumferenceIndication >> viewColorAlertIndication language) zscore
                        ]
                    ]
                , viewPreviousMeasurement language previousValue Translate.CentimeterShorthand
                , div [ class "ui large header z-score age" ]
                    [ text <| translate language Translate.ZScoreHeadCircumferenceForAge
                    , span [ class "sub header" ]
                        [ text zScoreText ]
                    ]
                ]

        measurementNotTakenChecked =
            form.measurementNotTaken == Just True
    in
    [ div [ class "ui form head-circumference" ] <|
        [ viewLabel language <| Translate.NutritionAssessmentTask TaskHeadCircumference
        , p [ class "activity-helper" ] [ text <| translate language Translate.HeadCircumferenceHelper ]
        ]
            ++ inputSection
    , div
        [ class "ui checkbox activity"
        , onClick ToggleHeadCircumferenceNotTaken
        ]
        [ input
            [ type_ "checkbox"
            , checked measurementNotTakenChecked
            , classList [ ( "checked", measurementNotTakenChecked ) ]
            ]
            []
        , label [] [ text <| translate language Translate.HeadCircumferenceNotTakenLabel ]
        ]
    ]


viewVaccinationHistoryForm : Language -> NominalDate -> Bool -> AssembledData -> VaccinationHistoryForm -> List (Html Msg)
viewVaccinationHistoryForm language currentDate isChw assembled vaccinationHistoryForm =
    let
        form =
            assembled.measurements.vaccinationHistory
                |> getMeasurementValueFunc
                |> vaccinationHistoryFormWithDefault vaccinationHistoryForm

        ( vaccinesCompleted, vaccinesToProcess ) =
            Dict.toList allVaccinesWithDoses
                |> List.filterMap (filterExpextedDosesForPerson currentDate assembled.person)
                |> List.partition isVaccineCompleted

        vaccinesForView =
            List.map
                (\( vaccineType, doses ) ->
                    List.filterMap
                        (\dose ->
                            if isJust <| wasDoseCompleted ( vaccineType, dose ) then
                                Just ( vaccineType, dose )

                            else
                                Nothing
                        )
                        doses
                )
                vaccinesCompleted
                |> List.concat

        vaccineToProcess =
            List.head vaccinesToProcess
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        isVaccineCompleted ( vaccineType, doses ) =
            List.all
                (\dose ->
                    (wasVaccineAdministered vaccineType dose == Just True)
                        && wasDateSet vaccineType dose
                )
                doses
                || List.any
                    (\dose -> wasVaccineAdministered vaccineType dose == Just False)
                    doses

        ( dosesCompleted, dosesToProcess ) =
            vaccineToProcess
                |> List.map
                    (\( vaccineType, doses ) ->
                        List.map (\dose -> ( vaccineType, dose )) doses
                    )
                |> List.concat
                |> List.partition wasDoseProcessed

        doseToProcess =
            List.head dosesToProcess
                |> Maybe.map List.singleton
                |> Maybe.withDefault []

        wasDoseProcessed ( vaccineType, dose ) =
            wasDoseCompleted ( vaccineType, dose )
                |> Maybe.withDefault False

        wasDoseCompleted ( vaccineType, dose ) =
            wasVaccineAdministered vaccineType dose
                |> Maybe.andThen
                    (\wasAdministered ->
                        if wasAdministered then
                            Just <| wasDateSet vaccineType dose

                        else
                            Just True
                    )

        wasVaccineAdministered vaccineType dose =
            Dict.get vaccineType form.administeredVaccines
                |> Maybe.andThen (Dict.get dose)
                |> Maybe.withDefault
                    (if wasVaccineSuggested vaccineType dose then
                        Just False

                     else
                        Nothing
                    )

        wasVaccineSuggested vaccineType dose =
            Dict.get vaccineType form.suggestedVaccines
                |> Maybe.map (EverySet.member dose)
                |> Maybe.withDefault False

        wasDateSet vaccineType dose =
            Dict.get vaccineType form.vaccinationDates
                |> Maybe.andThen (Dict.get dose)
                |> Maybe.map isJust
                |> Maybe.withDefault False

        suggested =
            vaccinesForView
                ++ dosesCompleted
                ++ doseToProcess

        totalItemsforView =
            List.length suggested

        birthDate =
            assembled.person.birthDate
                -- We should not get here, because child with no birth date
                -- would not be allowed to participate in encounter.
                |> Maybe.withDefault (Date.add Months -6 currentDate)

        itemsForView =
            List.indexedMap
                (\index ( vaccineType, dose ) ->
                    let
                        startDate =
                            vacinationDateByPreviousDoseDate previousDoseDate birthDate ( vaccineType, dose )

                        previousDoseDate =
                            getPreviousVaccineDose dose
                                |> Maybe.andThen
                                    (\previouseDose ->
                                        let
                                            dateRecorded =
                                                Dict.get vaccineType form.vaccinationDates
                                                    |> Maybe.andThen (Dict.get previouseDose)
                                                    |> Maybe.Extra.join

                                            dateFromHistory =
                                                Dict.get vaccineType assembled.vaccinationHistory
                                                    |> Maybe.andThen (Dict.get previouseDose)
                                        in
                                        Maybe.Extra.or dateRecorded dateFromHistory
                                    )
                    in
                    { type_ = vaccineType
                    , dose = dose

                    -- The date from which this vaccine dose can be administered.
                    , startDate = startDate

                    -- Only last 2 questions will have active inputs
                    , active = index + 2 >= totalItemsforView
                    }
                )
                suggested

        catchUpRequired =
            form.catchUpRequired == Just True

        catchUpInputsAndTasks =
            if not catchUpRequired then
                []

            else
                itemsForView
                    |> List.map
                        (\item ->
                            let
                                type_ =
                                    item.type_

                                dose =
                                    item.dose

                                setBoolInputAction =
                                    if item.active then
                                        SetVaccinationHistoryBoolInput type_ dose

                                    else
                                        always NoOp

                                vaccineAdministered =
                                    wasVaccineAdministered type_ dose

                                ( derrivedInput, derrivedTask ) =
                                    if vaccineAdministered == Just True then
                                        let
                                            selectorState =
                                                Dict.get ( type_, dose ) form.dateSelectorsState
                                                    |> Maybe.withDefault False

                                            vaccinationDate =
                                                Dict.get type_ form.vaccinationDates
                                                    |> Maybe.andThen (Dict.get dose)
                                                    |> Maybe.withDefault Nothing

                                            ( setDateAction, toggleAction ) =
                                                if item.active then
                                                    ( SetVaccinationHistoryDateInput type_ dose
                                                    , ToggleVaccinationHistoryDateSelectorInput type_ dose
                                                    )

                                                else
                                                    ( always NoOp, NoOp )
                                        in
                                        ( [ div [ class "form-input date previous" ]
                                                [ viewLabel language Translate.SelectDate
                                                , DateSelector.SelectorDropdown.view
                                                    toggleAction
                                                    setDateAction
                                                    selectorState
                                                    item.startDate
                                                    (Date.add Days -1 currentDate)
                                                    vaccinationDate
                                                ]
                                          ]
                                        , Just vaccinationDate
                                        )

                                    else
                                        ( [], Nothing )
                            in
                            ( [ viewQuestionLabel language <| Translate.VaccineDoseAdministeredQuestion type_ dose False False
                              , viewBoolInput
                                    language
                                    vaccineAdministered
                                    setBoolInputAction
                                    ""
                                    Nothing
                              ]
                                ++ derrivedInput
                            , { boolTask = vaccineAdministered
                              , dateTask = derrivedTask
                              }
                            )
                        )

        catchUpInputs =
            List.map Tuple.first catchUpInputsAndTasks
                |> List.concat

        catchUpTasks =
            List.map Tuple.second catchUpInputsAndTasks

        boolTasks =
            List.map .boolTask catchUpTasks

        dateTasks =
            List.filterMap .dateTask catchUpTasks

        ( tasksCompleted, totalTasks ) =
            ( taskCompleted form.catchUpRequired
                + (List.map taskCompleted boolTasks |> List.sum)
                + (List.map taskCompleted dateTasks |> List.sum)
            , 1
                + List.length boolTasks
                + List.length dateTasks
            )

        suggestedVaccines =
            if not catchUpRequired then
                Dict.empty

            else
                List.foldl
                    (\( vaccineType, dose ) accum ->
                        let
                            updated =
                                Dict.get vaccineType accum
                                    |> Maybe.map (EverySet.insert dose)
                                    |> Maybe.withDefault (EverySet.singleton dose)
                        in
                        Dict.insert vaccineType updated accum
                    )
                    Dict.empty
                    suggested

        disabled =
            tasksCompleted /= totalTasks
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form vaccination-history" ] <|
                [ viewLabel language Translate.ReviewVaccinationHistoryLabel
                , button
                    [ classList
                        [ ( "ui primary button review-history", True )
                        , ( "disabled", catchUpRequired )
                        ]
                    , onClick <| SetWarningPopupState <| Just <| PopupVaccinationHistory assembled.vaccinationHistory
                    ]
                    [ text <| translate language Translate.ReviewVaccinationHistory ]
                , viewQuestionLabel language Translate.VaccinationCatchUpRequiredQuestion
                , viewBoolInput
                    language
                    form.catchUpRequired
                    SetCatchUpRequired
                    ""
                    Nothing
                ]
                    ++ catchUpInputs
            ]
        , viewAction language (SaveVaccinationHistory assembled.participant.person suggestedVaccines assembled.measurements.vaccinationHistory) disabled
        ]
    ]


vacinationDateByPreviousDoseDate : Maybe NominalDate -> NominalDate -> ( VaccineType, VaccineDose ) -> NominalDate
vacinationDateByPreviousDoseDate previousDoseDate birthDate ( vaccineType, dose ) =
    Maybe.map
        (\previousDate ->
            let
                ( interval, unit ) =
                    getIntervalForVaccine vaccineType
            in
            -- Allow grace of one unit.
            Date.add unit (interval - 1) previousDate
        )
        previousDoseDate
        |> Maybe.withDefault (initialVaccinationDateByBirthDate birthDate ( vaccineType, dose ))


viewImmunisationForm : Language -> NominalDate -> Bool -> AssembledData -> ImmunisationForm -> List (Html Msg)
viewImmunisationForm language currentDate isChw assembled immunisationForm =
    let
        form =
            assembled.measurements.immunisation
                |> getMeasurementValueFunc
                |> immunisationFormWithDefault immunisationForm

        ( tasksCompleted, totalTasks ) =
            ( (List.map taskCompleted vaccinationAdministeredTasks |> List.sum)
                + (List.map taskCompleted vaccinationDateTasks |> List.sum)
                + (List.map taskCompleted vaccinationNoteTasks |> List.sum)
            , List.length vaccinationAdministeredTasks
                + List.length vaccinationDateTasks
                + List.length vaccinationNoteTasks
            )

        suggestedVaccines =
            generateSuggestedVaccinations currentDate isChw assembled

        inputsAndTasks =
            List.map (inputsAndTasksForSuggestedVaccine language currentDate isChw assembled form) suggestedVaccines

        inputs =
            List.map Tuple.first inputsAndTasks
                |> List.concat

        tasks =
            List.map Tuple.second inputsAndTasks

        vaccinationAdministeredTasks =
            List.map .vaccinationAdministeredTask tasks

        vaccinationDateTasks =
            List.filterMap .vaccinationDateTask tasks

        vaccinationNoteTasks =
            List.filterMap .vaccinationNoteTask tasks

        futureVaccines =
            List.filterMap
                (\( vaccineType, dose ) ->
                    if doseAdministrationQuestionAnswered vaccineType form then
                        case getVaccinationDateFromImmunisationForm vaccineType form of
                            Just administationDate ->
                                -- If the date was set, we show the date for the next dose.
                                Just ( vaccineType, nextVaccinationDataForVaccine administationDate dose vaccineType )

                            Nothing ->
                                -- Otherwise, we pull last dose administration date from vaccination history.
                                Dict.get vaccineType assembled.vaccinationHistory
                                    |> Maybe.andThen
                                        (Dict.toList
                                            >> List.sortBy (Tuple.first >> vaccineDoseToComparable)
                                            >> List.reverse
                                            >> List.head
                                            >> Maybe.map Tuple.second
                                        )
                                    |> Maybe.map
                                        (\lastAdministationDate ->
                                            Just ( vaccineType, nextVaccinationDataForVaccine lastAdministationDate dose vaccineType )
                                        )
                                    |> Maybe.withDefault (Just ( vaccineType, Just ( VaccineDoseFirst, currentDate ) ))

                    else
                        -- Question(s) at form are not yet completed, so we do not show an entry.
                        Nothing
                )
                -- We only show next doses due date for vaccines offered today.
                suggestedVaccines
                |> List.map viewNextDoseForVaccine

        viewNextDoseForVaccine ( vaccineType, nextVaccinationData ) =
            let
                dueDate =
                    Maybe.map (Tuple.second >> formatDDMMyyyy) nextVaccinationData
                        |> Maybe.withDefault (translate language Translate.Done)
            in
            div [ class "next-vaccination" ]
                [ div [ class "name" ] [ text <| translate language <| Translate.VaccineType vaccineType ]
                , div [ class "due-date" ] [ text dueDate ]
                ]

        disabled =
            tasksCompleted /= totalTasks
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form immunisation" ]
                inputs
            , div [ class "future-vaccinations" ] <|
                viewLabel language Translate.NextDoseDue
                    :: futureVaccines
            ]
        , viewAction language (SaveImmunisation assembled.participant.person (Dict.fromList suggestedVaccines) assembled.measurements.immunisation) disabled
        ]
    ]


type alias SuggestedVaccineTasks =
    { vaccinationAdministeredTask : Maybe Bool
    , vaccinationDateTask : Maybe (Maybe NominalDate)
    , vaccinationNoteTask : Maybe (Maybe AdministrationNote)
    }


inputsAndTasksForSuggestedVaccine : Language -> NominalDate -> Bool -> AssembledData -> ImmunisationForm -> ( VaccineType, VaccineDose ) -> ( List (Html Msg), SuggestedVaccineTasks )
inputsAndTasksForSuggestedVaccine language currentDate isChw assembled form ( vaccineType, dose ) =
    let
        ( vaccinationAdministeredInput, vaccinationAdministeredTask ) =
            ( [ viewQuestionLabel language <| Translate.VaccineDoseAdministeredQuestion vaccineType dose isChw True
              , viewBoolInput
                    language
                    (config.getVaccinationAdministeredFunc form)
                    (SetImmunisationBoolInput config.setBoolInputFunc)
                    ""
                    Nothing
              ]
            , config.getVaccinationAdministeredFunc form
            )

        birthDate =
            assembled.person.birthDate
                -- We should not get here, because child with no birth date
                -- would not be allowed to participate in encounter.
                |> Maybe.withDefault (Date.add Months -6 currentDate)

        ( vaccinationDerrivedInputs, vaccinationDateTask, vaccinationNoteTask ) =
            if config.getVaccinationAdministeredFunc form == Just False then
                let
                    ( vaccinationDateInput, dateTask ) =
                        if config.getVaccinationNoteFunc form == Just AdministeredPreviously then
                            let
                                startDate =
                                    vacinationDateByPreviousDoseDate previousDoseDate birthDate ( vaccineType, dose )

                                previousDoseDate =
                                    getPreviousVaccineDose dose
                                        |> Maybe.andThen
                                            (\previouseDose ->
                                                Dict.get vaccineType assembled.vaccinationHistory
                                                    |> Maybe.andThen (Dict.get previouseDose)
                                            )
                            in
                            ( [ div [ class "form-input date previous" ]
                                    [ viewLabel language Translate.SelectDate
                                    , DateSelector.SelectorDropdown.view
                                        (ToggleImmunisationDateSelectorInput config.toggleDateSelectorFunc)
                                        (SetImmunisationDateInput config.setDateInputFunc)
                                        (config.getVaccinationDateSelectorOpenFunc form)
                                        startDate
                                        (Date.add Days -1 currentDate)
                                        (config.getVaccinationDateFunc form)
                                    ]
                              ]
                            , Just <| config.getVaccinationDateFunc form
                            )

                        else
                            ( [], Nothing )

                    ( leftOptions, rightOptions ) =
                        if isChw then
                            ( [ AdministeredPreviously, NonAdministrationLackOfStock, NonAdministrationPatientDeclined, NonAdministrationKnownAllergy ]
                            , [ NonAdministrationPatientUnableToAfford, NonAdministrationHomeBirth, NonAdministrationOther ]
                            )

                        else
                            ( [ AdministeredPreviously, NonAdministrationLackOfStock, NonAdministrationPatientDeclined ]
                            , [ NonAdministrationKnownAllergy, NonAdministrationPatientUnableToAfford, NonAdministrationOther ]
                            )
                in
                ( [ div [ class "why-not" ]
                        [ viewQuestionLabel language Translate.WhyNot
                        , viewCheckBoxSelectInput language
                            leftOptions
                            rightOptions
                            (config.getVaccinationNoteFunc form)
                            (SetImmunisationAdministrationNoteInput config.setAdministrationNoteFunc)
                            Translate.AdministrationNote
                        ]
                  ]
                    ++ vaccinationDateInput
                , dateTask
                , Just <| config.getVaccinationNoteFunc form
                )

            else
                ( [], Nothing, Nothing )

        config =
            case vaccineType of
                VaccineBCG ->
                    { setBoolInputFunc =
                        \value form_ ->
                            let
                                ( vaccinationDate, vaccinationNote ) =
                                    if value == True then
                                        ( if isChw then
                                            assembled.person.birthDate
                                                |> Maybe.withDefault currentDate
                                                |> Just

                                          else
                                            Just currentDate
                                        , Just AdministeredToday
                                        )

                                    else
                                        ( Nothing, Nothing )
                            in
                            { form_
                                | bcgVaccinationAdministered = Just value
                                , bcgVaccinationNote = vaccinationNote
                                , bcgVaccinationDate = vaccinationDate
                            }
                    , toggleDateSelectorFunc = \form_ -> { form_ | bcgVaccinationDateSelectorOpen = not form_.bcgVaccinationDateSelectorOpen }
                    , setDateInputFunc = \value form_ -> { form_ | bcgVaccinationDate = Just value }
                    , setAdministrationNoteFunc = \value form_ -> { form_ | bcgVaccinationNote = Just value, bcgVaccinationDate = Nothing }
                    , getVaccinationAdministeredFunc = .bcgVaccinationAdministered
                    , getVaccinationDateFunc = .bcgVaccinationDate
                    , getVaccinationNoteFunc = .bcgVaccinationNote
                    , getVaccinationDateSelectorOpenFunc = .bcgVaccinationDateSelectorOpen
                    }

                VaccineOPV ->
                    { setBoolInputFunc =
                        \value form_ ->
                            let
                                ( vaccinationDate, vaccinationNote ) =
                                    if value == True then
                                        ( if isChw then
                                            assembled.person.birthDate
                                                |> Maybe.withDefault currentDate
                                                |> Just

                                          else
                                            Just currentDate
                                        , Just AdministeredToday
                                        )

                                    else
                                        ( Nothing, Nothing )
                            in
                            { form_
                                | opvVaccinationAdministered = Just value
                                , opvVaccinationNote = vaccinationNote
                                , opvVaccinationDate = vaccinationDate
                            }
                    , toggleDateSelectorFunc = \form_ -> { form_ | opvVaccinationDateSelectorOpen = not form_.opvVaccinationDateSelectorOpen }
                    , setDateInputFunc = \value form_ -> { form_ | opvVaccinationDate = Just value }
                    , setAdministrationNoteFunc = \value form_ -> { form_ | opvVaccinationNote = Just value, opvVaccinationDate = Nothing }
                    , getVaccinationAdministeredFunc = .opvVaccinationAdministered
                    , getVaccinationDateFunc = .opvVaccinationDate
                    , getVaccinationNoteFunc = .opvVaccinationNote
                    , getVaccinationDateSelectorOpenFunc = .opvVaccinationDateSelectorOpen
                    }

                VaccineDTP ->
                    { setBoolInputFunc =
                        \value form_ ->
                            let
                                ( vaccinationDate, vaccinationNote ) =
                                    if value == True then
                                        ( Just currentDate, Just AdministeredToday )

                                    else
                                        ( Nothing, Nothing )
                            in
                            { form_
                                | dtpVaccinationAdministered = Just value
                                , dtpVaccinationNote = vaccinationNote
                                , dtpVaccinationDate = vaccinationDate
                            }
                    , toggleDateSelectorFunc = \form_ -> { form_ | dtpVaccinationDateSelectorOpen = not form_.dtpVaccinationDateSelectorOpen }
                    , setDateInputFunc = \value form_ -> { form_ | dtpVaccinationDate = Just value }
                    , setAdministrationNoteFunc = \value form_ -> { form_ | dtpVaccinationNote = Just value, dtpVaccinationDate = Nothing }
                    , getVaccinationAdministeredFunc = .dtpVaccinationAdministered
                    , getVaccinationDateFunc = .dtpVaccinationDate
                    , getVaccinationNoteFunc = .dtpVaccinationNote
                    , getVaccinationDateSelectorOpenFunc = .dtpVaccinationDateSelectorOpen
                    }

                VaccinePCV13 ->
                    { setBoolInputFunc =
                        \value form_ ->
                            let
                                ( vaccinationDate, vaccinationNote ) =
                                    if value == True then
                                        ( Just currentDate, Just AdministeredToday )

                                    else
                                        ( Nothing, Nothing )
                            in
                            { form_
                                | pcv13VaccinationAdministered = Just value
                                , pcv13VaccinationNote = vaccinationNote
                                , pcv13VaccinationDate = vaccinationDate
                            }
                    , toggleDateSelectorFunc = \form_ -> { form_ | pcv13VaccinationDateSelectorOpen = not form_.pcv13VaccinationDateSelectorOpen }
                    , setDateInputFunc = \value form_ -> { form_ | pcv13VaccinationDate = Just value }
                    , setAdministrationNoteFunc = \value form_ -> { form_ | pcv13VaccinationNote = Just value, pcv13VaccinationDate = Nothing }
                    , getVaccinationAdministeredFunc = .pcv13VaccinationAdministered
                    , getVaccinationDateFunc = .pcv13VaccinationDate
                    , getVaccinationNoteFunc = .pcv13VaccinationNote
                    , getVaccinationDateSelectorOpenFunc = .pcv13VaccinationDateSelectorOpen
                    }

                VaccineRotarix ->
                    { setBoolInputFunc =
                        \value form_ ->
                            let
                                ( vaccinationDate, vaccinationNote ) =
                                    if value == True then
                                        ( Just currentDate, Just AdministeredToday )

                                    else
                                        ( Nothing, Nothing )
                            in
                            { form_
                                | rotarixVaccinationAdministered = Just value
                                , rotarixVaccinationNote = vaccinationNote
                                , rotarixVaccinationDate = vaccinationDate
                            }
                    , toggleDateSelectorFunc = \form_ -> { form_ | rotarixVaccinationDateSelectorOpen = not form_.rotarixVaccinationDateSelectorOpen }
                    , setDateInputFunc = \value form_ -> { form_ | rotarixVaccinationDate = Just value }
                    , setAdministrationNoteFunc = \value form_ -> { form_ | rotarixVaccinationNote = Just value, rotarixVaccinationDate = Nothing }
                    , getVaccinationAdministeredFunc = .rotarixVaccinationAdministered
                    , getVaccinationDateFunc = .rotarixVaccinationDate
                    , getVaccinationNoteFunc = .rotarixVaccinationNote
                    , getVaccinationDateSelectorOpenFunc = .rotarixVaccinationDateSelectorOpen
                    }

                VaccineIPV ->
                    { setBoolInputFunc =
                        \value form_ ->
                            let
                                ( vaccinationDate, vaccinationNote ) =
                                    if value == True then
                                        ( Just currentDate, Just AdministeredToday )

                                    else
                                        ( Nothing, Nothing )
                            in
                            { form_
                                | ipvVaccinationAdministered = Just value
                                , ipvVaccinationNote = vaccinationNote
                                , ipvVaccinationDate = vaccinationDate
                            }
                    , toggleDateSelectorFunc = \form_ -> { form_ | ipvVaccinationDateSelectorOpen = not form_.ipvVaccinationDateSelectorOpen }
                    , setDateInputFunc = \value form_ -> { form_ | ipvVaccinationDate = Just value }
                    , setAdministrationNoteFunc = \value form_ -> { form_ | ipvVaccinationNote = Just value, ipvVaccinationDate = Nothing }
                    , getVaccinationAdministeredFunc = .ipvVaccinationAdministered
                    , getVaccinationDateFunc = .ipvVaccinationDate
                    , getVaccinationNoteFunc = .ipvVaccinationNote
                    , getVaccinationDateSelectorOpenFunc = .ipvVaccinationDateSelectorOpen
                    }

                VaccineMR ->
                    { setBoolInputFunc =
                        \value form_ ->
                            let
                                ( vaccinationDate, vaccinationNote ) =
                                    if value == True then
                                        ( Just currentDate, Just AdministeredToday )

                                    else
                                        ( Nothing, Nothing )
                            in
                            { form_
                                | mrVaccinationAdministered = Just value
                                , mrVaccinationNote = vaccinationNote
                                , mrVaccinationDate = vaccinationDate
                            }
                    , toggleDateSelectorFunc = \form_ -> { form_ | mrVaccinationDateSelectorOpen = not form_.mrVaccinationDateSelectorOpen }
                    , setDateInputFunc = \value form_ -> { form_ | mrVaccinationDate = Just value }
                    , setAdministrationNoteFunc = \value form_ -> { form_ | mrVaccinationNote = Just value, mrVaccinationDate = Nothing }
                    , getVaccinationAdministeredFunc = .mrVaccinationAdministered
                    , getVaccinationDateFunc = .mrVaccinationDate
                    , getVaccinationNoteFunc = .mrVaccinationNote
                    , getVaccinationDateSelectorOpenFunc = .mrVaccinationDateSelectorOpen
                    }

                VaccineHPV ->
                    { setBoolInputFunc =
                        \value form_ ->
                            let
                                ( vaccinationDate, vaccinationNote ) =
                                    if value == True then
                                        ( Just currentDate, Just AdministeredToday )

                                    else
                                        ( Nothing, Nothing )
                            in
                            { form_
                                | hpvVaccinationAdministered = Just value
                                , hpvVaccinationNote = vaccinationNote
                                , hpvVaccinationDate = vaccinationDate
                            }
                    , toggleDateSelectorFunc = \form_ -> { form_ | hpvVaccinationDateSelectorOpen = not form_.hpvVaccinationDateSelectorOpen }
                    , setDateInputFunc = \value form_ -> { form_ | hpvVaccinationDate = Just value }
                    , setAdministrationNoteFunc = \value form_ -> { form_ | hpvVaccinationNote = Just value, hpvVaccinationDate = Nothing }
                    , getVaccinationAdministeredFunc = .hpvVaccinationAdministered
                    , getVaccinationDateFunc = .hpvVaccinationDate
                    , getVaccinationNoteFunc = .hpvVaccinationNote
                    , getVaccinationDateSelectorOpenFunc = .hpvVaccinationDateSelectorOpen
                    }
    in
    ( vaccinationAdministeredInput ++ vaccinationDerrivedInputs
    , SuggestedVaccineTasks vaccinationAdministeredTask vaccinationDateTask vaccinationNoteTask
    )


viewECDForm : Language -> NominalDate -> AssembledData -> WellChildECDForm -> List (Html Msg)
viewECDForm language currentDate assembled ecdForm =
    let
        totalTasks =
            List.length tasks

        tasksCompleted =
            List.map taskCompleted tasks
                |> List.sum

        ( inputs, tasks ) =
            ecdFormInputsAndTasks language currentDate assembled ecdForm

        disabled =
            tasksCompleted /= totalTasks
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form ecd" ]
                inputs
            ]
        , viewAction language (SaveECD assembled.participant.person assembled.measurements.ecd) disabled
        ]
    ]


ecdFormInputsAndTasks : Language -> NominalDate -> AssembledData -> WellChildECDForm -> ( List (Html Msg), List (Maybe Bool) )
ecdFormInputsAndTasks language currentDate assembled ecdForm =
    let
        form =
            assembled.measurements.ecd
                |> getMeasurementValueFunc
                |> wellChildECDFormWithDefault ecdForm

        persentedECDSignsData =
            generateRemianingECDSignsBeforeCurrentEncounter currentDate assembled
                |> List.map inputAndTaskForSign

        inputAndTaskForSign sign =
            case sign of
                FollowMothersEyes ->
                    let
                        followMothersEyesUpdateFunc value form_ =
                            { form_ | followMothersEyes = Just value }
                    in
                    ( viewECDInput FollowMothersEyes form.followMothersEyes followMothersEyesUpdateFunc
                    , form.followMothersEyes
                    )

                MoveArmsAndLegs ->
                    let
                        moveArmsAndLegsUpdateFunc value form_ =
                            { form_ | moveArmsAndLegs = Just value }
                    in
                    ( viewECDInput MoveArmsAndLegs form.moveArmsAndLegs moveArmsAndLegsUpdateFunc
                    , form.moveArmsAndLegs
                    )

                RaiseHandsUp ->
                    let
                        raiseHandsUpUpdateFunc value form_ =
                            { form_ | raiseHandsUp = Just value }
                    in
                    ( viewECDInput RaiseHandsUp form.raiseHandsUp raiseHandsUpUpdateFunc
                    , form.raiseHandsUp
                    )

                Smile ->
                    let
                        smileUpdateFunc value form_ =
                            { form_ | smile = Just value }
                    in
                    ( viewECDInput Smile form.smile smileUpdateFunc
                    , form.smile
                    )

                RollSideways ->
                    let
                        rollSidewaysUpdateFunc value form_ =
                            { form_ | rollSideways = Just value }
                    in
                    ( viewECDInput RollSideways form.rollSideways rollSidewaysUpdateFunc
                    , form.rollSideways
                    )

                BringHandsToMouth ->
                    let
                        bringHandsToMouthUpdateFunc value form_ =
                            { form_ | bringHandsToMouth = Just value }
                    in
                    ( viewECDInput BringHandsToMouth form.bringHandsToMouth bringHandsToMouthUpdateFunc
                    , form.bringHandsToMouth
                    )

                HoldHeadWithoutSupport ->
                    let
                        holdHeadWithoutSupportUpdateFunc value form_ =
                            { form_ | holdHeadWithoutSupport = Just value }
                    in
                    ( viewECDInput HoldHeadWithoutSupport form.holdHeadWithoutSupport holdHeadWithoutSupportUpdateFunc
                    , form.holdHeadWithoutSupport
                    )

                HoldAndShakeToys ->
                    let
                        holdAndShakeToysUpdateFunc value form_ =
                            { form_ | holdAndShakeToys = Just value }
                    in
                    ( viewECDInput HoldAndShakeToys form.holdAndShakeToys holdAndShakeToysUpdateFunc
                    , form.holdAndShakeToys
                    )

                ReactToSuddenSounds ->
                    let
                        reactToSuddenSoundsUpdateFunc value form_ =
                            { form_ | reactToSuddenSounds = Just value }
                    in
                    ( viewECDInput ReactToSuddenSounds form.reactToSuddenSounds reactToSuddenSoundsUpdateFunc
                    , form.reactToSuddenSounds
                    )

                UseConsonantSounds ->
                    let
                        useConsonantSoundsUpdateFunc value form_ =
                            { form_ | useConsonantSounds = Just value }
                    in
                    ( viewECDInput UseConsonantSounds form.useConsonantSounds useConsonantSoundsUpdateFunc
                    , form.useConsonantSounds
                    )

                RespondToSoundWithSound ->
                    let
                        respondToSoundWithSoundUpdateFunc value form_ =
                            { form_ | respondToSoundWithSound = Just value }
                    in
                    ( viewECDInput RespondToSoundWithSound form.respondToSoundWithSound respondToSoundWithSoundUpdateFunc
                    , form.respondToSoundWithSound
                    )

                TurnHeadWhenCalled ->
                    let
                        turnHeadWhenCalledUpdateFunc value form_ =
                            { form_ | turnHeadWhenCalled = Just value }
                    in
                    ( viewECDInput TurnHeadWhenCalled form.turnHeadWhenCalled turnHeadWhenCalledUpdateFunc
                    , form.turnHeadWhenCalled
                    )

                SitWithoutSupport ->
                    let
                        sitWithoutSupportUpdateFunc value form_ =
                            { form_ | sitWithoutSupport = Just value }
                    in
                    ( viewECDInput SitWithoutSupport form.sitWithoutSupport sitWithoutSupportUpdateFunc
                    , form.sitWithoutSupport
                    )

                SmileBack ->
                    let
                        smileBackUpdateFunc value form_ =
                            { form_ | smileBack = Just value }
                    in
                    ( viewECDInput SmileBack form.smileBack smileBackUpdateFunc
                    , form.smileBack
                    )

                RollTummyToBack ->
                    let
                        rollTummyToBackUpdateFunc value form_ =
                            { form_ | rollTummyToBack = Just value }
                    in
                    ( viewECDInput RollTummyToBack form.rollTummyToBack rollTummyToBackUpdateFunc
                    , form.rollTummyToBack
                    )

                ReachForToys ->
                    let
                        reachForToysUpdateFunc value form_ =
                            { form_ | reachForToys = Just value }
                    in
                    ( viewECDInput ReachForToys form.reachForToys reachForToysUpdateFunc
                    , form.reachForToys
                    )

                UseSimpleGestures ->
                    let
                        useSimpleGesturesUpdateFunc value form_ =
                            { form_ | useSimpleGestures = Just value }
                    in
                    ( viewECDInput UseSimpleGestures form.useSimpleGestures useSimpleGesturesUpdateFunc
                    , form.useSimpleGestures
                    )

                StandOnTheirOwn ->
                    let
                        standOnTheirOwnUpdateFunc value form_ =
                            { form_ | standOnTheirOwn = Just value }
                    in
                    ( viewECDInput StandOnTheirOwn form.standOnTheirOwn standOnTheirOwnUpdateFunc
                    , form.standOnTheirOwn
                    )

                CopyDuringPlay ->
                    let
                        copyDuringPlayUpdateFunc value form_ =
                            { form_ | copyDuringPlay = Just value }
                    in
                    ( viewECDInput CopyDuringPlay form.copyDuringPlay copyDuringPlayUpdateFunc
                    , form.copyDuringPlay
                    )

                SayMamaDada ->
                    let
                        sayMamaDadaUpdateFunc value form_ =
                            { form_ | sayMamaDada = Just value }
                    in
                    ( viewECDInput SayMamaDada form.sayMamaDada sayMamaDadaUpdateFunc
                    , form.sayMamaDada
                    )

                CanHoldSmallObjects ->
                    let
                        canHoldSmallObjectsUpdateFunc value form_ =
                            { form_ | canHoldSmallObjects = Just value }
                    in
                    ( viewECDInput CanHoldSmallObjects form.canHoldSmallObjects canHoldSmallObjectsUpdateFunc
                    , form.canHoldSmallObjects
                    )

                LooksWhenPointedAt ->
                    let
                        looksWhenPointedAtUpdateFunc value form_ =
                            { form_ | looksWhenPointedAt = Just value }
                    in
                    ( viewECDInput LooksWhenPointedAt form.looksWhenPointedAt looksWhenPointedAtUpdateFunc
                    , form.looksWhenPointedAt
                    )

                UseSingleWords ->
                    let
                        useSingleWordsUpdateFunc value form_ =
                            { form_ | useSingleWords = Just value }
                    in
                    ( viewECDInput UseSingleWords form.useSingleWords useSingleWordsUpdateFunc
                    , form.useSingleWords
                    )

                WalkWithoutHelp ->
                    let
                        walkWithoutHelpUpdateFunc value form_ =
                            { form_ | walkWithoutHelp = Just value }
                    in
                    ( viewECDInput WalkWithoutHelp form.walkWithoutHelp walkWithoutHelpUpdateFunc
                    , form.walkWithoutHelp
                    )

                PlayPretend ->
                    let
                        playPretendUpdateFunc value form_ =
                            { form_ | playPretend = Just value }
                    in
                    ( viewECDInput PlayPretend form.playPretend playPretendUpdateFunc
                    , form.playPretend
                    )

                PointToThingsOfInterest ->
                    let
                        pointToThingsOfInterestUpdateFunc value form_ =
                            { form_ | pointToThingsOfInterest = Just value }
                    in
                    ( viewECDInput PointToThingsOfInterest form.pointToThingsOfInterest pointToThingsOfInterestUpdateFunc
                    , form.pointToThingsOfInterest
                    )

                UseShortPhrases ->
                    let
                        useShortPhrasesUpdateFunc value form_ =
                            { form_ | useShortPhrases = Just value }
                    in
                    ( viewECDInput UseShortPhrases form.useShortPhrases useShortPhrasesUpdateFunc
                    , form.useShortPhrases
                    )

                InterestedInOtherChildren ->
                    let
                        interestedInOtherChildrenUpdateFunc value form_ =
                            { form_ | interestedInOtherChildren = Just value }
                    in
                    ( viewECDInput InterestedInOtherChildren form.interestedInOtherChildren interestedInOtherChildrenUpdateFunc
                    , form.interestedInOtherChildren
                    )

                FollowSimpleInstructions ->
                    let
                        followSimlpeInstructionsUpdateFunc value form_ =
                            { form_ | followSimlpeInstructions = Just value }
                    in
                    ( viewECDInput FollowSimpleInstructions form.followSimlpeInstructions followSimlpeInstructionsUpdateFunc
                    , form.followSimlpeInstructions
                    )

                KickBall ->
                    let
                        kickBallUpdateFunc value form_ =
                            { form_ | kickBall = Just value }
                    in
                    ( viewECDInput KickBall form.kickBall kickBallUpdateFunc
                    , form.kickBall
                    )

                PointAtNamedObjects ->
                    let
                        pointAtNamedObjectsUpdateFunc value form_ =
                            { form_ | pointAtNamedObjects = Just value }
                    in
                    ( viewECDInput PointAtNamedObjects form.pointAtNamedObjects pointAtNamedObjectsUpdateFunc
                    , form.pointAtNamedObjects
                    )

                DressThemselves ->
                    let
                        dressThemselvesUpdateFunc value form_ =
                            { form_ | dressThemselves = Just value }
                    in
                    ( viewECDInput DressThemselves form.dressThemselves dressThemselvesUpdateFunc
                    , form.dressThemselves
                    )

                WashHandsGoToToiled ->
                    let
                        washHandsGoToToiledUpdateFunc value form_ =
                            { form_ | washHandsGoToToiled = Just value }
                    in
                    ( viewECDInput WashHandsGoToToiled form.washHandsGoToToiled washHandsGoToToiledUpdateFunc
                    , form.washHandsGoToToiled
                    )

                KnowsColorsAndNumbers ->
                    let
                        knowsColorsAndNumbersUpdateFunc value form_ =
                            { form_ | knowsColorsAndNumbers = Just value }
                    in
                    ( viewECDInput KnowsColorsAndNumbers form.knowsColorsAndNumbers knowsColorsAndNumbersUpdateFunc
                    , form.knowsColorsAndNumbers
                    )

                UseMediumPhrases ->
                    let
                        useMediumPhrasesUpdateFunc value form_ =
                            { form_ | useMediumPhrases = Just value }
                    in
                    ( viewECDInput UseMediumPhrases form.useMediumPhrases useMediumPhrasesUpdateFunc
                    , form.useMediumPhrases
                    )

                PlayMakeBelieve ->
                    let
                        playMakeBelieveUpdateFunc value form_ =
                            { form_ | playMakeBelieve = Just value }
                    in
                    ( viewECDInput PlayMakeBelieve form.playMakeBelieve playMakeBelieveUpdateFunc
                    , form.playMakeBelieve
                    )

                FollowThreeStepInstructions ->
                    let
                        followThreeStepInstructionsUpdateFunc value form_ =
                            { form_ | followThreeStepInstructions = Just value }
                    in
                    ( viewECDInput FollowThreeStepInstructions form.followThreeStepInstructions followThreeStepInstructionsUpdateFunc
                    , form.followThreeStepInstructions
                    )

                StandOnOneFootFiveSeconds ->
                    let
                        standOnOneFootFiveSecondsUpdateFunc value form_ =
                            { form_ | standOnOneFootFiveSeconds = Just value }
                    in
                    ( viewECDInput StandOnOneFootFiveSeconds form.standOnOneFootFiveSeconds standOnOneFootFiveSecondsUpdateFunc
                    , form.standOnOneFootFiveSeconds
                    )

                UseLongPhrases ->
                    let
                        useLongPhrasesUpdateFunc value form_ =
                            { form_ | useLongPhrases = Just value }
                    in
                    ( viewECDInput UseLongPhrases form.useLongPhrases useLongPhrasesUpdateFunc
                    , form.useLongPhrases
                    )

                ShareWithOtherChildren ->
                    let
                        shareWithOtherChildrenUpdateFunc value form_ =
                            { form_ | shareWithOtherChildren = Just value }
                    in
                    ( viewECDInput ShareWithOtherChildren form.shareWithOtherChildren shareWithOtherChildrenUpdateFunc
                    , form.shareWithOtherChildren
                    )

                CountToTen ->
                    let
                        countToTenUpdateFunc value form_ =
                            { form_ | countToTen = Just value }
                    in
                    ( viewECDInput CountToTen form.countToTen countToTenUpdateFunc
                    , form.countToTen
                    )

                NoECDSigns ->
                    ( [], Nothing )

        viewECDInput sign value updateFunc =
            [ viewQuestionLabel language <| Translate.ECDSignQuestion sign
            , viewBoolInput
                language
                value
                (SetECDBoolInput updateFunc)
                ""
                Nothing
            ]
    in
    ( List.map Tuple.first persentedECDSignsData |> List.concat
    , List.map Tuple.second persentedECDSignsData
    )


viewMedicationContent :
    Language
    -> NominalDate
    -> Bool
    -> AssembledData
    -> MedicationData
    -> List (Html Msg)
viewMedicationContent language currentDate isChw assembled data =
    let
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            allMedicationTasks
                |> List.filter (expectMedicationTask currentDate isChw assembled)

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        TaskAlbendazole ->
                            ( "albendazole"
                            , isJust measurements.albendazole
                            )

                        TaskMebendezole ->
                            ( "mebendezole"
                            , isJust measurements.mebendezole
                            )

                        TaskVitaminA ->
                            ( "treatment-review"
                            , isJust measurements.vitaminA
                            )

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
                    , text <| translate language (Translate.WellChildMedicationTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map (\task -> ( task, medicationTasksCompletedFromTotal measurements data task ))
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just TaskAlbendazole ->
                    let
                        config =
                            { medication = Albendazole
                            , setMedicationAdministeredMsg = SetAlbendazoleAdministered
                            , setReasonForNonAdministration = SetAlbendazoleReasonForNonAdministration
                            , resolveDosageAndIconFunc = resolveAlbendazoleDosageAndIcon
                            , helper = Translate.AdministerAlbendazoleHelper
                            }
                    in
                    measurements.albendazole
                        |> getMeasurementValueFunc
                        |> medicationAdministrationFormWithDefault data.albendazoleForm
                        |> viewMedicationAdministrationForm language currentDate assembled config

                Just TaskMebendezole ->
                    let
                        config =
                            { medication = Mebendezole
                            , setMedicationAdministeredMsg = SetMebendezoleAdministered
                            , setReasonForNonAdministration = SetMebendezoleReasonForNonAdministration
                            , resolveDosageAndIconFunc = resolveMebendezoleDosageAndIcon
                            , helper = Translate.AdministerMebendezoleHelper
                            }
                    in
                    measurements.mebendezole
                        |> getMeasurementValueFunc
                        |> medicationAdministrationFormWithDefault data.mebendezoleForm
                        |> viewMedicationAdministrationForm language currentDate assembled config

                Just TaskVitaminA ->
                    let
                        config =
                            { medication = VitaminA
                            , setMedicationAdministeredMsg = SetVitaminAAdministered
                            , setReasonForNonAdministration = SetVitaminAReasonForNonAdministration
                            , resolveDosageAndIconFunc = resolveVitaminADosageAndIcon
                            , helper = Translate.AdministeVitaminAHelper
                            }
                    in
                    measurements.vitaminA
                        |> getMeasurementValueFunc
                        |> medicationAdministrationFormWithDefault data.vitaminAForm
                        |> viewMedicationAdministrationForm language currentDate assembled config

                Nothing ->
                    []

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
                                    TaskAlbendazole ->
                                        SaveAlbendazole personId measurements.albendazole nextTask

                                    TaskMebendezole ->
                                        SaveMebendezole personId measurements.mebendezole nextTask

                                    TaskVitaminA ->
                                        SaveVitaminA personId measurements.vitaminA nextTask

                            disabled =
                                tasksCompleted /= totalTasks
                        in
                        viewAction language saveMsg disabled
                    )
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue" ]
        [ div [ class "ui four column grid" ] <|
            List.map viewTask tasks
        ]
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            (viewForm ++ [ actions ])
        ]
    ]


type alias MedicationAdministrationFormConfig =
    { medication : MedicationDistributionSign
    , setMedicationAdministeredMsg : Bool -> Msg
    , setReasonForNonAdministration : AdministrationNote -> Msg
    , resolveDosageAndIconFunc : NominalDate -> Person -> Maybe ( String, String )
    , helper : TranslationId
    }


viewMedicationAdministrationForm : Language -> NominalDate -> AssembledData -> MedicationAdministrationFormConfig -> MedicationAdministrationForm -> List (Html Msg)
viewMedicationAdministrationForm language currentDate assembled config form =
    let
        instructions =
            config.resolveDosageAndIconFunc currentDate assembled.person
                |> Maybe.map
                    (\( dosage, icon ) ->
                        div [ class "instructions" ]
                            [ viewAdministeredMedicationLabel language Translate.Administer (Translate.MedicationDistributionSign config.medication) icon dosage
                            , div [ class "prescription" ] [ text <| translate language config.helper ++ "." ]
                            ]
                    )
                |> Maybe.withDefault emptyNode

        questions =
            [ viewAdministeredMedicationQuestion language (Translate.MedicationDistributionSign config.medication)
            , viewBoolInput
                language
                form.medicationAdministered
                config.setMedicationAdministeredMsg
                ""
                Nothing
            ]
                ++ derrivedQuestion

        derrivedQuestion =
            if form.medicationAdministered == Just False then
                [ viewQuestionLabel language Translate.WhyNot
                , viewCheckBoxSelectInput language
                    [ NonAdministrationLackOfStock, NonAdministrationKnownAllergy, NonAdministrationPatientUnableToAfford ]
                    [ NonAdministrationPatientDeclined, NonAdministrationOther ]
                    form.reasonForNonAdministration
                    config.setReasonForNonAdministration
                    Translate.AdministrationNote
                ]

            else
                []
    in
    [ div [ class "ui form medication-administration" ] <|
        [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
        , instructions
        ]
            ++ questions
    ]


viewAdministeredMedicationLabel : Language -> TranslationId -> TranslationId -> String -> String -> Html any
viewAdministeredMedicationLabel language administerTranslationId medicineTranslationId iconClass dosage =
    viewAdministeredMedicationCustomLabel language administerTranslationId medicineTranslationId iconClass dosage Nothing


viewNextStepsContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> WellChildEncounterId
    -> Bool
    -> AssembledData
    -> ModelIndexedDb
    -> NextStepsData
    -> List (Html Msg)
viewNextStepsContent language currentDate zscores id isChw assembled db data =
    let
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            List.filter (expectNextStepsTask currentDate zscores isChw assembled db) nextStepsTasks

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        TaskContributingFactors ->
                            ( "next-steps-contributing-factors"
                            , isJust measurements.contributingFactors
                            )

                        TaskHealthEducation ->
                            ( "next-steps-health-education"
                            , isJust measurements.healthEducation
                            )

                        TaskFollowUp ->
                            ( "next-steps-follow-up"
                            , isJust measurements.followUp
                            )

                        TaskSendToHC ->
                            ( "next-steps-send-to-hc"
                            , isJust measurements.sendToHC
                            )

                        TaskNextVisit ->
                            ( "next-steps-next-visit"
                            , isJust measurements.nextVisit
                            )

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
                    , text <| translate language (Translate.WellChildNextStepsTask isChw task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            List.map (\task -> ( task, nextStepsTasksCompletedFromTotal isChw measurements data task )) tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        nextVisitForm =
            measurements.nextVisit
                |> getMeasurementValueFunc
                |> nextVisitFormWithDefault data.nextVisitForm

        viewForm =
            case activeTask of
                Just TaskContributingFactors ->
                    measurements.contributingFactors
                        |> getMeasurementValueFunc
                        |> contributingFactorsFormWithDefault data.contributingFactorsForm
                        |> viewContributingFactorsForm language currentDate SetContributingFactorsSign
                        |> List.singleton

                Just TaskHealthEducation ->
                    measurements.healthEducation
                        |> getMeasurementValueFunc
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> viewHealthEducationForm language
                            currentDate
                            SetProvidedEducationForDiagnosis
                            SetReasonForNotProvidingHealthEducation
                        |> List.singleton

                Just TaskFollowUp ->
                    measurements.followUp
                        |> getMeasurementValueFunc
                        |> followUpFormWithDefault data.followUpForm
                        |> viewFollowUpForm language currentDate SetFollowUpOption
                        |> List.singleton

                Just TaskSendToHC ->
                    let
                        viewFormFunc =
                            if isChw then
                                viewSendToHCForm language
                                    currentDate
                                    SetReferToHealthCenter
                                    SetReasonForNotSendingToHC
                                    SetHandReferralForm
                                    Nothing

                            else
                                viewReferToProgramForm language
                                    currentDate
                                    SetEnrollToNutritionProgram
                                    SetReferToNutritionProgram
                    in
                    measurements.sendToHC
                        |> getMeasurementValueFunc
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> viewFormFunc
                        |> List.singleton

                Just TaskNextVisit ->
                    viewNextVisitForm language currentDate isChw assembled db nextVisitForm
                        |> List.singleton

                Nothing ->
                    []

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
                                    TaskContributingFactors ->
                                        SaveContributingFactors personId measurements.contributingFactors nextTask

                                    TaskHealthEducation ->
                                        SaveHealthEducation personId measurements.healthEducation nextTask

                                    TaskFollowUp ->
                                        let
                                            assesment =
                                                generateNutritionAssessment currentDate zscores db assembled
                                                    |> nutritionAssessmentForBackend
                                        in
                                        SaveFollowUp personId measurements.followUp assesment nextTask

                                    TaskSendToHC ->
                                        SaveSendToHC personId measurements.sendToHC nextTask

                                    TaskNextVisit ->
                                        let
                                            ( nextDateForImmunisationVisit, nextDateForPediatricVisit ) =
                                                resolveNextVisitDates currentDate isChw assembled db nextVisitForm
                                        in
                                        SaveNextVisit personId measurements.nextVisit nextDateForImmunisationVisit nextDateForPediatricVisit nextTask

                            disabled =
                                if task == TaskNextVisit then
                                    False

                                else
                                    tasksCompleted /= totalTasks
                        in
                        viewAction language saveMsg disabled
                    )
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


viewNextVisitForm : Language -> NominalDate -> Bool -> AssembledData -> ModelIndexedDb -> NextVisitForm -> Html Msg
viewNextVisitForm language currentDate isChw assembled db form =
    let
        ( nextDateForImmunisationVisit, nextDateForPediatricVisit ) =
            resolveNextVisitDates currentDate isChw assembled db form

        viewSection value label =
            Maybe.map
                (\date ->
                    [ viewLabel language label
                    , div [ class "date" ] [ text <| formatDDMMyyyy date ]
                    ]
                )
                value
                |> Maybe.withDefault []
    in
    div [ class "ui form next-visit" ] <|
        viewSection nextDateForImmunisationVisit Translate.NextImmunisationVisit
            ++ viewSection nextDateForPediatricVisit Translate.NextPediatricVisit


{-| We use saved values. If not found, fallback to logcal generation of next visit dates.
-}
resolveNextVisitDates : NominalDate -> Bool -> AssembledData -> ModelIndexedDb -> NextVisitForm -> ( Maybe NominalDate, Maybe NominalDate )
resolveNextVisitDates currentDate isChw assembled db form =
    let
        ( nextDateForImmunisationVisit, nextDateForPediatricVisit ) =
            generateNextVisitDates currentDate isChw assembled db
    in
    ( Maybe.Extra.or form.immunisationDate nextDateForImmunisationVisit
    , Maybe.Extra.or form.pediatricVisitDate nextDateForPediatricVisit
    )



-- HELPER FUNCTIONS


viewAction : Language -> Msg -> Bool -> Html Msg
viewAction language saveMsg disabled =
    div [ class "actions" ]
        [ button
            [ classList [ ( "ui fluid primary button", True ), ( "disabled", disabled ) ]
            , onClick saveMsg
            ]
            [ text <| translate language Translate.Save ]
        ]
