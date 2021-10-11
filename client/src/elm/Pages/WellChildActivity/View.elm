module Pages.WellChildActivity.View exposing (view, viewVaccinationOverview)

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
import Gizra.Html exposing (emptyNode, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYY, formatDDMMyyyy)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import List.Extra
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Model exposing (InvokationModule(..), PhotoForm, VitalsForm, VitalsFormMode(..))
import Measurement.Utils exposing (..)
import Measurement.View
    exposing
        ( renderDatePart
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
import Pages.WellChildEncounter.View exposing (viewPersonDetails)
import RemoteData exposing (RemoteData(..), WebData)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)
import ZScore.Model exposing (Centimetres(..), Kilograms(..), ZScore)
import ZScore.Utils exposing (diffDays, viewZScore, zScoreHeadCircumferenceForAge)


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
    ((viewPersonDetails language currentDate assembled.person |> div [ class "item" ])
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


viewActivity :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> WellChildEncounterId
    -> Bool
    -> WellChildActivity
    -> AssembledData
    -> ModelIndexedDb
    -> Model
    -> List (Html Msg)
viewActivity language currentDate zscores id isChw activity assembled db model =
    case activity of
        WellChildPregnancySummary ->
            viewPregnancySummaryForm language currentDate assembled model.pregnancySummaryForm

        WellChildDangerSigns ->
            viewDangerSignsContent language currentDate assembled model.dangerSignsData

        WellChildNutritionAssessment ->
            viewNutritionAssessmenContent language currentDate zscores id isChw assembled db model.nutritionAssessmentData

        WellChildImmunisation ->
            viewImmunisationContent language currentDate isChw assembled db model.immunisationData

        WellChildECD ->
            viewECDForm language currentDate assembled model.ecdForm

        WellChildMedication ->
            viewMedicationContent language currentDate isChw assembled model.medicationData

        WellChildNextSteps ->
            viewNextStepsContent language currentDate zscores id isChw assembled db model.nextStepsData

        WellChildPhoto ->
            viewPhotoContent language currentDate assembled model.photoForm


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
                + taskCompleted form.deliveryComplicationsPresent
                + deliveryComplicationsCompleted
            , 2 + deliveryComplicationsActive
            )

        expectedDateConcludedInput =
            DateSelector.SelectorDropdown.view
                ToggleExpectedDateConcluded
                SetExpectedDateConcluded
                form.isExpectedDateConcludedSelectorOpen
                (Date.add Months -3 currentDate)
                (Date.add Months 4 currentDate)
                form.expectedDateConcluded

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
                    [ viewLabel language Translate.DifferenceBetweenDueAndDeliveryDates
                    , div [ class "form-input" ] [ text viewDiff ]
                    ]
                )
                form.expectedDateConcluded
                assembled.person.birthDate
                |> Maybe.withDefault []

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
                    [ ComplicationGestationalDiabetes, ComplicationEmergencyCSection, ComplicationPreclampsia, ComplicationOther ]
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
                ]
                    ++ viewDatesDiff
                    ++ [ viewQuestionLabel language Translate.DeliveryComplicationsPresentQuestion
                       , deliveryComplicationsPresentInput
                       ]
                    ++ deliveryComplicationsSection
            ]
        , viewSaveAction language (SavePregnancySummary assembled.participant.person assembled.measurements.pregnancySummary) disabled
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
                    measurements.vitals
                        |> getMeasurementValueFunc
                        |> vitalsFormWithDefault data.vitalsForm
                        |> viewVitalsForm language
                            currentDate
                            assembled
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
                                    TaskSymptomsReview ->
                                        SaveSymptomsReview personId measurements.symptomsReview nextTask

                                    TaskVitals ->
                                        SaveVitals personId measurements.vitals nextTask

                            disabled =
                                tasksCompleted /= totalTasks
                        in
                        viewSaveAction language saveMsg disabled
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


viewVitalsForm : Language -> NominalDate -> AssembledData -> VitalsForm -> Html Msg
viewVitalsForm language currentDate assembled form =
    let
        formConfig =
            { setIntInputMsg = SetVitalsIntInput
            , setFloatInputMsg = SetVitalsFloatInput
            , sysBloodPressurePreviousValue = Nothing
            , diaBloodPressurePreviousValue = Nothing
            , heartRatePreviousValue = Nothing
            , respiratoryRatePreviousValue =
                resolvePreviousValue assembled .vitals .respiratoryRate
                    |> Maybe.map toFloat
            , bodyTemperaturePreviousValue = resolvePreviousValue assembled .vitals .bodyTemperature
            , birthDate = assembled.person.birthDate
            , formClass = "vitals"
            , mode = VitalsFormBasic
            , invokationModule = InvokationModuleWellChild
            }
    in
    Measurement.View.viewVitalsForm language currentDate formConfig form


viewSymptomsReviewForm : Language -> NominalDate -> Person -> SymptomsReviewForm -> List (Html Msg)
viewSymptomsReviewForm language currentDate person form =
    [ div [ class "ui form symptoms-review" ]
        [ viewQuestionLabel language Translate.PatientGotAnySymptoms
        , viewCustomLabel language Translate.CheckAllThatApply "." "helper"
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

        tasks =
            resolveNutritionAssessmentTasks isChw
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
            resolvePreviousValuesSetForChild currentDate assembled.participant.person db

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

                Just TaskWeight ->
                    let
                        heightValue =
                            assembled.measurements.height
                                |> getMeasurementValueFunc

                        showWeightForHeightZScore =
                            not isChw
                    in
                    measurements.weight
                        |> getMeasurementValueFunc
                        |> weightFormWithDefault data.weightForm
                        |> viewWeightForm language currentDate zscores assembled.person heightValue previousValuesSet.weight showWeightForHeightZScore SetWeight

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
                                        PreSaveHeadCircumference personId headCircumferenceZScore measurements.headCircumference nextTask

                                    TaskMuac ->
                                        SaveMuac personId measurements.muac nextTask

                                    TaskNutrition ->
                                        let
                                            assessment =
                                                generateNutritionAssessment currentDate zscores db assembled
                                                    |> nutritionAssessmentForBackend
                                        in
                                        SaveNutrition personId measurements.nutrition assessment nextTask

                                    TaskWeight ->
                                        SaveWeight personId measurements.weight nextTask

                            disabled =
                                tasksCompleted /= totalTasks
                        in
                        viewSaveAction language saveMsg disabled
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


viewImmunisationContent :
    Language
    -> NominalDate
    -> Bool
    -> AssembledData
    -> ModelIndexedDb
    -> ImmunisationData
    -> List (Html Msg)
viewImmunisationContent language currentDate isChw assembled db data =
    let
        personId =
            assembled.participant.person

        person =
            assembled.person

        measurements =
            assembled.measurements

        tasks =
            List.filter (expectImmunisationTask currentDate isChw assembled db) immunisationTasks

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        TaskBCG ->
                            ( "bcg-vaccine"
                            , isJust measurements.bcgImmunisation
                            )

                        TaskDTP ->
                            ( "dtp-vaccine"
                            , isJust measurements.dtpImmunisation
                            )

                        TaskHPV ->
                            ( "hpv-vaccine"
                            , isJust measurements.hpvImmunisation
                            )

                        TaskIPV ->
                            ( "ipv-vaccine"
                            , isJust measurements.ipvImmunisation
                            )

                        TaskMR ->
                            ( "mr-vaccine"
                            , isJust measurements.mrImmunisation
                            )

                        TaskOPV ->
                            ( "opv-vaccine"
                            , isJust measurements.opvImmunisation
                            )

                        TaskPCV13 ->
                            ( "pcv13-vaccine"
                            , isJust measurements.pcv13Immunisation
                            )

                        TaskRotarix ->
                            ( "rotarix-vaccine"
                            , isJust measurements.rotarixImmunisation
                            )

                        TaskOverview ->
                            ( "vaccination-overview"
                            , False
                            )

                isActive =
                    activeTask == Just task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveImmunisationTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.WellChildImmunisationTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            List.map (\task -> ( task, immunisationTasksCompletedFromTotal language currentDate isChw assembled data task )) tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        ( formForView, fullScreen ) =
            Maybe.andThen immunisationTaskToVaccineType activeTask
                |> Maybe.map
                    (\vaccineType ->
                        let
                            vaccinationForm =
                                case vaccineType of
                                    VaccineBCG ->
                                        measurements.bcgImmunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.bcgForm

                                    VaccineDTP ->
                                        measurements.dtpImmunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.dtpForm

                                    VaccineHPV ->
                                        measurements.hpvImmunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.hpvForm

                                    VaccineIPV ->
                                        measurements.ipvImmunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.ipvForm

                                    VaccineMR ->
                                        measurements.mrImmunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.mrForm

                                    VaccineOPV ->
                                        measurements.opvImmunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.opvForm

                                    VaccinePCV13 ->
                                        measurements.pcv13Immunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.pcv13Form

                                    VaccineRotarix ->
                                        measurements.rotarixImmunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.rotarixForm
                        in
                        ( viewVaccinationForm language currentDate isChw assembled vaccineType vaccinationForm
                        , False
                        )
                    )
                |> Maybe.withDefault
                    ( viewVaccinationOverviewForm language currentDate assembled.person assembled.vaccinationProgress db
                    , True
                    )

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
                                    TaskBCG ->
                                        SaveBCGImmunisation personId measurements.bcgImmunisation nextTask

                                    TaskDTP ->
                                        SaveDTPImmunisation personId measurements.dtpImmunisation nextTask

                                    TaskHPV ->
                                        SaveHPVImmunisation personId measurements.hpvImmunisation nextTask

                                    TaskIPV ->
                                        SaveIPVImmunisation personId measurements.ipvImmunisation nextTask

                                    TaskMR ->
                                        SaveMRImmunisation personId measurements.mrImmunisation nextTask

                                    TaskOPV ->
                                        SaveOPVImmunisation personId measurements.opvImmunisation nextTask

                                    TaskPCV13 ->
                                        SavePCV13Immunisation personId measurements.pcv13Immunisation nextTask

                                    TaskRotarix ->
                                        SaveRotarixImmunisation personId measurements.rotarixImmunisation nextTask

                                    TaskOverview ->
                                        SetActivePage <| UserPage <| WellChildEncounterPage assembled.id

                            disabled =
                                tasksCompleted /= totalTasks
                        in
                        viewSaveAction language saveMsg disabled
                    )
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui five column grid" ] <|
            List.map viewTask tasks
        ]
    , div
        [ classList
            [ ( "tasks-count", True )
            , ( "full-screen", fullScreen )
            ]
        ]
        [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div
        [ classList
            [ ( "ui full segment", True )
            , ( "full-screen", fullScreen )
            ]
        ]
        [ div [ class "full content" ]
            [ formForView
            , actions
            ]
        ]
    ]


immunisationTasksCompletedFromTotal : Language -> NominalDate -> Bool -> AssembledData -> ImmunisationData -> Pages.WellChildActivity.Model.ImmunisationTask -> ( Int, Int )
immunisationTasksCompletedFromTotal language currentDate isChw assembled data task =
    Maybe.map
        (\vaccineType ->
            let
                form =
                    case vaccineType of
                        VaccineBCG ->
                            assembled.measurements.bcgImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.bcgForm

                        VaccineDTP ->
                            assembled.measurements.dtpImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.dtpForm

                        VaccineHPV ->
                            assembled.measurements.hpvImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.hpvForm

                        VaccineIPV ->
                            assembled.measurements.ipvImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.ipvForm

                        VaccineMR ->
                            assembled.measurements.mrImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.mrForm

                        VaccineOPV ->
                            assembled.measurements.opvImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.opvForm

                        VaccinePCV13 ->
                            assembled.measurements.pcv13Immunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.pcv13Form

                        VaccineRotarix ->
                            assembled.measurements.rotarixImmunisation
                                |> getMeasurementValueFunc
                                |> vaccinationFormWithDefault data.rotarixForm

                ( _, tasksActive, tasksCompleted ) =
                    vaccinationFormDynamicContentAndTasks language currentDate isChw assembled vaccineType form
            in
            ( tasksActive, tasksCompleted )
        )
        (immunisationTaskToVaccineType task)
        |> Maybe.withDefault ( 0, 0 )


viewVaccinationForm : Language -> NominalDate -> Bool -> AssembledData -> VaccineType -> VaccinationForm -> Html Msg
viewVaccinationForm language currentDate isChw assembled vaccineType form =
    let
        ( contentByViewMode, _, _ ) =
            vaccinationFormDynamicContentAndTasks language currentDate isChw assembled vaccineType form
    in
    div [ class "ui form vaccination" ] <|
        [ h2 [] [ text <| translate language <| Translate.WellChildImmunisationHeader vaccineType ]
        , div [ class "instructions" ] <|
            [ div [ class "header icon-label" ] <|
                [ i [ class "icon-open-book" ] []
                , div []
                    [ div [ class "description" ] [ text <| translate language <| Translate.WellChildImmunisationDescription vaccineType ]
                    , div [ class "dosage" ] [ text <| translate language <| Translate.WellChildImmunisationDosage vaccineType ]
                    ]
                ]
            , viewLabel language (Translate.WellChildImmunisationHistory vaccineType)
            ]
                ++ contentByViewMode
        ]


viewVaccinationOverviewForm : Language -> NominalDate -> Person -> VaccinationProgressDict -> ModelIndexedDb -> Html any
viewVaccinationOverviewForm language currentDate child vaccinationProgress db =
    div [ class "ui form vaccination-overview" ] <|
        viewVaccinationOverview language currentDate child vaccinationProgress db


viewVaccinationOverview : Language -> NominalDate -> Person -> VaccinationProgressDict -> ModelIndexedDb -> List (Html any)
viewVaccinationOverview language currentDate child vaccinationProgress db =
    let
        entriesHeading =
            div [ class "heading vaccination" ]
                [ div [ class "name" ] [ text <| translate language Translate.Immunisation ]
                , div [ class "date" ] [ text <| translate language Translate.DateReceived ]
                , div [ class "next-due" ] [ text <| translate language Translate.NextDue ]
                , div [ class "status" ] [ text <| translate language Translate.StatusLabel ]
                ]

        futureVaccinationsData =
            generateFutureVaccinationsData currentDate child False vaccinationProgress
                |> Dict.fromList

        entries =
            Dict.toList vaccinationProgress
                |> List.map viewVaccinationEntry

        viewVaccinationEntry ( vaccineType, doses ) =
            let
                nextDue =
                    Dict.get vaccineType futureVaccinationsData
                        |> Maybe.Extra.join
                        |> Maybe.map Tuple.second

                nextDueText =
                    Maybe.map formatDDMMYY nextDue
                        |> Maybe.withDefault ""

                ( status, statusClass ) =
                    Maybe.map
                        (\dueDate ->
                            if Date.compare dueDate currentDate == LT then
                                ( StatusBehind, "behind" )

                            else
                                ( StatusUpToDate, "up-to-date" )
                        )
                        nextDue
                        |> Maybe.withDefault ( StatusDone, "done" )
            in
            div [ class "entry vaccination" ]
                [ div [ class "cell name" ] [ text <| translate language <| Translate.VaccineType vaccineType ]
                , Dict.values doses
                    |> List.sortWith Date.compare
                    |> List.map (formatDDMMYY >> text >> List.singleton >> p [])
                    |> div [ class "cell date" ]
                , div [ class "cell next-due" ]
                    [ text nextDueText ]
                , div [ class <| "cell status " ++ statusClass ]
                    [ text <| translate language <| Translate.VaccinationStatus status ]
                ]
    in
    entriesHeading :: entries


vaccinationFormDynamicContentAndTasks :
    Language
    -> NominalDate
    -> Bool
    -> AssembledData
    -> VaccineType
    -> VaccinationForm
    -> ( List (Html Msg), Int, Int )
vaccinationFormDynamicContentAndTasks language currentDate isChw assembled vaccineType form =
    Maybe.map
        (\birthDate ->
            let
                expectedDoses =
                    if isChw then
                        [ VaccineDoseFirst ]

                    else
                        getAllDosesForVaccine vaccineType
                            |> List.filter
                                (\dose -> expectVaccineDoseForPerson currentDate assembled.person ( vaccineType, dose ))

                dosesFromPreviousEncountersData =
                    Dict.get vaccineType assembled.vaccinationHistory
                        |> Maybe.withDefault Dict.empty
                        |> Dict.toList

                dosesFromCurrentEncounterData =
                    Maybe.map2
                        (\doses dates ->
                            let
                                orderedDoses =
                                    EverySet.toList doses
                                        |> List.sortBy vaccineDoseToComparable

                                orderedDates =
                                    EverySet.toList dates
                                        |> List.sortWith Date.compare
                            in
                            List.Extra.zip orderedDoses orderedDates
                        )
                        form.administeredDoses
                        form.administrationDates
                        |> Maybe.withDefault []

                allDosesGivenData =
                    dosesFromPreviousEncountersData
                        ++ dosesFromCurrentEncounterData

                allDosesGiven =
                    List.map Tuple.first allDosesGivenData

                dosesMissing =
                    List.filter (\dose -> not <| List.member dose allDosesGiven)
                        expectedDoses

                lastDoseData =
                    List.filter (\( dose, date ) -> date /= currentDate)
                        allDosesGivenData
                        |> List.reverse
                        |> List.head

                doseGivenToday =
                    List.filter
                        (\( dose, date ) ->
                            date == currentDate
                        )
                        dosesFromCurrentEncounterData
                        |> List.head
                        |> Maybe.map Tuple.first

                ( interval, unit ) =
                    getIntervalForVaccine vaccineType

                historySection =
                    case form.viewMode of
                        ViewModeInitial ->
                            let
                                allowPreviousVaccinesUpdate =
                                    (form.allowPreviousVaccinesUpdate == Just True)
                                        && (form.administrationNote /= Just AdministeredToday)

                                doseAllowedForDeletion =
                                    List.filter
                                        (\( dose, date ) ->
                                            date /= currentDate
                                        )
                                        dosesFromCurrentEncounterData
                                        |> List.reverse
                                        |> List.head
                                        |> Maybe.map Tuple.first

                                dosesFromPreviousEncountersForView =
                                    List.map (\( dose, date ) -> viewHistoryEntry dose StatusDone (Just date) False False)
                                        dosesFromPreviousEncountersData

                                dosesFromCurrentEncounterForView =
                                    List.map
                                        (\( dose, date ) ->
                                            let
                                                allowDelete =
                                                    (form.willReceiveVaccineToday /= Just True)
                                                        && (doseAllowedForDeletion == Just dose)
                                            in
                                            viewHistoryEntry dose StatusDone (Just date) False allowDelete
                                        )
                                        dosesFromCurrentEncounterData

                                dosesMissingForView =
                                    List.indexedMap
                                        (\index dose ->
                                            let
                                                step =
                                                    -- Index starts from 0.
                                                    -- If there were no vaccinations given, we'll use
                                                    -- expected due date of first dose, therefore, no
                                                    -- need to add 1 interval.
                                                    -- Otherwise, we want to add 1 interval to the
                                                    -- date of last vaccination.
                                                    if isNothing lastDoseData then
                                                        index

                                                    else
                                                        index + 1

                                                expectedOnDate =
                                                    Maybe.map Tuple.second lastDoseData
                                                        |> Maybe.withDefault (initialVaccinationDateByBirthDate birthDate ( vaccineType, VaccineDoseFirst ))
                                                        |> Date.add unit (step * interval)
                                            in
                                            if not <| Date.compare expectedOnDate currentDate == LT then
                                                Nothing

                                            else
                                                Just <|
                                                    viewHistoryEntry dose
                                                        StatusBehind
                                                        Nothing
                                                        (index == 0 && allowPreviousVaccinesUpdate)
                                                        False
                                        )
                                        dosesMissing
                                        |> Maybe.Extra.values
                            in
                            [ div [ class "history" ] <|
                                dosesFromPreviousEncountersForView
                                    ++ dosesFromCurrentEncounterForView
                                    ++ dosesMissingForView
                            ]

                        ViewModeVaccinationUpdate dose ->
                            [ div [ class "history" ]
                                [ viewHistoryEntry dose StatusBehind Nothing False False ]
                            ]

                viewHistoryEntry dose status date updateAllowed deleteAllowed =
                    let
                        dateForView =
                            Maybe.map formatDDMMyyyy date
                                |> Maybe.withDefault "--/--/----"

                        statusClass =
                            case status of
                                StatusDone ->
                                    "done"

                                StatusBehind ->
                                    "behind"

                                _ ->
                                    ""

                        deleteButton =
                            Maybe.map
                                (\date_ ->
                                    div
                                        [ class "delete"
                                        , onClick <| DeleteVaccinationUpdateDate vaccineType dose date_
                                        ]
                                        [ text <| translate language Translate.Delete ]
                                )
                                date
                                |> Maybe.withDefault emptyNode
                    in
                    div [ class "history-entry" ]
                        [ div [ class "dose" ] [ text <| String.fromInt <| vaccineDoseToComparable dose ]
                        , div [ class <| "status " ++ statusClass ] [ text <| translate language <| Translate.VaccinationStatus status ]
                        , div [ class "date" ] [ text dateForView ]
                        , showIf updateAllowed <|
                            div
                                [ class "update"
                                , onClick <| SetVaccinationFormViewMode vaccineType (ViewModeVaccinationUpdate dose)
                                ]
                                [ text <| translate language Translate.Update ]
                        , showIf deleteAllowed <| deleteButton
                        ]

                ( inputs, tasksActive, tasksCompleted ) =
                    case form.viewMode of
                        ViewModeInitial ->
                            let
                                vaccineTypeLabel =
                                    translate language <| Translate.WellChildVaccineLabel vaccineType

                                ( derrivedInputs, derrivedTasksActive, derrivedTasksCompleted ) =
                                    Maybe.map2
                                        (\todaysDose _ ->
                                            let
                                                -- This is the date starting from which we allow
                                                -- vaccine administration for todays dose.
                                                expectedOnDate =
                                                    Maybe.map
                                                        (\( _, lastDoseDate ) ->
                                                            Date.add unit interval lastDoseDate
                                                        )
                                                        lastDoseData
                                                        |> Maybe.withDefault (initialVaccinationDateByBirthDate birthDate ( vaccineType, VaccineDoseFirst ))
                                            in
                                            if Date.compare expectedOnDate currentDate == GT then
                                                -- We've not reached the date on which tadays dose
                                                -- administration is allowed, therefore, we do not
                                                -- show the input.
                                                ( [], 0, 0 )

                                            else
                                                let
                                                    ( whyNotIpnut, whyNotTaskActive, whyNotTaskCompleted ) =
                                                        if form.willReceiveVaccineToday == Just False then
                                                            ( [ div [ class "why-not" ]
                                                                    [ viewQuestionLabel language Translate.WhyNot
                                                                    , viewCheckBoxSelectInput language
                                                                        [ NonAdministrationLackOfStock, NonAdministrationPatientDeclined, NonAdministrationKnownAllergy ]
                                                                        [ NonAdministrationPatientUnableToAfford, NonAdministrationChildsCondition, NonAdministrationOther ]
                                                                        form.administrationNote
                                                                        (SetAdministrationNote vaccineType)
                                                                        Translate.AdministrationNoteForWellChild
                                                                    ]
                                                              ]
                                                            , taskCompleted form.administrationNote
                                                            , 1
                                                            )

                                                        else
                                                            ( [], 0, 0 )
                                                in
                                                ( [ viewQuestionLabel language <| Translate.VaccineDoseAdministeredTodayQuestion vaccineTypeLabel
                                                  , viewBoolInput
                                                        language
                                                        form.willReceiveVaccineToday
                                                        (SetWillReceiveVaccineToday vaccineType todaysDose)
                                                        ""
                                                        Nothing
                                                  ]
                                                    ++ whyNotIpnut
                                                , taskCompleted form.willReceiveVaccineToday + whyNotTaskActive
                                                , 1 + whyNotTaskCompleted
                                                )
                                        )
                                        (Maybe.Extra.or doseGivenToday (List.head dosesMissing))
                                        form.allowPreviousVaccinesUpdate
                                        |> Maybe.withDefault ( [], 0, 0 )
                            in
                            ( [ viewQuestionLabel language <| Translate.VaccineDoseAdministeredPreviouslyQuestion vaccineTypeLabel
                              , viewBoolInput
                                    language
                                    form.allowPreviousVaccinesUpdate
                                    (SetAllowPreviousVaccinesUpdate vaccineType)
                                    ""
                                    Nothing
                              ]
                                ++ derrivedInputs
                            , taskCompleted form.allowPreviousVaccinesUpdate + derrivedTasksActive
                            , 1 + derrivedTasksCompleted
                            )

                        ViewModeVaccinationUpdate dose ->
                            let
                                startDate =
                                    Maybe.andThen
                                        (\( lastDoseAdministered, lastDoseDate ) ->
                                            nextVaccinationDataForVaccine lastDoseDate lastDoseAdministered vaccineType
                                        )
                                        lastDoseData
                                        |> Maybe.map Tuple.second
                                        -- No doses were given yet, so we will set start date to
                                        -- expected due date of first dose.
                                        |> Maybe.withDefault (initialVaccinationDateByBirthDate birthDate ( vaccineType, VaccineDoseFirst ))
                            in
                            ( [ div [ class "form-input date previous" ]
                                    [ viewLabel language Translate.SelectDate
                                    , DateSelector.SelectorDropdown.view
                                        (ToggleDateSelectorInput vaccineType)
                                        (SetVaccinationUpdateDate vaccineType)
                                        form.dateSelectorOpen
                                        startDate
                                        (Date.add Days -1 currentDate)
                                        form.vaccinationUpdateDate
                                    ]
                              , div [ class "update actions" ]
                                    [ div
                                        [ class "ui primary button"
                                        , onClick <| SetVaccinationFormViewMode vaccineType ViewModeInitial
                                        ]
                                        [ text <| translate language Translate.Cancel
                                        ]
                                    , div
                                        [ classList
                                            [ ( "ui primary button", True )
                                            , ( "disabled", isNothing form.vaccinationUpdateDate )
                                            ]
                                        , onClick <| SaveVaccinationUpdateDate vaccineType dose
                                        ]
                                        [ text <| translate language Translate.Save ]
                                    ]
                              ]
                            , taskCompleted form.vaccinationUpdateDate
                            , 1
                            )
            in
            ( historySection ++ inputs, tasksActive, tasksCompleted )
        )
        assembled.person.birthDate
        |> Maybe.withDefault ( [], 0, 1 )


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
        , viewSaveAction language (SaveECD assembled.participant.person assembled.measurements.ecd) disabled
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
            medicationTasks
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
                        viewSaveAction language saveMsg disabled
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
                        viewSaveAction language saveMsg disabled
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


viewPhotoContent : Language -> NominalDate -> AssembledData -> PhotoForm -> List (Html Msg)
viewPhotoContent language currentDate assembled form =
    let
        photoId =
            Maybe.map Tuple.first assembled.measurements.photo

        -- If we have a photo that we've just taken, but not saved, that is in
        -- `data.url`. We show that if we have it. Otherwise, we'll show the saved
        -- measurement, if we have that.
        ( displayPhoto, saveMsg, isDisabled ) =
            case form.url of
                Just url ->
                    ( Just url
                    , [ onClick <| SavePhoto assembled.participant.person photoId url ]
                    , False
                    )

                Nothing ->
                    ( getMeasurementValueFunc assembled.measurements.photo
                    , []
                    , True
                    )

        totalTasks =
            1

        tasksCompleted =
            taskCompleted displayPhoto
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            viewPhotoForm language currentDate displayPhoto DropZoneComplete
        , div [ class "actions" ]
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



-- HELPER FUNCTIONS


viewSaveAction : Language -> Msg -> Bool -> Html Msg
viewSaveAction language saveMsg disabled =
    div [ class "actions" ]
        [ button
            [ classList [ ( "ui fluid primary button", True ), ( "disabled", disabled ) ]
            , onClick saveMsg
            ]
            [ text <| translate language Translate.Save ]
        ]
