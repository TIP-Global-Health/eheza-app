module Pages.WellChildActivity.View exposing (view)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (headCircumferenceIndication)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (nutritionAssesmentForBackend, resolvePreviousValuesSetForChild)
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
import Pages.WellChildEncounter.Model exposing (AssembledData)
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
viewHeaderAndContent language currentDate zscores id isChw activity db model data =
    let
        header =
            viewHeader language id activity

        content =
            viewContent language currentDate zscores id isChw activity db model data
    in
    div [ class "page-activity well-child" ]
        [ header
        , content
        , viewModal <|
            warningPopup language
                currentDate
                SetWarningPopupState
                model.warningPopupState
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


viewActivity : Language -> NominalDate -> ZScore.Model.Model -> WellChildEncounterId -> Bool -> WellChildActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate zscores id isChw activity assembled db model =
    case activity of
        WellChildPregnancySummary ->
            viewPregnancySummaryForm language currentDate assembled model.pregnancySummaryForm

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


viewPregnancySummaryForm : Language -> NominalDate -> AssembledData -> PregnancySummaryForm -> List (Html Msg)
viewPregnancySummaryForm language currentDate assembled form_ =
    let
        form =
            assembled.measurements.pregnancySummary
                |> Maybe.map (Tuple.second >> .value)
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
                        |> Maybe.map (Tuple.second >> .value)
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
                        |> Maybe.map (Tuple.second >> .value)
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

        tasks =
            allNutritionAssesmentTasks
                |> List.filter (expectNutritionAssessmentTask currentDate zscores isChw assembled db)

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

                isActive =
                    activeTask == Just task

                attributes =
                    classList [ ( "link-section", True ), ( "active", isActive ), ( "completed", not isActive && isCompleted ) ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveNutritionAssesmentTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.NutritionAssesmentTask task)
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

        viewForm =
            case activeTask of
                Just TaskHeight ->
                    measurements.height
                        |> Maybe.map (Tuple.second >> .value)
                        |> heightFormWithDefault data.heightForm
                        |> viewHeightForm language currentDate zscores assembled.person previousValuesSet.height SetHeight

                Just TaskHeadCircumference ->
                    measurements.headCircumference
                        |> Maybe.map (Tuple.second >> .value)
                        |> headCircumferenceFormWithDefault data.headCircumferenceForm
                        |> viewHeadCircumferenceForm language currentDate zscores assembled.person previousValuesSet.headCircumference

                Just TaskMuac ->
                    measurements.muac
                        |> Maybe.map (Tuple.second >> .value)
                        |> muacFormWithDefault data.muacForm
                        |> viewMuacForm language currentDate assembled.person previousValuesSet.muac SetMuac

                Just TaskNutrition ->
                    measurements.nutrition
                        |> Maybe.map (Tuple.second >> .value)
                        |> nutritionFormWithDefault data.nutritionForm
                        |> viewNutritionForm language currentDate SetNutritionSign

                Just TaskPhoto ->
                    let
                        displayPhoto =
                            case data.photoForm.url of
                                Just url ->
                                    Just url

                                Nothing ->
                                    Maybe.map (Tuple.second >> .value) assembled.measurements.photo
                    in
                    viewPhotoForm language currentDate displayPhoto DropZoneComplete

                Just TaskWeight ->
                    let
                        heightValue =
                            assembled.measurements.height
                                |> Maybe.map (Tuple.second >> .value)
                    in
                    measurements.weight
                        |> Maybe.map (Tuple.second >> .value)
                        |> weightFormWithDefault data.weightForm
                        |> viewWeightForm language currentDate zscores assembled.person heightValue previousValuesSet.weight SetWeight

                Just TaskContributingFactors ->
                    measurements.contributingFactors
                        |> Maybe.map (Tuple.second >> .value)
                        |> contributingFactorsFormWithDefault data.contributingFactorsForm
                        |> viewContributingFactorsForm language currentDate SetContributingFactorsSign
                        |> List.singleton

                Just TaskHealthEducation ->
                    measurements.healthEducation
                        |> Maybe.map (Tuple.second >> .value)
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> viewHealthEducationForm language
                            currentDate
                            SetProvidedEducationForDiagnosis
                            SetReasonForNotProvidingHealthEducation
                        |> List.singleton

                Just TaskFollowUp ->
                    measurements.followUp
                        |> Maybe.map (Tuple.second >> .value)
                        |> followUpFormWithDefault data.followUpForm
                        |> viewFollowUpForm language currentDate SetFollowUpOption
                        |> List.singleton

                Just TaskSendToHC ->
                    measurements.sendToHC
                        |> Maybe.map (Tuple.second >> .value)
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> viewSendToHCForm language
                            currentDate
                            SetReferToHealthCenter
                            SetReasonForNotSendingToHC
                            SetHandReferralForm
                            Nothing
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

        tasksTray =
            let
                ( topTasks, bottomTasks ) =
                    List.Extra.splitAt 5 tasks

                topSection =
                    List.map viewTask topTasks
                        |> div [ class "ui five column grid" ]

                bottomSection =
                    if List.isEmpty bottomTasks then
                        emptyNode

                    else
                        List.map viewTask bottomTasks
                            |> div [ class "ui four column grid" ]
            in
            div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
                [ topSection, bottomSection ]

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
                                        SaveHeadCircumference personId measurements.headCircumference nextTask

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

                                    TaskContributingFactors ->
                                        SaveContributingFactors personId measurements.contributingFactors nextTask

                                    TaskHealthEducation ->
                                        SaveHealthEducation personId measurements.healthEducation nextTask

                                    TaskFollowUp ->
                                        let
                                            assesment =
                                                generateNutritionAssesment currentDate zscores db assembled
                                                    |> nutritionAssesmentForBackend
                                        in
                                        SaveFollowUp personId measurements.followUp assesment nextTask

                                    TaskSendToHC ->
                                        SaveSendToHC personId measurements.sendToHC nextTask

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
    [ tasksTray
    , div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            (viewForm ++ [ actions ])
        ]
    ]


viewHeadCircumferenceForm :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Person
    -> Maybe Float
    -> HeadCircumferenceForm
    -> List (Html Msg)
viewHeadCircumferenceForm language currentDate zscores person previousValue form =
    let
        maybeAgeInDays =
            Maybe.map
                (\birthDate -> diffDays birthDate currentDate)
                person.birthDate

        zScoreValue =
            form.headCircumference
                |> Maybe.andThen
                    (\headCircumference ->
                        Maybe.andThen
                            (\ageInDays ->
                                zScoreHeadCircumferenceForAge zscores ageInDays person.gender (Centimetres headCircumference)
                            )
                            maybeAgeInDays
                    )

        zScoreText =
            Maybe.map viewZScore zScoreValue
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
                            Maybe.map (HeadCircumferenceInCm >> headCircumferenceIndication >> viewColorAlertIndication language) zScoreValue
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
        [ viewLabel language <| Translate.NutritionAssesmentTask TaskHeadCircumference
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


viewImmunisationForm : Language -> NominalDate -> Bool -> AssembledData -> ImmunisationForm -> List (Html Msg)
viewImmunisationForm language currentDate isChw assembled immunisationForm =
    let
        form =
            assembled.measurements.immunisation
                |> Maybe.map (Tuple.second >> .value)
                |> immunisationFormWithDefault immunisationForm

        ( tasksCompleted, totalTasks ) =
            ( (List.map taskCompleted vaccinationGivenTasks |> List.sum)
                + (List.map taskCompleted vaccinationDateTasks |> List.sum)
                + (List.map taskCompleted vaccinationNoteTasks |> List.sum)
            , List.length vaccinationGivenTasks
                + List.length vaccinationDateTasks
                + List.length vaccinationNoteTasks
            )

        suggestedVaccines =
            generateSuggestedVaccines currentDate isChw assembled

        inputsAndtasks =
            List.map (inputsAndTasksForSuggestedVaccine language currentDate isChw assembled form) suggestedVaccines

        inputs =
            List.map Tuple.first inputsAndtasks
                |> List.concat

        tasks =
            List.map Tuple.second inputsAndtasks

        vaccinationGivenTasks =
            List.map .vaccinationGivenTask tasks

        vaccinationDateTasks =
            List.filterMap .vaccinationDateTask tasks

        vaccinationNoteTasks =
            List.filterMap .vaccinationNoteTask tasks

        futureVaccines =
            generateFutureVaccines currentDate isChw assembled
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
    { vaccinationGivenTask : Maybe Bool
    , vaccinationDateTask : Maybe (Maybe NominalDate)
    , vaccinationNoteTask : Maybe (Maybe AdministrationNote)
    }


inputsAndTasksForSuggestedVaccine : Language -> NominalDate -> Bool -> AssembledData -> ImmunisationForm -> ( VaccineType, VaccineDose ) -> ( List (Html Msg), SuggestedVaccineTasks )
inputsAndTasksForSuggestedVaccine language currentDate isChw assembled form ( vaccineType, vaccineDose ) =
    let
        ( vaccinationGivenInput, vaccinationGivenTask ) =
            ( [ viewQuestionLabel language <| Translate.VaccineDoseGivenQuestion vaccineType vaccineDose isChw
              , viewBoolInput
                    language
                    (config.getVaccinationGivenFunc form)
                    (SetImmunisationBoolInput config.setBoolInputFunc)
                    ""
                    Nothing
              ]
            , config.getVaccinationGivenFunc form
            )

        ( vaccinationDerrivedInputs, vaccinationDateTask, vaccinationNoteTask ) =
            if config.getVaccinationGivenFunc form == Just False then
                let
                    ( vaccinationDateInput, dateTask ) =
                        if config.getVaccinationNoteFunc form == Just AdministeredPreviously then
                            ( [ div [ class "form-input date previous" ]
                                    [ viewLabel language Translate.SelectDate
                                    , DateSelector.SelectorDropdown.view
                                        (ToggleImmunisationDateSelectorInput config.toggleDateSelectorFunc)
                                        (SetImmunisationDateInput config.setDateInputFunc)
                                        (config.getVaccinationDateSelectorOpenFunc form)
                                        (Date.add Months -6 currentDate)
                                        (Date.add Days -1 currentDate)
                                        (config.getVaccinationDateFunc form)
                                    ]
                              ]
                            , Just <| config.getVaccinationDateFunc form
                            )

                        else
                            ( [], Nothing )
                in
                ( [ div [ class "why-not" ]
                        [ viewQuestionLabel language Translate.WhyNot
                        , viewCheckBoxSelectInput language
                            [ AdministeredPreviously, NonAdministrationLackOfStock, NonAdministrationPatientDeclined ]
                            [ NonAdministrationKnownAllergy, NonAdministrationPatientUnableToAfford, NonAdministrationOther ]
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
                                | bcgVaccinationGiven = Just value
                                , bcgVaccinationNote = vaccinationNote
                                , bcgVaccinationDate = vaccinationDate
                            }
                    , toggleDateSelectorFunc = \form_ -> { form_ | bcgVaccinationDateSelectorOpen = not form_.bcgVaccinationDateSelectorOpen }
                    , setDateInputFunc = \value form_ -> { form_ | bcgVaccinationDate = Just value }
                    , setAdministrationNoteFunc = \value form_ -> { form_ | bcgVaccinationNote = Just value, bcgVaccinationDate = Nothing }
                    , getVaccinationGivenFunc = .bcgVaccinationGiven
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
                                | opvVaccinationGiven = Just value
                                , opvVaccinationNote = vaccinationNote
                                , opvVaccinationDate = vaccinationDate
                            }
                    , toggleDateSelectorFunc = \form_ -> { form_ | opvVaccinationDateSelectorOpen = not form_.opvVaccinationDateSelectorOpen }
                    , setDateInputFunc = \value form_ -> { form_ | opvVaccinationDate = Just value }
                    , setAdministrationNoteFunc = \value form_ -> { form_ | opvVaccinationNote = Just value, opvVaccinationDate = Nothing }
                    , getVaccinationGivenFunc = .opvVaccinationGiven
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
                                | dtpVaccinationGiven = Just value
                                , dtpVaccinationNote = vaccinationNote
                                , dtpVaccinationDate = vaccinationDate
                            }
                    , toggleDateSelectorFunc = \form_ -> { form_ | dtpVaccinationDateSelectorOpen = not form_.dtpVaccinationDateSelectorOpen }
                    , setDateInputFunc = \value form_ -> { form_ | dtpVaccinationDate = Just value }
                    , setAdministrationNoteFunc = \value form_ -> { form_ | dtpVaccinationNote = Just value, dtpVaccinationDate = Nothing }
                    , getVaccinationGivenFunc = .dtpVaccinationGiven
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
                                | pcv13VaccinationGiven = Just value
                                , pcv13VaccinationNote = vaccinationNote
                                , pcv13VaccinationDate = vaccinationDate
                            }
                    , toggleDateSelectorFunc = \form_ -> { form_ | pcv13VaccinationDateSelectorOpen = not form_.pcv13VaccinationDateSelectorOpen }
                    , setDateInputFunc = \value form_ -> { form_ | pcv13VaccinationDate = Just value }
                    , setAdministrationNoteFunc = \value form_ -> { form_ | pcv13VaccinationNote = Just value, pcv13VaccinationDate = Nothing }
                    , getVaccinationGivenFunc = .pcv13VaccinationGiven
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
                                | rotarixVaccinationGiven = Just value
                                , rotarixVaccinationNote = vaccinationNote
                                , rotarixVaccinationDate = vaccinationDate
                            }
                    , toggleDateSelectorFunc = \form_ -> { form_ | rotarixVaccinationDateSelectorOpen = not form_.rotarixVaccinationDateSelectorOpen }
                    , setDateInputFunc = \value form_ -> { form_ | rotarixVaccinationDate = Just value }
                    , setAdministrationNoteFunc = \value form_ -> { form_ | rotarixVaccinationNote = Just value, rotarixVaccinationDate = Nothing }
                    , getVaccinationGivenFunc = .rotarixVaccinationGiven
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
                                | ipvVaccinationGiven = Just value
                                , ipvVaccinationNote = vaccinationNote
                                , ipvVaccinationDate = vaccinationDate
                            }
                    , toggleDateSelectorFunc = \form_ -> { form_ | ipvVaccinationDateSelectorOpen = not form_.ipvVaccinationDateSelectorOpen }
                    , setDateInputFunc = \value form_ -> { form_ | ipvVaccinationDate = Just value }
                    , setAdministrationNoteFunc = \value form_ -> { form_ | ipvVaccinationNote = Just value, ipvVaccinationDate = Nothing }
                    , getVaccinationGivenFunc = .ipvVaccinationGiven
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
                                | mrVaccinationGiven = Just value
                                , mrVaccinationNote = vaccinationNote
                                , mrVaccinationDate = vaccinationDate
                            }
                    , toggleDateSelectorFunc = \form_ -> { form_ | mrVaccinationDateSelectorOpen = not form_.mrVaccinationDateSelectorOpen }
                    , setDateInputFunc = \value form_ -> { form_ | mrVaccinationDate = Just value }
                    , setAdministrationNoteFunc = \value form_ -> { form_ | mrVaccinationNote = Just value, mrVaccinationDate = Nothing }
                    , getVaccinationGivenFunc = .mrVaccinationGiven
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
                                | hpvVaccinationGiven = Just value
                                , hpvVaccinationNote = vaccinationNote
                                , hpvVaccinationDate = vaccinationDate
                            }
                    , toggleDateSelectorFunc = \form_ -> { form_ | hpvVaccinationDateSelectorOpen = not form_.hpvVaccinationDateSelectorOpen }
                    , setDateInputFunc = \value form_ -> { form_ | hpvVaccinationDate = Just value }
                    , setAdministrationNoteFunc = \value form_ -> { form_ | hpvVaccinationNote = Just value, hpvVaccinationDate = Nothing }
                    , getVaccinationGivenFunc = .hpvVaccinationGiven
                    , getVaccinationDateFunc = .hpvVaccinationDate
                    , getVaccinationNoteFunc = .hpvVaccinationNote
                    , getVaccinationDateSelectorOpenFunc = .hpvVaccinationDateSelectorOpen
                    }
    in
    ( vaccinationGivenInput ++ vaccinationDerrivedInputs
    , SuggestedVaccineTasks vaccinationGivenTask vaccinationDateTask vaccinationNoteTask
    )


viewECDForm : Language -> NominalDate -> AssembledData -> WellChildECDForm -> List (Html Msg)
viewECDForm language currentDate assembled ecdForm =
    ageInMonths currentDate assembled.person
        |> Maybe.map
            (\ageMonths ->
                let
                    totalTasks =
                        List.length tasks

                    tasksCompleted =
                        List.map taskCompleted tasks
                            |> List.sum

                    ( inputs, tasks ) =
                        ecdFormInputsAndTasks language currentDate assembled ageMonths ecdForm

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
            )
        |> Maybe.withDefault []


ecdFormInputsAndTasks : Language -> NominalDate -> AssembledData -> Int -> WellChildECDForm -> ( List (Html Msg), List (Maybe Bool) )
ecdFormInputsAndTasks language currentDate assembled ageMonths ecdForm =
    let
        form =
            assembled.measurements.ecd
                |> Maybe.map (Tuple.second >> .value)
                |> wellChildECDFormWithDefault ecdForm

        completed =
            generateCompletedECDSigns assembled

        expected =
            expectedECDSignsByAge ageMonths
                |> List.filter (\sign -> not <| List.member sign completed)
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
    ( List.map Tuple.first expected |> List.concat
    , List.map Tuple.second expected
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
                        |> Maybe.map (Tuple.second >> .value)
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
                        |> Maybe.map (Tuple.second >> .value)
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
                        |> Maybe.map (Tuple.second >> .value)
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


viewAction : Language -> Msg -> Bool -> Html Msg
viewAction language saveMsg disabled =
    div [ class "actions" ]
        [ button
            [ classList [ ( "ui fluid primary button", True ), ( "disabled", disabled ) ]
            , onClick saveMsg
            ]
            [ text <| translate language Translate.Save ]
        ]
