module Pages.WellChild.Activity.View exposing (view, viewVaccinationOverview)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils
    exposing
        ( getNewbornExamPregnancySummary
        , nutritionAssessmentForBackend
        , resolveNCDANeverFilled
        , resolveNCDANotFilledAfterAgeOfSixMonths
        , resolvePreviousValuesSetForChild
        )
import Backend.Person.Model exposing (Person)
import Backend.WellChildActivity.Model exposing (WellChildActivity(..))
import Backend.WellChildEncounter.Model exposing (WellChildEncounterType(..))
import Date exposing (Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (isJust)
import Measurement.Model
    exposing
        ( ImmunisationTask(..)
        , NCDAData
        , PhotoForm
        , VaccinationFormViewMode(..)
        , VaccinationProgressDict
        , VaccinationStatus(..)
        , VitalsForm
        )
import Measurement.Utils exposing (contributingFactorsFormWithDefault, generateFutureVaccinationsData, healthEducationFormWithDefault, heightFormWithDefault, immunisationTaskToVaccineType, medicationAdministrationFormWithDefault, muacFormWithDefault, ncdaFormWithDefault, nutritionCaringFormWithDefault, nutritionFeedingFormWithDefault, nutritionFollowUpFormWithDefault, nutritionFoodSecurityFormWithDefault, nutritionFormWithDefault, nutritionHygieneFormWithDefault, sendToHCFormWithDefault, vaccinationFormWithDefault, vitalsFormWithDefault, weightFormWithDefault)
import Measurement.View
    exposing
        ( birthWeightInputsAndTasks
        , nutritionCaringInputsAndTasks
        , nutritionFeedingInputsAndTasks
        , nutritionFoodSecurityInputsAndTasks
        , nutritionHygieneInputsAndTasks
        , viewContributingFactorsForm
        , viewHealthEducationForm
        , viewHeightForm
        , viewMedicationAdministrationForm
        , viewMuacForm
        , viewNutritionFollowUpForm
        , viewNutritionForm
        , viewReferToProgramForm
        , viewSendToHealthCenterForm
        , viewWeightForm
        )
import Pages.Nutrition.Activity.View exposing (viewPhotoForm, warningPopup)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Utils
    exposing
        ( isTaskCompleted
        , maybeToBoolTask
        , resolveActiveTask
        , resolveNextTask
        , resolveTasksCompletedFromTotal
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCustomAction
        , viewLabel
        , viewMeasurementInput
        , viewPersonDetailsExtended
        , viewQuestionLabel
        , viewSaveAction
        , viewTasksCount
        )
import Pages.WellChild.Activity.Model exposing (DangerSignsData, HeadCircumferenceForm, HomeVisitData, ImmunisationData, MedicationData, Model, Msg(..), NextStepsData, NextVisitForm, NutritionAssessmentData, PregnancySummaryForm, SymptomsReviewForm, WarningPopupType(..), WellChildECDForm, WellChildVaccinationForm, medicationTasks)
import Pages.WellChild.Activity.Types exposing (DangerSignsTask(..), HomeVisitTask(..), MedicationTask(..), NextStepsTask(..), NutritionAssessmentTask(..))
import Pages.WellChild.Activity.Utils exposing (albendazoleAdministrationFormConfig, dangerSignsTasksCompletedFromTotal, expectImmunisationTask, expectMedicationTask, expectNextStepsTask, expectNutritionAssessmentTask, generateASAPImmunisationDate, generateNextVisitDates, generateNutritionAssessment, generateRemianingECDSignsBeforeCurrentEncounter, generateVitalsFormConfig, headCircumferenceFormAndTasks, headCircumferenceFormWithDefault, immunisationTasks, immunisationTasksCompletedFromTotal, mebendezoleAdministrationFormConfig, medicationTasksCompletedFromTotal, nextStepsTasks, nextStepsTasksCompletedFromTotal, nextVisitFormWithDefault, nutritionAssessmentTasksCompletedFromTotal, pregnancySummaryFormWithDefault, resolveNutritionAssessmentTasks, symptomsReviewFormInputsAndTasks, symptomsReviewFormWithDefault, vitaminAAdministrationFormConfig, wellChildECDFormWithDefault)
import Pages.WellChild.Encounter.Model exposing (AssembledData)
import Pages.WellChild.Encounter.Utils exposing (generateAssembledData)
import SyncManager.Model exposing (Site, SiteFeature)
import Translate exposing (Language, TranslationId, translate)
import Utils.Html exposing (viewModal)
import Utils.WebData exposing (viewWebData)
import ZScore.Model exposing (Centimetres(..))
import ZScore.Utils exposing (diffDays, zScoreHeadCircumferenceForAge)


view :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> WellChildEncounterId
    -> WellChildActivity
    -> ModelIndexedDb
    -> Model
    -> Html Msg
view language currentDate zscores site features id activity db model =
    let
        data =
            generateAssembledData site id db
    in
    viewWebData language (viewHeaderAndContent language currentDate zscores site features id activity db model) identity data


viewHeaderAndContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> WellChildEncounterId
    -> WellChildActivity
    -> ModelIndexedDb
    -> Model
    -> AssembledData
    -> Html Msg
viewHeaderAndContent language currentDate zscores site features id activity db model assembled =
    let
        header =
            viewHeader language id activity

        content =
            viewContent language currentDate zscores site features activity db model assembled
    in
    div [ class "page-activity well-child" ]
        [ header
        , content
        , viewModal <|
            viewWarningPopup language model.warningPopupState
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
            ]
        ]


viewContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> WellChildActivity
    -> ModelIndexedDb
    -> Model
    -> AssembledData
    -> Html Msg
viewContent language currentDate zscores site features activity db model assembled =
    ((viewPersonDetailsExtended language currentDate assembled.person |> div [ class "item" ])
        :: viewActivity language currentDate zscores site features activity assembled db model
    )
        |> div [ class "ui unstackable items" ]


viewWarningPopup : Language -> Maybe WarningPopupType -> Maybe (Html Msg)
viewWarningPopup language warningPopupState =
    warningPopupState
        |> Maybe.andThen
            (\popupType ->
                case popupType of
                    PopupNutritionAssessment assessment ->
                        warningPopup language
                            (SetWarningPopupState Nothing)
                            assessment

                    PopupMacrocephaly personId saved nextTask ->
                        headCircumferencePopup language ( personId, saved, nextTask ) Translate.WellChildMacrocephalyWarning

                    PopupMicrocephaly personId saved nextTask ->
                        headCircumferencePopup language ( personId, saved, nextTask ) Translate.WellChildMicrocephalyWarning
            )


headCircumferencePopup :
    Language
    -> ( PersonId, Maybe ( WellChildHeadCircumferenceId, WellChildHeadCircumference ), Maybe NutritionAssessmentTask )
    -> TranslationId
    -> Maybe (Html Msg)
headCircumferencePopup language ( personId, saved, nextTask ) message =
    Just <|
        div [ class "ui active modal danger-signs-popup" ]
            [ div [ class "content" ]
                [ div [ class "popup-heading-wrapper" ]
                    [ img [ src "assets/images/exclamation-red.png" ] []
                    , div [ class "popup-heading warning" ] [ text <| translate language Translate.Warning ++ "!" ]
                    ]
                , div [ class "popup-action" ] [ text <| translate language message ]
                ]
            , viewCustomAction language
                (CloseHeadCircumferencePopup personId saved nextTask)
                False
                Translate.Continue
            ]


viewActivity :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> WellChildActivity
    -> AssembledData
    -> ModelIndexedDb
    -> Model
    -> List (Html Msg)
viewActivity language currentDate zscores site features activity assembled db model =
    case activity of
        WellChildPregnancySummary ->
            viewPregnancySummaryForm language currentDate assembled model.pregnancySummaryForm

        WellChildDangerSigns ->
            viewDangerSignsContent language currentDate assembled model.dangerSignsData

        WellChildNutritionAssessment ->
            viewNutritionAssessmenContent language currentDate site zscores assembled db model.nutritionAssessmentData

        WellChildImmunisation ->
            viewImmunisationContent language currentDate site assembled model.immunisationData

        WellChildECD ->
            viewECDForm language currentDate assembled model.ecdForm

        WellChildMedication ->
            viewMedicationContent language currentDate site assembled model.medicationData

        WellChildNextSteps ->
            viewNextStepsContent language currentDate zscores site features assembled db model.nextStepsData

        WellChildPhoto ->
            viewPhotoContent language assembled model.photoForm

        WellChildNCDA ->
            viewNCDAContent language currentDate site assembled model.ncdaData db

        WellChildHomeVisit ->
            viewHomeVisitContent language assembled model.homeVisitData db


viewPregnancySummaryForm : Language -> NominalDate -> AssembledData -> PregnancySummaryForm -> List (Html Msg)
viewPregnancySummaryForm language currentDate assembled form_ =
    let
        form =
            assembled.measurements.pregnancySummary
                |> getMeasurementValueFunc
                |> pregnancySummaryFormWithDefault form_

        ( apgarScoresSection, apgarScoresTasks ) =
            let
                ( apgarMeasurementsSection, apgarMeasurementsTasks ) =
                    if form.apgarScoresAvailable == Just True then
                        ( [ viewLabel language <| Translate.Minutes 1
                          , viewMeasurementInput language
                                form.apgarOneMin
                                (SetPregnancySummaryNumberInput
                                    (\value pregnancySummaryForm ->
                                        { pregnancySummaryForm
                                            | apgarOneMin = String.toFloat value
                                            , apgarDirty = True
                                        }
                                    )
                                )
                                "apgar one-min"
                                Translate.EmptyString
                          , viewLabel language <| Translate.Minutes 5
                          , viewMeasurementInput language
                                form.apgarFiveMin
                                (SetPregnancySummaryNumberInput
                                    (\value pregnancySummaryForm ->
                                        { pregnancySummaryForm
                                            | apgarFiveMin = String.toFloat value
                                            , apgarDirty = True
                                        }
                                    )
                                )
                                "apgar five-min"
                                Translate.EmptyString
                          ]
                        , [ maybeToBoolTask form.apgarOneMin, maybeToBoolTask form.apgarFiveMin ]
                        )

                    else
                        ( [], [] )
            in
            ( [ viewQuestionLabel language <| Translate.PregnancySummarySignQuestion ApgarScores
              , viewBoolInput
                    language
                    form.apgarScoresAvailable
                    (SetPregnancySummaryBoolInput
                        (\value pregnancySummaryForm ->
                            { pregnancySummaryForm
                                | apgarScoresAvailable = Just value
                                , apgarOneMin = Nothing
                                , apgarFiveMin = Nothing
                                , apgarDirty = True
                            }
                        )
                    )
                    ""
                    Nothing
              ]
                ++ apgarMeasurementsSection
            , form.apgarScoresAvailable :: apgarMeasurementsTasks
            )

        ( birthWeightSection, birthWeightTasks ) =
            birthWeightInputsAndTasks language
                form.birthWeight
                (SetPregnancySummaryNumberInput
                    (\value pregnancySummaryForm ->
                        { pregnancySummaryForm
                            | birthWeight = String.toFloat value |> Maybe.map WeightInGrm
                        }
                    )
                )

        ( birthLengthSection, birthLengthTasks ) =
            let
                ( lengthMeasurementSection, lengthMeasurementTasks ) =
                    if form.birthLengthAvailable == Just True then
                        let
                            birthLengthAsFloat =
                                Maybe.map (\(HeightInCm length) -> length)
                                    form.birthLength
                        in
                        ( [ viewMeasurementInput language
                                birthLengthAsFloat
                                (SetPregnancySummaryNumberInput
                                    (\value pregnancySummaryForm ->
                                        { pregnancySummaryForm
                                            | birthLength = String.toFloat value |> Maybe.map HeightInCm
                                            , birthLengthDirty = True
                                        }
                                    )
                                )
                                "birth-length"
                                Translate.UnitCentimeter
                          ]
                        , [ maybeToBoolTask form.birthLength ]
                        )

                    else
                        ( [], [] )
            in
            ( [ viewQuestionLabel language <| Translate.PregnancySummarySignQuestion BirthLength
              , viewBoolInput
                    language
                    form.birthLengthAvailable
                    (SetPregnancySummaryBoolInput
                        (\value pregnancySummaryForm ->
                            { pregnancySummaryForm
                                | birthLengthAvailable = Just value
                                , birthLength = Nothing
                                , birthLengthDirty = True
                            }
                        )
                    )
                    ""
                    Nothing
              ]
                ++ lengthMeasurementSection
            , form.birthLengthAvailable :: lengthMeasurementTasks
            )

        ( estimatedDueDateSection, estimatedDueDateTasks ) =
            let
                dateSelectorConfig =
                    { select = SetExpectedDateConcluded
                    , close = SetExpectedDateConcludedSelectorState Nothing
                    , dateFrom = Date.add Months -3 currentDate
                    , dateTo = Date.add Months 4 currentDate
                    , dateDefault = Nothing
                    }

                expectedDateConcludedForView =
                    Maybe.map formatDDMMYYYY form.expectedDateConcluded
                        |> Maybe.withDefault ""

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
            in
            ( [ viewQuestionLabel language Translate.DateConcludedEstimatedQuestion
              , div
                    [ class "form-input date"
                    , onClick <| SetExpectedDateConcludedSelectorState (Just dateSelectorConfig)
                    ]
                    [ text expectedDateConcludedForView ]
              , viewModal <| viewCalendarPopup language form.dateSelectorPopupState form.expectedDateConcluded
              ]
                ++ viewDatesDiff
            , [ maybeToBoolTask form.expectedDateConcluded ]
            )

        ( deliveryComplicationsSection, deliveryComplicationsTasks ) =
            let
                ( selectComplicationsHtml, selectComplicationsTasks ) =
                    if form.deliveryComplicationsPresent == Just True then
                        ( [ viewLabel language Translate.DeliveryComplicationsSelectionLabel
                          , viewCheckBoxMultipleSelectInput language
                                [ ComplicationGestationalDiabetes, ComplicationEmergencyCSection, ComplicationPreclampsia, ComplicationOther ]
                                [ ComplicationMaternalHemmorhage, ComplicationHiv, ComplicationMaternalDeath ]
                                (form.deliveryComplications |> Maybe.withDefault [])
                                Nothing
                                SetDeliveryComplication
                                Translate.DeliveryComplication
                          ]
                        , [ maybeToBoolTask form.deliveryComplications ]
                        )

                    else
                        ( [], [] )
            in
            ( [ viewQuestionLabel language Translate.DeliveryComplicationsPresentQuestion
              , viewBoolInput
                    language
                    form.deliveryComplicationsPresent
                    (SetPregnancySummaryBoolInput
                        (\value pregnancySummaryForm ->
                            { pregnancySummaryForm
                                | deliveryComplicationsPresent = Just value
                                , deliveryComplications = Nothing
                            }
                        )
                    )
                    ""
                    Nothing
              ]
                ++ selectComplicationsHtml
            , form.deliveryComplicationsPresent :: selectComplicationsTasks
            )

        ( birthDefectsSection, birthDefectsTasks ) =
            let
                ( selectDefectsHtml, selectDefectsTasks ) =
                    if form.birthDefectsPresent == Just True then
                        ( [ viewLabel language Translate.BirthDefectsSelectionLabel
                          , viewCheckBoxMultipleSelectInput language
                                [ DefectBirthInjury
                                , DefectCleftLipWithCleftPalate
                                , DefectCleftPalate
                                , DefectClubFoot
                                , DefectMacrocephaly
                                , DefectGastroschisis
                                , DefectHearingLoss
                                , DefectUndescendedTestes
                                ]
                                [ DefectHypospadias
                                , DefectInguinalHernia
                                , DefectMicrocephaly
                                , DefectNeuralTubes
                                , DefectDownSyndrome
                                , DefectCongenitalHeart
                                , DefectVentricalSeptal
                                , DefectPulmonaryValveAtresiaAndStenosis
                                ]
                                (Maybe.withDefault [] form.birthDefects)
                                Nothing
                                SetBirthDefect
                                Translate.BirthDefect
                          ]
                        , [ maybeToBoolTask form.birthDefects ]
                        )

                    else
                        ( [], [] )
            in
            ( [ viewQuestionLabel language Translate.BirthDefectsPresentQuestion
              , viewBoolInput
                    language
                    form.birthDefectsPresent
                    (SetPregnancySummaryBoolInput
                        (\value pregnancySummaryForm ->
                            { pregnancySummaryForm
                                | birthDefectsPresent = Just value
                                , birthDefects = Nothing
                            }
                        )
                    )
                    ""
                    Nothing
              ]
                ++ selectDefectsHtml
            , form.birthDefectsPresent :: selectDefectsTasks
            )

        tasks =
            estimatedDueDateTasks
                ++ apgarScoresTasks
                ++ birthWeightTasks
                ++ birthLengthTasks
                ++ deliveryComplicationsTasks
                ++ birthDefectsTasks

        ( tasksCompleted, totalTasks ) =
            resolveTasksCompletedFromTotal tasks

        disabled =
            tasksCompleted /= totalTasks
    in
    [ viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form pregnancy-summary" ] <|
                estimatedDueDateSection
                    ++ apgarScoresSection
                    ++ birthWeightSection
                    ++ birthLengthSection
                    ++ deliveryComplicationsSection
                    ++ birthDefectsSection
            ]
        , viewSaveAction language
            (SavePregnancySummary assembled.participant.person assembled.measurements.pregnancySummary)
            disabled
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
        measurements =
            assembled.measurements

        tasks =
            [ TaskSymptomsReview, TaskVitals ]

        activeTask =
            resolveActiveTask tasks data.activeTask

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
                |> List.map (\task -> ( task, dangerSignsTasksCompletedFromTotal currentDate assembled data task ))
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
                        |> viewSymptomsReviewForm language

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
                                TaskSymptomsReview ->
                                    SaveSymptomsReview personId measurements.symptomsReview nextTask

                                TaskVitals ->
                                    SaveVitals personId measurements.vitals nextTask

                        disabled =
                            tasksCompleted /= totalTasks
                    in
                    viewSaveAction language saveMsg disabled
                )
                activeTask
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue" ]
        [ div [ class "ui four column grid" ] <|
            List.map viewTask tasks
        ]
    , viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] (viewForm ++ [ actions ])
        ]
    ]


viewVitalsForm : Language -> NominalDate -> AssembledData -> VitalsForm -> Html Msg
viewVitalsForm language currentDate assembled form =
    let
        formConfig =
            generateVitalsFormConfig assembled
    in
    Measurement.View.viewVitalsForm language currentDate formConfig form


viewSymptomsReviewForm : Language -> SymptomsReviewForm -> List (Html Msg)
viewSymptomsReviewForm language form =
    let
        ( inputs, _ ) =
            symptomsReviewFormInputsAndTasks language form
    in
    [ div [ class "ui form symptoms-review" ]
        inputs
    ]


viewNutritionAssessmenContent :
    Language
    -> NominalDate
    -> Site
    -> ZScore.Model.Model
    -> AssembledData
    -> ModelIndexedDb
    -> NutritionAssessmentData
    -> List (Html Msg)
viewNutritionAssessmenContent language currentDate site zscores assembled db data =
    let
        measurements =
            assembled.measurements

        tasks =
            resolveNutritionAssessmentTasks assembled
                |> List.filter (expectNutritionAssessmentTask currentDate assembled)

        activeTask =
            resolveActiveTask tasks data.activeTask

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
            List.map (\task -> ( task, nutritionAssessmentTasksCompletedFromTotal currentDate zscores assembled data task )) tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        previousValuesSet =
            resolvePreviousValuesSetForChild currentDate site assembled.participant.person db

        headCircumferenceForm =
            getMeasurementValueFunc measurements.headCircumference
                |> headCircumferenceFormWithDefault data.headCircumferenceForm

        headCircumferenceZScore =
            if headCircumferenceForm.measurementNotTaken == Just True then
                Nothing

            else
                Maybe.andThen
                    (\headCircumference ->
                        Maybe.map
                            (\birthDate -> diffDays birthDate currentDate)
                            assembled.person.birthDate
                            |> Maybe.andThen
                                (\ageInDays ->
                                    zScoreHeadCircumferenceForAge zscores ageInDays assembled.person.gender (Centimetres headCircumference)
                                )
                    )
                    headCircumferenceForm.headCircumference

        viewForm =
            case activeTask of
                Just TaskHeight ->
                    getMeasurementValueFunc measurements.height
                        |> heightFormWithDefault data.heightForm
                        |> viewHeightForm language currentDate zscores assembled.person previousValuesSet.height SetHeight

                Just TaskHeadCircumference ->
                    viewHeadCircumferenceForm language headCircumferenceZScore previousValuesSet.headCircumference headCircumferenceForm

                Just TaskMuac ->
                    getMeasurementValueFunc measurements.muac
                        |> muacFormWithDefault data.muacForm
                        |> viewMuacForm language site previousValuesSet.muac SetMuac

                Just TaskNutrition ->
                    getMeasurementValueFunc measurements.nutrition
                        |> nutritionFormWithDefault data.nutritionForm
                        |> viewNutritionForm language SetNutritionSign

                Just TaskWeight ->
                    let
                        heightValue =
                            getMeasurementValueFunc assembled.measurements.height

                        showWeightForHeightZScore =
                            assembled.encounter.encounterType /= NewbornExam
                    in
                    getMeasurementValueFunc measurements.weight
                        |> weightFormWithDefault data.weightForm
                        |> viewWeightForm language currentDate zscores assembled.person heightValue previousValuesSet.weight showWeightForHeightZScore SetWeight

                Nothing ->
                    []

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
                activeTask
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui five column grid" ] <|
            List.map viewTask tasks
        ]
    , viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] (viewForm ++ [ actions ])
        ]
    ]


viewHeadCircumferenceForm :
    Language
    -> Maybe Float
    -> Maybe Float
    -> HeadCircumferenceForm
    -> List (Html Msg)
viewHeadCircumferenceForm language zscore previousValue form =
    let
        ( formForView, _ ) =
            headCircumferenceFormAndTasks language zscore previousValue form
    in
    formForView


viewImmunisationContent :
    Language
    -> NominalDate
    -> Site
    -> AssembledData
    -> ImmunisationData
    -> List (Html Msg)
viewImmunisationContent language currentDate site assembled data =
    let
        measurements =
            assembled.measurements

        tasks =
            List.filter (expectImmunisationTask currentDate site assembled) immunisationTasks

        activeTask =
            resolveActiveTask tasks data.activeTask

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

                        TaskDTPStandalone ->
                            ( "dtp-vaccine"
                            , isJust measurements.dtpStandaloneImmunisation
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
                    , text <| translate language (Translate.WellChildImmunisationTask site task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            List.map (\task -> ( task, immunisationTasksCompletedFromTotal language currentDate site assembled data task )) tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        ( formForView, fullScreen, allowSave ) =
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

                                    VaccineDTPStandalone ->
                                        measurements.dtpStandaloneImmunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.dtpStandaloneForm

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
                        ( viewVaccinationForm language currentDate site assembled vaccineType vaccinationForm
                        , False
                        , vaccinationForm.viewMode == ViewModeInitial
                        )
                    )
                |> Maybe.withDefault
                    ( viewVaccinationOverviewForm language currentDate site assembled.person assembled.vaccinationProgress
                    , True
                    , True
                    )

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
                                TaskBCG ->
                                    SaveBCGImmunisation personId measurements.bcgImmunisation nextTask

                                TaskDTP ->
                                    SaveDTPImmunisation personId measurements.dtpImmunisation nextTask

                                TaskDTPStandalone ->
                                    SaveDTPStandaloneImmunisation personId measurements.dtpStandaloneImmunisation nextTask

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
                activeTask
                |> Maybe.withDefault emptyNode
                |> showIf allowSave
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


viewVaccinationForm : Language -> NominalDate -> Site -> AssembledData -> WellChildVaccineType -> WellChildVaccinationForm -> Html Msg
viewVaccinationForm language currentDate site assembled vaccineType form =
    let
        ( contentByViewMode, _, _ ) =
            Pages.WellChild.Activity.Utils.vaccinationFormDynamicContentAndTasks language
                currentDate
                site
                assembled
                vaccineType
                form
    in
    div [ class "ui form vaccination" ]
        [ h2 [] [ text <| translate language <| Translate.WellChildImmunisationHeader vaccineType ]
        , div [ class "instructions" ] <|
            [ div [ class "header icon-label" ]
                [ i [ class "icon-open-book" ] []
                , div []
                    [ div [ class "description" ] [ text <| translate language <| Translate.WellChildImmunisationDescription site vaccineType ]
                    , div [ class "dosage" ] [ text <| translate language <| Translate.WellChildImmunisationDosage site vaccineType ]
                    ]
                ]
            , viewLabel language (Translate.WellChildImmunizationHistory site vaccineType)
            ]
                ++ contentByViewMode
        ]


viewVaccinationOverviewForm : Language -> NominalDate -> Site -> Person -> VaccinationProgressDict -> Html any
viewVaccinationOverviewForm language currentDate site child vaccinationProgress =
    div [ class "ui form vaccination-overview" ] <|
        viewVaccinationOverview language currentDate site child vaccinationProgress


viewVaccinationOverview :
    Language
    -> NominalDate
    -> Site
    -> Person
    -> VaccinationProgressDict
    -> List (Html any)
viewVaccinationOverview language currentDate site child vaccinationProgress =
    let
        entriesHeading =
            div [ class "heading vaccination" ]
                [ div [ class "name" ] [ text <| translate language Translate.Immunisation ]
                , div [ class "date" ] [ text <| translate language Translate.DateReceived ]
                , div [ class "next-due" ] [ text <| translate language Translate.NextDue ]
                , div [ class "status" ] [ text <| translate language Translate.StatusLabel ]
                ]

        futureVaccinationsData =
            generateFutureVaccinationsData currentDate site child.birthDate child.gender False vaccinationProgress
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
                    Maybe.map formatDDMMYYYY nextDue
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
                        |> Maybe.withDefault ( StatusCompleted, "completed" )
            in
            div [ class "entry vaccination" ]
                [ div [ class "cell name" ] [ text <| translate language <| Translate.VaccineType site (WellChildVaccine vaccineType) ]
                , Dict.values doses
                    |> List.sortWith Date.compare
                    |> List.map (formatDDMMYYYY >> text >> List.singleton >> p [])
                    |> div [ class "cell date" ]
                , div [ classList [ ( "cell next-due ", True ), ( "red", status == StatusBehind ) ] ]
                    [ text nextDueText ]
                , div [ class <| "cell status " ++ statusClass ]
                    [ text <| translate language <| Translate.VaccinationStatus status ]
                ]
    in
    entriesHeading :: entries


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
    [ viewTasksCount language tasksCompleted totalTasks
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
    ( List.concatMap Tuple.first persentedECDSignsData
    , List.map Tuple.second persentedECDSignsData
    )


viewMedicationContent :
    Language
    -> NominalDate
    -> Site
    -> AssembledData
    -> MedicationData
    -> List (Html Msg)
viewMedicationContent language currentDate site assembled data =
    let
        measurements =
            assembled.measurements

        tasks =
            medicationTasks
                |> List.filter (expectMedicationTask currentDate site assembled)

        activeTask =
            resolveActiveTask tasks data.activeTask

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
                |> List.map (\task -> ( task, medicationTasksCompletedFromTotal currentDate site assembled data task ))
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just TaskAlbendazole ->
                    measurements.albendazole
                        |> getMeasurementValueFunc
                        |> medicationAdministrationFormWithDefault data.albendazoleForm
                        |> viewMedicationAdministrationForm language
                            currentDate
                            assembled.person
                            (albendazoleAdministrationFormConfig site)

                Just TaskMebendezole ->
                    measurements.mebendezole
                        |> getMeasurementValueFunc
                        |> medicationAdministrationFormWithDefault data.mebendezoleForm
                        |> viewMedicationAdministrationForm language
                            currentDate
                            assembled.person
                            (mebendezoleAdministrationFormConfig site)

                Just TaskVitaminA ->
                    measurements.vitaminA
                        |> getMeasurementValueFunc
                        |> medicationAdministrationFormWithDefault data.vitaminAForm
                        |> viewMedicationAdministrationForm language
                            currentDate
                            assembled.person
                            (vitaminAAdministrationFormConfig site)

                Nothing ->
                    []

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
                activeTask
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue" ]
        [ div [ class "ui four column grid" ] <|
            List.map viewTask tasks
        ]
    , viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] (viewForm ++ [ actions ])
        ]
    ]


viewNextStepsContent :
    Language
    -> NominalDate
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> AssembledData
    -> ModelIndexedDb
    -> NextStepsData
    -> List (Html Msg)
viewNextStepsContent language currentDate zscores site features assembled db data =
    let
        isChw =
            assembled.encounter.encounterType /= PediatricCare

        measurements =
            assembled.measurements

        tasks =
            List.filter (expectNextStepsTask currentDate zscores site features isChw assembled db) nextStepsTasks

        activeTask =
            resolveActiveTask tasks data.activeTask

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
                        |> viewContributingFactorsForm language SetContributingFactorsSign
                        |> List.singleton

                Just TaskHealthEducation ->
                    measurements.healthEducation
                        |> getMeasurementValueFunc
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> viewHealthEducationForm language
                            SetProvidedEducationForDiagnosis
                            SetReasonForNotProvidingHealthEducation
                        |> List.singleton

                Just TaskFollowUp ->
                    measurements.followUp
                        |> getMeasurementValueFunc
                        |> nutritionFollowUpFormWithDefault data.followUpForm
                        |> viewNutritionFollowUpForm language SetFollowUpOption
                        |> List.singleton

                Just TaskSendToHC ->
                    let
                        viewFormFunc =
                            if isChw then
                                viewSendToHealthCenterForm language
                                    currentDate
                                    SetReferToHealthCenter
                                    SetReasonForNonReferral
                                    SetHandReferralForm
                                    Nothing

                            else
                                viewReferToProgramForm language
                                    SetEnrollToNutritionProgram
                                    SetReferToNutritionProgram
                    in
                    getMeasurementValueFunc measurements.sendToHC
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> viewFormFunc
                        |> List.singleton

                Just TaskNextVisit ->
                    viewNextVisitForm language currentDate site assembled nextVisitForm
                        |> List.singleton

                Nothing ->
                    []

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
                                            resolveNextVisitDates currentDate site assembled nextVisitForm

                                        asapImmunisationDate =
                                            generateASAPImmunisationDate currentDate site assembled
                                    in
                                    SaveNextVisit personId
                                        measurements.nextVisit
                                        nextDateForImmunisationVisit
                                        nextDateForPediatricVisit
                                        asapImmunisationDate
                                        nextTask

                        disabled =
                            if task == TaskNextVisit then
                                False

                            else
                                tasksCompleted /= totalTasks
                    in
                    viewSaveAction language saveMsg disabled
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
        [ div [ class "full content" ] (viewForm ++ [ actions ])
        ]
    ]


viewNextVisitForm : Language -> NominalDate -> Site -> AssembledData -> NextVisitForm -> Html Msg
viewNextVisitForm language currentDate site assembled form =
    let
        isChw =
            assembled.encounter.encounterType /= PediatricCare

        ( nextDateForImmunisationVisit, nextDateForPediatricVisit ) =
            resolveNextVisitDates currentDate site assembled form

        viewSection value label =
            Maybe.map
                (\date ->
                    [ viewLabel language label
                    , div [ class "date" ] [ text <| formatDDMMYYYY date ]
                    ]
                )
                value
                |> Maybe.withDefault []
    in
    div [ class "ui form next-visit" ] <|
        viewSection nextDateForImmunisationVisit (Translate.NextImmunisationVisit isChw)
            ++ viewSection nextDateForPediatricVisit (Translate.NextPediatricVisit isChw)


{-| We use saved values. If not found, fallback to logcal generation of next visit dates.
-}
resolveNextVisitDates :
    NominalDate
    -> Site
    -> AssembledData
    -> NextVisitForm
    -> ( Maybe NominalDate, Maybe NominalDate )
resolveNextVisitDates currentDate site assembled form =
    let
        ( nextDateForImmunisationVisit, nextDateForPediatricVisit ) =
            generateNextVisitDates currentDate site assembled
    in
    ( Maybe.Extra.or form.immunisationDate nextDateForImmunisationVisit
    , Maybe.Extra.or form.pediatricVisitDate nextDateForPediatricVisit
    )


viewPhotoContent : Language -> AssembledData -> PhotoForm -> List (Html Msg)
viewPhotoContent language assembled form =
    let
        -- If we have a photo that we've just taken, but not saved, that is in
        -- `data.url`. We show that if we have it. Otherwise, we'll show the saved
        -- measurement, if we have that.
        ( displayPhoto, saveMsg, disabled ) =
            case form.url of
                Just url ->
                    let
                        photoId =
                            Maybe.map Tuple.first assembled.measurements.photo
                    in
                    ( Just url
                    , SavePhoto assembled.participant.person photoId url
                    , False
                    )

                Nothing ->
                    ( getMeasurementValueFunc assembled.measurements.photo
                    , NoOp
                    , True
                    )

        totalTasks =
            1

        tasksCompleted =
            taskCompleted displayPhoto
    in
    [ viewTasksCount language tasksCompleted totalTasks
    , div [ class "ui full segment" ]
        [ div [ class "full content" ] <|
            viewPhotoForm language displayPhoto DropZoneComplete
        , viewSaveAction language saveMsg disabled
        ]
    ]


viewNCDAContent :
    Language
    -> NominalDate
    -> Site
    -> AssembledData
    -> NCDAData
    -> ModelIndexedDb
    -> List (Html Msg)
viewNCDAContent language currentDate site assembled data db =
    let
        form =
            getMeasurementValueFunc assembled.measurements.ncda
                |> ncdaFormWithDefault data.form

        personId =
            assembled.participant.person

        config =
            { atHealthCenter = True
            , showTasksTray = True
            , pregnancySummary = getNewbornExamPregnancySummary personId db
            , ncdaNeverFilled = resolveNCDANeverFilled currentDate personId db
            , ncdaNotFilledAfterAgeOfSixMonths = resolveNCDANotFilledAfterAgeOfSixMonths currentDate personId assembled.person db
            , setUpdateANCVisitsMsg = SetUpdateANCVisits
            , toggleANCVisitDateMsg = ToggleANCVisitDate
            , setBoolInputMsg = SetNCDABoolInput
            , setBirthWeightMsg = SetBirthWeight
            , setChildReceivesVitaminAMsg = SetChildReceivesVitaminA
            , setStuntingLevelMsg = SetStuntingLevel
            , setWeightMsg = SetWeightForNCDA
            , setMuacMsg = SetMuacForNCDA
            , setStepMsg = SetNCDAFormStep
            , setHelperStateMsg = SetNCDAHelperState
            , saveMsg = SaveNCDA personId assembled.measurements.ncda
            }
    in
    Measurement.View.viewNCDAContent language
        currentDate
        site
        personId
        assembled.person
        config
        data.helperState
        form
        db


viewHomeVisitContent :
    Language
    -> AssembledData
    -> HomeVisitData
    -> ModelIndexedDb
    -> List (Html Msg)
viewHomeVisitContent language assembled data db =
    let
        tasks =
            [ TaskFeeding, TaskCaring, TaskHygiene, TaskFoodSecurity ]

        activeTask =
            resolveActiveTask tasks data.activeTask

        viewTask task =
            let
                iconClass =
                    case task of
                        TaskFeeding ->
                            "feeding"

                        TaskCaring ->
                            "caring"

                        TaskHygiene ->
                            "hygiene"

                        TaskFoodSecurity ->
                            "food-security"

                isActive =
                    activeTask == Just task

                isCompleted =
                    isTaskCompleted tasksCompletedFromTotalDict task

                attributes =
                    classList
                        [ ( "link-section", True )
                        , ( "active", isActive )
                        , ( "completed", not isActive && isCompleted )
                        ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveHomeVisitTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.WellChildHomeVisitTask task)
                    ]
                ]

        inputsAndTasksDict =
            List.map
                (\task ->
                    let
                        inputsAndTasks =
                            case task of
                                TaskFeeding ->
                                    assembled.measurements.feeding
                                        |> getMeasurementValueFunc
                                        |> nutritionFeedingFormWithDefault data.feedingForm
                                        |> nutritionFeedingInputsAndTasks language
                                            assembled.participant.person
                                            SetFeedingBoolInput
                                            SetNutritionSupplementType
                                            SetSachetsPerDay
                                            db

                                TaskCaring ->
                                    assembled.measurements.caring
                                        |> getMeasurementValueFunc
                                        |> nutritionCaringFormWithDefault data.caringForm
                                        |> nutritionCaringInputsAndTasks language
                                            SetParentsAliveAndHealthy
                                            SetNutritionCaringOption
                                            SetChildClean

                                TaskHygiene ->
                                    assembled.measurements.hygiene
                                        |> getMeasurementValueFunc
                                        |> nutritionHygieneFormWithDefault data.hygieneForm
                                        |> nutritionHygieneInputsAndTasks language
                                            SetHygieneBoolInput
                                            SetMainWaterSource
                                            SetWaterPreparationOption

                                TaskFoodSecurity ->
                                    assembled.measurements.foodSecurity
                                        |> getMeasurementValueFunc
                                        |> nutritionFoodSecurityFormWithDefault data.foodSecurityForm
                                        |> nutritionFoodSecurityInputsAndTasks language
                                            SetFoodSecurityBoolInput
                                            SetMainIncomeSource
                    in
                    ( task, inputsAndTasks )
                )
                tasks
                |> Dict.fromList

        tasksCompletedFromTotalDict =
            Dict.map
                (\_ ( _, tasks_ ) ->
                    ( List.map taskCompleted tasks_
                        |> List.sum
                    , List.length tasks_
                    )
                )
                inputsAndTasksDict

        ( viewForm, tasksCompleted, totalTasks ) =
            Maybe.map
                (\task ->
                    let
                        html =
                            Dict.get task inputsAndTasksDict
                                |> Maybe.map Tuple.first
                                |> Maybe.withDefault []

                        ( completed, total ) =
                            Dict.get task tasksCompletedFromTotalDict
                                |> Maybe.withDefault ( 0, 0 )
                    in
                    ( html, completed, total )
                )
                activeTask
                |> Maybe.withDefault ( [], 0, 0 )

        actions =
            Maybe.map
                (\task ->
                    let
                        measurements =
                            assembled.measurements

                        personId =
                            assembled.participant.person

                        nextTask =
                            resolveNextTask task tasksCompletedFromTotalDict tasks

                        saveMsg =
                            case task of
                                TaskFeeding ->
                                    SaveFeeding personId measurements.feeding nextTask

                                TaskCaring ->
                                    SaveNutritionCaring personId measurements.caring nextTask

                                TaskHygiene ->
                                    SaveHygiene personId measurements.hygiene nextTask

                                TaskFoodSecurity ->
                                    SaveFoodSecurity personId measurements.foodSecurity nextTask

                        disabled =
                            tasksCompleted /= totalTasks
                    in
                    viewSaveAction language saveMsg disabled
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
        [ div [ class "full content" ] (viewForm ++ [ actions ])
        ]
    ]
