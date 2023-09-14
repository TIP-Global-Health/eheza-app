module Pages.Prenatal.Activity.View exposing (view, warningPopup)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils
    exposing
        ( getHeightValue
        , getMeasurementValueFunc
        , muacValueFunc
        , pregnancyTestResultToString
        , weightValueFunc
        )
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model
import Backend.PrenatalActivity.Model exposing (PrenatalActivity(..))
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Date exposing (Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import EverySet exposing (EverySet)
import Gizra.Html exposing (divKeyed, emptyNode, keyed, keyedDivKeyed, showIf, showMaybe)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Decoder exposing (decodeDropZoneFile)
import Measurement.Model
    exposing
        ( ContentAndTasksForPerformedLaboratoryTestConfig
        , ContentAndTasksLaboratoryTestInitialConfig
        , CorePhysicalExamForm
        , InvokationModule(..)
        , LaboratoryTask(..)
        , OutsideCareStep(..)
        , VaccinationFormViewMode(..)
        , VitalsForm
        , VitalsFormMode(..)
        )
import Measurement.Utils
    exposing
        ( corePhysicalExamFormWithDefault
        , emptyContentAndTasksForPerformedLaboratoryTestConfig
        , emptyContentAndTasksLaboratoryTestInitialConfig
        , familyPlanningFormWithDefault
        , hivTestFormWithDefault
        , laboratoryTaskIconClass
        , malariaTestFormWithDefault
        , nonRDTFormWithDefault
        , outsideCareFormInputsAndTasks
        , outsideCareFormWithDefault
        , partnerHIVTestFormWithDefault
        , randomBloodSugarFormWithDefault
        , urineDipstickFormWithDefault
        , vaccinationFormWithDefault
        , viewHIVTestForm
        , viewMalariaTestForm
        , viewNonRDTForm
        , viewNonRDTFormCheckKnownAsPositive
        , viewPartnerHIVTestForm
        , viewRandomBloodSugarForm
        , viewUrineDipstickForm
        , vitalsFormWithDefault
        )
import Measurement.View
    exposing
        ( viewFamilyPlanningForm
        , viewFamilyPlanningInput
        )
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Prenatal.Activity.Model exposing (..)
import Pages.Prenatal.Activity.Types exposing (..)
import Pages.Prenatal.Activity.Utils exposing (..)
import Pages.Prenatal.Encounter.Utils exposing (..)
import Pages.Prenatal.Encounter.View exposing (generateActivityData, viewMotherAndMeasurements)
import Pages.Prenatal.Model exposing (..)
import Pages.Prenatal.Utils exposing (..)
import Pages.Prenatal.View
    exposing
        ( customWarningPopup
        , viewMalariaPreventionContent
        , viewMedicationDistributionForm
        , viewPauseEncounterButton
        )
import Pages.Utils
    exposing
        ( customSaveButton
        , isTaskCompleted
        , maybeToBoolTask
        , saveButton
        , taskCompleted
        , tasksBarId
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewConditionalAlert
        , viewCustomLabel
        , viewCustomSelectListInput
        , viewInstructionsLabel
        , viewLabel
        , viewMeasurementInput
        , viewPhotoThumbFromImageUrl
        , viewPreviousMeasurement
        , viewQuestionLabel
        , viewRedAlertForBool
        , viewRedAlertForSelect
        , viewSaveAction
        , viewSelectListInput
        , viewYellowAlertForSelect
        )
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
            warningPopup language currentDate isChw assembled.encounter.diagnoses SetWarningPopupState model.warningPopupState
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
        , span
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
            ++ viewActivity language currentDate isChw activity assembled db model


warningPopup :
    Language
    -> NominalDate
    -> Bool
    -> EverySet PrenatalDiagnosis
    -> (Maybe (WarningPopupType msg) -> msg)
    -> Maybe (WarningPopupType msg)
    -> Maybe (Html msg)
warningPopup language currentDate isChw encounterDiagnoses setStateMsg state =
    Maybe.andThen
        (\popupType ->
            let
                data =
                    case popupType of
                        WarningPopupRegular ->
                            let
                                nonUrgentDiagnoses =
                                    EverySet.toList encounterDiagnoses |> filterNonUrgentDiagnoses
                            in
                            if List.isEmpty nonUrgentDiagnoses then
                                Nothing

                            else
                                let
                                    ( undetermined, determined ) =
                                        List.partition (\diagnosis -> List.member diagnosis undeterminedPostpartumDiagnoses) nonUrgentDiagnoses

                                    top =
                                        case determined of
                                            [] ->
                                                emptyNode

                                            [ diagnosis ] ->
                                                p [] [ text <| translate language <| Translate.PrenatalDiagnosisNonUrgentMessage diagnosis ]

                                            _ ->
                                                List.map
                                                    (Translate.PrenatalDiagnosisNonUrgentMessage
                                                        >> translate language
                                                        >> text
                                                        >> List.singleton
                                                        >> li []
                                                    )
                                                    determined
                                                    |> ul []

                                    bottom =
                                        if List.isEmpty undetermined then
                                            emptyNode

                                        else
                                            let
                                                undeterminedDiagnoses =
                                                    List.map (Translate.PrenatalDiagnosisNonUrgentMessage >> translate language) undetermined
                                                        |> String.join ", "
                                            in
                                            div [ class "bottom-message" ]
                                                [ p []
                                                    [ text <| translate language Translate.UndeterminedDiagnoses
                                                    , text " - "
                                                    , text undeterminedDiagnoses
                                                    ]
                                                , p [] [ text <| translate language Translate.FollowPostpartumProtocols ]
                                                ]
                                in
                                Just <|
                                    ( top
                                    , bottom
                                    , setStateMsg Nothing
                                    )

                        WarningPopupUrgent ( top, bottom ) ->
                            Just <|
                                ( p [] [ text top ]
                                , p [] [ text bottom ]
                                , setStateMsg Nothing
                                )

                        WarningPopupTuberculosis ->
                            Just <|
                                ( p [] [ text <| translate language Translate.TuberculosisWarning ]
                                , p [] [ text <| translate language Translate.TuberculosisInstructions ]
                                , setStateMsg Nothing
                                )

                        WarningPopupMentalHealth mentalHealthAction ->
                            Just <|
                                ( p [] [ text <| translate language Translate.PrenatalMentalHealthWarningPopupMessage ]
                                , p [] [ text <| translate language Translate.PrenatalMentalHealthWarningPopupInstructions ]
                                , mentalHealthAction
                                )

                        WarningPopupTreatmentReview treatmentReviewAtion ->
                            Just <|
                                ( p [] [ text <| translate language Translate.TreatmentReviewWarningPopupMessage ]
                                , p [] [ text <| translate language Translate.TreatmentReviewWarningPopupInstructions ]
                                , treatmentReviewAtion
                                )

                        WarningPopupVitaminA treatmentReviewAtion ->
                            Just <|
                                ( p [] [ text <| translate language Translate.VitaminAWarningPopupMessage ]
                                , emptyNode
                                , treatmentReviewAtion
                                )
            in
            Maybe.map (customWarningPopup language) data
        )
        state


viewActivity : Language -> NominalDate -> Bool -> PrenatalActivity -> AssembledData -> ModelIndexedDb -> Model -> List (Html Msg)
viewActivity language currentDate isChw activity assembled db model =
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
            viewLaboratoryContent language currentDate assembled model.laboratoryData

        Backend.PrenatalActivity.Model.HealthEducation ->
            viewHealthEducationContent language currentDate assembled model.healthEducationData

        BirthPlan ->
            viewBirthPlanContent language currentDate assembled model.birthPlanData

        Backend.PrenatalActivity.Model.MalariaPrevention ->
            viewMalariaPreventionContent language
                currentDate
                assembled
                SetMalariaPreventionBoolInput
                SaveMalariaPrevention
                model.malariaPreventionData

        Backend.PrenatalActivity.Model.Medication ->
            viewMedicationContent language currentDate assembled model.medicationData

        SymptomReview ->
            viewSymptomReviewContent language currentDate assembled model.symptomReviewData

        PrenatalTreatmentReview ->
            viewTreatmentReviewContent language currentDate assembled model.treatmentReviewData

        MaternalMentalHealth ->
            viewMentalHealthContent language currentDate assembled model.mentalHealthData

        PrenatalImmunisation ->
            viewImmunisationContent language currentDate assembled model.immunisationData

        Backend.PrenatalActivity.Model.Breastfeeding ->
            viewBreastfeedingContent language currentDate assembled model.breastfeedingData

        PostpartumTreatmentReview ->
            viewPostpartumTreatmentReviewContent language currentDate assembled model.postpartumTreatmentReviewData

        SpecialityCare ->
            viewSpecialityCareContent language currentDate assembled model.specialityCareData

        NextSteps ->
            viewNextStepsContent language currentDate isChw assembled model.nextStepsData

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

        chwLmpConfirmationSection lmpValueByChw =
            [ viewCustomLabel language Translate.LmpDateConfirmationLabel "." "label"
            , viewLabel language Translate.LmpLabel
            , p [ class "chw-lmp" ] [ text <| formatDDMMYYYY lmpValueByChw.date ]
            , viewQuestionLabel language Translate.LmpDateConfirmationQuestion
            , viewBoolInput language
                form.chwLmpConfirmation
                (SetConfirmLmpDate lmpValueByChw)
                "confirm-lmp"
                Nothing
            ]

        ( newLmpInputSection, newLmpInputTasksCompleted, newLmpInputTasksTotal ) =
            let
                ( derivedSection, derivedTasksCompleted, derivedTasksTotal ) =
                    if form.lmpDateConfident == Just False then
                        ( [ viewQuestionLabel language Translate.LmpDateNotConfidentQuestion
                          , viewCheckBoxSelectInput language
                                [ ReasonIrregularMenses
                                , ReasonOnFamilyPlanningMethod
                                , ReasonCanNotRememberDates
                                ]
                                []
                                form.lmpDateNotConfidentReason
                                SetLmpDateNotConfidentReason
                                Translate.LmpDateNotConfidentReason
                          ]
                        , taskCompleted form.lmpDateNotConfidentReason
                        , 1
                        )

                    else
                        ( [], 0, 0 )
            in
            ( let
                lmpRangeInput =
                    viewSelectListInput language
                        form.lmpRange
                        [ Pages.Prenatal.Activity.Types.OneMonth
                        , Pages.Prenatal.Activity.Types.ThreeMonths
                        , SixMonthsOrMore
                        ]
                        lmpRangeToString
                        SetLmpRange
                        Translate.LmpRange
                        "select"

                lmpDateInput =
                    let
                        lmpDateAction =
                            Maybe.map
                                (\range ->
                                    let
                                        dateFrom =
                                            case range of
                                                Pages.Prenatal.Activity.Types.OneMonth ->
                                                    Date.add Months -1 currentDate

                                                Pages.Prenatal.Activity.Types.ThreeMonths ->
                                                    Date.add Months -3 currentDate

                                                SixMonthsOrMore ->
                                                    Date.add Months -36 currentDate

                                        dateSelectorConfig =
                                            { select = SetLmpDate
                                            , close = SetLmpDateSelectorState Nothing
                                            , dateFrom = dateFrom
                                            , dateTo = currentDate
                                            , dateDefault = Just dateFrom
                                            }
                                    in
                                    [ onClick <| SetLmpDateSelectorState (Just dateSelectorConfig) ]
                                )
                                form.lmpRange
                                |> Maybe.withDefault []

                        lmpdDateForView =
                            Maybe.map formatDDMMYYYY form.lmpDate
                                |> Maybe.withDefault ""
                    in
                    div (class "form-input date" :: lmpDateAction)
                        [ text lmpdDateForView ]
              in
              [ viewQuestionLabel language Translate.LmpRangeHeader
              , lmpRangeInput
              , viewLabel language Translate.LmpDateHeader
              , lmpDateInput
              , viewQuestionLabel language Translate.LmpDateConfidentHeader
              , viewBoolInput language form.lmpDateConfident SetLmpDateConfident "is-confident" Nothing
              , viewModal <| viewCalendarPopup language form.dateSelectorPopupState form.lmpDate
              ]
                ++ derivedSection
            , taskCompleted form.lmpDate
                + taskCompleted form.lmpDateConfident
                + derivedTasksCompleted
            , 2 + derivedTasksTotal
            )

        ( inputs, tasksCompleted, totalTasks ) =
            if assembled.encounter.encounterType == NurseEncounter then
                let
                    lmpValueTakenByChw =
                        List.head assembled.chwPreviousMeasurementsWithDates
                            |> Maybe.andThen
                                (\( _, _, measurements ) ->
                                    getLmpValue measurements
                                )
                in
                Maybe.map
                    (\lmpValueByChw ->
                        let
                            chwLmpConfirmationTasksCompleted =
                                taskCompleted form.chwLmpConfirmation
                        in
                        if form.chwLmpConfirmation == Just False then
                            ( chwLmpConfirmationSection lmpValueByChw ++ newLmpInputSection
                            , chwLmpConfirmationTasksCompleted + newLmpInputTasksCompleted
                            , 1 + newLmpInputTasksTotal
                            )

                        else
                            ( chwLmpConfirmationSection lmpValueByChw
                            , chwLmpConfirmationTasksCompleted
                            , 1
                            )
                    )
                    lmpValueTakenByChw
                    |> Maybe.withDefault
                        ( newLmpInputSection
                        , newLmpInputTasksCompleted
                        , newLmpInputTasksTotal
                        )

            else
                ( newLmpInputSection
                , newLmpInputTasksCompleted
                , newLmpInputTasksTotal
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
viewHistoryContent language currentDate assembled data =
    let
        viewTask task =
            let
                isActive =
                    activeTask == Just task

                isCompleted =
                    historyTaskCompleted assembled task

                iconClass =
                    case task of
                        Obstetric ->
                            "obstetric"

                        Medical ->
                            "medical"

                        Social ->
                            "social"

                        OutsideCare ->
                            "outside-care"

                navigationAction =
                    if isActive then
                        []

                    else
                        [ onClick <| SetActiveHistoryTask task ]

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
                    , text <| translate language (Translate.HistoryTask task)
                    ]
                ]

        tasks =
            resolveHistoryTasks assembled

        tasksCompletedFromTotalDict =
            List.map
                (\task ->
                    case task of
                        Obstetric ->
                            ( Obstetric
                            , case data.obstetricHistoryStep of
                                ObstetricHistoryFirstStep ->
                                    ( Maybe.Extra.values obstetricFormFirstStepTasks
                                        |> List.length
                                    , List.length obstetricFormFirstStepTasks
                                    )

                                ObstetricHistorySecondStep ->
                                    ( Maybe.Extra.values obstetricFormSecondStepTasks
                                        |> List.length
                                    , List.length obstetricFormSecondStepTasks
                                    )
                            )

                        Medical ->
                            ( Medical
                            , ( Maybe.Extra.values medicalFormTasks
                                    |> List.length
                              , List.length medicalFormTasks
                              )
                            )

                        Social ->
                            ( Social
                            , ( Maybe.Extra.values socialFormTasks
                                    |> List.length
                              , List.length socialFormTasks
                              )
                            )

                        OutsideCare ->
                            ( OutsideCare
                            , ( Maybe.Extra.values outsideCareTasks
                                    |> List.length
                              , List.length outsideCareTasks
                              )
                            )
                )
                tasks
                |> Dict.fromList

        activeTask =
            Maybe.map
                (\task ->
                    if List.member task tasks then
                        Just task

                    else
                        List.head tasks
                )
                data.activeTask
                |> Maybe.withDefault (List.head tasks)

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        ( obstetricFormFirstStepInputs, obstetricFormFirstStepTasks ) =
            getMeasurementValueFunc assembled.measurements.obstetricHistory
                |> obstetricHistoryFormWithDefault data.obstetricFormFirstStep
                |> obstetricFormFirstStepInputsAndTasks language currentDate assembled

        ( obstetricFormSecondStepInputs, obstetricFormSecondStepTasks ) =
            getMeasurementValueFunc assembled.measurements.obstetricHistoryStep2
                |> obstetricHistoryStep2FormWithDefault data.obstetricFormSecondStep
                |> obstetricFormSecondStepInputsAndTasks language currentDate assembled

        ( medicalFormInputs, medicalFormTasks ) =
            assembled.measurements.medicalHistory
                |> getMeasurementValueFunc
                |> medicalHistoryFormWithDefault data.medicalForm
                |> medicalFormInputsAndTasks language currentDate assembled

        ( socialFormInputs, socialFormTasks ) =
            assembled.measurements.socialHistory
                |> getMeasurementValueFunc
                |> socialHistoryFormWithDefault data.socialForm
                |> socialFormInputsAndTasks language currentDate assembled

        outsideCareForm =
            assembled.measurements.outsideCare
                |> getMeasurementValueFunc
                |> outsideCareFormWithDefault data.outsideCareForm

        ( outsideCareInputs, outsideCareTasks ) =
            case outsideCareForm.step of
                OutsideCareStepDiagnoses ->
                    ( outsideCareInputsStep1, outsideCareTasksStep1 )

                OutsideCareStepMedications ->
                    ( outsideCareInputsStep2, outsideCareTasksStep2 )

        outsideCareConfig =
            { setBoolInputMsg = SetOutsideCareSignBoolInput
            , setDiagnosisMsg = SetOutsideCareDiagnosis
            , setMalariaMedicationMsg = SetOutsideCareMalariaMedication
            , setHypertensionMedicationMsg = SetOutsideCareHypertensionMedication
            , setSyphilisMedicationMsg = SetOutsideCareSyphilisMedication
            , setAnemiaMedicationMsg = SetOutsideCareAnemiaMedication
            , setHIVMedicationMsg = SetOutsideCareHIVMedication
            , malariaDiagnoses = [ DiagnosisMalaria ]
            , hypertensionDiagnoses =
                [ DiagnosisGestationalHypertensionImmediate
                , DiagnosisChronicHypertensionImmediate
                , DiagnosisModeratePreeclampsiaInitialPhase
                ]
            , syphilisDiagnoses = [ DiagnosisSyphilis ]
            , anemiaDiagnoses = [ DiagnosisModerateAnemia ]
            , hivDiagnoses = [ DiagnosisHIV ]
            , malariaHeaderTransId = Translate.PrenatalDiagnosis DiagnosisMalaria
            , resolveHypertensionHeaderTransId =
                \diagnoses ->
                    if List.member DiagnosisModeratePreeclampsiaInitialPhase diagnoses then
                        Translate.ModeratePreeclampsia

                    else
                        Translate.Hypertension
            , syphilisHeaderTransId = Translate.PrenatalDiagnosis DiagnosisSyphilis
            , anemiaHeaderTransId = Translate.PrenatalDiagnosis DiagnosisModerateAnemia
            , hivHeaderTransId = Translate.PrenatalDiagnosis DiagnosisHIV
            , diagnosesLeftColumn = outsideCareDiagnosesLeftColumn
            , diagnosesRightColumn = outsideCareDiagnosesRightColumn
            , otherDiagnosis = DiagnosisOther
            , diagnosisTransId = Translate.PrenatalDiagnosis
            }

        ( outsideCareInputsStep1, outsideCareTasksStep1 ) =
            outsideCareFormInputsAndTasks language outsideCareConfig OutsideCareStepDiagnoses outsideCareForm

        ( outsideCareInputsStep2, outsideCareTasksStep2 ) =
            outsideCareFormInputsAndTasks language outsideCareConfig OutsideCareStepMedications outsideCareForm

        viewForm =
            case activeTask of
                Just Obstetric ->
                    case data.obstetricHistoryStep of
                        ObstetricHistoryFirstStep ->
                            div [ class "form history obstetric first" ]
                                obstetricFormFirstStepInputs

                        ObstetricHistorySecondStep ->
                            div [ class "form history obstetric second" ]
                                obstetricFormSecondStepInputs

                Just Medical ->
                    div [ class "form history medical" ]
                        medicalFormInputs

                Just Social ->
                    div [ class "form history social" ]
                        socialFormInputs

                Just OutsideCare ->
                    div [ class "ui form history outside-care" ]
                        outsideCareInputs

                Nothing ->
                    emptyNode

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
                            saveButtonActive =
                                tasksCompleted == totalTasks

                            buttons =
                                case task of
                                    Obstetric ->
                                        case data.obstetricHistoryStep of
                                            ObstetricHistoryFirstStep ->
                                                [ customSaveButton language
                                                    saveButtonActive
                                                    (SaveOBHistoryStep1
                                                        assembled.participant.person
                                                        assembled.measurements.obstetricHistory
                                                    )
                                                    Translate.SaveAndNext
                                                ]

                                            ObstetricHistorySecondStep ->
                                                [ button
                                                    [ class "ui fluid primary button"
                                                    , onClick BackToOBHistoryStep1
                                                    ]
                                                    [ text <| ("< " ++ translate language Translate.Back) ]
                                                , saveButton language
                                                    saveButtonActive
                                                    (SaveOBHistoryStep2
                                                        assembled.participant.person
                                                        assembled.measurements.obstetricHistoryStep2
                                                        nextTask
                                                    )
                                                ]

                                    Medical ->
                                        [ saveButton language
                                            saveButtonActive
                                            (SaveMedicalHistory
                                                assembled.participant.person
                                                assembled.measurements.medicalHistory
                                                nextTask
                                            )
                                        ]

                                    Social ->
                                        [ saveButton language
                                            saveButtonActive
                                            (SaveSocialHistory
                                                assembled.participant.person
                                                assembled.measurements.socialHistory
                                                nextTask
                                            )
                                        ]

                                    OutsideCare ->
                                        let
                                            saveAction =
                                                SaveOutsideCare assembled.participant.person assembled.measurements.outsideCare nextTask
                                        in
                                        case outsideCareForm.step of
                                            OutsideCareStepDiagnoses ->
                                                let
                                                    actionMsg =
                                                        if List.isEmpty outsideCareTasksStep2 then
                                                            saveAction

                                                        else
                                                            SetOutsideCareStep OutsideCareStepMedications
                                                in
                                                [ saveButton language saveButtonActive actionMsg ]

                                            OutsideCareStepMedications ->
                                                [ button
                                                    [ class "ui fluid primary button"
                                                    , onClick <| SetOutsideCareStep OutsideCareStepDiagnoses
                                                    ]
                                                    [ text <| ("< " ++ translate language Translate.Back) ]
                                                , saveButton language saveButtonActive saveAction
                                                ]
                        in
                        div
                            [ classList
                                [ ( "actions", True )
                                , ( "two"
                                  , (task == Obstetric && data.obstetricHistoryStep == ObstetricHistorySecondStep)
                                        || (task == OutsideCare && outsideCareForm.step == OutsideCareStepMedications)
                                  )
                                ]
                            ]
                            buttons
                    )
                |> Maybe.withDefault emptyNode
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
            resolveExaminationTasks assembled

        activeTask =
            Maybe.map
                (\task ->
                    if List.member task tasks then
                        Just task

                    else
                        List.head tasks
                )
                data.activeTask
                |> Maybe.withDefault (List.head tasks)

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

                        GUExam ->
                            "gu-exam"

                isActive =
                    activeTask == Just task

                isCompleted =
                    examinationTaskCompleted assembled task

                attributes =
                    classList
                        [ ( "link-section", True )
                        , ( "active", isActive )
                        , ( "completed", not isActive && isCompleted )
                        ]
                        :: navigationAction

                navigationAction =
                    if isActive then
                        []

                    else
                        [ onClick <| SetActiveExaminationTask task ]
            in
            div [ class <| "column " ++ iconClass ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.ExaminationTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            List.map
                (\task ->
                    case task of
                        ObstetricalExam ->
                            ( ObstetricalExam
                            , ( Maybe.Extra.values breastExamTasks
                                    |> List.length
                              , List.length breastExamTasks
                              )
                            )

                        BreastExam ->
                            ( ObstetricalExam
                            , ( Maybe.Extra.values obstetricalExamTasks
                                    |> List.length
                              , List.length obstetricalExamTasks
                              )
                            )

                        _ ->
                            ( task, examinationTasksCompletedFromTotal assembled data task )
                )
                tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        measuredHeight =
            resolveMeasuredHeight assembled

        ( obstetricalExamInputs, obstetricalExamTasks ) =
            getMeasurementValueFunc assembled.measurements.obstetricalExam
                |> obstetricalExamFormWithDefault data.obstetricalExamForm
                |> obstetricalExamFormInputsAndTasks language currentDate assembled

        ( breastExamInputs, breastExamTasks ) =
            getMeasurementValueFunc assembled.measurements.breastExam
                |> breastExamFormWithDefault data.breastExamForm
                |> breastExamInputsAndTasks language currentDate assembled

        viewForm =
            case activeTask of
                Just Vitals ->
                    getMeasurementValueFunc assembled.measurements.vitals
                        |> vitalsFormWithDefault data.vitalsForm
                        |> viewVitalsForm language currentDate assembled

                Just NutritionAssessment ->
                    let
                        form =
                            assembled.measurements.nutrition
                                |> getMeasurementValueFunc
                                |> prenatalNutritionFormWithDefault data.nutritionAssessmentForm

                        formWithMeasuredHeight =
                            Maybe.map (\(HeightInCm height) -> { form | height = Just height }) measuredHeight
                                |> Maybe.withDefault form

                        hideHeightInput =
                            isJust measuredHeight
                    in
                    viewNutritionAssessmentForm language currentDate assembled formWithMeasuredHeight hideHeightInput

                Just CorePhysicalExam ->
                    getMeasurementValueFunc assembled.measurements.corePhysicalExam
                        |> corePhysicalExamFormWithDefault data.corePhysicalExamForm
                        |> viewCorePhysicalExamForm language currentDate

                Just ObstetricalExam ->
                    div [ class "ui form examination obstetrical-exam" ]
                        obstetricalExamInputs

                Just BreastExam ->
                    div [ class "ui form examination breast-exam" ]
                        breastExamInputs

                Just GUExam ->
                    getMeasurementValueFunc assembled.measurements.guExam
                        |> guExamFormWithDefault data.guExamForm
                        |> viewGUExamForm language currentDate assembled

                Nothing ->
                    emptyNode

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
                        personId =
                            assembled.participant.person

                        measurements =
                            assembled.measurements

                        saveAction =
                            case task of
                                Vitals ->
                                    SaveVitals personId measurements.vitals nextTask

                                NutritionAssessment ->
                                    SaveNutritionAssessment personId measurements.nutrition (Maybe.map getHeightValue measuredHeight) nextTask

                                CorePhysicalExam ->
                                    SaveCorePhysicalExam personId measurements.corePhysicalExam nextTask

                                ObstetricalExam ->
                                    SaveObstetricalExam personId measurements.obstetricalExam nextTask

                                BreastExam ->
                                    SaveBreastExam personId measurements.breastExam nextTask

                                GUExam ->
                                    SaveGUExam personId measurements.guExam nextTask
                    in
                    div [ class "actions examination" ]
                        [ button
                            [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                            , onClick saveAction
                            ]
                            [ text <| translate language Translate.Save ]
                        ]
                )
                activeTask
                |> Maybe.withDefault emptyNode
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
            [ viewFamilyPlanningForm language Translate.FamilyPlanningInFutureQuestion SetFamilyPlanningSign form
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

        ( tasksCompleted, totalTasks ) =
            let
                displayMebendazoleQuestion =
                    showMebendazoleQuestion currentDate assembled

                tasks =
                    [ ( form.receivedIronFolicAcid, True )
                    , ( form.receivedMebendazole, displayMebendazoleQuestion )
                    ]
                        |> List.filterMap
                            (\( task, showTask ) ->
                                if showTask then
                                    Just task

                                else
                                    Nothing
                            )
            in
            ( List.map taskCompleted tasks
                |> List.sum
            , List.length tasks
            )
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewPrenatalMedicationForm language currentDate SetMedicationBoolInput assembled form ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SaveMedication assembled.participant.person assembled.measurements.medication
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
                            [ VaginalBleeding
                            , HeadacheBlurredVision
                            , Convulsions
                            , AbdominalPain
                            , SevereVomiting
                            , Unconscious
                            , GushLeakingVaginalFluid
                            ]
                            [ DifficultyBreathing
                            , Fever
                            , ExtremeWeakness
                            , LooksVeryIll
                            , Labor
                            , ImminentDelivery
                            , PrematureOnsetContractions
                            ]
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
        -- If we have a photo that we've just taken, but not saved, that is in
        -- `data.url`. We show that if we have it. Otherwise, we'll show the saved
        -- measurement, if we have that.
        ( displayPhoto, saveMsg, isDisabled ) =
            case data.url of
                Just url ->
                    let
                        photoId =
                            Maybe.map Tuple.first assembled.measurements.prenatalPhoto
                    in
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
                [ Maybe.map viewPhotoThumbFromImageUrl displayPhoto
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
                    (classList
                        [ ( "ui fluid primary button", True )
                        , ( "disabled", isDisabled )
                        ]
                        :: saveMsg
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
                , viewFamilyPlanningInput language SetBirthPlanFamilyPlanning form.familyPlanning
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


viewLaboratoryContent : Language -> NominalDate -> AssembledData -> LaboratoryData -> List (Html Msg)
viewLaboratoryContent language currentDate assembled data =
    if assembled.encounter.encounterType == NurseEncounter then
        viewLaboratoryContentForNurse language currentDate assembled data

    else
        viewLaboratoryContentForChw language currentDate assembled data


viewLaboratoryContentForNurse : Language -> NominalDate -> AssembledData -> LaboratoryData -> List (Html Msg)
viewLaboratoryContentForNurse language currentDate assembled data =
    let
        measurements =
            assembled.measurements

        tasks =
            List.filter (expectLaboratoryTask currentDate assembled) laboratoryTasks

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

        viewTask task =
            let
                iconClass =
                    laboratoryTaskIconClass task

                isActive =
                    activeTask == Just task

                isCompleted =
                    laboratoryTaskCompleted currentDate assembled task

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
                    , text <| translate language (Translate.LaboratoryTask task)
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
                                |> hivTestFormWithDefault data.hivTestForm
                                |> viewHIVTestForm language
                                    currentDate
                                    contentAndTasksLaboratoryTestInitialConfig
                                    contentAndTasksForPerformedLaboratoryTestConfig

                        TaskSyphilisTest ->
                            measurements.syphilisTest
                                |> getMeasurementValueFunc
                                |> nonRDTFormWithDefault data.syphilisTestForm
                                |> viewNonRDTForm language
                                    currentDate
                                    contentAndTasksLaboratoryTestInitialConfig
                                    contentAndTasksForPerformedLaboratoryTestConfig
                                    TaskSyphilisTest

                        TaskHepatitisBTest ->
                            measurements.hepatitisBTest
                                |> getMeasurementValueFunc
                                |> nonRDTFormWithDefault data.hepatitisBTestForm
                                |> viewNonRDTFormCheckKnownAsPositive language
                                    currentDate
                                    contentAndTasksLaboratoryTestInitialConfig
                                    contentAndTasksForPerformedLaboratoryTestConfig
                                    TaskHepatitisBTest

                        TaskMalariaTest ->
                            measurements.malariaTest
                                |> getMeasurementValueFunc
                                |> malariaTestFormWithDefault data.malariaTestForm
                                |> viewMalariaTestForm language
                                    currentDate
                                    contentAndTasksLaboratoryTestInitialConfig
                                    contentAndTasksForPerformedLaboratoryTestConfig

                        TaskBloodGpRsTest ->
                            measurements.bloodGpRsTest
                                |> getMeasurementValueFunc
                                |> nonRDTFormWithDefault data.bloodGpRsTestForm
                                |> viewNonRDTForm language
                                    currentDate
                                    contentAndTasksLaboratoryTestInitialConfig
                                    contentAndTasksForPerformedLaboratoryTestConfig
                                    TaskBloodGpRsTest

                        TaskUrineDipstickTest ->
                            measurements.urineDipstickTest
                                |> getMeasurementValueFunc
                                |> urineDipstickFormWithDefault data.urineDipstickTestForm
                                |> viewUrineDipstickForm language
                                    currentDate
                                    contentAndTasksLaboratoryTestInitialConfig
                                    contentAndTasksForPerformedLaboratoryTestConfig

                        TaskHemoglobinTest ->
                            measurements.hemoglobinTest
                                |> getMeasurementValueFunc
                                |> nonRDTFormWithDefault data.hemoglobinTestForm
                                |> viewNonRDTForm language
                                    currentDate
                                    contentAndTasksLaboratoryTestInitialConfig
                                    contentAndTasksForPerformedLaboratoryTestConfig
                                    TaskHemoglobinTest

                        TaskRandomBloodSugarTest ->
                            measurements.randomBloodSugarTest
                                |> getMeasurementValueFunc
                                |> randomBloodSugarFormWithDefault data.randomBloodSugarTestForm
                                |> viewRandomBloodSugarForm language
                                    currentDate
                                    contentAndTasksLaboratoryTestInitialConfig
                                    contentAndTasksForPerformedLaboratoryTestConfig

                        TaskHIVPCRTest ->
                            measurements.hivPCRTest
                                |> getMeasurementValueFunc
                                |> nonRDTFormWithDefault data.hivPCRTestForm
                                |> viewNonRDTForm language
                                    currentDate
                                    contentAndTasksLaboratoryTestInitialConfig
                                    contentAndTasksForPerformedLaboratoryTestConfig
                                    TaskHIVPCRTest

                        TaskPartnerHIVTest ->
                            measurements.partnerHIVTest
                                |> getMeasurementValueFunc
                                |> partnerHIVTestFormWithDefault data.partnerHIVTestForm
                                |> viewPartnerHIVTestForm language
                                    currentDate
                                    contentAndTasksLaboratoryTestInitialConfig
                                    contentAndTasksForPerformedLaboratoryTestConfig

                        TaskCompletePreviousTests ->
                            viewLabsHistoryForm language currentDate assembled data.labsHistoryForm

                        -- Others do not participate at Prenatal.
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

        actions =
            Maybe.map
                (\task ->
                    let
                        personId =
                            assembled.participant.person

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

                                TaskHIVPCRTest ->
                                    SaveHIVPCRTest personId measurements.hivPCRTest nextTask

                                TaskPartnerHIVTest ->
                                    SavePartnerHIVTest personId measurements.partnerHIVTest nextTask

                                TaskCompletePreviousTests ->
                                    SaveLabsHistory

                                -- Others do not participate at Prenatal.
                                _ ->
                                    NoOp

                        disableSave =
                            if task == TaskCompletePreviousTests then
                                data.labsHistoryForm.completed /= Just True

                            else
                                tasksCompleted /= totalTasks
                    in
                    viewSaveAction language saveMsg disableSave
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

        resultInput =
            viewCustomSelectListInput form.pregnancyTestResult
                [ PregnancyTestPositive, PregnancyTestNegative, PregnancyTestIndeterminate, PregnancyTestUnableToConduct ]
                pregnancyTestResultToString
                SetPregnancyTestResult
                (Translate.PregnancyTestResult >> translate language)
                "form-input select"
                (isNothing form.pregnancyTestResult)
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


viewMentalHealthContent : Language -> NominalDate -> AssembledData -> MentalHealthData -> List (Html Msg)
viewMentalHealthContent language currentDate assembled data =
    let
        form =
            assembled.measurements.mentalHealth
                |> getMeasurementValueFunc
                |> mentalHealthFormWithDefault data.form

        ( input, tasksCompleted ) =
            case form.step of
                MentalHealthQuestion question ->
                    let
                        value =
                            Maybe.andThen (Dict.get question) form.signs
                    in
                    ( [ viewCustomLabel language (Translate.PrenatalMentalHealthQuestion question) "." "label"
                      , viewCheckBoxSelectInput language
                            (getMentalHealtOptionsForQuestion question)
                            []
                            value
                            (SetMentalHealthOptionForQuestion question)
                            (Translate.PrenatalMentalHealthOptionForQuestion question)
                      ]
                    , taskCompleted value
                    )

                MentalHealthSpecialistQuestion ->
                    ( [ viewQuestionLabel language Translate.PrenatalMentalHealthSpecialistQuestion
                      , viewBoolInput language
                            form.specialistAtHC
                            SetSpecialistAtHC
                            "specialist"
                            Nothing
                      ]
                    , taskCompleted form.specialistAtHC
                    )

        totalTasks =
            1

        getMentalHealtOptionsForQuestion value =
            if
                List.member value
                    [ MentalHealthQuestion1
                    , MentalHealthQuestion2
                    , MentalHealthQuestion4
                    ]
            then
                [ MentalHealthQuestionOption0
                , MentalHealthQuestionOption1
                , MentalHealthQuestionOption2
                , MentalHealthQuestionOption3
                ]

            else
                [ MentalHealthQuestionOption3
                , MentalHealthQuestionOption2
                , MentalHealthQuestionOption1
                , MentalHealthQuestionOption0
                ]

        actions =
            getPrevStep form.step
                |> Maybe.map
                    (\prevStep ->
                        div [ class "actions two" ]
                            [ button
                                [ class "ui fluid primary button"
                                , onClick <| SetMentalHealthStep prevStep
                                ]
                                [ text <| ("< " ++ translate language Translate.Back) ]
                            , saveButton language saveButtonActive saveAction
                            ]
                    )
                |> Maybe.withDefault
                    (div [ class "actions" ]
                        [ saveButton language saveButtonActive saveAction ]
                    )

        saveButtonActive =
            tasksCompleted == totalTasks

        saveAction =
            getNextStep form.step
                |> Maybe.map SetMentalHealthStep
                |> Maybe.withDefault
                    (let
                        suicideRiskDiagnosed =
                            Maybe.andThen suicideRiskDiagnosedBySigns form.signs
                                |> Maybe.withDefault False

                        saveMsg =
                            SaveMentalHealth assembled.participant.person assembled.measurements.mentalHealth
                     in
                     if suicideRiskDiagnosed then
                        SetWarningPopupState (Just <| WarningPopupMentalHealth saveMsg)

                     else
                        saveMsg
                    )

        getNextStep currentStep =
            case currentStep of
                MentalHealthQuestion question ->
                    case question of
                        MentalHealthQuestion1 ->
                            Just (MentalHealthQuestion MentalHealthQuestion2)

                        MentalHealthQuestion2 ->
                            Just (MentalHealthQuestion MentalHealthQuestion3)

                        MentalHealthQuestion3 ->
                            Just (MentalHealthQuestion MentalHealthQuestion4)

                        MentalHealthQuestion4 ->
                            Just (MentalHealthQuestion MentalHealthQuestion5)

                        MentalHealthQuestion5 ->
                            Just (MentalHealthQuestion MentalHealthQuestion6)

                        MentalHealthQuestion6 ->
                            Just (MentalHealthQuestion MentalHealthQuestion7)

                        MentalHealthQuestion7 ->
                            Just (MentalHealthQuestion MentalHealthQuestion8)

                        MentalHealthQuestion8 ->
                            Just (MentalHealthQuestion MentalHealthQuestion9)

                        MentalHealthQuestion9 ->
                            Just (MentalHealthQuestion MentalHealthQuestion10)

                        MentalHealthQuestion10 ->
                            Just MentalHealthSpecialistQuestion

                MentalHealthSpecialistQuestion ->
                    Nothing

        getPrevStep currentStep =
            case currentStep of
                MentalHealthQuestion question ->
                    case question of
                        MentalHealthQuestion1 ->
                            Nothing

                        MentalHealthQuestion2 ->
                            Just (MentalHealthQuestion MentalHealthQuestion1)

                        MentalHealthQuestion3 ->
                            Just (MentalHealthQuestion MentalHealthQuestion2)

                        MentalHealthQuestion4 ->
                            Just (MentalHealthQuestion MentalHealthQuestion3)

                        MentalHealthQuestion5 ->
                            Just (MentalHealthQuestion MentalHealthQuestion4)

                        MentalHealthQuestion6 ->
                            Just (MentalHealthQuestion MentalHealthQuestion5)

                        MentalHealthQuestion7 ->
                            Just (MentalHealthQuestion MentalHealthQuestion6)

                        MentalHealthQuestion8 ->
                            Just (MentalHealthQuestion MentalHealthQuestion7)

                        MentalHealthQuestion9 ->
                            Just (MentalHealthQuestion MentalHealthQuestion8)

                        MentalHealthQuestion10 ->
                            Just (MentalHealthQuestion MentalHealthQuestion9)

                MentalHealthSpecialistQuestion ->
                    Just (MentalHealthQuestion MentalHealthQuestion10)
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form mental-health" ]
                input
            ]
        , actions
        ]
    ]


viewNextStepsContent : Language -> NominalDate -> Bool -> AssembledData -> NextStepsData -> List (Html Msg)
viewNextStepsContent language currentDate isChw assembled data =
    let
        measurements =
            assembled.measurements

        tasks =
            resolveNextStepsTasks currentDate assembled

        activeTask =
            Maybe.map
                (\task ->
                    if List.member task tasksConsideringShowWaitTask then
                        Just task

                    else
                        List.head tasksConsideringShowWaitTask
                )
                data.activeTask
                |> Maybe.withDefault (List.head tasksConsideringShowWaitTask)

        tasksConsideringShowWaitTask =
            if showWaitTask then
                tasks

            else
                List.filter ((/=) NextStepsWait) tasks

        showWaitTask =
            -- Wait task is expected.
            List.member NextStepsWait tasks
                && -- There's one or less uncompleted task,
                   -- which is the Wait task.
                   (List.filter (nextStepsTaskCompleted currentDate assembled >> not) tasks
                        |> List.length
                        |> (\length -> length < 2)
                   )

        viewTask task =
            let
                iconClass =
                    case task of
                        NextStepsAppointmentConfirmation ->
                            "next-steps-send-to-hc"

                        NextStepsFollowUp ->
                            "next-steps-follow-up"

                        NextStepsSendToHC ->
                            "next-steps-referral"

                        NextStepsHealthEducation ->
                            "next-steps-health-education"

                        NextStepsNewbornEnrolment ->
                            "next-steps-newborn-enrolment"

                        NextStepsMedicationDistribution ->
                            "next-steps-treatment"

                        NextStepsWait ->
                            "next-steps-wait"

                isActive =
                    activeTask == Just task

                isCompleted =
                    nextStepsTaskCompleted currentDate assembled task

                attributes =
                    classList
                        [ ( "link-section", True )
                        , ( "active", isActive )
                        , ( "completed", not isActive && isCompleted )
                        ]
                        :: navigationAction

                navigationAction =
                    if isActive then
                        []

                    else
                        [ onClick <| SetActiveNextStepsTask task ]
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.PrenatalNextStepsTask isChw task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            tasks
                |> List.map
                    (\task ->
                        ( task, nextStepsTasksCompletedFromTotal language currentDate isChw assembled data task )
                    )
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        viewForm =
            case activeTask of
                Just NextStepsAppointmentConfirmation ->
                    getMeasurementValueFunc measurements.appointmentConfirmation
                        |> appointmentConfirmationFormWithDefault data.appointmentConfirmationForm
                        |> viewAppointmentConfirmationForm language currentDate assembled

                Just NextStepsFollowUp ->
                    getMeasurementValueFunc measurements.followUp
                        |> followUpFormWithDefault data.followUpForm
                        |> viewFollowUpForm language currentDate assembled

                Just NextStepsSendToHC ->
                    getMeasurementValueFunc measurements.sendToHC
                        |> referralFormWithDefault data.referralForm
                        |> viewReferralForm language currentDate assembled

                Just NextStepsHealthEducation ->
                    getMeasurementValueFunc measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> viewHealthEducationForm language currentDate assembled

                Just NextStepsNewbornEnrolment ->
                    viewNewbornEnrolmentForm language currentDate assembled

                Just NextStepsMedicationDistribution ->
                    viewMedicationDistributionForm language
                        currentDate
                        PrenatalEncounterPhaseInitial
                        assembled
                        SetMedicationDistributionBoolInput
                        SetMedicationDistributionAdministrationNote
                        SetRecommendedTreatmentSign
                        SetAvoidingGuidanceReason
                        medicationDistributionForm

                Just NextStepsWait ->
                    viewWaitForm language currentDate assembled

                Nothing ->
                    emptyNode

        medicationDistributionForm =
            getMeasurementValueFunc measurements.medicationDistribution
                |> medicationDistributionFormWithDefaultInitialPhase data.medicationDistributionForm

        tasksAfterSave =
            case activeTask of
                Just NextStepsMedicationDistribution ->
                    let
                        -- We know if patient was referred to hospital
                        -- due to Malaria, based on saved measurement.
                        -- We know if patient will be referred to hospital
                        -- due to Malaria, based on edited form.
                        referredToHospitalAfterSave =
                            Maybe.map (List.member TreatmentReferToHospital)
                                medicationDistributionForm.recommendedTreatmentSigns
                                |> Maybe.withDefault False
                    in
                    if referredToHospitalAfterSave then
                        EverySet.fromList tasks
                            |> EverySet.insert NextStepsSendToHC
                            |> EverySet.toList

                    else
                        let
                            referredToHospitalBeforeSave =
                                getMeasurementValueFunc measurements.medicationDistribution
                                    |> Maybe.andThen (.recommendedTreatmentSigns >> Maybe.map (EverySet.member TreatmentReferToHospital))
                                    |> Maybe.withDefault False
                        in
                        if referredToHospitalBeforeSave then
                            EverySet.fromList tasks
                                |> EverySet.remove NextStepsSendToHC
                                |> EverySet.toList

                        else
                            tasks

                _ ->
                    tasks

        nextTask =
            tasksAfterSave
                |> List.filter
                    (\task ->
                        (Just task /= activeTask)
                            && (not <| isTaskCompleted tasksCompletedFromTotalDict task)
                    )
                |> List.head

        actions =
            Maybe.map
                (\task ->
                    let
                        personId =
                            assembled.participant.person

                        secondPhase =
                            secondPhaseRequired assembled

                        saveMsg =
                            case task of
                                NextStepsAppointmentConfirmation ->
                                    SaveAppointmentConfirmation personId measurements.appointmentConfirmation secondPhase nextTask

                                NextStepsFollowUp ->
                                    let
                                        assesment =
                                            generatePrenatalAssesmentForChw assembled
                                    in
                                    SaveFollowUp personId assesment measurements.followUp secondPhase nextTask

                                NextStepsSendToHC ->
                                    SaveSendToHC personId measurements.sendToHC secondPhase nextTask

                                NextStepsHealthEducation ->
                                    SaveHealthEducationSubActivity personId measurements.healthEducation secondPhase nextTask

                                NextStepsNewbornEnrolment ->
                                    SaveNewbornEnrollment secondPhase nextTask

                                NextStepsMedicationDistribution ->
                                    SaveMedicationDistribution personId measurements.medicationDistribution secondPhase nextTask

                                NextStepsWait ->
                                    Maybe.map
                                        (\( measurementId, measurement ) ->
                                            let
                                                value =
                                                    measurement.value
                                            in
                                            SaveWait personId (Just measurementId) { value | patientNotified = True } secondPhase nextTask
                                        )
                                        measurements.labsResults
                                        |> Maybe.withDefault NoOp
                    in
                    case task of
                        NextStepsWait ->
                            viewPauseEncounterButton language
                                "primary"
                                -- Button is enabled because there are
                                -- no actual tasks to be performed.
                                True
                                -- When saving, we'll also 'pause' the encounter
                                -- which actualy navigates to main menu page.
                                -- The encounter is closed on second phase.
                                saveMsg

                        _ ->
                            div [ class "actions next-steps" ]
                                [ button
                                    [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                                    , onClick saveMsg
                                    ]
                                    [ text <| translate language Translate.Save ]
                                ]
                )
                activeTask
                |> Maybe.withDefault emptyNode
    in
    [ div [ class "ui task segment blue", Html.Attributes.id tasksBarId ]
        [ div [ class "ui four column grid" ] <|
            List.map viewTask tasksConsideringShowWaitTask
        ]
    , div
        [ classList
            [ ( "tasks-count", True )
            , ( "full-screen", activeTask == Just NextStepsWait )
            ]
        ]
        [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewForm
            , actions
            ]
        ]
    ]


viewSymptomReviewContent : Language -> NominalDate -> AssembledData -> SymptomReviewData -> List (Html Msg)
viewSymptomReviewContent language currentDate assembled data =
    let
        form =
            assembled.measurements.symptomReview
                |> getMeasurementValueFunc
                |> symptomReviewFormWithDefault data.form

        ( inputs, tasksCompleted, totalTasks ) =
            case data.step of
                SymptomReviewStepSymptoms ->
                    ( inputsStep1, tasksCompletedStep1, totalTasksStep1 )

                SymptomReviewStepQuestions ->
                    ( inputsStep2, tasksCompletedStep2, totalTasksStep2 )

        ( inputsStep1, tasksCompletedStep1, totalTasksStep1 ) =
            symptomReviewFormInputsAndTasks language assembled.encounter.encounterType SymptomReviewStepSymptoms form

        ( inputsStep2, tasksCompletedStep2, totalTasksStep2 ) =
            symptomReviewFormInputsAndTasks language assembled.encounter.encounterType SymptomReviewStepQuestions form

        actions =
            let
                saveAction =
                    SaveSymptomReview assembled.participant.person assembled.measurements.symptomReview

                saveButtonActive =
                    tasksCompleted == totalTasks
            in
            case data.step of
                SymptomReviewStepSymptoms ->
                    let
                        actionMsg =
                            if totalTasksStep2 == 0 then
                                saveAction

                            else
                                SetSymptomReviewStep SymptomReviewStepQuestions
                    in
                    div [ class "actions" ]
                        [ saveButton language saveButtonActive actionMsg ]

                SymptomReviewStepQuestions ->
                    div [ class "actions two" ]
                        [ button
                            [ class "ui fluid primary button"
                            , onClick <| SetSymptomReviewStep SymptomReviewStepSymptoms
                            ]
                            [ text <| ("< " ++ translate language Translate.Back) ]
                        , saveButton language saveButtonActive saveAction
                        ]

        instructionLabel =
            case data.step of
                SymptomReviewStepSymptoms ->
                    emptyNode

                SymptomReviewStepQuestions ->
                    div [ class "instructions" ]
                        [ viewLabel language Translate.PrenatalSymptomQuestionsHeader ]
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form symptom-review" ] <|
                instructionLabel
                    :: inputs
            ]
        , actions
        ]
    ]


viewTreatmentReviewContent : Language -> NominalDate -> AssembledData -> TreatmentReviewData -> List (Html Msg)
viewTreatmentReviewContent language currentDate assembled data =
    let
        measurements =
            assembled.measurements

        moderatePreeclampsiaPreviously =
            diagnosedModeratePreeclampsiaPrevoiusly assembled

        viewTask task =
            let
                isActive =
                    activeTask == Just task

                isCompleted =
                    treatmentReviewTaskCompleted assembled task

                navigationAction =
                    if isActive then
                        []

                    else
                        [ onClick <| SetActiveTreatmentReviewTask task ]

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
                    [ span [ class "icon-activity-task icon-medication" ] []
                    , text <| translate language (Translate.TreatmentReviewTask moderatePreeclampsiaPreviously task)
                    ]
                ]

        tasks =
            resolveTreatmentReviewTasks assembled

        tasksCompletedFromTotalDict =
            List.map
                (\task ->
                    ( task, treatmentReviewTasksCompletedFromTotal language currentDate assembled data task )
                )
                tasks
                |> Dict.fromList

        activeTask =
            Maybe.map
                (\task ->
                    if List.member task tasks then
                        Just task

                    else
                        List.head tasks
                )
                data.activeTask
                |> Maybe.withDefault (List.head tasks)

        ( tasksCompleted, totalTasks ) =
            Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict) activeTask
                |> Maybe.withDefault ( 0, 0 )

        form =
            measurements.medication
                |> getMeasurementValueFunc
                |> medicationFormWithDefault data.medicationForm

        viewForm =
            case activeTask of
                Just TreatmentReviewPrenatalMedication ->
                    viewPrenatalMedicationForm language currentDate SetMedicationSubActivityBoolInput assembled form

                Just task ->
                    viewMedicationTreatmentForm language currentDate SetMedicationSubActivityBoolInput assembled form task

                Nothing ->
                    emptyNode

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
                            personId =
                                assembled.participant.person

                            saveMsg =
                                SaveMedicationSubActivity personId measurements.medication nextTask

                            action =
                                case task of
                                    TreatmentReviewSyphilis ->
                                        if form.syphilisMissedDoses == Just True then
                                            SetWarningPopupState (Just (WarningPopupTreatmentReview saveMsg))

                                        else
                                            saveMsg

                                    _ ->
                                        saveMsg
                        in
                        div [ class "actions treatment-review" ]
                            [ button
                                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                                , onClick action
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
    , div [ class "tasks-count" ]
        [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ viewForm
            , actions
            ]
        ]
    ]


viewImmunisationContent :
    Language
    -> NominalDate
    -> AssembledData
    -> ImmunisationData
    -> List (Html Msg)
viewImmunisationContent language currentDate assembled data =
    let
        measurements =
            assembled.measurements

        tasks =
            List.filter (expectImmunisationTask currentDate assembled) immunisationTasks

        activeTask =
            Maybe.Extra.or data.activeTask (List.head tasks)

        viewTask task =
            let
                ( iconClass, isCompleted ) =
                    case task of
                        TaskTetanus ->
                            ( "tetanus-vaccine"
                            , isJust measurements.tetanusImmunisation
                            )

                isActive =
                    activeTask == Just task

                attributes =
                    classList
                        [ ( "link-section", True )
                        , ( "active", isActive )
                        , ( "completed", not isActive && isCompleted )
                        ]
                        :: (if isActive then
                                []

                            else
                                [ onClick <| SetActiveImmunisationTask task ]
                           )
            in
            div [ class "column" ]
                [ div attributes
                    [ span [ class <| "icon-activity-task icon-" ++ iconClass ] []
                    , text <| translate language (Translate.PrenatalImmunisationTask task)
                    ]
                ]

        tasksCompletedFromTotalDict =
            List.map (\task -> ( task, immunisationTasksCompletedFromTotal language currentDate assembled data task )) tasks
                |> Dict.fromList

        ( tasksCompleted, totalTasks ) =
            activeTask
                |> Maybe.andThen (\task -> Dict.get task tasksCompletedFromTotalDict)
                |> Maybe.withDefault ( 0, 0 )

        ( formForView, fullScreen, allowSave ) =
            Maybe.map immunisationTaskToVaccineType activeTask
                |> Maybe.map
                    (\vaccineType ->
                        let
                            vaccinationForm =
                                case vaccineType of
                                    VaccineTetanus ->
                                        measurements.tetanusImmunisation
                                            |> getMeasurementValueFunc
                                            |> vaccinationFormWithDefault data.tetanusForm
                        in
                        ( viewVaccinationForm language currentDate assembled vaccineType vaccinationForm
                        , False
                        , vaccinationForm.viewMode == ViewModeInitial
                        )
                    )
                |> Maybe.withDefault ( emptyNode, False, False )

        actions =
            Maybe.map
                (\task ->
                    let
                        saveMsg =
                            case task of
                                TaskTetanus ->
                                    let
                                        personId =
                                            assembled.participant.person
                                    in
                                    SaveTetanusImmunisation personId measurements.tetanusImmunisation

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


viewSpecialityCareContent : Language -> NominalDate -> AssembledData -> SpecialityCareData -> List (Html Msg)
viewSpecialityCareContent language currentDate assembled data =
    let
        form =
            getMeasurementValueFunc assembled.measurements.specialityCare
                |> specialityCareFormWithDefault data.form

        tasksCompleted =
            Maybe.Extra.values tasks
                |> List.length

        totalTasks =
            List.length tasks

        tasks =
            arvTasks ++ ncdTasks

        ( arvSection, arvTasks ) =
            resolveARVReferralDiagnosis assembled.nursePreviousEncountersData
                |> Maybe.map
                    (\referraDiagnosis ->
                        ( [ sectionHeader (translate language <| Translate.PrenatalDiagnosis referraDiagnosis)
                          , viewQuestionLabel language <| Translate.SpecialityCareSignQuestion EnrolledToARVProgram
                          , viewBoolInput
                                language
                                form.enrolledToARVProgram
                                (SetSpecialityCareBoolInput
                                    (\value form_ -> { form_ | enrolledToARVProgram = Just value })
                                )
                                "arv"
                                Nothing
                          , div [ class "separator" ] []
                          ]
                        , [ form.enrolledToARVProgram ]
                        )
                    )
                |> Maybe.withDefault ( [], [] )

        ( ncdSection, ncdTasks ) =
            let
                referraDiagnoses =
                    resolveNCDReferralDiagnoses assembled.nursePreviousEncountersData
            in
            if not <| List.isEmpty referraDiagnoses then
                ( [ List.map (Translate.PrenatalDiagnosis >> translate language) referraDiagnoses
                        |> String.join ", "
                        |> sectionHeader
                  , viewQuestionLabel language <| Translate.SpecialityCareSignQuestion EnrolledToNCDProgram
                  , viewBoolInput
                        language
                        form.enrolledToNCDProgram
                        (SetSpecialityCareBoolInput
                            (\value form_ -> { form_ | enrolledToNCDProgram = Just value })
                        )
                        "ncd"
                        Nothing
                  , div [ class "separator" ] []
                  ]
                , [ form.enrolledToNCDProgram ]
                )

            else
                ( [], [] )

        sectionHeader diagnoses =
            div [ class "label header" ]
                [ text <| translate language Translate.SpecialityCareHeaderPrefix
                , text " "
                , span [ class "highlight" ] [ text diagnoses ]
                , text " "
                , text <| translate language Translate.SpecialityCareHeaderSuffix
                , text "."
                ]
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form speciality-care" ] <|
                arvSection
                    ++ ncdSection
            ]
        , viewSaveAction language
            (SaveSpecialityCare assembled.participant.person assembled.measurements.specialityCare)
            (tasksCompleted /= totalTasks)
        ]
    ]


viewVaccinationForm : Language -> NominalDate -> AssembledData -> PrenatalVaccineType -> PrenatalVaccinationForm -> Html Msg
viewVaccinationForm language currentDate assembled vaccineType form =
    let
        ( contentByViewMode, _, _ ) =
            vaccinationFormDynamicContentAndTasks language currentDate assembled vaccineType form
    in
    div [ class "ui form vaccination" ] <|
        [ h2 [] [ text <| translate language <| Translate.PrenatalImmunisationHeader vaccineType ]
        , div [ class "instructions" ] <|
            [ div [ class "header icon-label" ] <|
                [ i [ class "icon-open-book" ] []
                , div [ class "description" ] [ text <| translate language <| Translate.PrenatalImmunisationDescription vaccineType ]
                ]
            , viewLabel language (Translate.PrenatalImmunizationHistory vaccineType)
            ]
                ++ contentByViewMode
        ]


obstetricFormFirstStepInputsAndTasks :
    Language
    -> NominalDate
    -> AssembledData
    -> ObstetricFormFirstStep
    -> ( List (Html Msg), List (Maybe Bool) )
obstetricFormFirstStepInputsAndTasks language currentDate assembled form =
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
    ( [ viewQuestionLabel language Translate.CurrentlyPregnant
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
    , List.map maybeToBoolTask
        [ form.termPregnancy
        , form.preTermPregnancy
        , form.stillbirthsAtTerm
        , form.stillbirthsPreTerm
        , form.abortions
        , form.liveChildren
        ]
        |> List.append [ form.currentlyPregnant ]
    )


obstetricFormSecondStepInputsAndTasks :
    Language
    -> NominalDate
    -> AssembledData
    -> ObstetricFormSecondStep
    -> ( List (Html Msg), List (Maybe Bool) )
obstetricFormSecondStepInputsAndTasks language currentDate assembled form =
    let
        ( cSectionsHtml, cSectionsTasks ) =
            let
                ( derivedHtml, derivedTasks ) =
                    Maybe.map
                        (\cSections ->
                            if cSections > 0 then
                                ( [ div [ class "ui grid" ]
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
                                        [ FailureToProgress, PreviousCSection ]
                                        form.cSectionReason
                                        SetCSectionReason
                                        Translate.CSectionReasons
                                  ]
                                , [ form.cSectionInPreviousDelivery
                                  , maybeToBoolTask form.cSectionReason
                                  ]
                                )

                            else
                                ( [], [] )
                        )
                        form.cSections
                        |> Maybe.withDefault ( [], [] )

                cSectionInPreviousDeliveryUpdateFunc value form_ =
                    { form_ | cSectionInPreviousDelivery = Just value, cSectionInPreviousDeliveryDirty = True }
            in
            ( viewNumberInput
                language
                form.cSections
                SetNumberOfCSections
                "c-sections"
                Translate.NumberOfCSections
                (Just ( [ [ (<) 0 ] ], [] ))
                :: derivedHtml
            , maybeToBoolTask form.cSections :: derivedTasks
            )

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
    ( cSectionsHtml
        ++ [ div [ class "ui grid" ]
                [ div [ class "twelve wide column" ]
                    [ viewCustomLabel language Translate.PreviousDelivery ":" "label previous-delivery" ]
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
    , cSectionsTasks
        ++ [ maybeToBoolTask form.previousDeliveryPeriod
           , form.successiveAbortions
           , form.successivePrematureDeliveries
           , form.stillbornPreviousDelivery
           , form.babyDiedOnDayOfBirthPreviousDelivery
           , form.partialPlacentaPreviousDelivery
           , form.severeHemorrhagingPreviousDelivery
           , form.preeclampsiaPreviousPregnancy
           , form.convulsionsPreviousDelivery
           , form.convulsionsAndUnconsciousPreviousDelivery
           , form.gestationalDiabetesPreviousPregnancy
           , form.incompleteCervixPreviousPregnancy
           , form.rhNegative
           ]
    )


medicalFormInputsAndTasks :
    Language
    -> NominalDate
    -> AssembledData
    -> MedicalHistoryForm
    -> ( List (Html Msg), List (Maybe Bool) )
medicalFormInputsAndTasks language currentDate assembled form =
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
    ( [ viewCustomLabel language Translate.MedicalFormHelper ":" "label helper"
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
    , [ form.uterineMyoma
      , form.diabetes
      , form.cardiacDisease
      , form.renalDisease
      , form.hypertensionBeforePregnancy
      , form.tuberculosisPast
      , form.tuberculosisPresent
      , form.asthma
      , form.hiv
      , form.mentalHealthHistory
      ]
    )


socialFormInputsAndTasks :
    Language
    -> NominalDate
    -> AssembledData
    -> SocialHistoryForm
    -> ( List (Html Msg), List (Maybe Bool) )
socialFormInputsAndTasks language currentDate assembled form =
    let
        accompaniedByPartnerUpdateFunc value form_ =
            { form_ | accompaniedByPartner = Just value }

        ( counselingHtml, counselingTasks ) =
            let
                showCounselingQuestion =
                    -- Show the question until we get a positive answer for it.
                    List.filter
                        (.measurements
                            >> .socialHistory
                            >> getMeasurementValueFunc
                            >> Maybe.map (EverySet.member PartnerHivCounseling)
                            >> Maybe.withDefault False
                        )
                        assembled.nursePreviousEncountersData
                        |> List.isEmpty
            in
            if showCounselingQuestion then
                let
                    partnerReceivedCounselingUpdateFunc value form_ =
                        { form_ | partnerReceivedCounseling = Just value }
                in
                ( [ div [ class "ui grid" ]
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
                , [ form.partnerReceivedCounseling ]
                )

            else
                ( [], [] )
    in
    ( [ div [ class "ui grid" ]
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
        ++ counselingHtml
    , form.accompaniedByPartner :: counselingTasks
    )


viewVitalsForm : Language -> NominalDate -> AssembledData -> VitalsForm -> Html Msg
viewVitalsForm language currentDate assembled form =
    let
        formConfig =
            { setIntInputMsg = SetVitalsIntInput
            , setFloatInputMsg = SetVitalsFloatInput
            , sysBloodPressurePreviousValue = resolvePreviousMaybeValue assembled .vitals .sys
            , diaBloodPressurePreviousValue = resolvePreviousMaybeValue assembled .vitals .dia
            , heartRatePreviousValue =
                resolvePreviousMaybeValue assembled .vitals .heartRate
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
                |> Maybe.map getHeightValue

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


viewCorePhysicalExamForm : Language -> NominalDate -> CorePhysicalExamForm -> Html Msg
viewCorePhysicalExamForm language currentDate form =
    let
        config =
            { setBoolInputMsg = SetCorePhysicalExamBoolInput
            , setNeckMsg = SetCorePhysicalExamNeck
            , setHeartMsg = SetCorePhysicalExamHeart
            , setLungsMsg = SetCorePhysicalExamLungs
            , setAbdomenMsg = SetCorePhysicalExamAbdomen
            , setHandsMsg = SetCorePhysicalExamHands
            , setLegsMsg = SetCorePhysicalExamLegs
            }
    in
    Measurement.View.viewCorePhysicalExamForm language currentDate config form


obstetricalExamFormInputsAndTasks :
    Language
    -> NominalDate
    -> AssembledData
    -> ObstetricalExamForm
    -> ( List (Html Msg), List (Maybe Bool) )
obstetricalExamFormInputsAndTasks language currentDate assembled form =
    let
        ( fundalHeightHtml, fundalHeightTasks ) =
            let
                fundalPalpableUpdateFunc value form_ =
                    { form_
                        | fundalPalpable = Just value
                        , fundalHeight = Nothing
                        , fundalHeightDirty = True
                        , -- Display popup, if value is set to 'No'.
                          displayFundalPalpablePopup = not value
                    }

                ( derivedHtml, derivedTasks ) =
                    if form.fundalPalpable == Just True then
                        let
                            fundalHeightPreviousValue =
                                resolvePreviousValue assembled .obstetricalExam .fundalHeight
                                    |> Maybe.Extra.join
                                    |> Maybe.map getHeightValue

                            fundalHeightUpdateFunc value form_ =
                                { form_ | fundalHeight = value, fundalHeightDirty = True }
                        in
                        ( [ viewMeasurementInput
                                language
                                form.fundalHeight
                                (SetObstetricalExamFloatMeasurement fundalHeightUpdateFunc)
                                "fundal-height"
                                Translate.CentimeterShorthand
                          , viewPreviousMeasurement language fundalHeightPreviousValue Translate.CentimeterShorthand
                          ]
                        , [ maybeToBoolTask form.fundalHeight ]
                        )

                    else
                        ( [], [] )

                fundalPalpablePopup =
                    if form.displayFundalPalpablePopup then
                        Just <|
                            customWarningPopup language
                                ( p [] [ text <| translate language Translate.FundalPalpableWarning ]
                                , emptyNode
                                , HideFundalPalpablePopup
                                )

                    else
                        Nothing
            in
            ( [ div [ class "ui grid" ]
                    [ div [ class "twelve wide column" ]
                        [ viewLabel language Translate.FundalHeight ]
                    , div [ class "four wide column" ]
                        [ alerts.fundalHeight ]
                    ]
              , viewCustomLabel language Translate.FundalPalpableQuestion "?" "label question"
              , viewBoolInput
                    language
                    form.fundalPalpable
                    (SetObstetricalExamBoolInput fundalPalpableUpdateFunc)
                    "fundal-palpable"
                    Nothing
              ]
                ++ derivedHtml
                ++ [ viewModal fundalPalpablePopup
                   , div [ class "separator" ] []
                   ]
            , form.fundalPalpable :: derivedTasks
            )

        alerts =
            Maybe.map
                (\lmpDate ->
                    let
                        egaInWeeks =
                            calculateEGAWeeks currentDate lmpDate |> toFloat

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
                assembled.globalLmpDate
                |> Maybe.withDefault
                    { fundalHeight = emptyNode
                    , fetalPresentation = emptyNode
                    , fetalMovement = emptyNode
                    , fetalHeartRate = emptyNode
                    }

        fetalHeartRateUpdateFunc value form_ =
            { form_ | fetalHeartRate = value, fetalHeartRateDirty = True }

        fetalMovementUpdateFunc value form_ =
            { form_ | fetalMovement = Just value }

        fetalHeartRatePreviousValue =
            resolvePreviousValue assembled .obstetricalExam .fetalHeartRate
                |> Maybe.map toFloat
    in
    ( fundalHeightHtml
        ++ [ div [ class "ui grid" ]
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
                Translate.BeatsPerMinuteUnitLabel
           , viewPreviousMeasurement language fetalHeartRatePreviousValue Translate.BeatsPerMinuteUnitLabel
           , div [ class "separator" ] []
           , viewLabel language Translate.PreviousCSectionScar
           , viewCheckBoxSelectInput language
                [ Vertical, Horizontal ]
                [ NoScar ]
                form.cSectionScar
                SetObstetricalExamCSectionScar
                Translate.CSectionScar
           ]
    , fundalHeightTasks
        ++ [ maybeToBoolTask form.fetalPresentation
           , form.fetalMovement
           , maybeToBoolTask form.fetalHeartRate
           , maybeToBoolTask form.cSectionScar
           ]
    )


breastExamInputsAndTasks :
    Language
    -> NominalDate
    -> AssembledData
    -> BreastExamForm
    -> ( List (Html Msg), List (Maybe Bool) )
breastExamInputsAndTasks language currentDate assembled form =
    let
        selfGuidanceUpdateFunc value form_ =
            { form_ | selfGuidance = Just value }

        breastExamOptionsLeft =
            if assembled.encounter.encounterType == NursePostpartumEncounter then
                [ Mass, Discharge, Warmth ]

            else
                [ Mass, Discharge ]

        breastExamOptionsRight =
            [ Infection, NormalBreast ]

        ( derivedHtml, derivedTasks ) =
            Maybe.map
                (\signs ->
                    if List.member Discharge signs then
                        ( [ viewCustomLabel language Translate.BreastExamDischargeQuestion "?" "label secondary"
                          , viewCheckBoxSelectInput language
                                [ DischargeMilky
                                , DischargeClear
                                , DischargeBrownOrBloody
                                ]
                                [ DischargeYellow, DischargeGreen ]
                                form.dischargeType
                                SetDischargeType
                                Translate.BreastExamDischargeType
                          ]
                        , [ maybeToBoolTask form.dischargeType ]
                        )

                    else
                        ( [], [] )
                )
                form.breast
                |> Maybe.withDefault ( [], [] )
    in
    ( [ div [ class "ui grid" ]
            [ div [ class "twelve wide column" ]
                [ viewLabel language Translate.BreastExam ]
            , div [ class "four wide column" ]
                [ viewYellowAlertForSelect
                    (form.breast |> Maybe.withDefault [])
                    [ NormalBreast ]
                ]
            ]
      , viewCheckBoxMultipleSelectInput language
            breastExamOptionsLeft
            breastExamOptionsRight
            (form.breast |> Maybe.withDefault [])
            Nothing
            SetBreastExamBreast
            Translate.BreastExamSign
      ]
        ++ derivedHtml
        ++ [ div [ class "separator double" ] []
           , viewCustomLabel language Translate.BreastExamQuestion "?" "label secondary"
           , viewBoolInput
                language
                form.selfGuidance
                (SetBreastExamBoolInput selfGuidanceUpdateFunc)
                "self-guidance"
                Nothing
           ]
    , [ maybeToBoolTask form.breast, form.selfGuidance ] ++ derivedTasks
    )


viewGUExamForm : Language -> NominalDate -> AssembledData -> GUExamForm -> Html Msg
viewGUExamForm language currentDate assembled form =
    let
        ( inputs, _ ) =
            guExamFormInputsAndTasks language assembled form
    in
    div [ class "ui form examination breast-exam" ]
        inputs


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
            [ ThreeDays
            , Backend.Measurement.Model.OneMonth
            , TwoMonths
            , Backend.Measurement.Model.ThreeMonths
            ]
            []
            form.option
            SetFollowUpOption
            Translate.FollowUpOption
        ]


viewAppointmentConfirmationForm : Language -> NominalDate -> AssembledData -> AppointmentConfirmationForm -> Html Msg
viewAppointmentConfirmationForm language currentDate assembled form =
    let
        appointmentDateForView =
            Maybe.map formatDDMMYYYY form.appointmentDate
                |> Maybe.withDefault ""

        dateSelectorConfig =
            { select = SetAppointmentConfirmation
            , close = SetAppointmentDateSelectorState Nothing
            , dateFrom = currentDate
            , dateTo = Date.add Months 9 currentDate
            , dateDefault = Nothing
            }
    in
    div [ class "form appointment-confirmation" ]
        [ viewLabel language Translate.AppointmentConfirmationInstrunction
        , div
            [ class "form-input date"
            , onClick <| SetAppointmentDateSelectorState (Just dateSelectorConfig)
            ]
            [ text appointmentDateForView ]
        , viewModal <| viewCalendarPopup language form.dateSelectorPopupState form.appointmentDate
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


contentAndTasksLaboratoryTestInitialConfig : ContentAndTasksLaboratoryTestInitialConfig Msg
contentAndTasksLaboratoryTestInitialConfig =
    emptyContentAndTasksLaboratoryTestInitialConfig NoOp
        |> (\config ->
                { config
                    | setHIVTestFormBoolInputMsg = SetHIVTestFormBoolInput
                    , setHIVTestExecutionNoteMsg = SetHIVTestExecutionNote
                    , setHIVTestResultMsg = SetHIVTestResult
                    , setSyphilisTestFormBoolInputMsg = SetSyphilisTestFormBoolInput
                    , setSyphilisTestExecutionNoteMsg = SetSyphilisTestExecutionNote
                    , setHepatitisBTestFormBoolInputMsg = SetHepatitisBTestFormBoolInput
                    , setHepatitisBTestExecutionNoteMsg = SetHepatitisBTestExecutionNote
                    , setMalariaTestFormBoolInputMsg = SetMalariaTestFormBoolInput
                    , setMalariaTestExecutionNoteMsg = SetMalariaTestExecutionNote
                    , setMalariaTestResultMsg = SetMalariaTestResult
                    , setBloodSmearResultMsg = SetBloodSmearResultMsg
                    , setBloodGpRsTestFormBoolInputMsg = SetBloodGpRsTestFormBoolInput
                    , setBloodGpRsTestExecutionNoteMsg = SetBloodGpRsTestExecutionNote
                    , setUrineDipstickTestFormBoolInputMsg = SetUrineDipstickTestFormBoolInput
                    , setUrineDipstickTestExecutionNoteMsg = SetUrineDipstickTestExecutionNote
                    , setUrineDipstickTestVariantMsg = SetUrineDipstickTestVariant
                    , setHemoglobinTestFormBoolInputMsg = SetHemoglobinTestFormBoolInput
                    , setHemoglobinTestExecutionNoteMsg = SetHemoglobinTestExecutionNote
                    , setRandomBloodSugarTestFormBoolInputMsg = SetRandomBloodSugarTestFormBoolInput
                    , setRandomBloodSugarTestExecutionNoteMsg = SetRandomBloodSugarTestExecutionNote
                    , setHIVPCRTestFormBoolInputMsg = SetHIVPCRTestFormBoolInput
                    , setHIVPCRTestExecutionNoteMsg = SetHIVPCRTestExecutionNote
                    , setPartnerHIVTestFormBoolInputMsg = SetPartnerHIVTestFormBoolInput
                    , setPartnerHIVTestExecutionNoteMsg = SetPartnerHIVTestExecutionNote
                    , setPartnerHIVTestResultMsg = SetPartnerHIVTestResult
                }
           )


contentAndTasksForPerformedLaboratoryTestConfig : ContentAndTasksForPerformedLaboratoryTestConfig Msg
contentAndTasksForPerformedLaboratoryTestConfig =
    emptyContentAndTasksForPerformedLaboratoryTestConfig NoOp
        |> (\config ->
                { config
                    | setHIVTestFormBoolInputMsg = SetHIVTestFormBoolInput
                    , setHIVTestExecutionDateMsg = SetHIVTestExecutionDate
                    , setHIVTestDateSelectorStateMsg = SetHIVTestDateSelectorState
                    , setSyphilisTestFormBoolInputMsg = SetSyphilisTestFormBoolInput
                    , setSyphilisTestExecutionDateMsg = SetSyphilisTestExecutionDate
                    , setSyphilisTestDateSelectorStateMsg = SetSyphilisTestDateSelectorState
                    , setHepatitisBTestFormBoolInputMsg = SetHepatitisBTestFormBoolInput
                    , setHepatitisBTestExecutionDateMsg = SetHepatitisBTestExecutionDate
                    , setHepatitisBTestDateSelectorStateMsg = SetHepatitisBTestDateSelectorState
                    , setMalariaTestFormBoolInputMsg = SetMalariaTestFormBoolInput
                    , setMalariaTestExecutionDateMsg = SetMalariaTestExecutionDate
                    , setMalariaTestDateSelectorStateMsg = SetMalariaTestDateSelectorState
                    , setBloodGpRsTestFormBoolInputMsg = SetBloodGpRsTestFormBoolInput
                    , setBloodGpRsTestExecutionDateMsg = SetBloodGpRsTestExecutionDate
                    , setBloodGpRsTestDateSelectorStateMsg = SetBloodGpRsTestDateSelectorState
                    , setUrineDipstickTestFormBoolInputMsg = SetUrineDipstickTestFormBoolInput
                    , setUrineDipstickTestExecutionDateMsg = SetUrineDipstickTestExecutionDate
                    , setUrineDipstickTestDateSelectorStateMsg = SetUrineDipstickTestDateSelectorState
                    , setHemoglobinTestFormBoolInputMsg = SetHemoglobinTestFormBoolInput
                    , setHemoglobinTestExecutionDateMsg = SetHemoglobinTestExecutionDate
                    , setHemoglobinTestDateSelectorStateMsg = SetHemoglobinTestDateSelectorState
                    , setRandomBloodSugarTestFormBoolInputMsg = SetRandomBloodSugarTestFormBoolInput
                    , setRandomBloodSugarTestExecutionDateMsg = SetRandomBloodSugarTestExecutionDate
                    , setRandomBloodSugarTestDateSelectorStateMsg = SetRandomBloodSugarTestDateSelectorState
                    , setRandomBloodSugarResultMsg = SetRandomBloodSugarResult
                    , setHIVPCRTestFormBoolInputMsg = SetHIVPCRTestFormBoolInput
                    , setHIVPCRTestExecutionDateMsg = SetHIVPCRTestExecutionDate
                    , setHIVPCRTestDateSelectorStateMsg = SetHIVPCRTestDateSelectorState
                    , setPartnerHIVTestFormBoolInputMsg = SetPartnerHIVTestFormBoolInput
                    , setPartnerHIVTestExecutionDateMsg = SetPartnerHIVTestExecutionDate
                    , setPartnerHIVTestDateSelectorStateMsg = SetPartnerHIVTestDateSelectorState
                }
           )


viewLabsHistoryForm : Language -> NominalDate -> AssembledData -> LabsHistoryForm -> ( Html Msg, Int, Int )
viewLabsHistoryForm language currentDate assembled form =
    let
        entries =
            List.indexedMap (\index ( date, encounterId, lab ) -> viewEntry date encounterId lab index) pendingLabs
                |> div [ class "history-entries" ]

        input =
            [ viewQuestionLabel language Translate.LabsHistoryCompletedQuestion
            , viewBoolInput
                language
                form.completed
                SetLabsHistoryCompleted
                "completed"
                Nothing
            ]

        pendingLabs =
            generatePendingLabsFromPreviousEncounters assembled
                |> List.concatMap
                    (\( date, encounterId, labs ) ->
                        List.map (\lab -> ( date, encounterId, lab )) labs
                    )

        viewEntry date encounterId lab index =
            div [ class "history-entry" ]
                [ div [ class "index" ] [ text <| String.fromInt (index + 1) ]
                , div [ class "name" ] [ text <| translate language <| Translate.LaboratoryTest lab ]
                , div [ class "date" ] [ text <| formatDDMMYYYY date ]
                , div
                    [ class "action"
                    , onClick <| SetActivePage <| UserPage <| PrenatalLabsHistoryPage assembled.id encounterId lab
                    ]
                    [ text <| translate language Translate.Update ]
                ]
    in
    ( div [ class "ui form laboratory labs-history" ] <|
        [ viewCustomLabel language Translate.LabsHistoryCompletedQuestion "." "label"
        , viewCustomLabel language Translate.LabsHistoryCompletedQuestion "." "instructions"
        ]
            ++ (entries :: input)
    , taskCompleted form.completed
    , 1
    )


viewWaitForm : Language -> NominalDate -> AssembledData -> Html Msg
viewWaitForm language currentDate assembled =
    let
        ( vitalsInstructions, labsResultsInstructions ) =
            getMeasurementValueFunc assembled.measurements.labsResults
                |> Maybe.map
                    (\value ->
                        let
                            labTestPerformed =
                                (not <| EverySet.isEmpty value.performedTests)
                                    && (EverySet.toList value.performedTests
                                            |> List.any ((/=) TestVitalsRecheck)
                                       )
                        in
                        ( if EverySet.member TestVitalsRecheck value.performedTests then
                            viewInstructionsLabel "icon-vitals" (text <| translate language Translate.WaitForVitalsRecheckHelper)

                          else
                            emptyNode
                        , if labTestPerformed then
                            viewInstructionsLabel "icon-labs" (text <| translate language Translate.WaitForLabsResultsHelper)

                          else
                            emptyNode
                        )
                    )
                |> Maybe.withDefault ( emptyNode, emptyNode )
    in
    div [ class "ui form wait" ]
        [ div [ class "instructions" ]
            [ labsResultsInstructions
            , vitalsInstructions
            ]
        , div [ class "instructions" ]
            [ text <| translate language Translate.WaitInstructions ]
        ]


viewPrenatalMedicationForm :
    Language
    -> NominalDate
    -> ((Bool -> MedicationForm -> MedicationForm) -> Bool -> Msg)
    -> AssembledData
    -> MedicationForm
    -> Html Msg
viewPrenatalMedicationForm language currentDate setBoolInputMsg assembled form =
    let
        ( inputs, _ ) =
            resolvePrenatalMedicationFormInputsAndTasks language currentDate setBoolInputMsg assembled form
    in
    div [ class "ui form medication" ] inputs


viewMedicationTreatmentForm :
    Language
    -> NominalDate
    -> ((Bool -> MedicationForm -> MedicationForm) -> Bool -> Msg)
    -> AssembledData
    -> MedicationForm
    -> TreatmentReviewTask
    -> Html Msg
viewMedicationTreatmentForm language currentDate setBoolInputMsg assembled form task =
    let
        ( inputs, _ ) =
            resolveMedicationTreatmentFormInputsAndTasks language currentDate setBoolInputMsg assembled form task
    in
    div [ class "ui form medication" ] inputs


viewBreastfeedingContent : Language -> NominalDate -> AssembledData -> BreastfeedingData -> List (Html Msg)
viewBreastfeedingContent language currentDate assembled data =
    let
        form =
            assembled.measurements.breastfeeding
                |> getMeasurementValueFunc
                |> breastfeedingFormWithDefault data.form

        ( derivedSection, derivedTasks ) =
            Maybe.map
                (\isBreastfeeding ->
                    if isBreastfeeding then
                        let
                            breastPainUpdateFunc value form_ =
                                { form_ | breastPain = Just value, breastPainDirty = True }

                            breastRednessUpdateFunc value form_ =
                                { form_ | breastRedness = Just value, breastRednessDirty = True }

                            enoughMilkUpdateFunc value form_ =
                                { form_ | enoughMilk = Just value, enoughMilkDirty = True }

                            latchingWellUpdateFunc value form_ =
                                { form_ | latchingWell = Just value, latchingWellDirty = True }
                        in
                        ( [ viewQuestionLabel language <| Translate.BreastfeedingSignQuestion BreastPain
                          , viewBoolInput
                                language
                                form.breastPain
                                (SetBreastfeedingBoolInput breastPainUpdateFunc)
                                "breast-pain"
                                Nothing
                          , viewQuestionLabel language <| Translate.BreastfeedingSignQuestion BreastRedness
                          , viewBoolInput
                                language
                                form.breastRedness
                                (SetBreastfeedingBoolInput breastRednessUpdateFunc)
                                "breast-redness"
                                Nothing
                          , viewQuestionLabel language <| Translate.BreastfeedingSignQuestion EnoughMilk
                          , viewBoolInput
                                language
                                form.enoughMilk
                                (SetBreastfeedingBoolInput enoughMilkUpdateFunc)
                                "enough-milk"
                                Nothing
                          , viewQuestionLabel language <| Translate.BreastfeedingSignQuestion LatchingWell
                          , viewBoolInput
                                language
                                form.latchingWell
                                (SetBreastfeedingBoolInput latchingWellUpdateFunc)
                                "latching-well"
                                Nothing
                          ]
                        , [ form.breastPain, form.breastRedness, form.enoughMilk, form.latchingWell ]
                        )

                    else
                        ( [ viewQuestionLabel language Translate.WhyNot
                          , viewCheckBoxSelectInput language
                                reasonsForNotBreastfeedingLeft
                                reasonsForNotBreastfeedingRight
                                form.reasonForNotBreastfeeding
                                SetReasonForNotBreastfeeding
                                Translate.ReasonForNotBreastfeeding
                          ]
                        , [ maybeToBoolTask form.reasonForNotBreastfeeding ]
                        )
                )
                form.isBreastfeeding
                |> Maybe.withDefault ( [], [] )

        tasks =
            form.isBreastfeeding :: derivedTasks

        tasksCompleted =
            Maybe.Extra.values tasks
                |> List.length

        totalTasks =
            List.length tasks

        isBreastfeedingUpdateFunc value form_ =
            { form_
                | isBreastfeeding = Just value
                , reasonForNotBreastfeeding = Nothing
                , reasonForNotBreastfeedingDirty = True
                , breastPain = Nothing
                , breastPainDirty = True
                , breastRedness = Nothing
                , breastRednessDirty = True
                , enoughMilk = Nothing
                , enoughMilkDirty = True
                , latchingWell = Nothing
                , latchingWellDirty = True
            }
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form breeastfeeding" ] <|
                [ viewQuestionLabel language <| Translate.BreastfeedingSignQuestion IsBreastfeeding
                , viewBoolInput
                    language
                    form.isBreastfeeding
                    (SetBreastfeedingBoolInput isBreastfeedingUpdateFunc)
                    "is-breastfeeding"
                    Nothing
                ]
                    ++ derivedSection
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick <| SaveBreastfeeding assembled.participant.person assembled.measurements.breastfeeding
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewPostpartumTreatmentReviewContent : Language -> NominalDate -> AssembledData -> PostpartumTreatmentReviewData -> List (Html Msg)
viewPostpartumTreatmentReviewContent language currentDate assembled data =
    let
        form =
            assembled.measurements.medication
                |> getMeasurementValueFunc
                |> medicationFormWithDefault data.form

        ( tasksCompleted, totalTasks ) =
            ( taskCompleted form.receivedFolicAcid + taskCompleted form.receivedVitaminA
            , 2
            )

        receivedFolicAcidInput =
            [ viewQuestionLabel language Translate.ReceivedFolicAcid
            , viewBoolInput
                language
                form.receivedFolicAcid
                (SetPostpartumTreatmentReviewBoolInput receivedFolicAcidUpdateFunc)
                "folic-acid"
                Nothing
            ]

        receivedFolicAcidUpdateFunc value form_ =
            { form_ | receivedFolicAcid = Just value }

        receivedVitaminAInput =
            [ viewQuestionLabel language Translate.ReceivedVitaminA
            , viewBoolInput
                language
                form.receivedVitaminA
                (SetPostpartumTreatmentReviewBoolInput receivedVitaminAUpdateFunc)
                "vitamin-a"
                Nothing
            ]

        receivedVitaminAUpdateFunc value form_ =
            { form_ | receivedVitaminA = Just value }

        action =
            let
                saveMsg =
                    SavePostpartumTreatmentReview assembled.participant.person assembled.measurements.medication
            in
            if form.receivedVitaminA == Just False then
                SetWarningPopupState (Just (WarningPopupVitaminA saveMsg))

            else
                saveMsg
    in
    [ div [ class "tasks-count" ] [ text <| translate language <| Translate.TasksCompleted tasksCompleted totalTasks ]
    , div [ class "ui full segment" ]
        [ div [ class "full content" ]
            [ div [ class "ui form medication" ] <|
                receivedVitaminAInput
                    ++ receivedFolicAcidInput
            ]
        , div [ class "actions" ]
            [ button
                [ classList [ ( "ui fluid primary button", True ), ( "disabled", tasksCompleted /= totalTasks ) ]
                , onClick action
                ]
                [ text <| translate language Translate.Save ]
            ]
        ]
    ]


viewReferralForm : Language -> NominalDate -> AssembledData -> ReferralForm -> Html Msg
viewReferralForm language currentDate assembled form =
    let
        ( inputs, _ ) =
            case assembled.encounter.encounterType of
                NurseEncounter ->
                    viewForNurse

                NursePostpartumEncounter ->
                    viewForNurse

                _ ->
                    resolveReferralInputsAndTasksForCHW language currentDate assembled form

        viewForNurse =
            resolveReferralInputsAndTasksForNurse language
                currentDate
                assembled
                SetReferralBoolInput
                SetFacilityNonReferralReason
                form
    in
    div [ class "ui form referral" ]
        inputs



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
