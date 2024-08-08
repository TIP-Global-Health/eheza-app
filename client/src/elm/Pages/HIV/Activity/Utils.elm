module Pages.HIV.Activity.Utils exposing (..)

import Backend.HIVActivity.Model exposing (HIVActivity(..))
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe.Extra exposing (isJust, or, unwrap)
import Measurement.Utils
    exposing
        ( followUpFormWithDefault
        , ongoingTreatmentReviewFormWithDefault
        , sendToHCFormWithDefault
        , treatmentReviewInputsAndTasks
        )
import Measurement.View exposing (followUpFormInputsAndTasks, sendToFacilityInputsAndTasks)
import Pages.HIV.Activity.Model exposing (..)
import Pages.HIV.Encounter.Model exposing (AssembledData, EncounterData)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , maybeValueConsideringIsDirtyField
        , resolveTasksCompletedFromTotal
        , taskCompleted
        , viewBoolInput
        , viewCheckBoxMultipleSelectSectionsInput
        , viewLabel
        , viewQuestionLabel
        )
import Translate exposing (TranslationId, translate)
import Translate.Model exposing (Language(..))


expectActivity : NominalDate -> AssembledData -> HIVActivity -> Bool
expectActivity currentDate assembled activity =
    case activity of
        Diagnostics ->
            assembled.initialEncounter

        Medication ->
            resolveMedicationTasks currentDate assembled
                |> List.isEmpty
                |> not

        SymptomReview ->
            not assembled.initialEncounter

        NextSteps ->
            mandatoryActivitiesForNextStepsCompleted currentDate assembled
                && (resolveNextStepsTasks currentDate assembled
                        |> List.isEmpty
                        |> not
                   )


activityCompleted : NominalDate -> AssembledData -> HIVActivity -> Bool
activityCompleted currentDate assembled activity =
    let
        notExpected activityToCheck =
            not <| expectActivity currentDate assembled activityToCheck
    in
    case activity of
        Diagnostics ->
            notExpected Diagnostics
                || isJust assembled.measurements.diagnostics

        Medication ->
            notExpected Medication
                || (resolveMedicationTasks currentDate assembled
                        |> List.all (medicationTaskCompleted assembled)
                   )

        SymptomReview ->
            notExpected SymptomReview
                || isJust assembled.measurements.symptomReview

        NextSteps ->
            notExpected NextSteps
                || (resolveNextStepsTasks currentDate assembled
                        |> List.all (nextStepsTaskCompleted assembled)
                   )


medicationTasks : List MedicationTask
medicationTasks =
    [ TaskPrescribedMedication, TaskTreatmentReview ]


resolveMedicationTasks : NominalDate -> AssembledData -> List MedicationTask
resolveMedicationTasks currentDate assembled =
    List.filter (expectMedicationTask currentDate assembled) medicationTasks


expectMedicationTask : NominalDate -> AssembledData -> MedicationTask -> Bool
expectMedicationTask currentDate assembled task =
    case task of
        TaskPrescribedMedication ->
            True

        TaskTreatmentReview ->
            not assembled.initialEncounter
                || isJust assembled.measurements.medication


medicationTaskCompleted : AssembledData -> MedicationTask -> Bool
medicationTaskCompleted assembled task =
    case task of
        TaskPrescribedMedication ->
            isJust assembled.measurements.medication

        TaskTreatmentReview ->
            isJust assembled.measurements.treatmentReview


medicationTasksCompletedFromTotal : Language -> NominalDate -> AssembledData -> MedicationData -> MedicationTask -> ( Int, Int )
medicationTasksCompletedFromTotal language currentDate assembled data task =
    case task of
        TaskPrescribedMedication ->
            let
                form =
                    getMeasurementValueFunc assembled.measurements.medication
                        |> prescribedMedicationFormWithDefault data.prescribedMedicationForm

                ( _, ( tasksCompleted, tasksTotal ) ) =
                    prescribedMedicationsInputsAndTasks language currentDate assembled form
            in
            ( tasksCompleted
            , tasksTotal
            )

        TaskTreatmentReview ->
            let
                form =
                    getMeasurementValueFunc assembled.measurements.treatmentReview
                        |> ongoingTreatmentReviewFormWithDefault data.treatmentReviewForm

                ( _, tasks ) =
                    treatmentReviewInputsAndTasks language
                        currentDate
                        SetTreatmentReviewBoolInput
                        SetReasonForNotTaking
                        SetTotalMissedDoses
                        SetAdverseEvent
                        form
            in
            resolveTasksCompletedFromTotal tasks


nextStepsTasks : List NextStepsTask
nextStepsTasks =
    [ TaskReferral, TaskHealthEducation, TaskFollowUp ]


resolveNextStepsTasks : NominalDate -> AssembledData -> List NextStepsTask
resolveNextStepsTasks currentDate assembled =
    List.filter (expectNextStepsTask currentDate assembled) nextStepsTasks


expectNextStepsTask : NominalDate -> AssembledData -> NextStepsTask -> Bool
expectNextStepsTask currentDate assembled task =
    case task of
        TaskReferral ->
            adverseEventReported assembled.measurements
                || symptomReported assembled.measurements

        TaskHealthEducation ->
            -- Always provide health education.
            True

        TaskFollowUp ->
            -- Always schedule follow up.
            True


adverseEventReported : HIVMeasurements -> Bool
adverseEventReported measurements =
    getMeasurementValueFunc measurements.treatmentReview
        |> Maybe.map
            (\value ->
                case EverySet.toList value.adverseEvents of
                    [] ->
                        False

                    [ NoAdverseEvent ] ->
                        False

                    _ ->
                        True
            )
        |> Maybe.withDefault False


symptomReported : HIVMeasurements -> Bool
symptomReported measurements =
    getMeasurementValueFunc measurements.symptomReview
        |> Maybe.map
            (\value ->
                case EverySet.toList value of
                    [] ->
                        False

                    [ NoHIVSymptoms ] ->
                        False

                    _ ->
                        True
            )
        |> Maybe.withDefault False


nextStepsTaskCompleted : AssembledData -> NextStepsTask -> Bool
nextStepsTaskCompleted assembled task =
    case task of
        TaskReferral ->
            isJust assembled.measurements.referral

        TaskHealthEducation ->
            isJust assembled.measurements.healthEducation

        TaskFollowUp ->
            isJust assembled.measurements.followUp


nextStepsTasksCompletedFromTotal : NominalDate -> HIVMeasurements -> NextStepsData -> NextStepsTask -> ( Int, Int )
nextStepsTasksCompletedFromTotal currentDate measurements data task =
    let
        ( _, tasks ) =
            case task of
                TaskHealthEducation ->
                    getMeasurementValueFunc measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> healthEducationFormInputsAndTasks English currentDate

                TaskFollowUp ->
                    getMeasurementValueFunc measurements.followUp
                        |> followUpFormWithDefault data.followUpForm
                        |> followUpFormInputsAndTasks English
                            currentDate
                            []
                            SetFollowUpOption

                TaskReferral ->
                    getMeasurementValueFunc measurements.referral
                        |> sendToHCFormWithDefault data.sendToHCForm
                        |> sendToFacilityInputsAndTasks English
                            currentDate
                            FacilityHealthCenter
                            SetReferToHealthCenter
                            SetReasonForNonReferral
                            SetHandReferralForm
                            Nothing
    in
    resolveTasksCompletedFromTotal tasks


healthEducationFormInputsAndTasks : Language -> NominalDate -> HealthEducationForm -> ( List (Html Msg), List (Maybe Bool) )
healthEducationFormInputsAndTasks language currentDate form =
    ( [ viewQuestionLabel language <| Translate.HIVHealthEducationQuestion EducationPositiveResult
      , viewBoolInput
            language
            form.positiveResult
            (SetHealthEducationBoolInput (\value form_ -> { form_ | positiveResult = Just value }))
            "positive-result"
            Nothing
      , viewQuestionLabel language <| Translate.HIVHealthEducationQuestion EducationSaferSexPractices
      , viewBoolInput
            language
            form.saferSexPractices
            (SetHealthEducationBoolInput (\value form_ -> { form_ | saferSexPractices = Just value }))
            "safer-sex-practices"
            Nothing
      , viewQuestionLabel language <| Translate.HIVHealthEducationQuestion EducationEncouragedPartnerTesting
      , viewBoolInput
            language
            form.encouragedPartnerTesting
            (SetHealthEducationBoolInput (\value form_ -> { form_ | encouragedPartnerTesting = Just value }))
            "encouraged-partner-testing"
            Nothing
      , viewQuestionLabel language <| Translate.HIVHealthEducationQuestion EducationFamilyPlanningOptions
      , viewBoolInput
            language
            form.familyPlanningOptions
            (SetHealthEducationBoolInput (\value form_ -> { form_ | familyPlanningOptions = Just value }))
            "family-planning-options"
            Nothing
      ]
    , [ form.positiveResult, form.saferSexPractices, form.encouragedPartnerTesting, form.familyPlanningOptions ]
    )


mandatoryActivitiesForNextStepsCompleted : NominalDate -> AssembledData -> Bool
mandatoryActivitiesForNextStepsCompleted currentDate assembled =
    List.all (activityCompleted currentDate assembled)
        [ Diagnostics, Medication, SymptomReview ]


diagnosticsFormWithDefault : DiagnosticsForm -> Maybe HIVDiagnosticsValue -> DiagnosticsForm
diagnosticsFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    positiveResultDateEstimatedByValue =
                        EverySet.member HIVResultDateEstimated value.signs

                    runHIVTestByValue =
                        EverySet.member HIVTestRun value.signs
                in
                { resultPositive = or form.resultPositive (EverySet.member HIVResultPositiveReported value.signs |> Just)
                , resultDateCorrect = or form.resultDateCorrect (EverySet.member HIVResultPositiveKnown value.signs |> Just)
                , positiveResultDate =
                    maybeValueConsideringIsDirtyField form.positiveResultDateDirty
                        form.positiveResultDate
                        value.positiveResultDate
                , positiveResultDateDirty = form.positiveResultDateDirty
                , positiveResultDateEstimated =
                    maybeValueConsideringIsDirtyField form.positiveResultDateEstimatedDirty
                        form.positiveResultDateEstimated
                        (Just positiveResultDateEstimatedByValue)
                , positiveResultDateEstimatedDirty = form.positiveResultDateEstimatedDirty
                , dateSelectorPopupState = form.dateSelectorPopupState
                , runHIVTest =
                    maybeValueConsideringIsDirtyField form.runHIVTestDirty
                        form.runHIVTest
                        (Just runHIVTestByValue)
                , runHIVTestDirty = form.runHIVTestDirty
                , testResult =
                    maybeValueConsideringIsDirtyField form.testResultDirty
                        form.testResult
                        value.testResult
                , testResultDirty = False
                }
            )


toDiagnosticsValueWithDefault : Bool -> Maybe HIVDiagnosticsValue -> DiagnosticsForm -> Maybe HIVDiagnosticsValue
toDiagnosticsValueWithDefault positiveResultRecorded saved form =
    diagnosticsFormWithDefault form saved
        |> toDiagnosticsValue positiveResultRecorded


toDiagnosticsValue : Bool -> DiagnosticsForm -> Maybe HIVDiagnosticsValue
toDiagnosticsValue positiveResultRecorded form =
    let
        esitmatedSign =
            if form.positiveResultDateEstimated == Just True then
                [ HIVResultDateEstimated ]

            else
                []
    in
    if positiveResultRecorded then
        Maybe.map
            (\resultDateCorrect ->
                let
                    mainSign =
                        if resultDateCorrect then
                            [ HIVResultPositiveKnown ]

                        else
                            []
                in
                { signs = EverySet.fromList <| mainSign ++ esitmatedSign
                , positiveResultDate = form.positiveResultDate
                , testResult = form.testResult
                }
            )
            form.resultDateCorrect

    else
        Maybe.map
            (\resultPositive ->
                let
                    mainSign =
                        if resultPositive then
                            [ HIVResultPositiveReported ]

                        else if form.runHIVTest == Just True then
                            [ HIVTestRun ]

                        else
                            []
                in
                { signs = EverySet.fromList <| mainSign ++ esitmatedSign
                , positiveResultDate = form.positiveResultDate
                , testResult = form.testResult
                }
            )
            form.resultPositive


toPrescribedMedicationValueWithDefault : Maybe HIVMedicationValue -> PrescribedMedicationForm -> Maybe HIVMedicationValue
toPrescribedMedicationValueWithDefault saved form =
    prescribedMedicationFormWithDefault form saved
        |> toPrescribedMedicationValue


prescribedMedicationFormWithDefault :
    PrescribedMedicationForm
    -> Maybe HIVMedicationValue
    -> PrescribedMedicationForm
prescribedMedicationFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    ( medicationsNotChangedFromValue, medicationsFromValue ) =
                        case EverySet.toList value of
                            [ HIVMedicationsNotChanged ] ->
                                ( Just True, Nothing )

                            _ ->
                                ( Just False, Just <| EverySet.toList value )
                in
                { medicationsNotChanged = or form.medicationsNotChanged medicationsNotChangedFromValue
                , medications = or form.medications medicationsFromValue
                }
            )


toPrescribedMedicationValue : PrescribedMedicationForm -> Maybe HIVMedicationValue
toPrescribedMedicationValue form =
    if form.medicationsNotChanged == Just True then
        EverySet.singleton HIVMedicationsNotChanged |> Just

    else
        Maybe.map EverySet.fromList form.medications


toSymptomReviewValueWithDefault : Maybe HIVSymptomReviewValue -> SymptomReviewForm -> Maybe HIVSymptomReviewValue
toSymptomReviewValueWithDefault saved form =
    symptomReviewFormWithDefault form saved
        |> toSymptomReviewValue


symptomReviewFormWithDefault :
    SymptomReviewForm
    -> Maybe HIVSymptomReviewValue
    -> SymptomReviewForm
symptomReviewFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { symptoms = or form.symptoms (Just <| EverySet.toList value)
                , symptomsDirty = form.symptomsDirty
                }
            )


toSymptomReviewValue : SymptomReviewForm -> Maybe HIVSymptomReviewValue
toSymptomReviewValue form =
    Maybe.map EverySet.fromList form.symptoms


toHealthEducationValueWithDefault : Maybe HIVHealthEducationValue -> HealthEducationForm -> Maybe HIVHealthEducationValue
toHealthEducationValueWithDefault saved form =
    healthEducationFormWithDefault form saved
        |> toHealthEducationValue


healthEducationFormWithDefault :
    HealthEducationForm
    -> Maybe HIVHealthEducationValue
    -> HealthEducationForm
healthEducationFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { positiveResult = or form.positiveResult (EverySet.member EducationPositiveResult value |> Just)
                , saferSexPractices = or form.saferSexPractices (EverySet.member EducationSaferSexPractices value |> Just)
                , encouragedPartnerTesting = or form.encouragedPartnerTesting (EverySet.member EducationEncouragedPartnerTesting value |> Just)
                , familyPlanningOptions = or form.familyPlanningOptions (EverySet.member EducationFamilyPlanningOptions value |> Just)
                }
            )


toHealthEducationValue : HealthEducationForm -> Maybe HIVHealthEducationValue
toHealthEducationValue form =
    [ ifNullableTrue EducationPositiveResult form.positiveResult
    , ifNullableTrue EducationSaferSexPractices form.saferSexPractices
    , ifNullableTrue EducationEncouragedPartnerTesting form.encouragedPartnerTesting
    , ifNullableTrue EducationFamilyPlanningOptions form.familyPlanningOptions
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoHIVHealthEducationSigns)


mostCommonAntiRetroviralMedications : List HIVPrescribedMedication
mostCommonAntiRetroviralMedications =
    [ HIVMedicationDolutegravirLamivudineTenofovir
    , HIVMedicationAtazanavirRitonavir
    , HIVMedicationDolutegravir
    , HIVMedicationAbacavirLamivudine
    , HIVMedicationLamivudineTenofovir
    , HIVMedicationZidovudine
    ]


lessCommonAntiRetroviralMedications : List HIVPrescribedMedication
lessCommonAntiRetroviralMedications =
    [ HIVMedicationLamivudineZidovudineNevirapine
    , HIVMedicationEfavirenzLamivudineTenofovir
    , HIVMedicationLamivudineZidovudine
    , HIVMedicationLopinavirRitonavir
    , HIVMedicationDarunavirRitonavir
    , HIVMedicationDarunavirCobicistat
    , HIVMedicationRaltegravir
    , HIVMedicationEfavirenz
    , HIVMedicationNevirapine
    , HIVMedicationEtravirine
    , HIVMedicationTenofovir
    , HIVMedicationLamivudine
    , HIVMedicationAbacavir
    ]


prophylaxisMedications : List HIVPrescribedMedication
prophylaxisMedications =
    [ HIVMedicationBactrim
    , HIVMedicationTrimethoprimSulfamethoxazole
    , HIVMedicationCoTrimoxazoleTablets
    , HIVMedicationCoTrimoxazoleOralSuspension
    ]


prescribedMedicationsInputsAndTasks :
    Language
    -> NominalDate
    -> AssembledData
    -> PrescribedMedicationForm
    -> ( List (Html Msg), ( Int, Int ) )
prescribedMedicationsInputsAndTasks language currentDate assembled form =
    let
        ( recordMedicationsForm, recordMedicationsTasks ) =
            recordMedicationsFormAndTasks language Translate.PrescribedMedicationsTakenQuestion form
    in
    if assembled.initialEncounter then
        ( recordMedicationsForm, recordMedicationsTasks )

    else
        generateAllEncountersData assembled
            |> resolvePrescribedMedicationSets HIVMedicationsNotChanged
            |> Tuple.first
            |> Maybe.map
                (\prescribedMedication ->
                    let
                        ( derivedInputs, ( derivedTasksCompleted, derivedTasksTotal ) ) =
                            if form.medicationsNotChanged /= Just False then
                                ( [], ( 0, 0 ) )

                            else
                                ( recordMedicationsForm, recordMedicationsTasks )

                        prescribedMedicationForView =
                            EverySet.toList prescribedMedication
                                |> List.map
                                    (\medication ->
                                        li [] [ text <| translate language <| Translate.HIVPrescribedMedication medication ]
                                    )
                                |> ul []
                    in
                    ( [ viewLabel language Translate.PrescribedMedication
                      , prescribedMedicationForView
                      , viewQuestionLabel language Translate.PrescribedMedicationsChangedQuestion
                      , viewBoolInput
                            language
                            form.medicationsNotChanged
                            SetPrescribedMedicationsNotChanged
                            "medications-changed"
                            Nothing
                      ]
                        ++ derivedInputs
                    , ( taskCompleted form.medicationsNotChanged + derivedTasksCompleted
                      , 1 + derivedTasksTotal
                      )
                    )
                )
            |> Maybe.withDefault ( recordMedicationsForm, recordMedicationsTasks )


recordMedicationsFormAndTasks :
    Language
    -> TranslationId
    -> PrescribedMedicationForm
    -> ( List (Html Msg), ( Int, Int ) )
recordMedicationsFormAndTasks language questionTransId form =
    ( [ div [ class "ui form prescribed-medication" ]
            [ viewQuestionLabel language questionTransId
            , viewCheckBoxMultipleSelectSectionsInput language
                [ ( Translate.MostCommonAntiRetroviralMedications, mostCommonAntiRetroviralMedications )
                , ( Translate.LessCommonAntiRetroviralMedications, lessCommonAntiRetroviralMedications )
                , ( Translate.ProphylaxisMedications, prophylaxisMedications )
                ]
                (Maybe.withDefault [] form.medications)
                SetPrescribedMedication
                Translate.HIVPrescribedMedication
            ]
      ]
    , ( Maybe.map
            (\medications ->
                let
                    mandatoryGroup =
                        mostCommonAntiRetroviralMedications ++ lessCommonAntiRetroviralMedications
                in
                if List.any (\medication -> List.member medication medications) mandatoryGroup then
                    1

                else
                    0
            )
            form.medications
            |> Maybe.withDefault 0
      , 1
      )
    )


generateAllEncountersData : AssembledData -> List EncounterData
generateAllEncountersData assembled =
    { id = assembled.id
    , startDate = assembled.encounter.startDate
    , measurements = assembled.measurements
    }
        :: assembled.previousEncountersData


resolvePrescribedMedicationSets :
    HIVPrescribedMedication
    -> List EncounterData
    -> ( Maybe (EverySet HIVPrescribedMedication), Maybe (EverySet HIVPrescribedMedication) )
resolvePrescribedMedicationSets notChangedOption allEncountersData =
    let
        prescribedMedicationSets =
            List.filterMap (.measurements >> .medication >> getMeasurementValueFunc)
                allEncountersData
                |> List.filter (not << EverySet.member notChangedOption)
    in
    ( List.head prescribedMedicationSets
    , List.drop 1 prescribedMedicationSets |> List.head
    )
