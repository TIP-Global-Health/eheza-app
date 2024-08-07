module Pages.Tuberculosis.Activity.Utils exposing (..)

import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.TuberculosisActivity.Model exposing (TuberculosisActivity(..))
import EverySet
import Gizra.Html exposing (emptyNode)
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
import Pages.Tuberculosis.Activity.Model exposing (..)
import Pages.Tuberculosis.Encounter.Model exposing (AssembledData)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , maybeToBoolTask
        , maybeValueConsideringIsDirtyField
        , resolveTasksCompletedFromTotal
        , taskCompleted
        , viewBoolInput
        , viewCheckBoxSelectInput
        , viewQuestionLabel
        )
import Translate exposing (translate)
import Translate.Model exposing (Language(..))


expectActivity : NominalDate -> AssembledData -> TuberculosisActivity -> Bool
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


activityCompleted : NominalDate -> AssembledData -> TuberculosisActivity -> Bool
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
    [ TaskPrescribedMedication, TaskDOT, TaskTreatmentReview ]


resolveMedicationTasks : NominalDate -> AssembledData -> List MedicationTask
resolveMedicationTasks currentDate assembled =
    List.filter (expectMedicationTask currentDate assembled) medicationTasks


expectMedicationTask : NominalDate -> AssembledData -> MedicationTask -> Bool
expectMedicationTask currentDate assembled task =
    case task of
        TaskPrescribedMedication ->
            assembled.initialEncounter

        TaskDOT ->
            not assembled.initialEncounter
                || isJust assembled.measurements.medication

        TaskTreatmentReview ->
            expectMedicationTask currentDate assembled TaskDOT


medicationTaskCompleted : AssembledData -> MedicationTask -> Bool
medicationTaskCompleted assembled task =
    case task of
        TaskPrescribedMedication ->
            isJust assembled.measurements.medication

        TaskDOT ->
            isJust assembled.measurements.dot

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
            in
            ( taskCompleted form.medications
            , 1
            )

        TaskDOT ->
            let
                form =
                    getMeasurementValueFunc assembled.measurements.dot
                        |> dotFormWithDefault data.dotForm

                ( _, tasks ) =
                    dotInputsAndTasks language currentDate assembled form
            in
            resolveTasksCompletedFromTotal tasks

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


adverseEventReported : TuberculosisMeasurements -> Bool
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


symptomReported : TuberculosisMeasurements -> Bool
symptomReported measurements =
    getMeasurementValueFunc measurements.symptomReview
        |> Maybe.map
            (\value ->
                case EverySet.toList value of
                    [] ->
                        False

                    [ NoTuberculosisSymptoms ] ->
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


nextStepsTasksCompletedFromTotal : NominalDate -> TuberculosisMeasurements -> NextStepsData -> NextStepsTask -> ( Int, Int )
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
    let
        followUpTestingTable =
            let
                viewRow stage =
                    div [ class "row" ]
                        [ div [ class "item label" ] [ text <| translate language <| Translate.TuberculosisFollowUpTestingStageLabel stage ]
                        , div [ class "item test" ] [ text <| translate language <| Translate.TuberculosisFollowUpTestingStageTest stage ]
                        , div [ class "item guidance" ] [ text <| translate language <| Translate.TuberculosisFollowUpTestingStageInstructions stage ]
                        ]
            in
            div [ class "follow-up-testing-table" ] <|
                List.map viewRow
                    [ FollowUpTestingMonth1
                    , FollowUpTestingMonth2
                    , FollowUpTestingEndMonth2
                    , FollowUpTestingEndMonth5
                    , FollowUpTestingEndMonth6
                    ]
    in
    ( [ div [ class "ui form health-education" ]
            [ followUpTestingTable
            , viewQuestionLabel language <| Translate.TuberculosisHealthEducationQuestion EducationFollowUpTesting
            , viewBoolInput
                language
                form.followUpTesting
                (SetHealthEducationBoolInput (\value form_ -> { form_ | followUpTesting = Just value }))
                "followup-testing"
                Nothing
            ]
      ]
    , [ maybeToBoolTask form.followUpTesting ]
    )


mandatoryActivitiesForNextStepsCompleted : NominalDate -> AssembledData -> Bool
mandatoryActivitiesForNextStepsCompleted currentDate assembled =
    List.all (activityCompleted currentDate assembled)
        [ Diagnostics, Medication, SymptomReview ]


diagnosticsFormWithDefault : DiagnosticsForm -> Maybe TuberculosisDiagnosticsValue -> DiagnosticsForm
diagnosticsFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    ( diagnosed, isPulmonary ) =
                        case value of
                            TuberculosisPulmonary ->
                                ( Just True, Just True )

                            TuberculosisExtrapulmonary ->
                                ( Just True, Just False )

                            NoTuberculosis ->
                                ( Just False, Nothing )
                in
                { diagnosed = or form.diagnosed diagnosed
                , isPulmonary =
                    maybeValueConsideringIsDirtyField form.isPulmonaryDirty
                        form.isPulmonary
                        isPulmonary
                , isPulmonaryDirty = form.isPulmonaryDirty
                }
            )


toDiagnosticsValueWithDefault : Maybe TuberculosisDiagnosticsValue -> DiagnosticsForm -> Maybe TuberculosisDiagnosticsValue
toDiagnosticsValueWithDefault saved form =
    diagnosticsFormWithDefault form saved
        |> toDiagnosticsValue


toDiagnosticsValue : DiagnosticsForm -> Maybe TuberculosisDiagnosticsValue
toDiagnosticsValue form =
    Maybe.andThen
        (\diagnosed ->
            if not diagnosed then
                Just NoTuberculosis

            else
                Maybe.map
                    (\isPulmonary ->
                        if isPulmonary then
                            TuberculosisPulmonary

                        else
                            TuberculosisExtrapulmonary
                    )
                    form.isPulmonary
        )
        form.diagnosed


symptomReviewFormWithDefault : SymptomReviewForm -> Maybe TuberculosisSymptomReviewValue -> SymptomReviewForm
symptomReviewFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { nightSweats = or form.nightSweats (EverySet.member TuberculosisSymptomNightSweats value |> Just)
                , bloodInSputum = or form.bloodInSputum (EverySet.member TuberculosisSymptomBloodInSputum value |> Just)
                , weightLoss = or form.weightLoss (EverySet.member TuberculosisSymptomWeightLoss value |> Just)
                , severeFatigue = or form.severeFatigue (EverySet.member TuberculosisSymptomSevereFatigue value |> Just)
                }
            )


toSymptomReviewValueWithDefault : Maybe TuberculosisSymptomReviewValue -> SymptomReviewForm -> Maybe TuberculosisSymptomReviewValue
toSymptomReviewValueWithDefault saved form =
    symptomReviewFormWithDefault form saved
        |> toSymptomReviewValue


toSymptomReviewValue : SymptomReviewForm -> Maybe TuberculosisSymptomReviewValue
toSymptomReviewValue form =
    [ ifNullableTrue TuberculosisSymptomNightSweats form.nightSweats
    , ifNullableTrue TuberculosisSymptomBloodInSputum form.bloodInSputum
    , ifNullableTrue TuberculosisSymptomWeightLoss form.weightLoss
    , ifNullableTrue TuberculosisSymptomSevereFatigue form.severeFatigue
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoTuberculosisSymptoms)


toHealthEducationValueWithDefault : Maybe TuberculosisHealthEducationValue -> HealthEducationForm -> Maybe TuberculosisHealthEducationValue
toHealthEducationValueWithDefault saved form =
    healthEducationFormWithDefault form saved
        |> toHealthEducationValue


healthEducationFormWithDefault :
    HealthEducationForm
    -> Maybe TuberculosisHealthEducationValue
    -> HealthEducationForm
healthEducationFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { followUpTesting = or form.followUpTesting (EverySet.member EducationFollowUpTesting value |> Just) }
            )


toHealthEducationValue : HealthEducationForm -> Maybe TuberculosisHealthEducationValue
toHealthEducationValue form =
    [ ifNullableTrue EducationFollowUpTesting form.followUpTesting ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoTuberculosisHealthEducationSigns)


toPrescribedMedicationValueWithDefault : Maybe TuberculosisMedicationValue -> PrescribedMedicationForm -> Maybe TuberculosisMedicationValue
toPrescribedMedicationValueWithDefault saved form =
    prescribedMedicationFormWithDefault form saved
        |> toPrescribedMedicationValue


prescribedMedicationFormWithDefault :
    PrescribedMedicationForm
    -> Maybe TuberculosisMedicationValue
    -> PrescribedMedicationForm
prescribedMedicationFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { medications = or form.medications (Just <| EverySet.toList value)
                , medicationsDirty = form.medicationsDirty
                }
            )


toPrescribedMedicationValue : PrescribedMedicationForm -> Maybe TuberculosisMedicationValue
toPrescribedMedicationValue form =
    Maybe.map EverySet.fromList form.medications


toDOTValueWithDefault : Maybe TuberculosisDOTValue -> DOTForm -> Maybe TuberculosisDOTValue
toDOTValueWithDefault saved form =
    dotFormWithDefault form saved
        |> toDOTValue


dotFormWithDefault :
    DOTForm
    -> Maybe TuberculosisDOTValue
    -> DOTForm
dotFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    provideTodayFromValue =
                        Just <| value.sign == DOTPositive

                    reasonNotProvidedTodayFromValue =
                        if value.sign == DOTPositive then
                            Nothing

                        else
                            Just value.sign

                    distributeMedicationsFromValue =
                        Just <| value.medicationDistributionSign == DOTPositive

                    reasonNotDistributeMedicationsFromValue =
                        if value.medicationDistributionSign == DOTPositive then
                            Nothing

                        else
                            Just value.medicationDistributionSign
                in
                { provideToday = or form.provideToday provideTodayFromValue
                , reasonNotProvidedToday =
                    maybeValueConsideringIsDirtyField form.reasonNotProvidedTodayDirty
                        form.reasonNotProvidedToday
                        reasonNotProvidedTodayFromValue
                , reasonNotProvidedTodayDirty = form.reasonNotProvidedTodayDirty
                , distributeMedications = or form.distributeMedications distributeMedicationsFromValue
                , reasonNotDistributedMedications =
                    maybeValueConsideringIsDirtyField form.reasonNotDistributedMedicationsDirty
                        form.reasonNotDistributedMedications
                        reasonNotDistributeMedicationsFromValue
                , reasonNotDistributedMedicationsDirty = form.reasonNotDistributedMedicationsDirty
                }
            )


toDOTValue : DOTForm -> Maybe TuberculosisDOTValue
toDOTValue form =
    let
        maybeSign =
            let
                signPositive =
                    if form.provideToday == Just True then
                        Just DOTPositive

                    else
                        Nothing
            in
            or signPositive form.reasonNotProvidedToday

        maybeDistributeMedications =
            let
                distributeMedicationsPositive =
                    if form.distributeMedications == Just True then
                        Just DOTPositive

                    else
                        Nothing
            in
            or distributeMedicationsPositive form.reasonNotDistributedMedications
    in
    Maybe.map2
        (\sign medicationDistributionSign ->
            { sign = sign
            , medicationDistributionSign = medicationDistributionSign
            }
        )
        maybeSign
        maybeDistributeMedications


dotInputsAndTasks : Language -> NominalDate -> AssembledData -> DOTForm -> ( List (Html Msg), List (Maybe Bool) )
dotInputsAndTasks language currentDate assembled form =
    let
        ( provideTodayInputs, provideTodayTasks ) =
            let
                ( derivedInputs, derivedTasks ) =
                    if form.provideToday == Just False then
                        ( [ viewQuestionLabel language Translate.WhyNot
                          , viewCheckBoxSelectInput language
                                [ DOTNegativeTakenToday
                                , DOTNegativeNotIndicated
                                , DOTNegativeUnavailable
                                , DOTNegativeSideEffects
                                , DOTNegativePatientRefused
                                ]
                                []
                                form.reasonNotProvidedToday
                                SetReasonNotProvidedToday
                                Translate.TuberculosisReasonNotProvidedToday
                          ]
                        , [ maybeToBoolTask form.reasonNotProvidedToday ]
                        )

                    else
                        ( [], [] )
            in
            ( [ viewQuestionLabel language Translate.TuberculosisProvideDOTTodayQuestion
              , viewBoolInput
                    language
                    form.provideToday
                    (SetDOTBoolInput
                        (\value form_ ->
                            { form_
                                | provideToday = Just value
                                , reasonNotProvidedToday = Nothing
                                , reasonNotProvidedTodayDirty = True
                            }
                        )
                    )
                    "provide-today"
                    Nothing
              ]
                ++ derivedInputs
            , form.provideToday :: derivedTasks
            )

        ( distributeMedicationsInputs, distributeMedicationsTasks ) =
            let
                prescribedMedications =
                    let
                        medicationMeasurement =
                            if assembled.initialEncounter then
                                assembled.measurements.medication

                            else
                                List.filterMap (.measurements >> .medication)
                                    assembled.previousEncountersData
                                    |> List.head
                    in
                    getMeasurementValueFunc medicationMeasurement
                        |> Maybe.map (EverySet.remove NoTuberculosisPrescribedMedications)
                        |> Maybe.withDefault EverySet.empty

                prescribedMedicationsForView =
                    if EverySet.isEmpty prescribedMedications then
                        emptyNode

                    else
                        EverySet.toList prescribedMedications
                            |> List.map
                                (\medicatiopn ->
                                    p [] [ text <| translate language <| Translate.TuberculosisPrescribedMedication medicatiopn ]
                                )
                            |> div [ class "prescribed-medications" ]

                ( derivedInputs, derivedTasks ) =
                    if form.distributeMedications == Just False then
                        ( [ viewQuestionLabel language Translate.WhyNot
                          , viewCheckBoxSelectInput language
                                [ DOTNegativeTakenToday
                                , DOTNegativeUnavailable
                                , DOTNegativeSideEffects
                                , DOTNegativePatientRefused
                                ]
                                []
                                form.reasonNotDistributedMedications
                                SetReasonMedicationsNotDistributed
                                Translate.TuberculosisReasonMedicationsNotDistributed
                          ]
                        , [ maybeToBoolTask form.reasonNotDistributedMedications ]
                        )

                    else
                        ( [], [] )
            in
            ( [ viewQuestionLabel language Translate.TuberculosisDistributeMedicationsQuestion
              , prescribedMedicationsForView
              , viewBoolInput
                    language
                    form.distributeMedications
                    (SetDOTBoolInput
                        (\value form_ ->
                            { form_
                                | distributeMedications = Just value
                                , reasonNotDistributedMedications = Nothing
                                , reasonNotDistributedMedicationsDirty = True
                            }
                        )
                    )
                    "distribute-medications"
                    Nothing
              ]
                ++ derivedInputs
            , form.distributeMedications :: derivedTasks
            )
    in
    ( provideTodayInputs ++ distributeMedicationsInputs
    , provideTodayTasks ++ distributeMedicationsTasks
    )
