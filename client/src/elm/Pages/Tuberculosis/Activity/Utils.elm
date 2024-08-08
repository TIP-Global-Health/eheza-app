module Pages.Tuberculosis.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.TuberculosisActivity.Model exposing (TuberculosisActivity(..))
import Backend.TuberculosisActivity.Utils exposing (allActivities)
import Date
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe.Extra exposing (andMap, isJust, or, unwrap)
import Measurement.Utils
    exposing
        ( followUpFormWithDefault
        , ongoingTreatmentReviewFormWithDefault
        , sendToHCFormWithDefault
        , treatmentReviewInputsAndTasks
        )
import Pages.Tuberculosis.Activity.Model exposing (..)
import Pages.Tuberculosis.Encounter.Model exposing (AssembledData, EncounterData)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeToBoolTask
        , maybeValueConsideringIsDirtyField
        , taskCompleted
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomLabel
        , viewLabel
        , viewNumberInput
        , viewQuestionLabel
        )
import Translate exposing (TranslationId, translate)
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
            True

        TaskDOT ->
            isJust assembled.measurements.medication

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

                ( _, ( tasksCompleted, tasksTotal ) ) =
                    prescribedMedicationsInputsAndTasks language currentDate assembled form
            in
            ( tasksCompleted
            , tasksTotal
            )

        TaskDOT ->
            let
                form =
                    getMeasurementValueFunc assembled.measurements.dot
                        |> dotFormWithDefault data.dotForm

                ( _, tasks ) =
                    dotInputsAndTasks language currentDate assembled form
            in
            ( Maybe.Extra.values tasks
                |> List.length
            , List.length tasks
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
            ( Maybe.Extra.values tasks
                |> List.length
            , List.length tasks
            )


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


nextStepsTasksCompletedFromTotal : TuberculosisMeasurements -> NextStepsData -> NextStepsTask -> ( Int, Int )
nextStepsTasksCompletedFromTotal measurements data task =
    case task of
        TaskHealthEducation ->
            let
                form =
                    getMeasurementValueFunc measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm
            in
            ( taskCompleted form.followUpTesting
            , 1
            )

        TaskFollowUp ->
            let
                form =
                    getMeasurementValueFunc measurements.followUp
                        |> followUpFormWithDefault data.followUpForm
            in
            ( taskCompleted form.option
            , 1
            )

        TaskReferral ->
            let
                form =
                    getMeasurementValueFunc measurements.referral
                        |> sendToHCFormWithDefault data.sendToHCForm

                ( reasonForNotSentActive, reasonForNotSentCompleted ) =
                    form.referToHealthCenter
                        |> Maybe.map
                            (\sentToHC ->
                                if not sentToHC then
                                    if isJust form.reasonForNotSendingToHC then
                                        ( 2, 2 )

                                    else
                                        ( 1, 2 )

                                else
                                    ( 1, 1 )
                            )
                        |> Maybe.withDefault ( 0, 1 )
            in
            ( reasonForNotSentActive + taskCompleted form.handReferralForm
            , reasonForNotSentCompleted + 1
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
                let
                    ( medicationsNotChangedFromValue, medicationsFromValue ) =
                        case EverySet.toList value of
                            [ TuberculosisMedicationsNotChanged ] ->
                                ( Just True, Nothing )

                            _ ->
                                ( Just False, Just <| EverySet.toList value )
                in
                { medications = or form.medications medicationsFromValue
                , medicationsNotChanged = or form.medicationsNotChanged medicationsNotChangedFromValue
                }
            )


toPrescribedMedicationValue : PrescribedMedicationForm -> Maybe TuberculosisMedicationValue
toPrescribedMedicationValue form =
    if form.medicationsNotChanged == Just True then
        EverySet.singleton TuberculosisMedicationsNotChanged |> Just

    else
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
                prescribedMedicationsForView =
                    generateAllEncountersData assembled
                        |> resolvePrescribedMedicationSets TuberculosisMedicationsNotChanged
                        |> Tuple.first
                        |> Maybe.map
                            (EverySet.toList
                                >> List.map
                                    (\medicatiopn ->
                                        p [] [ text <| translate language <| Translate.TuberculosisPrescribedMedication medicatiopn ]
                                    )
                                >> div [ class "prescribed-medications" ]
                            )
                        |> Maybe.withDefault emptyNode

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


prescribedMedicationsInputsAndTasks :
    Language
    -> NominalDate
    -> AssembledData
    -> PrescribedMedicationForm
    -> ( List (Html Msg), ( Int, Int ) )
prescribedMedicationsInputsAndTasks language currentDate assembled form =
    let
        ( recordMedicationsInputs, recordMedicationsTasks ) =
            recordMedicationsInputsAndTasks language Translate.PrescribedMedicationsTakenQuestion form
    in
    if assembled.initialEncounter then
        ( recordMedicationsInputs, recordMedicationsTasks )

    else
        generateAllEncountersData assembled
            |> resolvePrescribedMedicationSets TuberculosisMedicationsNotChanged
            |> Tuple.first
            |> Maybe.map
                (\prescribedMedication ->
                    let
                        ( derivedInputs, ( derivedTasksCompleted, derivedTasksTotal ) ) =
                            if form.medicationsNotChanged /= Just False then
                                ( [], ( 0, 0 ) )

                            else
                                ( recordMedicationsInputs, recordMedicationsTasks )

                        prescribedMedicationForView =
                            EverySet.toList prescribedMedication
                                |> List.map
                                    (\medication ->
                                        li [] [ text <| translate language <| Translate.TuberculosisPrescribedMedication medication ]
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
            |> Maybe.withDefault ( recordMedicationsInputs, recordMedicationsTasks )


recordMedicationsInputsAndTasks :
    Language
    -> TranslationId
    -> PrescribedMedicationForm
    -> ( List (Html Msg), ( Int, Int ) )
recordMedicationsInputsAndTasks language questionTransId form =
    ( [ div [ class "ui form prescribed-medication" ]
            [ viewQuestionLabel language questionTransId
            , viewCheckBoxMultipleSelectInput language
                [ MedicationRHZE
                , MedicationRH
                , MedicationOther
                ]
                []
                (Maybe.withDefault [] form.medications)
                Nothing
                SetPrescribedMedication
                Translate.TuberculosisPrescribedMedication
            ]
      ]
    , ( taskCompleted form.medications
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
    TuberculosisPrescribedMedication
    -> List EncounterData
    -> ( Maybe (EverySet TuberculosisPrescribedMedication), Maybe (EverySet TuberculosisPrescribedMedication) )
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
