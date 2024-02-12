module Pages.Tuberculosis.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.TuberculosisActivity.Model exposing (TuberculosisActivity(..))
import Backend.TuberculosisActivity.Utils exposing (allActivities)
import Date
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe.Extra exposing (andMap, isJust, or, unwrap)
import Measurement.Utils
    exposing
        ( followUpFormWithDefault
        , ongoingTreatmentReviewFormWithDefault
        , sendToHCFormWithDefault
        )
import Pages.Tuberculosis.Activity.Model exposing (..)
import Pages.Tuberculosis.Encounter.Model exposing (AssembledData)
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
        , viewNumberInput
        , viewQuestionLabel
        )
import Translate
import Translate.Model exposing (Language(..))


expectActivity : NominalDate -> AssembledData -> TuberculosisActivity -> Bool
expectActivity currentDate assembled activity =
    case activity of
        Diagnostics ->
            True

        Medication ->
            True

        SymptomReview ->
            True

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
                || isJust assembled.measurements.medication

        SymptomReview ->
            notExpected SymptomReview
                || isJust assembled.measurements.symptomReview

        NextSteps ->
            resolveNextStepsTasks currentDate assembled
                |> List.all (nextStepsTaskCompleted assembled)


medicationTasks : List MedicationTask
medicationTasks =
    [ TaskPrescribedMedication, TaskDOT, TaskTreatmentReview ]


resolveMedicationTasks : NominalDate -> AssembledData -> List MedicationTask
resolveMedicationTasks currentDate assembled =
    List.filter (expectMedicationTask currentDate assembled) medicationTasks


expectMedicationTask : NominalDate -> AssembledData -> MedicationTask -> Bool
expectMedicationTask currentDate assembled task =
    case task of
        _ ->
            -- @todo:
            True


medicationTaskCompleted : AssembledData -> MedicationTask -> Bool
medicationTaskCompleted assembled task =
    case task of
        TaskPrescribedMedication ->
            isJust assembled.measurements.medication

        TaskDOT ->
            isJust assembled.measurements.dot

        TaskTreatmentReview ->
            isJust assembled.measurements.treatmentReview


medicationTasksCompletedFromTotal : TuberculosisMeasurements -> MedicationData -> MedicationTask -> ( Int, Int )
medicationTasksCompletedFromTotal measurements data task =
    case task of
        TaskPrescribedMedication ->
            let
                form =
                    getMeasurementValueFunc measurements.medication
                        |> prescribedMedicationFormWithDefault data.prescribedMedicationForm
            in
            ( taskCompleted form.medication
            , 1
            )

        TaskDOT ->
            let
                form =
                    getMeasurementValueFunc measurements.dot
                        |> dotFormWithDefault data.dotForm

                ( provideTodayActive, provideTodayCompleted ) =
                    Maybe.map
                        (\provideToday ->
                            if provideToday then
                                ( 1, 1 )

                            else if isJust form.reasonNotProvidedToday then
                                ( 2, 2 )

                            else
                                ( 1, 2 )
                        )
                        form.provideToday
                        |> Maybe.withDefault ( 0, 1 )

                ( distributeMedicationsActive, distributeMedicationsCompleted ) =
                    Maybe.map
                        (\distributeMedications ->
                            if distributeMedications then
                                ( 1, 1 )

                            else if isJust form.reasonNotDistributedMedications then
                                ( 2, 2 )

                            else
                                ( 1, 2 )
                        )
                        form.distributeMedications
                        |> Maybe.withDefault ( 0, 1 )
            in
            ( provideTodayActive + distributeMedicationsActive
            , provideTodayCompleted + distributeMedicationsCompleted
            )

        TaskTreatmentReview ->
            let
                form =
                    getMeasurementValueFunc measurements.treatmentReview
                        |> ongoingTreatmentReviewFormWithDefault data.treatmentReviewForm

                ( takenAsPrescribedActive, takenAsPrescribedComleted ) =
                    form.takenAsPrescribed
                        |> Maybe.map
                            (\takenAsPrescribed ->
                                if not takenAsPrescribed then
                                    if isJust form.reasonForNotTaking then
                                        ( 2, 2 )

                                    else
                                        ( 1, 2 )

                                else
                                    ( 1, 1 )
                            )
                        |> Maybe.withDefault ( 0, 1 )

                ( missedDosesActive, missedDosesCompleted ) =
                    form.missedDoses
                        |> Maybe.map
                            (\missedDoses ->
                                if missedDoses then
                                    if isJust form.totalMissedDoses then
                                        ( 2, 2 )

                                    else
                                        ( 1, 2 )

                                else
                                    ( 1, 1 )
                            )
                        |> Maybe.withDefault ( 0, 1 )

                ( adverseEventsActive, adverseEventsCompleted ) =
                    form.sideEffects
                        |> Maybe.map
                            (\sideEffects ->
                                if sideEffects then
                                    if isJust form.adverseEvents then
                                        ( 2, 2 )

                                    else
                                        ( 1, 2 )

                                else
                                    ( 1, 1 )
                            )
                        |> Maybe.withDefault ( 0, 1 )
            in
            ( takenAsPrescribedActive + missedDosesActive + adverseEventsActive + taskCompleted form.feelingBetter
            , takenAsPrescribedComleted + missedDosesCompleted + adverseEventsCompleted + 1
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
        _ ->
            -- @todo:
            True


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
    -- todo:
    True


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
                { nightSweats = or form.nightSweats (EverySet.member SymptomNightSweats value |> Just)
                , bloodInSputum = or form.bloodInSputum (EverySet.member SymptomBloodInSputum value |> Just)
                , weightLoss = or form.weightLoss (EverySet.member SymptomWeightLoss value |> Just)
                , severeFatigue = or form.severeFatigue (EverySet.member SymptomSevereFatigue value |> Just)
                }
            )


toSymptomReviewValueWithDefault : Maybe TuberculosisSymptomReviewValue -> SymptomReviewForm -> Maybe TuberculosisSymptomReviewValue
toSymptomReviewValueWithDefault saved form =
    symptomReviewFormWithDefault form saved
        |> toSymptomReviewValue


toSymptomReviewValue : SymptomReviewForm -> Maybe TuberculosisSymptomReviewValue
toSymptomReviewValue form =
    [ ifNullableTrue SymptomNightSweats form.nightSweats
    , ifNullableTrue SymptomBloodInSputum form.bloodInSputum
    , ifNullableTrue SymptomWeightLoss form.weightLoss
    , ifNullableTrue SymptomSevereFatigue form.severeFatigue
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
                { medication = or form.medication (Just value) }
            )


toPrescribedMedicationValue : PrescribedMedicationForm -> Maybe TuberculosisMedicationValue
toPrescribedMedicationValue form =
    Maybe.map identity form.medication


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
                            Just value.sign
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
