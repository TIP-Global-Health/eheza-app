module Pages.HIV.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.HIVActivity.Model exposing (HIVActivity(..))
import Backend.HIVActivity.Utils exposing (allActivities)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
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
import Pages.HIV.Activity.Model exposing (..)
import Pages.HIV.Encounter.Model exposing (AssembledData)
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
import Translate exposing (translate)
import Translate.Model exposing (Language(..))



-- expectActivity : NominalDate -> AssembledData -> HIVActivity -> Bool
-- expectActivity currentDate assembled activity =
--     case activity of
--         Diagnostics ->
--             assembled.initialEncounter
--
--         Medication ->
--             resolveMedicationTasks currentDate assembled
--                 |> List.isEmpty
--                 |> not
--
--         SymptomReview ->
--             not assembled.initialEncounter
--
--         NextSteps ->
--             mandatoryActivitiesForNextStepsCompleted currentDate assembled
--                 && (resolveNextStepsTasks currentDate assembled
--                         |> List.isEmpty
--                         |> not
--                    )
--
--
-- activityCompleted : NominalDate -> AssembledData -> HIVActivity -> Bool
-- activityCompleted currentDate assembled activity =
--     let
--         notExpected activityToCheck =
--             not <| expectActivity currentDate assembled activityToCheck
--     in
--     case activity of
--         Diagnostics ->
--             notExpected Diagnostics
--                 || isJust assembled.measurements.diagnostics
--
--         Medication ->
--             notExpected Medication
--                 || (resolveMedicationTasks currentDate assembled
--                         |> List.all (medicationTaskCompleted assembled)
--                    )
--
--         SymptomReview ->
--             notExpected SymptomReview
--                 || isJust assembled.measurements.symptomReview
--
--         NextSteps ->
--             notExpected NextSteps
--                 || (resolveNextStepsTasks currentDate assembled
--                         |> List.all (nextStepsTaskCompleted assembled)
--                    )
--
--
-- medicationTasks : List MedicationTask
-- medicationTasks =
--     [ TaskPrescribedMedication, TaskTreatmentReview ]
--
--
-- resolveMedicationTasks : NominalDate -> AssembledData -> List MedicationTask
-- resolveMedicationTasks currentDate assembled =
--     List.filter (expectMedicationTask currentDate assembled) medicationTasks
--
--
-- expectMedicationTask : NominalDate -> AssembledData -> MedicationTask -> Bool
-- expectMedicationTask currentDate assembled task =
--     case task of
--         TaskPrescribedMedication ->
--             assembled.initialEncounter
--
--         TaskTreatmentReview ->
--             not assembled.initialEncounter
--                 || isJust assembled.measurements.medication
--
--
-- medicationTaskCompleted : AssembledData -> MedicationTask -> Bool
-- medicationTaskCompleted assembled task =
--     case task of
--         TaskPrescribedMedication ->
--             isJust assembled.measurements.medication
--
--         TaskTreatmentReview ->
--             isJust assembled.measurements.treatmentReview
--
--
-- medicationTasksCompletedFromTotal : Language -> NominalDate -> AssembledData -> MedicationData -> MedicationTask -> ( Int, Int )
-- medicationTasksCompletedFromTotal language currentDate assembled data task =
--     case task of
--         TaskPrescribedMedication ->
--             let
--                 form =
--                     getMeasurementValueFunc assembled.measurements.medication
--                         |> prescribedMedicationFormWithDefault data.prescribedMedicationForm
--             in
--             ( taskCompleted form.medications
--             , 1
--             )
--
--         TaskTreatmentReview ->
--             let
--                 form =
--                     getMeasurementValueFunc assembled.measurements.treatmentReview
--                         |> ongoingTreatmentReviewFormWithDefault data.treatmentReviewForm
--
--                 ( _, tasks ) =
--                     treatmentReviewInputsAndTasks language
--                         currentDate
--                         SetTreatmentReviewBoolInput
--                         SetReasonForNotTaking
--                         SetTotalMissedDoses
--                         SetAdverseEvent
--                         form
--             in
--             ( Maybe.Extra.values tasks
--                 |> List.length
--             , List.length tasks
--             )
--
--
-- nextStepsTasks : List NextStepsTask
-- nextStepsTasks =
--     [ TaskReferral, TaskHealthEducation, TaskFollowUp ]
--
--
-- resolveNextStepsTasks : NominalDate -> AssembledData -> List NextStepsTask
-- resolveNextStepsTasks currentDate assembled =
--     List.filter (expectNextStepsTask currentDate assembled) nextStepsTasks
--
--
-- expectNextStepsTask : NominalDate -> AssembledData -> NextStepsTask -> Bool
-- expectNextStepsTask currentDate assembled task =
--     case task of
--         TaskReferral ->
--             adverseEventReported assembled.measurements
--                 || symptomReported assembled.measurements
--
--         TaskHealthEducation ->
--             -- Always provide health education.
--             True
--
--         TaskFollowUp ->
--             -- Always schedule follow up.
--             True
-- adverseEventReported : HIVMeasurements -> Bool
-- adverseEventReported measurements =
--     getMeasurementValueFunc measurements.treatmentReview
--         |> Maybe.map
--             (\value ->
--                 case EverySet.toList value.adverseEvents of
--                     [] ->
--                         False
--
--                     [ NoAdverseEvent ] ->
--                         False
--
--                     _ ->
--                         True
--             )
--         |> Maybe.withDefault False
--
--
-- symptomReported : HIVMeasurements -> Bool
-- symptomReported measurements =
--     getMeasurementValueFunc measurements.symptomReview
--         |> Maybe.map
--             (\value ->
--                 case EverySet.toList value of
--                     [] ->
--                         False
--
--                     [ NoHIVSymptoms ] ->
--                         False
--
--                     _ ->
--                         True
--             )
--         |> Maybe.withDefault False
--
--
-- nextStepsTaskCompleted : AssembledData -> NextStepsTask -> Bool
-- nextStepsTaskCompleted assembled task =
--     case task of
--         TaskReferral ->
--             isJust assembled.measurements.referral
--
--         TaskHealthEducation ->
--             isJust assembled.measurements.healthEducation
--
--         TaskFollowUp ->
--             isJust assembled.measurements.followUp
--
--
-- nextStepsTasksCompletedFromTotal : HIVMeasurements -> NextStepsData -> NextStepsTask -> ( Int, Int )
-- nextStepsTasksCompletedFromTotal measurements data task =
--     case task of
--         TaskHealthEducation ->
--             let
--                 form =
--                     getMeasurementValueFunc measurements.healthEducation
--                         |> healthEducationFormWithDefault data.healthEducationForm
--             in
--             ( taskCompleted form.followUpTesting
--             , 1
--             )
--
--         TaskFollowUp ->
--             let
--                 form =
--                     getMeasurementValueFunc measurements.followUp
--                         |> followUpFormWithDefault data.followUpForm
--             in
--             ( taskCompleted form.option
--             , 1
--             )
--
--         TaskReferral ->
--             let
--                 form =
--                     getMeasurementValueFunc measurements.referral
--                         |> sendToHCFormWithDefault data.sendToHCForm
--
--                 ( reasonForNotSentActive, reasonForNotSentCompleted ) =
--                     form.referToHealthCenter
--                         |> Maybe.map
--                             (\sentToHC ->
--                                 if not sentToHC then
--                                     if isJust form.reasonForNotSendingToHC then
--                                         ( 2, 2 )
--
--                                     else
--                                         ( 1, 2 )
--
--                                 else
--                                     ( 1, 1 )
--                             )
--                         |> Maybe.withDefault ( 0, 1 )
--             in
--             ( reasonForNotSentActive + taskCompleted form.handReferralForm
--             , reasonForNotSentCompleted + 1
--             )
--
--
-- mandatoryActivitiesForNextStepsCompleted : NominalDate -> AssembledData -> Bool
-- mandatoryActivitiesForNextStepsCompleted currentDate assembled =
--     List.all (activityCompleted currentDate assembled)
--         [ Diagnostics, Medication, SymptomReview ]
--
--
-- diagnosticsFormWithDefault : DiagnosticsForm -> Maybe HIVDiagnosticsValue -> DiagnosticsForm
-- diagnosticsFormWithDefault form saved =
--     saved
--         |> unwrap
--             form
--             (\value ->
--                 --@todo
--                 { diagnosed = or form.diagnosed Nothing
--                 }
--             )
--
--
-- toDiagnosticsValueWithDefault : Maybe HIVDiagnosticsValue -> DiagnosticsForm -> Maybe HIVDiagnosticsValue
-- toDiagnosticsValueWithDefault saved form =
--     diagnosticsFormWithDefault form saved
--         |> toDiagnosticsValue
--
--
-- toDiagnosticsValue : DiagnosticsForm -> Maybe HIVDiagnosticsValue
-- toDiagnosticsValue form =
--     Maybe.andThen
--         (\diagnosed ->
--             if not diagnosed then
--                 Just NoHIV
--
--             else
--                 Maybe.map
--                     (\isPulmonary ->
--                         if isPulmonary then
--                             HIVPulmonary
--
--                         else
--                             HIVExtrapulmonary
--                     )
--                     form.isPulmonary
--         )
--         form.diagnosed
--
--
-- symptomReviewFormWithDefault : SymptomReviewForm -> Maybe HIVSymptomReviewValue -> SymptomReviewForm
-- symptomReviewFormWithDefault form saved =
--     saved
--         |> unwrap
--             form
--             (\value ->
--                 { nightSweats = or form.nightSweats (EverySet.member SymptomNightSweats value |> Just)
--                 , bloodInSputum = or form.bloodInSputum (EverySet.member SymptomBloodInSputum value |> Just)
--                 , weightLoss = or form.weightLoss (EverySet.member SymptomWeightLoss value |> Just)
--                 , severeFatigue = or form.severeFatigue (EverySet.member SymptomSevereFatigue value |> Just)
--                 }
--             )
--
--
-- toSymptomReviewValueWithDefault : Maybe HIVSymptomReviewValue -> SymptomReviewForm -> Maybe HIVSymptomReviewValue
-- toSymptomReviewValueWithDefault saved form =
--     symptomReviewFormWithDefault form saved
--         |> toSymptomReviewValue
--
--
-- toSymptomReviewValue : SymptomReviewForm -> Maybe HIVSymptomReviewValue
-- toSymptomReviewValue form =
--     [ ifNullableTrue SymptomNightSweats form.nightSweats
--     , ifNullableTrue SymptomBloodInSputum form.bloodInSputum
--     , ifNullableTrue SymptomWeightLoss form.weightLoss
--     , ifNullableTrue SymptomSevereFatigue form.severeFatigue
--     ]
--         |> Maybe.Extra.combine
--         |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoHIVSymptoms)
--
--
-- toHealthEducationValueWithDefault : Maybe HIVHealthEducationValue -> HealthEducationForm -> Maybe HIVHealthEducationValue
-- toHealthEducationValueWithDefault saved form =
--     healthEducationFormWithDefault form saved
--         |> toHealthEducationValue
--
--
-- healthEducationFormWithDefault :
--     HealthEducationForm
--     -> Maybe HIVHealthEducationValue
--     -> HealthEducationForm
-- healthEducationFormWithDefault form saved =
--     saved
--         |> unwrap
--             form
--             (\value ->
--                 { followUpTesting = or form.followUpTesting (EverySet.member EducationFollowUpTesting value |> Just) }
--             )
--
--
-- toHealthEducationValue : HealthEducationForm -> Maybe HIVHealthEducationValue
-- toHealthEducationValue form =
--     [ ifNullableTrue EducationFollowUpTesting form.followUpTesting ]
--         |> Maybe.Extra.combine
--         |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoHIVHealthEducationSigns)
--
--
-- toPrescribedMedicationValueWithDefault : Maybe HIVMedicationValue -> PrescribedMedicationForm -> Maybe HIVMedicationValue
-- toPrescribedMedicationValueWithDefault saved form =
--     prescribedMedicationFormWithDefault form saved
--         |> toPrescribedMedicationValue
--
--
-- prescribedMedicationFormWithDefault :
--     PrescribedMedicationForm
--     -> Maybe HIVMedicationValue
--     -> PrescribedMedicationForm
-- prescribedMedicationFormWithDefault form saved =
--     saved
--         |> unwrap
--             form
--             (\value ->
--                 { medications = or form.medications (Just <| EverySet.toList value)
--                 , medicationsDirty = form.medicationsDirty
--                 }
--             )
--
--
-- toPrescribedMedicationValue : PrescribedMedicationForm -> Maybe HIVMedicationValue
-- toPrescribedMedicationValue form =
--     Maybe.map EverySet.fromList form.medications
