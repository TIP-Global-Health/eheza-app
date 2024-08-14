module Pages.Prenatal.RecurrentActivity.Utils exposing (..)

import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import EverySet
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe.Extra exposing (isJust, or, unwrap)
import Measurement.Model exposing (LaboratoryTask(..))
import Measurement.Utils
    exposing
        ( bloodSmearResultSet
        , expectUniversalTestResultTask
        , testNotPerformedByWhyNotAtExecutionNote
        , testPerformedByExecutionNote
        , testPerformedByValue
        , vitalsFormWithDefault
        )
import Pages.Prenatal.Model exposing (AssembledData, HealthEducationForm, PrenatalEncounterPhase(..), ReferralForm)
import Pages.Prenatal.RecurrentActivity.Model exposing (..)
import Pages.Prenatal.RecurrentActivity.Types exposing (..)
import Pages.Prenatal.Utils exposing (..)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , taskAllCompleted
        , taskCompleted
        , viewBoolInput
        , viewCustomLabel
        , viewQuestionLabel
        )
import Translate
import Translate.Model exposing (Language(..))


expectActivity : NominalDate -> Bool -> AssembledData -> PrenatalRecurrentActivity -> Bool
expectActivity currentDate isLabTech assembled activity =
    case activity of
        LabResults ->
            resolveLaboratoryResultTasks currentDate isLabTech assembled
                |> List.isEmpty
                |> not

        RecurrentNextSteps ->
            resolveNextStepsTasks currentDate assembled
                |> List.isEmpty
                |> not

        RecurrentExamination ->
            resolveExaminationTasks currentDate assembled
                |> List.isEmpty
                |> not

        RecurrentMalariaPrevention ->
            expectMalariaPreventionActivity PhaseRecurrent assembled

        LabsResultsFollowUps ->
            resolveLaboratoryResultFollowUpsTasks currentDate assembled
                |> List.isEmpty
                |> not


activityCompleted : NominalDate -> Bool -> AssembledData -> PrenatalRecurrentActivity -> Bool
activityCompleted currentDate isLabTech assembled activity =
    case activity of
        LabResults ->
            (not <| expectActivity currentDate isLabTech assembled LabResults)
                || (resolveLaboratoryResultTasks currentDate isLabTech assembled
                        |> List.all (laboratoryResultTaskCompleted currentDate isLabTech assembled)
                   )

        RecurrentNextSteps ->
            (not <| expectActivity currentDate isLabTech assembled RecurrentNextSteps)
                || (resolveNextStepsTasks currentDate assembled
                        |> List.all (nextStepsTaskCompleted currentDate assembled)
                   )

        RecurrentExamination ->
            (not <| expectActivity currentDate isLabTech assembled RecurrentExamination)
                || (resolveExaminationTasks currentDate assembled
                        |> List.all (examinationMeasurementTaken assembled)
                   )

        RecurrentMalariaPrevention ->
            (not <| expectActivity currentDate isLabTech assembled RecurrentMalariaPrevention)
                || isJust assembled.measurements.malariaPrevention

        LabsResultsFollowUps ->
            (not <| expectActivity currentDate isLabTech assembled LabsResultsFollowUps)
                || (resolveLaboratoryResultFollowUpsTasks currentDate assembled
                        |> List.all (laboratoryResultFollowUpsTaskCompleted currentDate assembled)
                   )


laboratoryResultTasks : List LaboratoryTask
laboratoryResultTasks =
    [ TaskPartnerHIVTest
    , TaskHIVTest
    , TaskHIVPCRTest
    , TaskSyphilisTest
    , TaskHepatitisBTest
    , TaskMalariaTest
    , TaskBloodGpRsTest
    , TaskUrineDipstickTest
    , TaskHemoglobinTest
    , TaskRandomBloodSugarTest
    ]


resolveLaboratoryResultTasks : NominalDate -> Bool -> AssembledData -> List LaboratoryTask
resolveLaboratoryResultTasks currentDate isLabTech assembled =
    List.filter (expectLaboratoryResultTask currentDate isLabTech assembled) laboratoryResultTasks


laboratoryResultTaskCompleted : NominalDate -> Bool -> AssembledData -> LaboratoryTask -> Bool
laboratoryResultTaskCompleted currentDate isLabTech assembled task =
    let
        taskExpected =
            expectLaboratoryResultTask currentDate isLabTech assembled

        testResultsCompleted getMeasurementFunc getResultFieldFunc =
            getMeasurementFunc assembled.measurements
                |> getMeasurementValueFunc
                |> Maybe.map
                    (\value ->
                        testNotPerformedByWhyNotAtExecutionNote value.executionNote
                            || (testPerformedByExecutionNote value.executionNote
                                    && (isJust <| getResultFieldFunc value)
                               )
                    )
                |> Maybe.withDefault False
    in
    case task of
        TaskPartnerHIVTest ->
            (not <| taskExpected TaskPartnerHIVTest) || testResultsCompleted .partnerHIVTest .testResult

        TaskHIVTest ->
            (not <| taskExpected TaskHIVTest) || testResultsCompleted .hivTest .testResult

        TaskSyphilisTest ->
            (not <| taskExpected TaskSyphilisTest) || testResultsCompleted .syphilisTest .testResult

        TaskHepatitisBTest ->
            (not <| taskExpected TaskHepatitisBTest) || testResultsCompleted .hepatitisBTest .testResult

        TaskMalariaTest ->
            let
                resultSet =
                    getMeasurementValueFunc assembled.measurements.malariaTest
                        |> Maybe.map
                            (\value ->
                                (testPerformedByExecutionNote value.executionNote && isJust value.testResult)
                                    || bloodSmearResultSet value.bloodSmearResult
                            )
                        |> Maybe.withDefault False
            in
            (not <| taskExpected TaskMalariaTest) || resultSet

        TaskBloodGpRsTest ->
            (not <| taskExpected TaskBloodGpRsTest) || testResultsCompleted .bloodGpRsTest .bloodGroup

        TaskUrineDipstickTest ->
            (not <| taskExpected TaskUrineDipstickTest) || testResultsCompleted .urineDipstickTest .protein

        TaskHemoglobinTest ->
            (not <| taskExpected TaskHemoglobinTest) || testResultsCompleted .hemoglobinTest .hemoglobinCount

        TaskRandomBloodSugarTest ->
            (not <| taskExpected TaskRandomBloodSugarTest) || testResultsCompleted .randomBloodSugarTest .sugarCount

        TaskHIVPCRTest ->
            (not <| taskExpected TaskHIVPCRTest) || testResultsCompleted .hivPCRTest .hivViralLoadStatus

        TaskCompletePreviousTests ->
            not <| taskExpected TaskCompletePreviousTests

        -- Others are not in use for Prenatal.
        _ ->
            False


expectLaboratoryResultTask : NominalDate -> Bool -> AssembledData -> LaboratoryTask -> Bool
expectLaboratoryResultTask currentDate isLabTech assembled task =
    let
        wasTestPerformed getMeasurementFunc =
            getMeasurementFunc assembled.measurements
                |> getMeasurementValueFunc
                |> Maybe.map expectUniversalTestResultTask
                |> Maybe.withDefault False

        -- For nurses, we don't want to show laboratory task if its results were
        -- set by Lab tech, and it got follow up questions (filled by nurse),
        -- as that task will be shown at Lab Results Follow Ups activity.
        followUpWasNotScheduled test =
            getMeasurementValueFunc assembled.measurements.labsResults
                |> Maybe.andThen .testsWithFollowUp
                |> Maybe.map (EverySet.member test >> not)
                |> Maybe.withDefault True
    in
    case task of
        TaskHIVTest ->
            wasTestPerformed .hivTest
                && (isLabTech || followUpWasNotScheduled TestHIV)

        TaskPartnerHIVTest ->
            wasTestPerformed .partnerHIVTest

        TaskSyphilisTest ->
            wasTestPerformed .syphilisTest && (isLabTech || followUpWasNotScheduled TestSyphilis)

        TaskHepatitisBTest ->
            wasTestPerformed .hepatitisBTest

        TaskMalariaTest ->
            wasTestPerformed .malariaTest

        TaskBloodGpRsTest ->
            wasTestPerformed .bloodGpRsTest

        TaskUrineDipstickTest ->
            wasTestPerformed .urineDipstickTest

        TaskHemoglobinTest ->
            wasTestPerformed .hemoglobinTest

        TaskRandomBloodSugarTest ->
            wasTestPerformed .randomBloodSugarTest

        TaskHIVPCRTest ->
            wasTestPerformed .hivPCRTest

        TaskCompletePreviousTests ->
            False

        -- Others are not in use for Prenatal.
        _ ->
            False


laboratoryResultFollowUpsTasks : List LaboratoryTask
laboratoryResultFollowUpsTasks =
    [ TaskHIVTest
    , TaskSyphilisTest
    ]


resolveLaboratoryResultFollowUpsTasks : NominalDate -> AssembledData -> List LaboratoryTask
resolveLaboratoryResultFollowUpsTasks currentDate assembled =
    List.filter (expectLaboratoryResultFollowUpsTask currentDate assembled) laboratoryResultFollowUpsTasks


laboratoryResultFollowUpsTaskCompleted : NominalDate -> AssembledData -> LaboratoryTask -> Bool
laboratoryResultFollowUpsTaskCompleted currentDate assembled task =
    let
        taskExpected =
            expectLaboratoryResultFollowUpsTask currentDate assembled

        testFollowUpCompleted getMeasurementFunc getResultFieldFunc pendingValue =
            getMeasurementFunc assembled.measurements
                |> getMeasurementValueFunc
                |> Maybe.andThen getResultFieldFunc
                |> Maybe.map (EverySet.member pendingValue >> not)
                |> Maybe.withDefault False
    in
    case task of
        TaskHIVTest ->
            (not <| taskExpected TaskHIVTest) || testFollowUpCompleted .hivTest .hivSigns PrenatalHIVSignPendingInput

        TaskSyphilisTest ->
            (not <| taskExpected TaskSyphilisTest) || testFollowUpCompleted .syphilisTest .symptoms IllnessSymptomPendingInput

        -- Others are not in use for Prenatal.
        _ ->
            False


expectLaboratoryResultFollowUpsTask : NominalDate -> AssembledData -> LaboratoryTask -> Bool
expectLaboratoryResultFollowUpsTask currentDate assembled task =
    let
        wasFollowUpScheduled test =
            getMeasurementValueFunc assembled.measurements.labsResults
                |> Maybe.andThen .testsWithFollowUp
                |> Maybe.map (EverySet.member test)
                |> Maybe.withDefault False
    in
    case task of
        TaskHIVTest ->
            wasFollowUpScheduled TestHIV

        TaskSyphilisTest ->
            wasFollowUpScheduled TestSyphilis
                && -- Lab tech entered result showing positive Syphilis,
                   -- which requires Syphilis symptoms question.
                   (getMeasurementValueFunc assembled.measurements.syphilisTest
                        |> Maybe.map (.testResult >> (==) (Just TestPositive))
                        |> Maybe.withDefault False
                   )

        -- Others are not in use for Prenatal.
        _ ->
            False


resolveNextStepsTasks : NominalDate -> AssembledData -> List NextStepsTask
resolveNextStepsTasks currentDate assembled =
    -- The order is important. Do not change.
    [ NextStepsHealthEducation, NextStepsMedicationDistribution, NextStepsSendToHC ]
        |> List.filter (expectNextStepsTask currentDate assembled)


expectNextStepsTask : NominalDate -> AssembledData -> NextStepsTask -> Bool
expectNextStepsTask currentDate assembled task =
    case task of
        NextStepsSendToHC ->
            resolveRequiredReferralFacilities assembled
                |> List.isEmpty
                |> not

        NextStepsMedicationDistribution ->
            -- Emergency referral is not required.
            (not <| emergencyReferalRequired assembled)
                && ((resolveRequiredMedicationsSet English currentDate PrenatalEncounterPhaseRecurrent assembled
                        |> List.isEmpty
                        |> not
                    )
                        || (diagnosedMalariaByPhase PrenatalEncounterPhaseRecurrent assembled
                                && (not <| referToHospitalDueToAdverseEventForMalariaTreatment assembled)
                           )
                        || diagnosedSyphilisByPhase PrenatalEncounterPhaseRecurrent assembled
                        || diagnosedHypertension PrenatalEncounterPhaseRecurrent assembled
                   )

        NextStepsHealthEducation ->
            -- Emergency referral is not required.
            (not <| emergencyReferalRequired assembled)
                && (provideHIVEducation PrenatalEncounterPhaseRecurrent assembled.measurements
                        || diagnosedAnyOf (DiagnosisHIVDetectableViralLoadRecurrentPhase :: diabetesDiagnosesRecurrentPhase) assembled
                   )


nextStepsTaskCompleted : NominalDate -> AssembledData -> NextStepsTask -> Bool
nextStepsTaskCompleted currentDate assembled task =
    case task of
        NextStepsSendToHC ->
            resolveRequiredReferralFacilities assembled
                |> List.all (referralToFacilityCompleted assembled)

        NextStepsMedicationDistribution ->
            let
                medicationDistributionCompleted =
                    let
                        medicationDistributionRequired =
                            resolveRequiredMedicationsSet English currentDate PrenatalEncounterPhaseRecurrent assembled
                                |> List.isEmpty
                                |> not
                    in
                    if medicationDistributionRequired then
                        let
                            allowedSigns =
                                NoMedicationDistributionSignsRecurrentPhase :: medicationsRecurrentPhase
                        in
                        medicationDistributionMeasurementTaken allowedSigns assembled.measurements

                    else
                        True

                malariaTreatmentCompleted =
                    if diagnosedMalariaByPhase PrenatalEncounterPhaseRecurrent assembled then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForMalaria assembled.measurements

                    else
                        True

                syphilisTreatmentCompleted =
                    if diagnosedSyphilisByPhase PrenatalEncounterPhaseRecurrent assembled then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForSyphilis assembled.measurements

                    else
                        True

                hypertensionTreatmentCompleted =
                    if diagnosedHypertension PrenatalEncounterPhaseRecurrent assembled then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForHypertension assembled.measurements

                    else
                        True
            in
            medicationDistributionCompleted
                && malariaTreatmentCompleted
                && syphilisTreatmentCompleted
                && hypertensionTreatmentCompleted

        NextStepsHealthEducation ->
            getMeasurementValueFunc assembled.measurements.healthEducation
                |> Maybe.map (.signsPhase2 >> isJust)
                |> Maybe.withDefault False


nextStepsTasksCompletedFromTotal : Language -> NominalDate -> AssembledData -> NextStepsData -> NextStepsTask -> ( Int, Int )
nextStepsTasksCompletedFromTotal language currentDate assembled data task =
    case task of
        NextStepsSendToHC ->
            let
                form =
                    assembled.measurements.sendToHC
                        |> getMeasurementValueFunc
                        |> referralFormWithDefault data.referralForm

                ( _, tasks ) =
                    resolveReferralInputsAndTasks language
                        currentDate
                        assembled
                        SetReferralBoolInput
                        SetFacilityNonReferralReason
                        form
            in
            ( Maybe.Extra.values tasks
                |> List.length
            , List.length tasks
            )

        NextStepsMedicationDistribution ->
            let
                form =
                    getMeasurementValueFunc assembled.measurements.medicationDistribution
                        |> medicationDistributionFormWithDefaultRecurrentPhase data.medicationDistributionForm

                ( _, completed, total ) =
                    resolveMedicationDistributionInputsAndTasks language
                        currentDate
                        PrenatalEncounterPhaseRecurrent
                        assembled
                        SetMedicationDistributionBoolInput
                        SetMedicationDistributionAdministrationNote
                        SetRecommendedTreatmentSign
                        (always NoOp)
                        form
            in
            ( completed, total )

        NextStepsHealthEducation ->
            let
                form =
                    getMeasurementValueFunc assembled.measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm

                ( _, tasks ) =
                    healthEducationFormInputsAndTasks language assembled form
            in
            ( List.map taskCompleted tasks
                |> List.sum
            , List.length tasks
            )


emergencyReferalRequired : AssembledData -> Bool
emergencyReferalRequired assembled =
    EverySet.toList assembled.encounter.diagnoses
        |> List.filter diagnosisRequiresEmergencyReferal
        |> List.isEmpty
        |> not


diagnosisRequiresEmergencyReferal : PrenatalDiagnosis -> Bool
diagnosisRequiresEmergencyReferal diagnosis =
    List.member diagnosis emergencyReferralDiagnosesRecurrent


resolveExaminationTasks : NominalDate -> AssembledData -> List ExaminationTask
resolveExaminationTasks currentDate assembled =
    List.filter (expectExaminationTask currentDate assembled)
        [ ExaminationVitals ]


expectExaminationTask : NominalDate -> AssembledData -> ExaminationTask -> Bool
expectExaminationTask currentDate assembled task =
    case task of
        ExaminationVitals ->
            -- Hypertension is a chronic diagnosis for whole duration
            -- of pregnancy. If diagnised, we do not need to recheck the BP.
            -- Measurement taken at initial phase of encounter is sufficient.
            (not <| diagnosedHypertensionPrevoiusly assembled)
                && (not <| diagnosedAnyOf hierarchalBloodPressureDiagnosesInitialPhase assembled)
                && (getMeasurementValueFunc assembled.measurements.vitals
                        |> Maybe.andThen
                            (\value ->
                                Maybe.map2
                                    marginalBloodPressureCondition
                                    value.dia
                                    value.sys
                            )
                        |> Maybe.withDefault False
                   )


examinationMeasurementTaken : AssembledData -> ExaminationTask -> Bool
examinationMeasurementTaken assembled task =
    case task of
        ExaminationVitals ->
            getMeasurementValueFunc assembled.measurements.vitals
                |> Maybe.map
                    -- We meassure sysRepeated and diaRepeated, but we know for sure
                    -- that if one is set, other one is set as well.
                    -- So, it's enough to check only one.
                    (.sysRepeated >> isJust)
                |> Maybe.withDefault False


examinationTasksCompletedFromTotal : AssembledData -> ExaminationData -> ExaminationTask -> ( Int, Int )
examinationTasksCompletedFromTotal assembled data task =
    case task of
        ExaminationVitals ->
            let
                form =
                    assembled.measurements.vitals
                        |> getMeasurementValueFunc
                        |> vitalsFormWithDefault data.vitalsForm
            in
            ( taskAllCompleted [ form.sysRepeated, form.diaRepeated ]
            , 1
            )


healthEducationFormInputsAndTasks : Language -> AssembledData -> HealthEducationForm -> ( List (Html Msg), List (Maybe Bool) )
healthEducationFormInputsAndTasks language assembled form =
    healthEducationFormInputsAndTasksForNurse language
        PrenatalEncounterPhaseRecurrent
        SetHealthEducationBoolInput
        assembled
        form


toHealthEducationValueWithDefault : Maybe PrenatalHealthEducationValue -> HealthEducationForm -> Maybe PrenatalHealthEducationValue
toHealthEducationValueWithDefault saved form =
    healthEducationFormWithDefault form saved
        |> toHealthEducationValue saved


healthEducationFormWithDefault :
    HealthEducationForm
    -> Maybe PrenatalHealthEducationValue
    -> HealthEducationForm
healthEducationFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { hivDetectableViralLoad = or form.hivDetectableViralLoad (Maybe.map (EverySet.member EducationHIVDetectableViralLoad) value.signsPhase2)
                , diabetes = or form.diabetes (Maybe.map (EverySet.member EducationDiabetes) value.signsPhase2)
                , positiveHIV = or form.positiveHIV (Maybe.map (EverySet.member EducationPositiveHIV) value.signsPhase2)
                , saferSexHIV = or form.saferSexHIV (Maybe.map (EverySet.member EducationSaferSexHIV) value.signsPhase2)
                , partnerTesting = or form.partnerTesting (Maybe.map (EverySet.member EducationPartnerTesting) value.signsPhase2)
                , familyPlanning = or form.familyPlanning (Maybe.map (EverySet.member EducationFamilyPlanning) value.signsPhase2)

                -- Signs that do not participate at recurrent phase. Resolved directly from value.
                , expectations = EverySet.member EducationExpectations value.signs |> Just
                , visitsReview = EverySet.member EducationVisitsReview value.signs |> Just
                , warningSigns = EverySet.member EducationWarningSigns value.signs |> Just
                , hemorrhaging = EverySet.member EducationHemorrhaging value.signs |> Just
                , breastfeeding = EverySet.member EducationBreastfeeding value.signs |> Just
                , immunization = EverySet.member EducationImmunization value.signs |> Just
                , hygiene = EverySet.member EducationHygiene value.signs |> Just
                , nauseaVomiting = EverySet.member EducationNauseaVomiting value.signs |> Just
                , legCramps = EverySet.member EducationLegCramps value.signs |> Just
                , lowBackPain = EverySet.member EducationLowBackPain value.signs |> Just
                , constipation = EverySet.member EducationConstipation value.signs |> Just
                , heartburn = EverySet.member EducationHeartburn value.signs |> Just
                , varicoseVeins = EverySet.member EducationVaricoseVeins value.signs |> Just
                , legPainRedness = EverySet.member EducationLegPainRedness value.signs |> Just
                , pelvicPain = EverySet.member EducationPelvicPain value.signs |> Just
                , saferSex = EverySet.member EducationSaferSex value.signs |> Just
                , mentalHealth = EverySet.member EducationMentalHealth value.signs |> Just
                , earlyMastitisOrEngorgment = EverySet.member EducationEarlyMastitisOrEngorgment value.signs |> Just
                , mastitis = EverySet.member EducationMastitis value.signs |> Just
                }
            )


toHealthEducationValue : Maybe PrenatalHealthEducationValue -> HealthEducationForm -> Maybe PrenatalHealthEducationValue
toHealthEducationValue saved form =
    [ ifNullableTrue EducationPositiveHIV form.positiveHIV
    , ifNullableTrue EducationSaferSexHIV form.saferSexHIV
    , ifNullableTrue EducationPartnerTesting form.partnerTesting
    , ifNullableTrue EducationFamilyPlanning form.familyPlanning
    , ifNullableTrue EducationHIVDetectableViralLoad form.hivDetectableViralLoad
    , ifNullableTrue EducationDiabetes form.diabetes
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoPrenatalHealthEducationSigns)
        |> Maybe.map
            (\signsPhase2 ->
                { signs =
                    Maybe.map .signs saved
                        |> Maybe.withDefault (EverySet.singleton NoPrenatalHealthEducationSigns)
                , signsPhase2 = Just signsPhase2
                }
            )


resolveReferralInputsAndTasks :
    Language
    -> NominalDate
    -> AssembledData
    -> ((Bool -> ReferralForm -> ReferralForm) -> Bool -> msg)
    -> (Maybe ReasonForNonReferral -> ReferralFacility -> ReasonForNonReferral -> msg)
    -> ReferralForm
    -> ( List (Html msg), List (Maybe Bool) )
resolveReferralInputsAndTasks language currentDate assembled setReferralBoolInputMsg setNonReferralReasonMsg form =
    let
        foldResults =
            List.foldr
                (\( inputs, tasks ) ( accumInputs, accumTasks ) ->
                    ( inputs ++ accumInputs, tasks ++ accumTasks )
                )
                ( [], [] )
    in
    resolveRequiredReferralFacilities assembled
        |> List.map (resolveReferralToFacilityInputsAndTasks language currentDate PrenatalEncounterPhaseRecurrent assembled setReferralBoolInputMsg setNonReferralReasonMsg form)
        |> foldResults


resolveRequiredReferralFacilities : AssembledData -> List ReferralFacility
resolveRequiredReferralFacilities assembled =
    List.filter (matchRequiredReferralFacility assembled) referralFacilities


matchRequiredReferralFacility : AssembledData -> ReferralFacility -> Bool
matchRequiredReferralFacility assembled facility =
    case facility of
        FacilityHospital ->
            diagnosesCausingHospitalReferralByPhase PrenatalEncounterPhaseRecurrent assembled
                |> EverySet.isEmpty
                |> not

        FacilityMentalHealthSpecialist ->
            False

        FacilityARVProgram ->
            referToARVProgram assembled

        FacilityNCDProgram ->
            -- NCD proram referral is based on diagnoses made at
            -- previous encounters, and therefore, can appear
            -- only on initial phase.
            False

        -- Explicit NCD facility.
        FacilityANCServices ->
            False

        FacilityUltrasound ->
            False

        FacilityHealthCenter ->
            -- We should never get here. HC inputs are resolved
            -- with resolveReferralInputsAndTasksForCHW.
            False


referToARVProgram : AssembledData -> Bool
referToARVProgram assembled =
    -- No need to check Speciality care, since there is no
    -- recurrent phase for postpartum encounter.
    diagnosed DiagnosisHIVRecurrentPhase assembled && hivProgramAtHC assembled.measurements


referralFacilities : List ReferralFacility
referralFacilities =
    [ FacilityHospital
    , FacilityARVProgram
    ]
