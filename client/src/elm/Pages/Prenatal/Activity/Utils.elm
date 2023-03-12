module Pages.Prenatal.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (PrenatalEncounterId)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils
    exposing
        ( diabetesBySugarCount
        , diabetesByUrineGlucose
        , getHeightValue
        , getMeasurementValueFunc
        , labExpirationPeriod
        , muacIndication
        , muacValueFunc
        , weightValueFunc
        )
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType(..), PrenatalIndicator(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Backend.PrenatalEncounter.Utils exposing (isNurseEncounter)
import Date exposing (Unit(..))
import DateSelector.Model exposing (DateSelectorConfig)
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, diffDays, diffWeeks, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (FamilyPlanningForm, LaboratoryTask(..), OutsideCareForm, VaccinationFormViewMode(..), VitalsForm)
import Measurement.Utils
    exposing
        ( corePhysicalExamFormWithDefault
        , getNextVaccineDose
        , isTestResultValid
        , resolveLabTestDate
        , testPerformedByExecutionNote
        , toEverySet
        , vaccinationFormWithDefault
        , vaccineDoseToComparable
        , vitalsFormWithDefault
        )
import Measurement.View exposing (viewActionTakenLabel)
import Pages.AcuteIllness.Activity.View exposing (viewAdministeredMedicationCustomLabel, viewAdministeredMedicationLabel, viewAdministeredMedicationQuestion)
import Pages.Prenatal.Activity.Model exposing (..)
import Pages.Prenatal.Activity.Types exposing (..)
import Pages.Prenatal.Encounter.Utils exposing (diagnosisRequiresEmergencyReferal, emergencyReferalRequired, getAllActivities)
import Pages.Prenatal.Model exposing (AssembledData, HealthEducationForm, PrenatalEncounterPhase(..), ReferralForm, VaccinationProgressDict)
import Pages.Prenatal.Utils exposing (..)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeToBoolTask
        , maybeValueConsideringIsDirtyField
        , taskAllCompleted
        , taskCompleted
        , taskCompletedWithException
        , valueConsideringIsDirtyField
        , viewBoolInput
        , viewCheckBoxMultipleSelectCustomInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectCustomInput
        , viewCheckBoxSelectInput
        , viewCustomBoolInput
        , viewCustomLabel
        , viewInstructionsLabel
        , viewLabel
        , viewQuestionLabel
        )
import Translate exposing (Language, translate)
import Translate.Model exposing (Language(..))
import Utils.Html exposing (viewModal)


expectActivity : NominalDate -> AssembledData -> PrenatalActivity -> Bool
expectActivity currentDate assembled activity =
    case assembled.encounter.encounterType of
        -- Note that for nurse it's always used after
        -- Pages.Prenatal.Encounter.Utils.getAllActivities, which supplies
        -- different activities, depending if nurse encounter was performed
        -- previously, or not.
        NurseEncounter ->
            case activity of
                PregnancyDating ->
                    True

                History ->
                    True

                Examination ->
                    True

                FamilyPlanning ->
                    True

                Backend.PrenatalActivity.Model.MalariaPrevention ->
                    expectMalariaPreventionActivity PhaseInitial assembled

                Backend.PrenatalActivity.Model.Medication ->
                    True

                DangerSigns ->
                    True

                Laboratory ->
                    -- Always True, as there are sub activities that should be
                    -- presented on every encounter.
                    True

                PrenatalPhoto ->
                    expectPrenatalPhoto currentDate assembled

                NextSteps ->
                    mandatoryActivitiesForNextStepsCompleted currentDate assembled
                        && (resolveNextStepsTasks currentDate assembled
                                |> List.isEmpty
                                |> not
                           )

                SymptomReview ->
                    True

                PrenatalTreatmentReview ->
                    -- There will always be at least the Prenatal Medication
                    -- task to complete.
                    True

                MaternalMentalHealth ->
                    -- From 28 weeks EGA and not done already.
                    Maybe.map
                        (\lmpDate ->
                            let
                                egaInWeeks =
                                    calculateEGAWeeks currentDate lmpDate

                                performedPreviously =
                                    assembled.nursePreviousEncountersData
                                        |> List.filter (.measurements >> .mentalHealth >> isJust)
                                        |> List.isEmpty
                                        |> not
                            in
                            egaInWeeks >= 28 && not performedPreviously
                        )
                        assembled.globalLmpDate
                        |> Maybe.withDefault False

                PrenatalImmunisation ->
                    Maybe.map
                        (\lmpDate ->
                            let
                                egaInWeeks =
                                    calculateEGAWeeks currentDate lmpDate
                            in
                            generateSuggestedVaccinations currentDate egaInWeeks assembled
                                |> List.isEmpty
                                |> not
                        )
                        assembled.globalLmpDate
                        |> Maybe.withDefault False

                -- Activities that do not participate at Nurse encounter.
                _ ->
                    False

        NursePostpartumEncounter ->
            case activity of
                PregnancyOutcome ->
                    True

                SymptomReview ->
                    True

                MaternalMentalHealth ->
                    True

                Backend.PrenatalActivity.Model.Breastfeeding ->
                    True

                Examination ->
                    True

                FamilyPlanning ->
                    True

                PostpartumTreatmentReview ->
                    True

                SpecialityCare ->
                    List.any (expectSpecialityCareSignSection assembled)
                        specialityCareSections

                NextSteps ->
                    mandatoryActivitiesForNextStepsCompleted currentDate assembled
                        && (resolveNextStepsTasks currentDate assembled
                                |> List.isEmpty
                                |> not
                           )

                -- Activities that do not participate at Nurse Postpartum encounter.
                _ ->
                    False

        ChwFirstEncounter ->
            case activity of
                PregnancyDating ->
                    -- Do not show, if patient already visited health center.
                    nurseEncounterNotPerformed assembled

                Laboratory ->
                    -- Do not show, if patient already visited health center.
                    nurseEncounterNotPerformed assembled

                DangerSigns ->
                    True

                Backend.PrenatalActivity.Model.HealthEducation ->
                    activityCompleted currentDate assembled DangerSigns
                        && noDangerSigns assembled

                NextSteps ->
                    mandatoryActivitiesForNextStepsCompleted currentDate assembled

                -- Activities that do not participate at CHW encounter 1.
                _ ->
                    False

        ChwSecondEncounter ->
            case activity of
                DangerSigns ->
                    True

                BirthPlan ->
                    activityCompleted currentDate assembled DangerSigns
                        && noDangerSigns assembled

                Backend.PrenatalActivity.Model.HealthEducation ->
                    activityCompleted currentDate assembled DangerSigns
                        && noDangerSigns assembled

                NextSteps ->
                    mandatoryActivitiesForNextStepsCompleted currentDate assembled

                -- Activities that do not participate at CHW encounter 2.
                _ ->
                    False

        ChwThirdPlusEncounter ->
            case activity of
                DangerSigns ->
                    True

                Backend.PrenatalActivity.Model.HealthEducation ->
                    activityCompleted currentDate assembled DangerSigns
                        && noDangerSigns assembled

                NextSteps ->
                    mandatoryActivitiesForNextStepsCompleted currentDate assembled

                -- Activities that do not participate at CHW encounter 3.
                _ ->
                    False

        ChwPostpartumEncounter ->
            case activity of
                PregnancyOutcome ->
                    True

                DangerSigns ->
                    True

                NextSteps ->
                    mandatoryActivitiesForNextStepsCompleted currentDate assembled

                -- Activities that do not participate at CHW Postpartum encounter.
                _ ->
                    False


activityCompleted : NominalDate -> AssembledData -> PrenatalActivity -> Bool
activityCompleted currentDate assembled activity =
    case activity of
        PregnancyDating ->
            isJust assembled.measurements.lastMenstrualPeriod

        History ->
            if nurseEncounterNotPerformed assembled then
                -- First antenatal encounter - all tasks should be completed
                isJust assembled.measurements.obstetricHistory
                    && isJust assembled.measurements.obstetricHistoryStep2
                    && isJust assembled.measurements.medicalHistory
                    && isJust assembled.measurements.socialHistory

            else
                -- Subsequent antenatal encounter - only Social history task
                -- needs to be completed.
                isJust assembled.measurements.socialHistory

        Examination ->
            resolveExaminationTasks assembled
                |> List.all (examinationTaskCompleted assembled)

        FamilyPlanning ->
            isJust assembled.measurements.familyPlanning

        Backend.PrenatalActivity.Model.MalariaPrevention ->
            isJust assembled.measurements.malariaPrevention

        Backend.PrenatalActivity.Model.Medication ->
            isJust assembled.measurements.medication

        DangerSigns ->
            isJust assembled.measurements.dangerSigns

        PrenatalPhoto ->
            isJust assembled.measurements.prenatalPhoto

        Laboratory ->
            if isNurseEncounter assembled.encounter.encounterType then
                List.all (laboratoryTaskCompleted currentDate assembled) laboratoryTasks

            else
                -- CHW only got one lab on first encounter
                isJust assembled.measurements.pregnancyTest

        Backend.PrenatalActivity.Model.HealthEducation ->
            isJust assembled.measurements.healthEducation

        BirthPlan ->
            isJust assembled.measurements.birthPlan

        NextSteps ->
            resolveNextStepsTasks currentDate assembled
                |> List.all (nextStepsTaskCompleted currentDate assembled)

        PregnancyOutcome ->
            isJust assembled.participant.dateConcluded

        SymptomReview ->
            isJust assembled.measurements.symptomReview

        PrenatalTreatmentReview ->
            resolveTreatmentReviewTasks assembled
                |> List.all (treatmentReviewTaskCompleted assembled)

        MaternalMentalHealth ->
            isJust assembled.measurements.mentalHealth

        PrenatalImmunisation ->
            (not <| expectActivity currentDate assembled PrenatalImmunisation)
                || List.all (immunisationTaskCompleted currentDate assembled) immunisationTasks

        Backend.PrenatalActivity.Model.Breastfeeding ->
            isJust assembled.measurements.breastfeeding

        SpecialityCare ->
            isJust assembled.measurements.specialityCare

        PostpartumTreatmentReview ->
            isJust assembled.measurements.medication


resolveNextStepsTasks : NominalDate -> AssembledData -> List NextStepsTask
resolveNextStepsTasks currentDate assembled =
    let
        tasks =
            case assembled.encounter.encounterType of
                NurseEncounter ->
                    -- The order is important. Do not change.
                    [ NextStepsHealthEducation, NextStepsMedicationDistribution, NextStepsSendToHC, NextStepsWait ]

                NursePostpartumEncounter ->
                    [ NextStepsHealthEducation, NextStepsMedicationDistribution, NextStepsSendToHC ]

                _ ->
                    -- The order is important. Do not change.
                    [ NextStepsAppointmentConfirmation, NextStepsSendToHC, NextStepsFollowUp, NextStepsHealthEducation, NextStepsNewbornEnrolment ]
    in
    List.filter (expectNextStepsTask currentDate assembled) tasks


expectNextStepsTask : NominalDate -> AssembledData -> NextStepsTask -> Bool
expectNextStepsTask currentDate assembled task =
    let
        dangerSigns =
            dangerSignsPresent assembled
    in
    case task of
        -- Exclusive CHW task.
        NextStepsAppointmentConfirmation ->
            not dangerSigns && assembled.encounter.encounterType /= ChwPostpartumEncounter

        -- Exclusive CHW task.
        NextStepsFollowUp ->
            case assembled.encounter.encounterType of
                ChwPostpartumEncounter ->
                    dangerSigns

                _ ->
                    True

        -- Common task for nurse and CHW.
        NextStepsSendToHC ->
            case assembled.encounter.encounterType of
                NurseEncounter ->
                    resolveRequiredReferralFacilities assembled
                        |> List.isEmpty
                        |> not

                NursePostpartumEncounter ->
                    resolveRequiredReferralFacilities assembled
                        |> List.isEmpty
                        |> not

                _ ->
                    dangerSigns

        -- Common task for nurse and CHW.
        NextStepsHealthEducation ->
            case assembled.encounter.encounterType of
                NurseEncounter ->
                    -- Emergency referral is not required.
                    (not <| emergencyReferalRequired assembled)
                        && (-- HIV education appears whenever HIV test was performed.
                            isJust assembled.measurements.hivTest
                                || provideNauseaAndVomitingEducation assembled
                                || List.any (symptomRecorded assembled.measurements)
                                    [ LegCramps, LowBackPain, Constipation, VaricoseVeins ]
                                || provideLegPainRednessEducation assembled
                                || providePelvicPainEducation assembled
                                || diagnosedAnyOf
                                    [ DiagnosisHeartburn
                                    , DiagnosisCandidiasis
                                    , DiagnosisGonorrhea
                                    , DiagnosisTrichomonasOrBacterialVaginosis
                                    ]
                                    assembled
                                || provideMentalHealthEducation assembled
                           )

                NursePostpartumEncounter ->
                    (not <| emergencyReferalRequired assembled)
                        && (provideNauseaAndVomitingEducation assembled
                                || List.any (symptomRecorded assembled.measurements)
                                    [ LegCramps, LowBackPain, Constipation, VaricoseVeins ]
                                || provideLegPainRednessEducation assembled
                                || providePelvicPainEducation assembled
                                || diagnosedAnyOf
                                    [ DiagnosisHeartburn
                                    , DiagnosisCandidiasis
                                    , DiagnosisGonorrhea
                                    , DiagnosisTrichomonasOrBacterialVaginosis
                                    , DiagnosisPostpartumEarlyMastitisOrEngorgment
                                    , DiagnosisPostpartumMastitis
                                    ]
                                    assembled
                                || provideMentalHealthEducation assembled
                           )

                ChwPostpartumEncounter ->
                    True

                _ ->
                    False

        -- Exclusive CHW task.
        NextStepsNewbornEnrolment ->
            assembled.encounter.encounterType == ChwPostpartumEncounter

        -- Exclusive task for Nurse.
        NextStepsMedicationDistribution ->
            case assembled.encounter.encounterType of
                NurseEncounter ->
                    let
                        hypertensionlikeDiagnosesCondition =
                            -- Given treatment to Hypertension / Moderate Preeclampsia, which needs updating.
                            (updateHypertensionTreatmentWithMedication assembled
                                && (-- Hypertension / Moderate Preeclamsia treatment
                                    -- did not cause an adverse event.
                                    not <| referToHospitalDueToAdverseEventForHypertensionTreatment assembled
                                   )
                                && (-- Moderate Preeclamsia not diagnosed at current encounter, since it results
                                    -- in referral to hospital.
                                    not <| diagnosedAnyOf moderatePreeclampsiaDiagnoses assembled
                                   )
                            )
                                || -- Diagnosed with Moderate Preeclampsia at previous encounter, and BP taken
                                   -- at current encounter does not indicate a need for hospitalization.
                                   (moderatePreeclampsiaAsPreviousHypertensionlikeDiagnosis assembled
                                        && (not <| bloodPressureAtHypertensionTreatmentRequiresHospitalization assembled)
                                   )
                    in
                    -- Emergency referral is not required.
                    (not <| emergencyReferalRequired assembled)
                        && ((resolveRequiredMedicationsSet English currentDate PrenatalEncounterPhaseInitial assembled
                                |> List.isEmpty
                                |> not
                            )
                                || (diagnosedMalaria assembled
                                        && (not <| referToHospitalDueToAdverseEventForMalariaTreatment assembled)
                                   )
                                || diagnosedHypertension PrenatalEncounterPhaseInitial assembled
                                || diagnosedAnyOf
                                    [ DiagnosisHeartburn
                                    , DiagnosisUrinaryTractInfection
                                    , DiagnosisCandidiasis
                                    , DiagnosisGonorrhea
                                    , DiagnosisTrichomonasOrBacterialVaginosis
                                    ]
                                    assembled
                                || (hypertensionlikeDiagnosesCondition
                                        && -- If Preeclampsia was diagnosed at current
                                           -- encounter, there's no need to medicate, because
                                           -- patient is sent to hospital anyway.
                                           (not <|
                                                diagnosedAnyOf
                                                    [ DiagnosisModeratePreeclampsiaInitialPhase
                                                    , DiagnosisSeverePreeclampsiaInitialPhase
                                                    ]
                                                    assembled
                                           )
                                   )
                           )

                NursePostpartumEncounter ->
                    -- Emergency referral is not required.
                    (not <| emergencyReferalRequired assembled)
                        && ((resolveRequiredMedicationsSet English currentDate PrenatalEncounterPhaseInitial assembled
                                |> List.isEmpty
                                |> not
                            )
                                || diagnosedAnyOf
                                    [ DiagnosisHeartburn
                                    , DiagnosisUrinaryTractInfection
                                    , DiagnosisCandidiasis
                                    , DiagnosisGonorrhea
                                    , DiagnosisTrichomonasOrBacterialVaginosis
                                    , DiagnosisPostpartumEarlyMastitisOrEngorgment
                                    , DiagnosisPostpartumMastitis
                                    ]
                                    assembled
                           )

                -- CHW encounter types where medication is not distributed.
                _ ->
                    False

        NextStepsWait ->
            -- Exclusive task for Nurse encounter.
            (assembled.encounter.encounterType == NurseEncounter)
                && -- If we refer patients somewhere, there's no need to wait.
                   (not <| expectNextStepsTask currentDate assembled NextStepsSendToHC)
                && -- We show Wait activity when there's at least one
                   -- test that was performed, or, 2 hours waiting is
                   -- required for blood pressure recheck.
                   (getMeasurementValueFunc assembled.measurements.labsResults
                        |> Maybe.map (.performedTests >> EverySet.isEmpty >> not)
                        |> Maybe.withDefault False
                   )


nextStepsTaskCompleted : NominalDate -> AssembledData -> NextStepsTask -> Bool
nextStepsTaskCompleted currentDate assembled task =
    case task of
        NextStepsAppointmentConfirmation ->
            isJust assembled.measurements.appointmentConfirmation

        NextStepsFollowUp ->
            isJust assembled.measurements.followUp

        NextStepsSendToHC ->
            resolveRequiredReferralFacilities assembled
                |> List.all (referralToFacilityCompleted assembled)

        NextStepsHealthEducation ->
            isJust assembled.measurements.healthEducation

        NextStepsNewbornEnrolment ->
            isJust assembled.participant.newborn

        NextStepsMedicationDistribution ->
            let
                allowedSigns =
                    NoMedicationDistributionSignsInitialPhase :: medicationsInitialPhase

                medicationDistributionRequired =
                    resolveRequiredMedicationsSet English currentDate PrenatalEncounterPhaseInitial assembled
                        |> List.isEmpty
                        |> not

                medicationDistributionCompleted =
                    if medicationDistributionRequired then
                        medicationDistributionMeasurementTaken allowedSigns assembled.measurements

                    else
                        True

                malariaTreatmentCompleted =
                    if diagnosedMalaria assembled then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForMalaria assembled.measurements

                    else
                        True

                heartburnTreatmentCompleted =
                    if diagnosed DiagnosisHeartburn assembled then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForHeartburn assembled.measurements

                    else
                        True

                hypertensionTreatmentCompleted =
                    if
                        diagnosedHypertension PrenatalEncounterPhaseInitial assembled
                            || -- Adding this to account for continuous treatment that may be
                               -- provided for Moderate Preeclampsia.
                               diagnosedPreviouslyAnyOf
                                [ DiagnosisModeratePreeclampsiaInitialPhase
                                , DiagnosisModeratePreeclampsiaRecurrentPhase
                                ]
                                assembled
                    then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForHypertension assembled.measurements

                    else
                        True

                candidiasisTreatmentCompleted =
                    if diagnosed DiagnosisCandidiasis assembled then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForCandidiasis assembled.measurements

                    else
                        True

                urinaryTractInfectionTreatmentCompleted =
                    if diagnosed DiagnosisUrinaryTractInfection assembled then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForUrinaryTractInfection assembled.measurements

                    else
                        True

                mastitisTreatmentCompleted =
                    if
                        diagnosedAnyOf
                            [ DiagnosisPostpartumEarlyMastitisOrEngorgment
                            , DiagnosisPostpartumMastitis
                            ]
                            assembled
                    then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForMastitis assembled.measurements

                    else
                        True
            in
            medicationDistributionCompleted
                && malariaTreatmentCompleted
                && heartburnTreatmentCompleted
                && hypertensionTreatmentCompleted
                && candidiasisTreatmentCompleted
                && urinaryTractInfectionTreatmentCompleted
                && mastitisTreatmentCompleted

        NextStepsWait ->
            getMeasurementValueFunc assembled.measurements.labsResults
                |> Maybe.map .patientNotified
                |> Maybe.withDefault False


resolveTreatmentReviewTasks : AssembledData -> List TreatmentReviewTask
resolveTreatmentReviewTasks assembled =
    let
        tasks =
            [ TreatmentReviewPrenatalMedication
            , TreatmentReviewHIV
            , TreatmentReviewHypertension
            , TreatmentReviewMalaria
            , TreatmentReviewAnemia
            , TreatmentReviewSyphilis
            ]
    in
    List.filter (expectTreatmentReviewTask assembled) tasks


expectTreatmentReviewTask : AssembledData -> TreatmentReviewTask -> Bool
expectTreatmentReviewTask assembled task =
    case task of
        TreatmentReviewPrenatalMedication ->
            True

        TreatmentReviewHIV ->
            -- Show, if there was medication treatment, or if patient was
            -- referred to HIV program.
            (isJust <| latestMedicationTreatmentForHIV assembled)
                || referredToHIVProgramPreviously assembled

        TreatmentReviewHypertension ->
            -- Show, if there was medication treatment.
            latestMedicationTreatmentForHypertension assembled
                |> isJust

        TreatmentReviewMalaria ->
            -- Show, if there was medication treatment.
            latestMedicationTreatmentForMalaria assembled
                |> isJust

        TreatmentReviewAnemia ->
            -- Show, if there was medication treatment.
            latestMedicationTreatmentForAnemia assembled
                |> isJust

        TreatmentReviewSyphilis ->
            -- Show, if there was medication treatment.
            latestMedicationTreatmentForSyphilis assembled
                |> isJust


treatmentReviewTaskCompleted : AssembledData -> TreatmentReviewTask -> Bool
treatmentReviewTaskCompleted assembled task =
    case task of
        TreatmentReviewPrenatalMedication ->
            getMeasurementValueFunc assembled.measurements.medication
                |> Maybe.map (.signs >> isJust)
                |> Maybe.withDefault False

        TreatmentReviewHIV ->
            getMeasurementValueFunc assembled.measurements.medication
                |> Maybe.map (.hivTreatment >> isJust)
                |> Maybe.withDefault False

        TreatmentReviewHypertension ->
            getMeasurementValueFunc assembled.measurements.medication
                |> Maybe.map (.hypertensionTreatment >> isJust)
                |> Maybe.withDefault False

        TreatmentReviewMalaria ->
            getMeasurementValueFunc assembled.measurements.medication
                |> Maybe.map (.malariaTreatment >> isJust)
                |> Maybe.withDefault False

        TreatmentReviewAnemia ->
            getMeasurementValueFunc assembled.measurements.medication
                |> Maybe.map (.anemiaTreatment >> isJust)
                |> Maybe.withDefault False

        TreatmentReviewSyphilis ->
            getMeasurementValueFunc assembled.measurements.medication
                |> Maybe.map (.syphilisTreatment >> isJust)
                |> Maybe.withDefault False


resolveHistoryTasks : AssembledData -> List HistoryTask
resolveHistoryTasks assembled =
    let
        tasks =
            [ Obstetric, Medical, Social, OutsideCare ]
    in
    List.filter (expectHistoryTask assembled) tasks


expectHistoryTask : AssembledData -> HistoryTask -> Bool
expectHistoryTask assembled task =
    let
        firstEnconter =
            nurseEncounterNotPerformed assembled
    in
    case task of
        Obstetric ->
            firstEnconter

        Medical ->
            firstEnconter

        Social ->
            True

        OutsideCare ->
            not firstEnconter


resolveExaminationTasks : AssembledData -> List ExaminationTask
resolveExaminationTasks assembled =
    let
        tasks =
            [ Vitals, NutritionAssessment, CorePhysicalExam, ObstetricalExam, BreastExam, GUExam ]
    in
    List.filter (expectExaminationTask assembled) tasks


expectExaminationTask : AssembledData -> ExaminationTask -> Bool
expectExaminationTask assembled task =
    case task of
        Vitals ->
            True

        NutritionAssessment ->
            True

        CorePhysicalExam ->
            True

        ObstetricalExam ->
            assembled.encounter.encounterType /= NursePostpartumEncounter

        BreastExam ->
            True

        GUExam ->
            assembled.encounter.encounterType == NursePostpartumEncounter


examinationTaskCompleted : AssembledData -> ExaminationTask -> Bool
examinationTaskCompleted assembled task =
    case task of
        Vitals ->
            isJust assembled.measurements.vitals

        NutritionAssessment ->
            isJust assembled.measurements.nutrition

        CorePhysicalExam ->
            isJust assembled.measurements.corePhysicalExam

        ObstetricalExam ->
            isJust assembled.measurements.obstetricalExam

        BreastExam ->
            isJust assembled.measurements.breastExam

        GUExam ->
            isJust assembled.measurements.guExam


expectSpecialityCareSignSection : AssembledData -> SpecialityCareSign -> Bool
expectSpecialityCareSignSection assembled sign =
    case sign of
        EnrolledToARVProgram ->
            resolveARVReferralDiagnosis assembled.nursePreviousEncountersData
                |> isJust

        EnrolledToNCDProgram ->
            resolveNCDReferralDiagnoses assembled.nursePreviousEncountersData
                |> List.isEmpty
                |> not

        NoSpecialityCareSigns ->
            False


specialityCareSections : List SpecialityCareSign
specialityCareSections =
    [ EnrolledToARVProgram, EnrolledToNCDProgram ]


referredToHIVProgramPreviously : AssembledData -> Bool
referredToHIVProgramPreviously assembled =
    List.filterMap
        (\data ->
            if EverySet.member DiagnosisHIV data.diagnoses then
                Just data.measurements

            else
                Nothing
        )
        assembled.nursePreviousEncountersData
        |> List.head
        |> Maybe.map hivProgramAtHC
        |> Maybe.withDefault False


latestMedicationTreatmentForHIV : AssembledData -> Maybe Translate.TranslationId
latestMedicationTreatmentForHIV assembled =
    let
        prescribedMedications =
            List.reverse assembled.nursePreviousEncountersData
                |> List.filterMap
                    (.measurements
                        >> .medicationDistribution
                        >> getMeasurementValueFunc
                        >> Maybe.andThen
                            (\value ->
                                let
                                    dolutegravirPrescribed =
                                        EverySet.member Dolutegravir value.distributionSigns

                                    arvsPrescribed =
                                        EverySet.member TDF3TC value.distributionSigns
                                in
                                if dolutegravirPrescribed || arvsPrescribed then
                                    Just ( dolutegravirPrescribed, arvsPrescribed )

                                else
                                    Nothing
                            )
                    )
                |> List.head
    in
    Maybe.map
        (\( dolutegravir, arvs ) ->
            Translate.TreatmentDetailsHIV dolutegravir arvs
        )
        prescribedMedications


{-| Note: Even though name says Hypertension, it includes Moderate Preeclamsia as well.
-}
latestMedicationTreatmentForHypertension : AssembledData -> Maybe Translate.TranslationId
latestMedicationTreatmentForHypertension assembled =
    getLatestTreatmentByTreatmentOptions recommendedTreatmentSignsForHypertension assembled
        |> Maybe.map
            (\treatment ->
                let
                    forModeratePreeclamsia =
                        diagnosedModeratePreeclampsiaPrevoiusly assembled
                in
                Translate.TreatmentDetailsHypertension forModeratePreeclamsia treatment
            )


latestMedicationTreatmentForMalaria : AssembledData -> Maybe Translate.TranslationId
latestMedicationTreatmentForMalaria assembled =
    let
        treatmentOptions =
            [ TreatmentQuinineSulphate
            , TreatmentCoartem
            ]
    in
    getLatestTreatmentByTreatmentOptions treatmentOptions assembled
        |> Maybe.map Translate.TreatmentDetailsMalaria


latestMedicationTreatmentForAnemia : AssembledData -> Maybe Translate.TranslationId
latestMedicationTreatmentForAnemia assembled =
    let
        medicationPrescribed =
            List.reverse assembled.nursePreviousEncountersData
                |> List.filter
                    (.measurements
                        >> .medicationDistribution
                        >> getMeasurementValueFunc
                        >> Maybe.map
                            (\value ->
                                List.any (\sign -> EverySet.member sign value.distributionSigns)
                                    [ Iron
                                    , FolicAcid
                                    ]
                            )
                        >> Maybe.withDefault False
                    )
                |> List.isEmpty
                |> not
    in
    if medicationPrescribed then
        Just Translate.TreatmentDetailsAnemia

    else
        Nothing


latestMedicationTreatmentForSyphilis : AssembledData -> Maybe Translate.TranslationId
latestMedicationTreatmentForSyphilis assembled =
    let
        treatmentOptions =
            [ TreatmentPenecilin1
            , TreatmentPenecilin3
            , TreatmentErythromycin
            , TreatmentAzithromycin
            , TreatmentCeftriaxon
            ]
    in
    getLatestTreatmentByTreatmentOptions treatmentOptions assembled
        |> Maybe.map Translate.TreatmentDetailsSyphilis


historyTaskCompleted : AssembledData -> HistoryTask -> Bool
historyTaskCompleted assembled task =
    case task of
        Obstetric ->
            isJust assembled.measurements.obstetricHistory
                && isJust assembled.measurements.obstetricHistoryStep2

        Medical ->
            isJust assembled.measurements.medicalHistory

        Social ->
            isJust assembled.measurements.socialHistory

        OutsideCare ->
            isJust assembled.measurements.outsideCare


referToHospital : AssembledData -> Bool
referToHospital =
    diagnosesCausingHospitalReferralByPhase PrenatalEncounterPhaseInitial >> EverySet.isEmpty >> not


referToMentalHealthSpecialist : AssembledData -> Bool
referToMentalHealthSpecialist assembled =
    mentalHealthSpecialistAtHC assembled && diagnosedAnyOf mentalHealthDiagnosesRequiringTreatment assembled


referToARVProgram : AssembledData -> Bool
referToARVProgram assembled =
    (diagnosed DiagnosisHIV assembled && hivProgramAtHC assembled.measurements)
        || referredToSpecialityCareProgram EnrolledToARVProgram assembled


referredToSpecialityCareProgram : SpecialityCareSign -> AssembledData -> Bool
referredToSpecialityCareProgram program assembled =
    getMeasurementValueFunc assembled.measurements.specialityCare
        |> Maybe.map (EverySet.member program >> not)
        |> Maybe.withDefault False


referToUltrasound : AssembledData -> Bool
referToUltrasound assembled =
    getMeasurementValueFunc assembled.measurements.lastMenstrualPeriod
        |> Maybe.map
            (\value ->
                (value.confident == False)
                    && isJust value.notConfidentReason
            )
        |> Maybe.withDefault False


provideNauseaAndVomitingEducation : AssembledData -> Bool
provideNauseaAndVomitingEducation assembled =
    let
        -- NauseaAndVomiting reported at current encounter, and
        -- all follow up questions were answered No.
        byCurrentEncounter =
            getMeasurementValueFunc assembled.measurements.symptomReview
                |> Maybe.map
                    (\value ->
                        EverySet.member NauseaAndVomiting value.symptoms
                            && List.all (\question -> not <| EverySet.member question value.symptomQuestions)
                                [ SymptomQuestionDizziness, SymptomQuestionLowUrineOutput, SymptomQuestionDarkUrine ]
                    )
                |> Maybe.withDefault False

        -- NauseaAndVomiting was not reported at any of previous encounters.
        byPreviousEncounters =
            not <| symptomRecordedPreviously assembled NauseaAndVomiting
    in
    byCurrentEncounter && byPreviousEncounters


hospitalizeDueToNauseaAndVomiting : AssembledData -> Bool
hospitalizeDueToNauseaAndVomiting assembled =
    let
        -- NauseaAndVomiting reported at current encounter, and
        -- any of follow up questions were answered Yes.
        byCurrentEncounter =
            getMeasurementValueFunc assembled.measurements.symptomReview
                |> Maybe.map
                    (\value ->
                        EverySet.member NauseaAndVomiting value.symptoms
                            && List.any (\question -> EverySet.member question value.symptomQuestions)
                                [ SymptomQuestionDizziness, SymptomQuestionLowUrineOutput, SymptomQuestionDarkUrine ]
                    )
                |> Maybe.withDefault False

        -- NauseaAndVomiting was reported at any of previous encounters.
        byPreviousEncounters =
            symptomRecorded assembled.measurements NauseaAndVomiting
                && symptomRecordedPreviously assembled NauseaAndVomiting
    in
    byCurrentEncounter || byPreviousEncounters


provideLegPainRednessEducation : AssembledData -> Bool
provideLegPainRednessEducation assembled =
    -- LegPainRedness reported at current encounter, and
    -- all follow up questions were answered No.
    getMeasurementValueFunc assembled.measurements.symptomReview
        |> Maybe.map
            (\value ->
                EverySet.member LegPainRedness value.symptoms
                    && List.all (\question -> not <| EverySet.member question value.symptomQuestions)
                        [ SymptomQuestionLegPainful, SymptomQuestionLegSwollen, SymptomQuestionLegWarm ]
            )
        |> Maybe.withDefault False


hospitalizeDueToLegPainRedness : AssembledData -> Bool
hospitalizeDueToLegPainRedness assembled =
    -- LegPainRedness reported at current encounter, and
    -- at least one follow up questions were answered Yes.
    getMeasurementValueFunc assembled.measurements.symptomReview
        |> Maybe.map
            (\value ->
                EverySet.member LegPainRedness value.symptoms
                    && List.any (\question -> EverySet.member question value.symptomQuestions)
                        [ SymptomQuestionLegPainful, SymptomQuestionLegSwollen, SymptomQuestionLegWarm ]
            )
        |> Maybe.withDefault False


providePelvicPainEducation : AssembledData -> Bool
providePelvicPainEducation assembled =
    let
        -- PelvicPain reported at current encounter, and
        -- all follow up questions were answered No.
        byCurrentEncounter =
            getMeasurementValueFunc assembled.measurements.symptomReview
                |> Maybe.map
                    (\value ->
                        EverySet.member PelvicPain value.symptoms
                            && (not <| EverySet.member SymptomQuestionPelvicPainHospitalization value.symptomQuestions)
                    )
                |> Maybe.withDefault False

        -- PelvicPain was not reported at any of previous encounters.
        byPreviousEncounters =
            not <| symptomRecordedPreviously assembled PelvicPain
    in
    byCurrentEncounter && byPreviousEncounters


provideMentalHealthEducation : AssembledData -> Bool
provideMentalHealthEducation assembled =
    -- Mental health survey was taken and none of
    -- mental health diagnoses was determined.
    -- No need to display at Postpartum encounter.
    (assembled.encounter.encounterType == NurseEncounter)
        && isJust assembled.measurements.mentalHealth
        && diagnosedNoneOf mentalHealthDiagnosesRequiringTreatment assembled


hospitalizeDueToPelvicPain : AssembledData -> Bool
hospitalizeDueToPelvicPain assembled =
    -- PelvicPain reported at current encounter, and
    -- any of follow up questions were answered Yes.
    getMeasurementValueFunc assembled.measurements.symptomReview
        |> Maybe.map
            (\value ->
                EverySet.member PelvicPain value.symptoms
                    && EverySet.member SymptomQuestionPelvicPainHospitalization value.symptomQuestions
            )
        |> Maybe.withDefault False


symptomRecorded : PrenatalMeasurements -> PrenatalSymptom -> Bool
symptomRecorded measurements symptom =
    getMeasurementValueFunc measurements.symptomReview
        |> Maybe.map (.symptoms >> EverySet.member symptom)
        |> Maybe.withDefault False


symptomRecordedPreviously : AssembledData -> PrenatalSymptom -> Bool
symptomRecordedPreviously assembled symptom =
    assembled.nursePreviousEncountersData
        |> List.filter
            (\data ->
                symptomRecorded data.measurements symptom
            )
        |> List.isEmpty
        |> not


mandatoryActivitiesForAssessmentCompleted : NominalDate -> AssembledData -> Bool
mandatoryActivitiesForAssessmentCompleted currentDate assembled =
    case assembled.encounter.encounterType of
        NurseEncounter ->
            activityCompleted currentDate assembled DangerSigns

        NursePostpartumEncounter ->
            True

        _ ->
            mandatoryActivitiesForNextStepsCompleted currentDate assembled


mandatoryActivitiesForNextStepsCompleted : NominalDate -> AssembledData -> Bool
mandatoryActivitiesForNextStepsCompleted currentDate assembled =
    let
        mandatoryActivitiesForNurseCompleted =
            -- All activities that will appear at
            -- current encounter are completed, besides
            -- Malaria Prevention and Photo (optional)
            --and the Next Steps itself.
            getAllActivities assembled
                |> EverySet.fromList
                |> EverySet.remove Backend.PrenatalActivity.Model.MalariaPrevention
                |> EverySet.remove PrenatalPhoto
                |> EverySet.remove NextSteps
                |> EverySet.toList
                |> List.filter (expectActivity currentDate assembled)
                |> List.all (activityCompleted currentDate assembled)
    in
    case assembled.encounter.encounterType of
        NurseEncounter ->
            -- If we have emergency diagnosis that require immediate referral,
            -- we allow displaying Next steps right away.
            diagnosedAnyOf emergencyReferralDiagnoses assembled
                || (-- Otherwise, we need all activities that will appear at
                    -- current encounter completed, besides Photo
                    -- and Next Steps itself.
                    mandatoryActivitiesForNurseCompleted
                   )

        NursePostpartumEncounter ->
            mandatoryActivitiesForNurseCompleted

        ChwFirstEncounter ->
            let
                commonMandatoryActivitiesCompleted =
                    ((not <| expectActivity currentDate assembled PregnancyDating)
                        || activityCompleted currentDate assembled PregnancyDating
                    )
                        && ((not <| expectActivity currentDate assembled Laboratory)
                                || activityCompleted currentDate assembled Laboratory
                           )
                        && activityCompleted currentDate assembled DangerSigns
            in
            if dangerSignsPresent assembled then
                commonMandatoryActivitiesCompleted

            else
                commonMandatoryActivitiesCompleted
                    && activityCompleted currentDate assembled Backend.PrenatalActivity.Model.HealthEducation

        ChwSecondEncounter ->
            let
                commonMandatoryActivitiesCompleted =
                    activityCompleted currentDate assembled DangerSigns
            in
            if dangerSignsPresent assembled then
                commonMandatoryActivitiesCompleted

            else
                commonMandatoryActivitiesCompleted
                    && activityCompleted currentDate assembled BirthPlan
                    && activityCompleted currentDate assembled Backend.PrenatalActivity.Model.HealthEducation

        ChwThirdPlusEncounter ->
            let
                commonMandatoryActivitiesCompleted =
                    activityCompleted currentDate assembled DangerSigns
            in
            if dangerSignsPresent assembled then
                commonMandatoryActivitiesCompleted

            else
                commonMandatoryActivitiesCompleted
                    && activityCompleted currentDate assembled Backend.PrenatalActivity.Model.HealthEducation

        ChwPostpartumEncounter ->
            activityCompleted currentDate assembled PregnancyOutcome
                && activityCompleted currentDate assembled DangerSigns


expectPrenatalPhoto : NominalDate -> AssembledData -> Bool
expectPrenatalPhoto currentDate assembled =
    let
        periods =
            -- Periods, where we want to have 1 photo:
            --  1. 12 weeks, or less.
            --  2. Between week 13 and week 27.
            --  3. Week 28, or more.
            [ [ (>) 13 ], [ (>) 28, (<=) 13 ], [ (<=) 28 ] ]

        nursePreviousMeasurements =
            List.map .measurements assembled.nursePreviousEncountersData
    in
    assembled.globalLmpDate
        |> Maybe.map
            (\lmpDate ->
                let
                    currentWeek =
                        calculateEGAWeeks currentDate lmpDate

                    conditionsForCurrentWeek =
                        periods
                            |> List.filter
                                (\periodConditions ->
                                    List.all (\condition -> condition currentWeek == True) periodConditions
                                )
                            |> List.head
                in
                conditionsForCurrentWeek
                    |> Maybe.map
                        (\conditions ->
                            -- There should be no encounters that are  within dates range,
                            -- that got a photo measurement.
                            assembled.nursePreviousEncountersData
                                |> List.filterMap
                                    (\data ->
                                        let
                                            encounterWeek =
                                                diffDays lmpDate data.startDate // 7
                                        in
                                        -- Encounter is within dates range, and it's has a photo measurement.
                                        if
                                            List.all (\condition -> condition encounterWeek == True) conditions
                                                && isJust data.measurements.prenatalPhoto
                                        then
                                            Just data.startDate

                                        else
                                            Nothing
                                    )
                                |> List.isEmpty
                        )
                    -- There are no period conditions, meaning we're not within required dates
                    -- range. Therefore, we do not allow photo activity.
                    |> Maybe.withDefault False
            )
        -- We do not allow photo activity when Lmp date is not known.
        |> Maybe.withDefault False


noDangerSigns : AssembledData -> Bool
noDangerSigns assembled =
    let
        getDangerSignsType getFunc =
            assembled.measurements.dangerSigns
                |> Maybe.map (Tuple.second >> .value >> getFunc >> EverySet.toList)
                |> Maybe.withDefault []

        dangerSignsEmpty emptySign signsList =
            List.isEmpty signsList || signsList == [ emptySign ]
    in
    case assembled.encounter.encounterType of
        ChwPostpartumEncounter ->
            let
                motherSignsEmpty =
                    getDangerSignsType .postpartumMother
                        |> dangerSignsEmpty NoPostpartumMotherDangerSigns

                childSignsEmpty =
                    getDangerSignsType .postpartumChild
                        |> dangerSignsEmpty NoPostpartumChildDangerSigns
            in
            motherSignsEmpty && childSignsEmpty

        _ ->
            getDangerSignsType .signs
                |> dangerSignsEmpty NoDangerSign


dangerSignsPresent : AssembledData -> Bool
dangerSignsPresent assembled =
    isJust assembled.measurements.dangerSigns && not (noDangerSigns assembled)


generateDangerSignsListForNurse : AssembledData -> List DangerSign
generateDangerSignsListForNurse assembled =
    case assembled.encounter.encounterType of
        NurseEncounter ->
            getDangerSignsListForType .signs
                identity
                NoDangerSign
                assembled.measurements

        NursePostpartumEncounter ->
            -- No need for this, becasue there's no
            -- Danger signs activity at Postpartum encounter.
            []

        _ ->
            []


generateDangerSignsListForChw : Language -> AssembledData -> List String
generateDangerSignsListForChw language assembled =
    case assembled.encounter.encounterType of
        NurseEncounter ->
            []

        NursePostpartumEncounter ->
            []

        ChwPostpartumEncounter ->
            let
                motherSigns =
                    getDangerSignsListForType .postpartumMother
                        (Translate.PostpartumMotherDangerSign >> translate language)
                        NoPostpartumMotherDangerSigns
                        assembled.measurements

                childSigns =
                    getDangerSignsListForType .postpartumChild
                        (Translate.PostpartumChildDangerSign >> translate language)
                        NoPostpartumChildDangerSigns
                        assembled.measurements
            in
            motherSigns ++ childSigns

        _ ->
            getDangerSignsListForType .signs
                (Translate.DangerSign >> translate language)
                NoDangerSign
                assembled.measurements


getDangerSignsListForType : (DangerSignsValue -> EverySet s) -> (s -> ms) -> s -> PrenatalMeasurements -> List ms
getDangerSignsListForType getFunc mappingFunc noSignsValue measurements =
    getMeasurementValueFunc measurements.dangerSigns
        |> Maybe.map
            (getFunc
                >> EverySet.toList
                >> List.filter ((/=) noSignsValue)
                >> List.map mappingFunc
            )
        |> Maybe.withDefault []


resolveMeasuredHeight : AssembledData -> Maybe HeightInCm
resolveMeasuredHeight assembled =
    let
        resolveHeight measurements =
            getMeasurementValueFunc measurements.nutrition
                |> Maybe.map .height

        heightMeasuredByNurse =
            List.filterMap (.measurements >> resolveHeight)
                assembled.nursePreviousEncountersData
                |> List.head

        heightMeasuredByCHW =
            List.filterMap
                (\( _, _, measurements ) ->
                    resolveHeight measurements
                )
                assembled.chwPreviousMeasurementsWithDates
                |> List.head
    in
    Maybe.Extra.or heightMeasuredByNurse heightMeasuredByCHW


generatePrenatalAssesmentForChw : AssembledData -> PrenatalAssesment
generatePrenatalAssesmentForChw assembled =
    if noDangerSigns assembled then
        AssesmentNormalPregnancy

    else
        AssesmentHighRiskPregnancy


generatePrenatalDiagnosesForNurse : NominalDate -> AssembledData -> EverySet PrenatalDiagnosis
generatePrenatalDiagnosesForNurse currentDate assembled =
    let
        egaInWeeks =
            Maybe.map
                (calculateEGAWeeks currentDate)
                assembled.globalLmpDate

        dangerSignsList =
            generateDangerSignsListForNurse assembled

        emergencyDiagnoses =
            List.filter
                (matchEmergencyReferalPrenatalDiagnosis
                    egaInWeeks
                    dangerSignsList
                    assembled
                )
                emergencyReferralDiagnoses
                |> EverySet.fromList

        diagnosesByLabResultsAndExamination =
            List.filter (matchLabResultsAndExaminationPrenatalDiagnosis egaInWeeks dangerSignsList assembled)
                labResultsAndExaminationDiagnoses
                |> EverySet.fromList

        diagnosesBySymptoms =
            List.filter (matchSymptomsPrenatalDiagnosis egaInWeeks assembled)
                symptomsDiagnoses
                |> EverySet.fromList

        diagnosesByMentalHealth =
            List.filter (matchMentalHealthPrenatalDiagnosis assembled)
                mentalHealthDiagnoses
                |> EverySet.fromList
    in
    EverySet.union emergencyDiagnoses diagnosesByLabResultsAndExamination
        |> EverySet.union diagnosesBySymptoms
        |> EverySet.union diagnosesByMentalHealth
        |> applyDiagnosesHierarchy


matchEmergencyReferalPrenatalDiagnosis : Maybe Int -> List DangerSign -> AssembledData -> PrenatalDiagnosis -> Bool
matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs assembled diagnosis =
    let
        measurements =
            assembled.measurements

        resolveEGAWeeksAndThen func =
            resolveEGAInWeeksAndThen func egaInWeeks
    in
    case diagnosis of
        DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus ->
            -- Moderate Preeclampsia is a chronic diagnosis for whole duration
            -- of pregnancy. Therefore, if diagnosed once, we do not need
            -- to diagnose it again.
            -- Instead, we adjust medication, or send to hospital, depending
            -- on current BP and previous treatment.
            (not <| diagnosedModeratePreeclampsiaPrevoiusly assembled)
                && resolveEGAWeeksAndThen
                    (\egaWeeks ->
                        (egaWeeks >= 37)
                            && moderatePreeclampsiaByMeasurements measurements
                    )

        DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus ->
            -- Moderate Preeclampsia is a chronic diagnosis for whole duration
            -- of pregnancy. Therefore, if diagnosed once, we do not need
            -- to diagnose it again.
            -- Instead, we adjust medication, or send to hospital, depending
            -- on current BP and previous treatment.
            (not <| diagnosedModeratePreeclampsiaPrevoiusly assembled)
                && (-- If diagnosed Moderate Preeclampsia at initial stage, we do not
                    -- need to diagnose again.
                    not <| diagnosed DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus assembled
                   )
                && resolveEGAWeeksAndThen
                    (\egaWeeks ->
                        (egaWeeks >= 37)
                            && moderatePreeclampsiaByMeasurementsRecurrentPhase measurements
                    )

        DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus ->
            resolveEGAWeeksAndThen
                (\egaWeeks ->
                    (egaWeeks >= 37)
                        && severePreeclampsiaByDangerSigns signs
                )

        DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus ->
            (-- If diagnosed Severe Preeclampsia at initial stage, we do not
             -- need to diagnose again.
             not <| diagnosed DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus assembled
            )
                && resolveEGAWeeksAndThen
                    (\egaWeeks ->
                        (egaWeeks >= 37)
                            && severePreeclampsiaRecurrentPhase signs measurements
                    )

        DiagnosisEclampsia ->
            List.member Convulsions signs

        DiagnosisMiscarriage ->
            Maybe.map
                (\egaWeeks ->
                    (egaWeeks <= 22) && List.member VaginalBleeding signs
                )
                egaInWeeks
                |> Maybe.withDefault False

        DiagnosisMolarPregnancy ->
            matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs assembled DiagnosisMiscarriage

        DiagnosisPlacentaPrevia ->
            Maybe.map
                (\egaWeeks ->
                    (egaWeeks > 22) && List.member VaginalBleeding signs
                )
                egaInWeeks
                |> Maybe.withDefault False

        DiagnosisPlacentalAbruption ->
            matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs assembled DiagnosisPlacentaPrevia
                || matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs assembled DiagnosisObstructedLabor

        DiagnosisUterineRupture ->
            matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs assembled DiagnosisPlacentaPrevia
                || matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs assembled DiagnosisObstructedLabor

        DiagnosisObstructedLabor ->
            Maybe.map
                (\egaWeeks ->
                    (egaWeeks >= 22) && List.member AbdominalPain signs
                )
                egaInWeeks
                |> Maybe.withDefault False

        DiagnosisPostAbortionSepsis ->
            List.member AbdominalPain signs
                || matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs assembled DiagnosisMiscarriage

        DiagnosisEctopicPregnancy ->
            matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs assembled DiagnosisMiscarriage
                || (Maybe.map
                        (\egaWeeks ->
                            (egaWeeks < 22) && List.member AbdominalPain signs
                        )
                        egaInWeeks
                        |> Maybe.withDefault False
                   )

        DiagnosisPROM ->
            Maybe.map
                (\egaWeeks ->
                    (egaWeeks > 37) && List.member GushLeakingVaginalFluid signs
                )
                egaInWeeks
                |> Maybe.withDefault False

        DiagnosisPPROM ->
            Maybe.map
                (\egaWeeks ->
                    (egaWeeks <= 37) && List.member GushLeakingVaginalFluid signs
                )
                egaInWeeks
                |> Maybe.withDefault False

        DiagnosisHyperemesisGravidum ->
            Maybe.map
                (\egaWeeks ->
                    (egaWeeks < 20) && List.member SevereVomiting signs
                )
                egaInWeeks
                |> Maybe.withDefault False

        DiagnosisSevereVomiting ->
            Maybe.map
                (\egaWeeks ->
                    (egaWeeks >= 20) && List.member SevereVomiting signs
                )
                egaInWeeks
                |> Maybe.withDefault False

        DiagnosisMaternalComplications ->
            List.member ExtremeWeakness signs
                || List.member Unconscious signs
                || List.member LooksVeryIll signs

        -- Infection diagnosis will be available at latter phase.
        DiagnosisInfection ->
            List.member Fever signs
                && (List.member ExtremeWeakness signs || respiratoryRateElevated assembled.measurements)

        DiagnosisImminentDelivery ->
            List.member ImminentDelivery signs

        DiagnosisLaborAndDelivery ->
            List.member Labor signs

        DiagnosisSevereAnemiaWithComplications ->
            severeAnemiaWithComplicationsDiagnosed signs assembled.measurements

        -- Non Emergency Referral diagnoses.
        _ ->
            False


resolveEGAInWeeksAndThen : (Int -> Bool) -> Maybe Int -> Bool
resolveEGAInWeeksAndThen func =
    Maybe.map func
        >> Maybe.withDefault False


severeAnemiaWithComplicationsDiagnosed : List DangerSign -> PrenatalMeasurements -> Bool
severeAnemiaWithComplicationsDiagnosed dangerSigns measurements =
    let
        hemoglobinCount =
            resolveHemoglobinCount measurements
    in
    (-- Hemoglobin test was performed, and, hemoglobin
     -- count indicates severe anemia.
     Maybe.map (\count -> count < 7) hemoglobinCount
        |> Maybe.withDefault False
    )
        && anemiaComplicationSignsPresent dangerSigns measurements


anemiaComplicationSignsPresent : List DangerSign -> PrenatalMeasurements -> Bool
anemiaComplicationSignsPresent dangerSigns measurements =
    let
        anemiaComplicationSignsByExamination =
            getMeasurementValueFunc measurements.corePhysicalExam
                |> Maybe.map
                    (\exam ->
                        EverySet.member PallorHands exam.hands || EverySet.member PaleConjuctiva exam.eyes
                    )
                |> Maybe.withDefault False
    in
    respiratoryRateElevated measurements
        || List.member DifficultyBreathing dangerSigns
        || anemiaComplicationSignsByExamination


resolveHemoglobinCount : PrenatalMeasurements -> Maybe Float
resolveHemoglobinCount measurements =
    getMeasurementValueFunc measurements.hemoglobinTest
        |> Maybe.andThen .hemoglobinCount


matchLabResultsAndExaminationPrenatalDiagnosis : Maybe Int -> List DangerSign -> AssembledData -> PrenatalDiagnosis -> Bool
matchLabResultsAndExaminationPrenatalDiagnosis egaInWeeks dangerSigns assembled diagnosis =
    let
        measurements =
            assembled.measurements

        positiveMalariaTest =
            getMeasurementValueFunc measurements.malariaTest
                |> Maybe.map
                    (\value ->
                        (-- Malaria RDT was run, and positive result was recorded.
                         testPerformedByExecutionNote value.executionNote
                            && (value.testResult == Just TestPositive)
                        )
                            || (-- Malaria RDT was not run, but blood smear test
                                -- was taken, and it's result indicates Malaria.
                                List.member value.executionNote
                                    [ TestNoteLackOfReagents
                                    , TestNoteLackOfOtherSupplies
                                    , TestNoteNoEquipment
                                    , TestNoteBrokenEquipment
                                    , TestNoteNotIndicated
                                    ]
                                    && List.member value.bloodSmearResult
                                        [ BloodSmearPlus
                                        , BloodSmearPlusPlus
                                        , BloodSmearPlusPlusPlus
                                        ]
                               )
                    )
                |> Maybe.withDefault False

        positiveSyphilisTest =
            testedPositiveAt .syphilisTest

        testedPositiveAt getMeasurementFunc =
            getMeasurementFunc measurements
                |> getMeasurementValueFunc
                |> Maybe.map
                    (\value ->
                        testPerformedByExecutionNote value.executionNote
                            && (value.testResult == Just TestPositive)
                    )
                |> Maybe.withDefault False

        hemoglobinCount =
            resolveHemoglobinCount measurements

        malariaDiagnosed =
            positiveMalariaTest
                && ((-- Either hemoglobin test was not performed, or,
                     -- hemoglobin count is within normal ranges.
                     Maybe.map (\count -> count >= 11) hemoglobinCount
                        |> Maybe.withDefault True
                    )
                        || -- When severe Anemia with complications is diagnosed,
                           -- we view Malaria as separate diagnosis.
                           -- There's not 'Malaria and Severe Anemia with
                           -- complications' diagnosis.
                           severeAnemiaWithComplicationsDiagnosed dangerSigns measurements
                   )

        malariaWithAnemiaDiagnosed =
            positiveMalariaTest
                && (-- Hemoglobin test was performed, and,
                    -- hemoglobin count indicates mild to moderate anemia.
                    Maybe.map (\count -> count >= 7 && count < 11) hemoglobinCount
                        |> Maybe.withDefault False
                   )

        diabetesDiagnosed =
            getMeasurementValueFunc measurements.randomBloodSugarTest
                |> Maybe.map
                    (\value ->
                        let
                            bySugarCount =
                                diabetesBySugarCount value

                            byUrineGlucose =
                                if testPerformedByExecutionNote value.executionNote then
                                    -- If random blood sugar test was perfomed, we determine by its results.
                                    False

                                else
                                    -- If random blood sugar test was not perfomed, we determine by
                                    -- glucose level at urine dipstick test.
                                    getMeasurementValueFunc measurements.urineDipstickTest
                                        |> Maybe.map diabetesByUrineGlucose
                                        |> Maybe.withDefault False
                        in
                        bySugarCount || byUrineGlucose
                    )
                |> Maybe.withDefault False

        resolveEGAWeeksAndThen func =
            resolveEGAInWeeksAndThen func egaInWeeks
    in
    case diagnosis of
        DiagnosisChronicHypertensionImmediate ->
            -- Hypertension is a chronic diagnosis for whole duration
            -- of pregnancy.
            -- Moderate Preeclamsia is higher level of Hypertension disease.
            -- and a chronic diagnosis as well.
            -- Therefore, if Hypertension or Moderate Preeclamsia were
            -- diagnosed once, we do not need to diagnose Hypertension again.
            -- Instead, we adjust medication, or send to hospital, depending
            -- on current BP and previous treatment.
            (not <| diagnosedHypertensionPrevoiusly assembled)
                && (not <| diagnosedModeratePreeclampsiaPrevoiusly assembled)
                && resolveEGAWeeksAndThen (chronicHypertensionByMeasurements measurements)

        DiagnosisChronicHypertensionAfterRecheck ->
            -- Hypertension is a chronic diagnosis for whole duration
            -- of pregnancy.
            -- Moderate Preeclamsia is higher level of Hypertension disease.
            -- and a chronic diagnosis as well.
            -- Therefore, if Hypertension or Moderate Preeclamsia were
            -- diagnosed once, we do not need to diagnose Hypertension again.
            -- Instead, we adjust medication, or send to hospital, depending
            -- on current BP and previous treatment.
            (not <| diagnosedHypertensionPrevoiusly assembled)
                && (not <| diagnosedModeratePreeclampsiaPrevoiusly assembled)
                && resolveEGAWeeksAndThen (chronicHypertensionByMeasurementsAfterRecheck measurements)

        DiagnosisGestationalHypertensionImmediate ->
            -- Hypertension is a chronic diagnosis for whole duration
            -- of pregnancy.
            -- Moderate Preeclamsia is higher level of Hypertension disease.
            -- and a chronic diagnosis as well.
            -- Therefore, if Hypertension or Moderate Preeclamsia were
            -- diagnosed once, we do not need to diagnose Hypertension again.
            -- Instead, we adjust medication, or send to hospital, depending
            -- on current BP and previous treatment.
            (not <| diagnosedHypertensionPrevoiusly assembled)
                && (not <| diagnosedModeratePreeclampsiaPrevoiusly assembled)
                && resolveEGAWeeksAndThen (gestationalHypertensionByMeasurements measurements)

        DiagnosisGestationalHypertensionAfterRecheck ->
            -- Hypertension is a chronic diagnosis for whole duration
            -- of pregnancy.
            -- Moderate Preeclamsia is higher level of Hypertension disease.
            -- and a chronic diagnosis as well.
            -- Therefore, if Hypertension or Moderate Preeclamsia were
            -- diagnosed once, we do not need to diagnose Hypertension again.
            -- Instead, we adjust medication, or send to hospital, depending
            -- on current BP and previous treatment.
            (not <| diagnosedHypertensionPrevoiusly assembled)
                && (not <| diagnosedModeratePreeclampsiaPrevoiusly assembled)
                && resolveEGAWeeksAndThen (gestationalHypertensionByMeasurementsAfterRecheck measurements)

        DiagnosisModeratePreeclampsiaInitialPhase ->
            -- Moderate Preeclampsia is a chronic diagnosis for whole duration
            -- of pregnancy. Therefore, if diagnosed once, we do not need
            -- to diagnose it again.
            -- Instead, we adjust medication, or send to hospital, depending
            -- on current BP and previous treatment.
            (not <| diagnosedModeratePreeclampsiaPrevoiusly assembled)
                && resolveEGAWeeksAndThen
                    (\egaWeeks ->
                        (egaWeeks >= 20)
                            && (egaWeeks < 37)
                            && moderatePreeclampsiaByMeasurements measurements
                    )

        DiagnosisModeratePreeclampsiaRecurrentPhase ->
            -- Moderate Preeclampsia is a chronic diagnosis for whole duration
            -- of pregnancy. Therefore, if diagnosed once, we do not need
            -- to diagnose it again.
            -- Instead, we adjust medication, or send to hospital, depending
            -- on current BP and previous treatment.
            (not <| diagnosedModeratePreeclampsiaPrevoiusly assembled)
                && -- If diagnosed Moderate Preeclampsia at initial stage, we do not
                   -- need to diagnose again.
                   (not <| diagnosed DiagnosisModeratePreeclampsiaInitialPhase assembled)
                && resolveEGAWeeksAndThen
                    (\egaWeeks ->
                        (egaWeeks >= 20)
                            && (egaWeeks < 37)
                            && moderatePreeclampsiaByMeasurementsRecurrentPhase measurements
                    )

        DiagnosisSeverePreeclampsiaInitialPhase ->
            resolveEGAWeeksAndThen
                (\egaWeeks ->
                    (egaWeeks < 37)
                        && severePreeclampsiaByDangerSigns dangerSigns
                )

        DiagnosisSeverePreeclampsiaRecurrentPhase ->
            (-- If diagnosed Severe Preeclampsia at initial stage, we do not
             -- need to diagnose again.
             not <| diagnosed DiagnosisSeverePreeclampsiaInitialPhase assembled
            )
                && resolveEGAWeeksAndThen
                    (\egaWeeks ->
                        (egaWeeks < 37)
                            && severePreeclampsiaRecurrentPhase dangerSigns measurements
                    )

        DiagnosisHIV ->
            testedPositiveAt .hivTest

        DiagnosisHIVDetectableViralLoad ->
            getMeasurementValueFunc measurements.hivPCRTest
                |> Maybe.andThen
                    (.hivViralLoad >> Maybe.map (\viralLoad -> viralLoad >= 20))
                |> Maybe.withDefault False

        DiagnosisDiscordantPartnership ->
            getMeasurementValueFunc measurements.hivTest
                |> Maybe.andThen .hivSigns
                |> Maybe.map
                    (\hivSigns ->
                        -- Partner is HIV positive.
                        EverySet.member PartnerHIVPositive hivSigns
                            && (-- Partner is not taking ARVs.
                                (not <| EverySet.member PartnerTakingARV hivSigns)
                                    || -- Partner is taking ARVs, but did not
                                       -- reach surpressed viral load.
                                       (EverySet.member PartnerTakingARV hivSigns
                                            && (not <| EverySet.member PartnerSurpressedViralLoad hivSigns)
                                       )
                               )
                    )
                |> Maybe.withDefault False

        DiagnosisSyphilis ->
            positiveSyphilisTest
                && -- No symptoms were reported.
                   (getMeasurementValueFunc measurements.syphilisTest
                        |> Maybe.andThen .symptoms
                        |> Maybe.map
                            (\symptoms ->
                                EverySet.isEmpty symptoms || (EverySet.toList symptoms == [ NoIllnessSymptoms ])
                            )
                        |> Maybe.withDefault True
                   )

        DiagnosisSyphilisWithComplications ->
            positiveSyphilisTest
                && (getMeasurementValueFunc measurements.syphilisTest
                        |> Maybe.andThen .symptoms
                        |> Maybe.map
                            (\symptoms ->
                                (EverySet.member IllnessSymptomRash symptoms
                                    || EverySet.member IllnessSymptomPainlessUlcerMouth symptoms
                                    || EverySet.member IllnessSymptomPainlessUlcerGenitals symptoms
                                )
                                    -- Exclude Neurosyphilis conditions.
                                    && (not <| EverySet.member IllnessSymptomHeadache symptoms)
                                    && (not <| EverySet.member IllnessSymptomVisionChanges symptoms)
                            )
                        |> Maybe.withDefault False
                   )

        DiagnosisNeurosyphilis ->
            positiveSyphilisTest
                && (getMeasurementValueFunc measurements.syphilisTest
                        |> Maybe.andThen .symptoms
                        |> Maybe.map
                            (\symptoms ->
                                EverySet.member IllnessSymptomHeadache symptoms
                                    || EverySet.member IllnessSymptomVisionChanges symptoms
                            )
                        |> Maybe.withDefault False
                   )

        DiagnosisHepatitisB ->
            testedPositiveAt .hepatitisBTest

        DiagnosisMalaria ->
            malariaDiagnosed
                && ((isNothing <| latestMedicationTreatmentForMalaria assembled)
                        || (not <| diagnosedPreviously DiagnosisMalaria assembled)
                   )

        DiagnosisMalariaMedicatedContinued ->
            malariaDiagnosed
                && (isJust <| latestMedicationTreatmentForMalaria assembled)
                && diagnosedPreviously DiagnosisMalaria assembled

        DiagnosisMalariaWithAnemia ->
            malariaWithAnemiaDiagnosed
                && ((isNothing <| latestMedicationTreatmentForMalaria assembled)
                        || (not <| diagnosedPreviously DiagnosisMalariaWithAnemia assembled)
                   )

        DiagnosisMalariaWithAnemiaMedicatedContinued ->
            malariaWithAnemiaDiagnosed
                && (isJust <| latestMedicationTreatmentForMalaria assembled)
                && diagnosedPreviously DiagnosisMalariaWithAnemia assembled

        DiagnosisMalariaWithSevereAnemia ->
            positiveMalariaTest
                && (-- Hemoglobin test was performed, and,
                    -- hemoglobin count indicates severe anemia.
                    Maybe.map (\count -> count < 7) hemoglobinCount
                        |> Maybe.withDefault False
                   )

        DiagnosisModerateAnemia ->
            not positiveMalariaTest
                && (-- No indication for being positive for malaria,
                    -- Hemoglobin test was performed, and, hemoglobin
                    -- count indicates mild to moderate anemia.
                    Maybe.map (\count -> count >= 7 && count < 11) hemoglobinCount
                        |> Maybe.withDefault False
                   )

        DiagnosisSevereAnemia ->
            not positiveMalariaTest
                && (-- No indication for being positive for malaria,
                    -- Hemoglobin test was performed, and, hemoglobin
                    -- count indicates severe anemia.
                    Maybe.map (\count -> count < 7) hemoglobinCount
                        |> Maybe.withDefault False
                   )
                && (not <| anemiaComplicationSignsPresent dangerSigns measurements)

        Backend.PrenatalEncounter.Types.DiagnosisDiabetes ->
            (not <| diagnosedPreviouslyAnyOf diabetesDiagnoses assembled)
                && resolveEGAWeeksAndThen
                    (\egaWeeks ->
                        egaWeeks <= 20 && diabetesDiagnosed
                    )

        Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetes ->
            (not <| diagnosedPreviouslyAnyOf diabetesDiagnoses assembled)
                && resolveEGAWeeksAndThen
                    (\egaWeeks ->
                        egaWeeks > 20 && diabetesDiagnosed
                    )

        DiagnosisRhesusNegative ->
            getMeasurementValueFunc measurements.bloodGpRsTest
                |> Maybe.andThen .rhesus
                |> Maybe.map ((==) RhesusNegative)
                |> Maybe.withDefault False

        -- If criterias for DiagnosisPostpartumMastitis also matches, this
        -- diagnosis will be filtered out when applying diagnoses hierarchy.
        DiagnosisPostpartumEarlyMastitisOrEngorgment ->
            let
                byBreastfeeding =
                    getMeasurementValueFunc assembled.measurements.breastfeeding
                        |> Maybe.map
                            (\signs ->
                                List.any (\sign -> EverySet.member sign signs)
                                    [ BreastPain, BreastRedness ]
                            )
                        |> Maybe.withDefault False

                byBreastExam =
                    getMeasurementValueFunc assembled.measurements.breastExam
                        |> Maybe.map
                            (\value ->
                                case EverySet.toList value.exam of
                                    [ NormalBreast ] ->
                                        False

                                    _ ->
                                        True
                            )
                        |> Maybe.withDefault False
            in
            (assembled.encounter.encounterType == NursePostpartumEncounter)
                && (byBreastfeeding || byBreastExam)

        DiagnosisPostpartumMastitis ->
            let
                byBreastfeeding =
                    getMeasurementValueFunc assembled.measurements.breastfeeding
                        |> Maybe.map
                            (\signs ->
                                List.any (\sign -> EverySet.member sign signs)
                                    [ BreastPain, BreastRedness ]
                            )
                        |> Maybe.withDefault False

                byBreastExam =
                    getMeasurementValueFunc assembled.measurements.breastExam
                        |> Maybe.map
                            (\value ->
                                List.any (\sign -> EverySet.member sign value.exam)
                                    [ Warmth, Discharge ]
                            )
                        |> Maybe.withDefault False
            in
            (assembled.encounter.encounterType == NursePostpartumEncounter)
                && symptomRecorded assembled.measurements PostpartumFever
                && (byBreastfeeding || byBreastExam)

        DiagnosisPostpartumInfection ->
            getMeasurementValueFunc assembled.measurements.guExam
                |> Maybe.map
                    (\value ->
                        EverySet.member FoulSmellingLochia value.vaginalExamSigns
                            || (EverySet.member EpisiotomyOrPerinealTear value.guExamSigns
                                    && (Maybe.map (EverySet.member NormalPostpartumHealing >> not)
                                            value.postpartumHealingProblems
                                            |> Maybe.withDefault False
                                       )
                               )
                    )
                |> Maybe.withDefault False

        DiagnosisPostpartumExcessiveBleeding ->
            getMeasurementValueFunc assembled.measurements.guExam
                |> Maybe.map (.vaginalExamSigns >> EverySet.member ExcessiveVaginalBleeding)
                |> Maybe.withDefault False

        -- Non Lab Results diagnoses.
        _ ->
            False


matchSymptomsPrenatalDiagnosis : Maybe Int -> AssembledData -> PrenatalDiagnosis -> Bool
matchSymptomsPrenatalDiagnosis egaInWeeks assembled diagnosis =
    let
        heartburnDiagnosed =
            symptomRecorded assembled.measurements Heartburn

        urinaryTractInfectionDiagnosed =
            getMeasurementValueFunc assembled.measurements.symptomReview
                |> Maybe.map
                    (\value ->
                        EverySet.member BurningWithUrination value.symptoms
                            && List.all (\question -> not <| EverySet.member question value.symptomQuestions)
                                [ SymptomQuestionVaginalItching, SymptomQuestionVaginalDischarge ]
                            && (not <| flankPainPresent value.flankPainSign)
                    )
                |> Maybe.withDefault False

        candidiasisDiagnosed =
            getMeasurementValueFunc assembled.measurements.symptomReview
                |> Maybe.map
                    (\value ->
                        -- Burning with Urination + vaginal itching or discharge.
                        (EverySet.member BurningWithUrination value.symptoms
                            && List.any (\question -> EverySet.member question value.symptomQuestions)
                                [ SymptomQuestionVaginalItching, SymptomQuestionVaginalDischarge ]
                        )
                            || -- Abnormal discharge +  vaginal itching.
                               (EverySet.member AbnormalVaginalDischarge value.symptoms
                                    && EverySet.member SymptomQuestionVaginalItching value.symptomQuestions
                               )
                    )
                |> Maybe.withDefault False

        gonorrheaDiagnosed =
            getMeasurementValueFunc assembled.measurements.symptomReview
                |> Maybe.map
                    (\value ->
                        EverySet.member AbnormalVaginalDischarge value.symptoms
                            && EverySet.member SymptomQuestionPartnerUrethralDischarge value.symptomQuestions
                    )
                |> Maybe.withDefault False

        trichomonasOrBacterialVaginosisDiagnosed =
            getMeasurementValueFunc assembled.measurements.symptomReview
                |> Maybe.map
                    (\value ->
                        EverySet.member AbnormalVaginalDischarge value.symptoms
                            && List.all (\question -> not <| EverySet.member question value.symptomQuestions)
                                [ SymptomQuestionVaginalItching, SymptomQuestionPartnerUrethralDischarge ]
                    )
                |> Maybe.withDefault False
    in
    case diagnosis of
        DiagnosisHyperemesisGravidumBySymptoms ->
            Maybe.map
                (\egaWeeks ->
                    (egaWeeks < 20) && hospitalizeDueToNauseaAndVomiting assembled
                )
                egaInWeeks
                |> Maybe.withDefault False

        DiagnosisSevereVomitingBySymptoms ->
            Maybe.map
                (\egaWeeks ->
                    (egaWeeks >= 20) && hospitalizeDueToNauseaAndVomiting assembled
                )
                egaInWeeks
                |> Maybe.withDefault False

        DiagnosisHeartburn ->
            heartburnDiagnosed
                && (not <| diagnosedPreviously DiagnosisHeartburn assembled)

        DiagnosisHeartburnPersistent ->
            heartburnDiagnosed
                && diagnosedPreviously DiagnosisHeartburn assembled

        DiagnosisDeepVeinThrombosis ->
            hospitalizeDueToLegPainRedness assembled

        DiagnosisPelvicPainIntense ->
            hospitalizeDueToPelvicPain assembled

        DiagnosisPelvicPainContinued ->
            -- Pelvic pain was reported previously.
            symptomRecorded assembled.measurements PelvicPain
                && symptomRecordedPreviously assembled PelvicPain

        DiagnosisUrinaryTractInfection ->
            urinaryTractInfectionDiagnosed
                && (not <| diagnosedPreviously DiagnosisUrinaryTractInfection assembled)

        DiagnosisUrinaryTractInfectionContinued ->
            urinaryTractInfectionDiagnosed
                && diagnosedPreviously DiagnosisUrinaryTractInfection assembled

        DiagnosisPyelonephritis ->
            getMeasurementValueFunc assembled.measurements.symptomReview
                |> Maybe.map
                    (\value ->
                        EverySet.member BurningWithUrination value.symptoms
                            && flankPainPresent value.flankPainSign
                    )
                |> Maybe.withDefault False

        DiagnosisCandidiasis ->
            candidiasisDiagnosed
                && (not <| diagnosedPreviously DiagnosisCandidiasis assembled)

        DiagnosisCandidiasisContinued ->
            candidiasisDiagnosed
                && diagnosedPreviously DiagnosisCandidiasis assembled

        DiagnosisGonorrhea ->
            gonorrheaDiagnosed
                && (not <| diagnosedPreviously DiagnosisGonorrhea assembled)

        DiagnosisGonorrheaContinued ->
            gonorrheaDiagnosed
                && diagnosedPreviously DiagnosisGonorrhea assembled

        DiagnosisTrichomonasOrBacterialVaginosis ->
            trichomonasOrBacterialVaginosisDiagnosed
                && (not <| diagnosedPreviously DiagnosisGonorrhea assembled)

        DiagnosisTrichomonasOrBacterialVaginosisContinued ->
            trichomonasOrBacterialVaginosisDiagnosed
                && diagnosedPreviously DiagnosisGonorrhea assembled

        Backend.PrenatalEncounter.Types.DiagnosisTuberculosis ->
            symptomRecorded assembled.measurements CoughContinuous

        DiagnosisPostpartumAbdominalPain ->
            symptomRecorded assembled.measurements PostpartumAbdominalPain

        DiagnosisPostpartumUrinaryIncontinence ->
            symptomRecorded assembled.measurements PostpartumUrinaryIncontinence

        DiagnosisPostpartumHeadache ->
            symptomRecorded assembled.measurements PostpartumHeadache

        DiagnosisPostpartumFatigue ->
            symptomRecorded assembled.measurements PostpartumFatigue

        DiagnosisPostpartumFever ->
            symptomRecorded assembled.measurements PostpartumFever

        DiagnosisPostpartumPerinealPainOrDischarge ->
            symptomRecorded assembled.measurements PostpartumPerinealPainOrDischarge

        -- Non Symptoms diagnoses.
        _ ->
            False


matchMentalHealthPrenatalDiagnosis : AssembledData -> PrenatalDiagnosis -> Bool
matchMentalHealthPrenatalDiagnosis assembled diagnosis =
    let
        suicideRiskDiagnosed =
            getMeasurementValueFunc assembled.measurements.mentalHealth
                |> Maybe.andThen (.signs >> suicideRiskDiagnosedBySigns)
                |> Maybe.withDefault False
    in
    if suicideRiskDiagnosed then
        diagnosis == DiagnosisSuicideRisk

    else
        getMeasurementValueFunc assembled.measurements.mentalHealth
            |> Maybe.map
                (.signs
                    >> Dict.values
                    >> List.map mentalHealthQuestionOptionToScore
                    >> List.sum
                    >> (\mentalHealthScore ->
                            case diagnosis of
                                DiagnosisDepressionNotLikely ->
                                    mentalHealthScore < 9

                                DiagnosisDepressionPossible ->
                                    mentalHealthScore >= 9 && mentalHealthScore < 12

                                DiagnosisDepressionHighlyPossible ->
                                    mentalHealthScore >= 12 && mentalHealthScore < 14

                                DiagnosisDepressionProbable ->
                                    mentalHealthScore >= 14

                                -- Others are not mental health diagnoses that
                                -- are being determined by score.
                                _ ->
                                    False
                       )
                )
            |> Maybe.withDefault False


suicideRiskDiagnosedBySigns : Dict PrenatalMentalHealthQuestion PrenatalMentalHealthQuestionOption -> Maybe Bool
suicideRiskDiagnosedBySigns signs =
    Dict.get MentalHealthQuestion10 signs
        |> Maybe.map
            (\answer ->
                List.member answer
                    [ MentalHealthQuestionOption3
                    , MentalHealthQuestionOption2
                    , MentalHealthQuestionOption1
                    ]
            )


mentalHealthQuestionOptionToScore : PrenatalMentalHealthQuestionOption -> Int
mentalHealthQuestionOptionToScore option =
    case option of
        MentalHealthQuestionOption0 ->
            0

        MentalHealthQuestionOption1 ->
            1

        MentalHealthQuestionOption2 ->
            2

        MentalHealthQuestionOption3 ->
            3


{-| Flank pain on left, right or both sides.
-}
flankPainPresent : Maybe PrenatalFlankPainSign -> Bool
flankPainPresent sign =
    case sign of
        Nothing ->
            False

        Just NoFlankPain ->
            False

        _ ->
            True


chronicHypertensionByMeasurements : PrenatalMeasurements -> Int -> Bool
chronicHypertensionByMeasurements measurements egaWeeks =
    egaWeeks < 20 && highBloodPressure measurements


chronicHypertensionByMeasurementsAfterRecheck : PrenatalMeasurements -> Int -> Bool
chronicHypertensionByMeasurementsAfterRecheck measurements egaWeeks =
    egaWeeks < 20 && repeatedTestForMarginalBloodPressure measurements


gestationalHypertensionByMeasurements : PrenatalMeasurements -> Int -> Bool
gestationalHypertensionByMeasurements measurements egaWeeks =
    (egaWeeks >= 20) && highBloodPressure measurements


gestationalHypertensionByMeasurementsAfterRecheck : PrenatalMeasurements -> Int -> Bool
gestationalHypertensionByMeasurementsAfterRecheck measurements egaWeeks =
    egaWeeks >= 20 && repeatedTestForMarginalBloodPressure measurements


moderatePreeclampsiaByMeasurements : PrenatalMeasurements -> Bool
moderatePreeclampsiaByMeasurements measurements =
    highBloodPressure measurements
        && edemaOnHandOrLegs measurements


moderatePreeclampsiaByMeasurementsRecurrentPhase : PrenatalMeasurements -> Bool
moderatePreeclampsiaByMeasurementsRecurrentPhase measurements =
    let
        highProtein =
            highUrineProtein measurements
    in
    (highBloodPressure measurements && highProtein)
        || (repeatedTestForMarginalBloodPressure measurements
                && (edemaOnHandOrLegs measurements || highProtein)
           )


severePreeclampsiaByDangerSigns : List DangerSign -> Bool
severePreeclampsiaByDangerSigns =
    List.member HeadacheBlurredVision


severePreeclampsiaRecurrentPhase : List DangerSign -> PrenatalMeasurements -> Bool
severePreeclampsiaRecurrentPhase dangerSigns measurements =
    let
        byBloodPressure =
            getMeasurementValueFunc measurements.vitals
                |> Maybe.map
                    (\value ->
                        let
                            byInital =
                                Maybe.map2
                                    (\dia sys -> dia >= 110 && sys >= 160)
                                    value.dia
                                    value.sys
                                    |> Maybe.withDefault False

                            byRecurrent =
                                Maybe.map2
                                    (\dia sys -> dia >= 110 && sys >= 160)
                                    value.diaRepeated
                                    value.sysRepeated
                                    |> Maybe.withDefault False
                        in
                        byInital || byRecurrent
                    )
                |> Maybe.withDefault False
    in
    byBloodPressure
        && highUrineProtein measurements
        && severePreeclampsiaSigns measurements


highBloodPressure : PrenatalMeasurements -> Bool
highBloodPressure measurements =
    getMeasurementValueFunc measurements.vitals
        |> Maybe.andThen
            (\value ->
                Maybe.map2 highBloodPressureCondition
                    value.dia
                    value.sys
            )
        |> Maybe.withDefault False


repeatedHighBloodPressure : PrenatalMeasurements -> Bool
repeatedHighBloodPressure measurements =
    getMeasurementValueFunc measurements.vitals
        |> Maybe.andThen
            (\value ->
                Maybe.map2 highBloodPressureCondition
                    value.diaRepeated
                    value.sysRepeated
            )
        |> Maybe.withDefault False


highBloodPressureCondition : Float -> Float -> Bool
highBloodPressureCondition dia sys =
    dia >= 110 || sys >= 160


{-| We measure BP again when we suspect Hypertension or Preeclamsia
(dia between 90 and 110, and sys between 140 and 160).
We diagnose Hypertension if repeated measurements are within
those boundries, or exceed them.
-}
repeatedTestForMarginalBloodPressure : PrenatalMeasurements -> Bool
repeatedTestForMarginalBloodPressure measurements =
    getMeasurementValueFunc measurements.vitals
        |> Maybe.andThen
            (\value ->
                Maybe.map2 (\dia sys -> dia >= 90 || sys >= 140)
                    value.diaRepeated
                    value.sysRepeated
            )
        |> Maybe.withDefault False


edemaOnHandOrLegs : PrenatalMeasurements -> Bool
edemaOnHandOrLegs measurements =
    getMeasurementValueFunc measurements.corePhysicalExam
        |> Maybe.map
            (\value ->
                EverySet.member EdemaHands value.hands
                    || EverySet.member EdemaLegs value.legs
            )
        |> Maybe.withDefault False


severePreeclampsiaSigns : PrenatalMeasurements -> Bool
severePreeclampsiaSigns measurements =
    getMeasurementValueFunc measurements.corePhysicalExam
        |> Maybe.map
            (\value ->
                EverySet.member Wheezes value.lungs
                    || EverySet.member Crackles value.lungs
                    || EverySet.member TPRightUpper value.abdomen
            )
        |> Maybe.withDefault False


highUrineProtein : PrenatalMeasurements -> Bool
highUrineProtein measurements =
    getMeasurementValueFunc measurements.urineDipstickTest
        |> Maybe.andThen .protein
        |> Maybe.map
            (\protein ->
                List.member protein
                    [ ProteinPlus1
                    , ProteinPlus2
                    , ProteinPlus3
                    , ProteinPlus4
                    ]
            )
        |> Maybe.withDefault False


respiratoryRateElevated : PrenatalMeasurements -> Bool
respiratoryRateElevated measurements =
    getMeasurementValueFunc measurements.vitals
        |> Maybe.map (\value -> value.respiratoryRate > 30)
        |> Maybe.withDefault False


maternityWardDiagnoses : List PrenatalDiagnosis
maternityWardDiagnoses =
    [ DiagnosisImminentDelivery
    , DiagnosisLaborAndDelivery
    ]


immediateDeliveryDiagnoses : List PrenatalDiagnosis
immediateDeliveryDiagnoses =
    [ DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus
    , DiagnosisModeratePreeclampsiaRecurrentPhaseEGA37Plus
    , DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus
    , DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus
    ]


labResultsAndExaminationDiagnoses : List PrenatalDiagnosis
labResultsAndExaminationDiagnoses =
    [ DiagnosisChronicHypertensionImmediate
    , DiagnosisChronicHypertensionAfterRecheck
    , DiagnosisGestationalHypertensionImmediate
    , DiagnosisGestationalHypertensionAfterRecheck
    , DiagnosisModeratePreeclampsiaInitialPhase
    , DiagnosisModeratePreeclampsiaRecurrentPhase
    , DiagnosisSeverePreeclampsiaInitialPhase
    , DiagnosisSeverePreeclampsiaRecurrentPhase
    , DiagnosisHIV
    , DiagnosisHIVDetectableViralLoad
    , DiagnosisDiscordantPartnership
    , DiagnosisSyphilis
    , DiagnosisSyphilisWithComplications
    , DiagnosisNeurosyphilis
    , DiagnosisHepatitisB
    , DiagnosisMalaria
    , DiagnosisMalariaMedicatedContinued
    , DiagnosisMalariaWithAnemia
    , DiagnosisMalariaWithAnemiaMedicatedContinued
    , DiagnosisMalariaWithSevereAnemia
    , DiagnosisModerateAnemia
    , DiagnosisSevereAnemia
    , DiagnosisSevereAnemiaWithComplications
    , Backend.PrenatalEncounter.Types.DiagnosisDiabetes
    , Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetes
    , DiagnosisRhesusNegative
    , DiagnosisPostpartumEarlyMastitisOrEngorgment
    , DiagnosisPostpartumMastitis
    , DiagnosisPostpartumInfection
    , DiagnosisPostpartumExcessiveBleeding
    ]


symptomsDiagnoses : List PrenatalDiagnosis
symptomsDiagnoses =
    [ DiagnosisHyperemesisGravidumBySymptoms
    , DiagnosisSevereVomitingBySymptoms
    , DiagnosisHeartburn
    , DiagnosisHeartburnPersistent
    , DiagnosisDeepVeinThrombosis
    , DiagnosisPelvicPainIntense
    , DiagnosisPelvicPainContinued
    , DiagnosisUrinaryTractInfection
    , DiagnosisUrinaryTractInfectionContinued
    , DiagnosisPyelonephritis
    , DiagnosisCandidiasis
    , DiagnosisCandidiasisContinued
    , DiagnosisGonorrhea
    , DiagnosisGonorrheaContinued
    , DiagnosisTrichomonasOrBacterialVaginosis
    , DiagnosisTrichomonasOrBacterialVaginosisContinued
    , Backend.PrenatalEncounter.Types.DiagnosisTuberculosis
    , DiagnosisPostpartumAbdominalPain
    , DiagnosisPostpartumUrinaryIncontinence
    , DiagnosisPostpartumHeadache
    , DiagnosisPostpartumFatigue
    , DiagnosisPostpartumFever
    , DiagnosisPostpartumPerinealPainOrDischarge
    ]


mentalHealthDiagnoses : List PrenatalDiagnosis
mentalHealthDiagnoses =
    DiagnosisDepressionNotLikely :: mentalHealthDiagnosesRequiringTreatment


healthEducationFormInputsAndTasks : Language -> AssembledData -> HealthEducationForm -> ( List (Html Msg), List (Maybe Bool) )
healthEducationFormInputsAndTasks language assembled healthEducationForm =
    let
        form =
            assembled.measurements.healthEducation
                |> getMeasurementValueFunc
                |> healthEducationFormWithDefault healthEducationForm
    in
    case assembled.encounter.encounterType of
        NurseEncounter ->
            healthEducationFormInputsAndTasksForNurse language assembled form

        NursePostpartumEncounter ->
            healthEducationFormInputsAndTasksForNurse language assembled form

        _ ->
            healthEducationFormInputsAndTasksForChw language assembled form


healthEducationFormInputsAndTasksForNurse : Language -> AssembledData -> HealthEducationForm -> ( List (Html Msg), List (Maybe Bool) )
healthEducationFormInputsAndTasksForNurse language assembled form =
    let
        ( hivInputs, hivTasks ) =
            if isJust assembled.measurements.hivTest then
                healthEducationFormInputsAndTasksForHIV language assembled form

            else
                ( [], [] )

        nauseaVomiting =
            if provideNauseaAndVomitingEducation assembled then
                ( [ viewCustomLabel language (Translate.PrenatalHealthEducationLabel EducationNauseaVomiting) "" "label header"
                  , viewCustomLabel language Translate.PrenatalHealthEducationNauseaAndVomitingInform "." "label paragraph"
                  , viewCustomLabel language Translate.PrenatalHealthEducationNauseaAndVomitingAdvise "." "label paragraph"
                  , viewQuestionLabel language Translate.PrenatalHealthEducationAppropriateProvided
                  , viewBoolInput
                        language
                        form.nauseaVomiting
                        (SetHealthEducationSubActivityBoolInput (\value form_ -> { form_ | nauseaVomiting = Just value }))
                        "nausea-vomiting"
                        Nothing
                  ]
                , Just form.nauseaVomiting
                )

            else
                ( [], Nothing )

        legCramps =
            if symptomRecorded assembled.measurements LegCramps then
                let
                    reliefMethods =
                        List.map
                            (Translate.LegCrampsReliefMethod
                                >> translate language
                                >> String.toLower
                                >> text
                                >> List.singleton
                                >> li []
                            )
                            [ ReliefMethodMuscleStretching
                            , ReliefMethodDorsiflexion
                            , ReliefMethodRelaxation
                            , ReliefMethodSleepWithPillowBetweenLegs
                            , ReliefMethodHeatTherapy
                            , ReliefMethodMassage
                            ]
                            |> ul []
                in
                ( [ viewCustomLabel language (Translate.PrenatalHealthEducationLabel EducationLegCramps) "" "label header"
                  , viewLabel language Translate.PrenatalHealthEducationLegCrampsInform
                  , reliefMethods
                  , viewQuestionLabel language Translate.PrenatalHealthEducationAppropriateProvided
                  , viewBoolInput
                        language
                        form.legCramps
                        (SetHealthEducationSubActivityBoolInput (\value form_ -> { form_ | legCramps = Just value }))
                        "leg-cramps"
                        Nothing
                  ]
                , Just form.legCramps
                )

            else
                ( [], Nothing )

        lowBackPain =
            if symptomRecorded assembled.measurements LowBackPain then
                ( [ viewCustomLabel language (Translate.PrenatalHealthEducationLabel EducationLowBackPain) "" "label header"
                  , viewCustomLabel language Translate.PrenatalHealthEducationLowBackPainInform "." "label paragraph"
                  , viewQuestionLabel language Translate.PrenatalHealthEducationAppropriateProvided
                  , viewBoolInput
                        language
                        form.lowBackPain
                        (SetHealthEducationSubActivityBoolInput (\value form_ -> { form_ | lowBackPain = Just value }))
                        "low-back-pain"
                        Nothing
                  ]
                , Just form.lowBackPain
                )

            else
                ( [], Nothing )

        constipation =
            if symptomRecorded assembled.measurements Constipation then
                ( [ viewCustomLabel language (Translate.PrenatalHealthEducationLabel EducationConstipation) "" "label header"
                  , viewCustomLabel language Translate.PrenatalHealthEducationConstipationInform "." "label paragraph"
                  , viewQuestionLabel language Translate.PrenatalHealthEducationAppropriateProvided
                  , viewBoolInput
                        language
                        form.constipation
                        (SetHealthEducationSubActivityBoolInput (\value form_ -> { form_ | constipation = Just value }))
                        "constipation"
                        Nothing
                  ]
                , Just form.constipation
                )

            else
                ( [], Nothing )

        heartburn =
            let
                reliefMethods =
                    List.map
                        (Translate.HeartburnReliefMethod
                            >> translate language
                            >> String.toLower
                            >> text
                            >> List.singleton
                            >> li []
                        )
                        [ ReliefMethodAvoidLargeMeals
                        , ReliefMethodCeaseSmoking
                        , ReliefMethodAvoidAlcohom
                        , ReliefMethodSleepWithHeadRaised
                        ]
                        |> ul []
            in
            if diagnosed DiagnosisHeartburn assembled then
                ( [ viewCustomLabel language (Translate.PrenatalHealthEducationLabel EducationHeartburn) "" "label header"
                  , viewLabel language Translate.PrenatalHealthEducationHeartburnInform
                  , reliefMethods
                  , viewQuestionLabel language Translate.PrenatalHealthEducationAppropriateProvided
                  , viewBoolInput
                        language
                        form.heartburn
                        (SetHealthEducationSubActivityBoolInput (\value form_ -> { form_ | heartburn = Just value }))
                        "heartburn"
                        Nothing
                  ]
                , Just form.heartburn
                )

            else
                ( [], Nothing )

        varicoseVeins =
            if symptomRecorded assembled.measurements VaricoseVeins then
                ( [ viewCustomLabel language (Translate.PrenatalHealthEducationLabel EducationVaricoseVeins) "" "label header"
                  , viewCustomLabel language Translate.PrenatalHealthEducationVaricoseVeinsInform "." "label paragraph"
                  , viewQuestionLabel language Translate.PrenatalHealthEducationAppropriateProvided
                  , viewBoolInput
                        language
                        form.varicoseVeins
                        (SetHealthEducationSubActivityBoolInput (\value form_ -> { form_ | varicoseVeins = Just value }))
                        "varicose-veins"
                        Nothing
                  ]
                , Just form.varicoseVeins
                )

            else
                ( [], Nothing )

        legPainRedness =
            if provideLegPainRednessEducation assembled then
                ( [ viewCustomLabel language (Translate.PrenatalHealthEducationLabel EducationLegPainRedness) "" "label header"
                  , viewCustomLabel language Translate.PrenatalHealthEducationLegPainRednessInform "." "label paragraph"
                  , viewQuestionLabel language Translate.PrenatalHealthEducationAppropriateProvided
                  , viewBoolInput
                        language
                        form.legPainRedness
                        (SetHealthEducationSubActivityBoolInput (\value form_ -> { form_ | legPainRedness = Just value }))
                        "leg-pain-redness"
                        Nothing
                  ]
                , Just form.legPainRedness
                )

            else
                ( [], Nothing )

        pelvicPain =
            if providePelvicPainEducation assembled then
                ( [ viewCustomLabel language (Translate.PrenatalHealthEducationLabel EducationPelvicPain) "" "label header"
                  , viewCustomLabel language Translate.PrenatalHealthEducationPelvicPainInform "." "label paragraph"
                  , viewQuestionLabel language Translate.PrenatalHealthEducationAppropriateProvided
                  , viewBoolInput
                        language
                        form.pelvicPain
                        (SetHealthEducationSubActivityBoolInput (\value form_ -> { form_ | pelvicPain = Just value }))
                        "pelvic-pain"
                        Nothing
                  ]
                , Just form.pelvicPain
                )

            else
                ( [], Nothing )

        saferSex =
            let
                saferSexDiagnoses =
                    List.filter
                        (\diagnosis ->
                            EverySet.member diagnosis assembled.encounter.diagnoses
                        )
                        -- Diagnoses that require safer sex practices education.
                        [ DiagnosisCandidiasis, DiagnosisGonorrhea, DiagnosisTrichomonasOrBacterialVaginosis ]
            in
            if not <| List.isEmpty saferSexDiagnoses then
                let
                    label =
                        List.filter (symptomRecorded assembled.measurements)
                            -- Symptoms that may  require safer sex practices education.
                            [ BurningWithUrination, AbnormalVaginalDischarge ]
                            |> List.map (Translate.PrenatalSymptom >> translate language)
                            |> String.join ", "
                in
                ( [ div [ class "label header" ] [ text label ]
                  , viewCustomLabel language Translate.PrenatalHealthEducationSaferSexInform "." "label paragraph"
                  , viewQuestionLabel language Translate.PrenatalHealthEducationAppropriateProvided
                  , viewBoolInput
                        language
                        form.saferSex
                        (SetHealthEducationSubActivityBoolInput (\value form_ -> { form_ | saferSex = Just value }))
                        "safer-sex"
                        Nothing
                  ]
                , Just form.saferSex
                )

            else
                ( [], Nothing )

        mentalHealth =
            if provideMentalHealthEducation assembled then
                ( [ viewCustomLabel language (Translate.PrenatalHealthEducationLabel EducationMentalHealth) "" "label header"
                  , viewCustomLabel language Translate.PrenatalHealthEducationMentalHealthInform "." "label paragraph"
                  , viewQuestionLabel language Translate.PrenatalHealthEducationAppropriateProvided
                  , viewBoolInput
                        language
                        form.mentalHealth
                        (SetHealthEducationSubActivityBoolInput (\value form_ -> { form_ | mentalHealth = Just value }))
                        "mental-health"
                        Nothing
                  ]
                , Just form.mentalHealth
                )

            else
                ( [], Nothing )

        hierarchalMastitis =
            if diagnosedAnyOf [ DiagnosisPostpartumEarlyMastitisOrEngorgment, DiagnosisPostpartumMastitis ] assembled then
                let
                    reliefMethods =
                        List.map
                            (Translate.EarlyMastitisOrEngorgmentReliefMethod
                                >> translate language
                                >> String.toLower
                                >> text
                                >> List.singleton
                                >> li []
                            )
                            [ ReliefMethodBreastMassage
                            , ReliefMethodIncreaseFluid
                            , ReliefMethodBreastfeedingOrHandExpression
                            ]
                            |> ul []

                    ( eudcationSign, formField, updateFunc ) =
                        if diagnosed DiagnosisPostpartumMastitis assembled then
                            ( EducationMastitis
                            , form.mastitis
                            , \value form_ -> { form_ | mastitis = Just value }
                            )

                        else
                            ( EducationEarlyMastitisOrEngorgment
                            , form.earlyMastitisOrEngorgment
                            , \value form_ -> { form_ | earlyMastitisOrEngorgment = Just value }
                            )
                in
                ( [ viewCustomLabel language (Translate.PrenatalHealthEducationLabel eudcationSign) "" "label header"
                  , viewCustomLabel language Translate.PrenatalHealthEducationEarlyMastitisOrEngorgmentInform ":" "label paragraph"
                  , reliefMethods
                  , viewQuestionLabel language Translate.PrenatalHealthEducationAppropriateProvided
                  , viewBoolInput
                        language
                        formField
                        (SetHealthEducationSubActivityBoolInput updateFunc)
                        "mastitis"
                        Nothing
                  ]
                , Just formField
                )

            else
                ( [], Nothing )

        inputsAndTasks =
            [ nauseaVomiting
            , legCramps
            , lowBackPain
            , constipation
            , heartburn
            , varicoseVeins
            , legPainRedness
            , pelvicPain
            , saferSex
            , mentalHealth
            , hierarchalMastitis
            ]
    in
    ( hivInputs
        ++ (List.map Tuple.first inputsAndTasks
                |> List.concat
           )
    , hivTasks
        ++ (List.map Tuple.second inputsAndTasks
                |> Maybe.Extra.values
           )
    )


healthEducationFormInputsAndTasksForHIV : Language -> AssembledData -> HealthEducationForm -> ( List (Html Msg), List (Maybe Bool) )
healthEducationFormInputsAndTasksForHIV language assembled form =
    let
        translatePrenatalHealthEducationQuestion =
            Translate.PrenatalHealthEducationQuestion False

        positiveHIVUpdateFunc value form_ =
            { form_ | positiveHIV = Just value }

        positiveHIVInput =
            [ viewQuestionLabel language <| translatePrenatalHealthEducationQuestion EducationPositiveHIV
            , viewBoolInput
                language
                form.positiveHIV
                (SetHealthEducationSubActivityBoolInput positiveHIVUpdateFunc)
                "positive-hiv"
                Nothing
            ]

        saferSexHIVUpdateFunc value form_ =
            { form_ | saferSexHIV = Just value }

        saferSexHIVInput =
            [ viewQuestionLabel language <| translatePrenatalHealthEducationQuestion EducationSaferSexHIV
            , viewBoolInput
                language
                form.saferSexHIV
                (SetHealthEducationSubActivityBoolInput saferSexHIVUpdateFunc)
                "safer-sex-hiv"
                Nothing
            ]

        partnerTestingUpdateFunc value form_ =
            { form_ | partnerTesting = Just value }

        partnerTestingInput =
            [ viewQuestionLabel language <| translatePrenatalHealthEducationQuestion EducationPartnerTesting
            , viewBoolInput
                language
                form.partnerTesting
                (SetHealthEducationSubActivityBoolInput partnerTestingUpdateFunc)
                "partner-testing"
                Nothing
            ]

        familyPlanningInput =
            healthEducationFormFamilyPlanningInput language False form

        partnerSurpressedViralLoad =
            getMeasurementValueFunc assembled.measurements.hivTest
                |> Maybe.andThen .hivSigns
                |> Maybe.map
                    (\hivSigns ->
                        -- Partner is HIV positive.
                        EverySet.member PartnerHIVPositive hivSigns
                            -- Partner is taking ARVs.
                            && EverySet.member PartnerTakingARV hivSigns
                            -- Partner reached surpressed viral load.
                            && EverySet.member PartnerSurpressedViralLoad hivSigns
                    )
                |> Maybe.withDefault False

        header =
            viewCustomLabel language Translate.HIV "" "label header"
    in
    if diagnosedAnyOf [ DiagnosisHIV, DiagnosisDiscordantPartnership ] assembled then
        ( header :: positiveHIVInput ++ saferSexHIVInput ++ partnerTestingInput ++ familyPlanningInput
        , [ form.positiveHIV, form.saferSexHIV, form.partnerTesting, form.familyPlanning ]
        )

    else if partnerSurpressedViralLoad then
        ( header :: saferSexHIVInput
        , [ form.saferSexHIV ]
        )

    else
        ( header :: saferSexHIVInput ++ partnerTestingInput
        , [ form.saferSexHIV, form.partnerTesting ]
        )


healthEducationFormFamilyPlanningInput : Language -> Bool -> HealthEducationForm -> List (Html Msg)
healthEducationFormFamilyPlanningInput language isChw form =
    let
        familyPlanningUpdateFunc value form_ =
            { form_ | familyPlanning = Just value }
    in
    [ viewQuestionLabel language <| Translate.PrenatalHealthEducationQuestion isChw EducationFamilyPlanning
    , viewBoolInput
        language
        form.familyPlanning
        (SetHealthEducationSubActivityBoolInput familyPlanningUpdateFunc)
        "family-planning"
        Nothing
    ]


healthEducationFormInputsAndTasksForChw : Language -> AssembledData -> HealthEducationForm -> ( List (Html Msg), List (Maybe Bool) )
healthEducationFormInputsAndTasksForChw language assembled form =
    let
        healthEducationCompletedAtEncounter encounterType =
            assembled.chwPreviousMeasurementsWithDates
                |> List.filterMap
                    (\( _, encounterType_, measurements ) ->
                        if encounterType == encounterType_ then
                            Just measurements

                        else
                            Nothing
                    )
                -- There's a posibility to have more than one
                -- 'Third' encounter, therefore, the check
                -- for ANY in list.
                |> List.any (.healthEducation >> isJust)

        firstEnconterInputs =
            [ expectationsInput, visitsReviewInput, warningSignsInput ]

        firstEnconterTasks =
            [ form.expectations, form.visitsReview, form.warningSigns ]

        secondEnconterInputs =
            [ hemorrhagingInput ]

        secondEnconterTasks =
            [ form.hemorrhaging ]

        thirdEnconterInputs =
            [ hemorrhagingInput, familyPlanningInput, breastfeedingInput ]

        thirdEnconterTasks =
            [ form.hemorrhaging, form.familyPlanning, form.breastfeeding ]

        postpartumEnconterInputs =
            [ breastfeedingInput, immunizationInput, hygieneInput ]

        postpartumEnconterTasks =
            [ form.breastfeeding, form.immunization, form.hygiene ]

        expectationsUpdateFunc value form_ =
            { form_ | expectations = Just value }

        setBoolInputMsg =
            case assembled.encounter.encounterType of
                ChwPostpartumEncounter ->
                    SetHealthEducationSubActivityBoolInput

                _ ->
                    SetHealthEducationBoolInput

        translatePrenatalHealthEducationQuestion =
            Translate.PrenatalHealthEducationQuestion True

        expectationsInput =
            [ viewQuestionLabel language <| translatePrenatalHealthEducationQuestion EducationExpectations
            , viewBoolInput
                language
                form.expectations
                (setBoolInputMsg expectationsUpdateFunc)
                "expectations"
                Nothing
            ]

        visitsReviewUpdateFunc value form_ =
            { form_ | visitsReview = Just value }

        visitsReviewInput =
            [ viewQuestionLabel language <| translatePrenatalHealthEducationQuestion EducationVisitsReview
            , viewBoolInput
                language
                form.visitsReview
                (setBoolInputMsg visitsReviewUpdateFunc)
                "visits-review"
                Nothing
            ]

        warningSignsUpdateFunc value form_ =
            { form_ | warningSigns = Just value }

        warningSignsInput =
            [ viewQuestionLabel language <| translatePrenatalHealthEducationQuestion EducationWarningSigns
            , viewBoolInput
                language
                form.warningSigns
                (setBoolInputMsg warningSignsUpdateFunc)
                "warning-signs"
                Nothing
            ]

        hemorrhagingUpdateFunc value form_ =
            { form_ | hemorrhaging = Just value }

        hemorrhagingInput =
            [ viewQuestionLabel language <| translatePrenatalHealthEducationQuestion EducationHemorrhaging
            , viewBoolInput
                language
                form.hemorrhaging
                (setBoolInputMsg hemorrhagingUpdateFunc)
                "hemorrhaging"
                Nothing
            ]

        familyPlanningInput =
            healthEducationFormFamilyPlanningInput language True form

        breastfeedingUpdateFunc value form_ =
            { form_ | breastfeeding = Just value }

        breastfeedingInput =
            [ viewQuestionLabel language <| translatePrenatalHealthEducationQuestion EducationBreastfeeding
            , viewBoolInput
                language
                form.breastfeeding
                (setBoolInputMsg breastfeedingUpdateFunc)
                "breastfeeding"
                Nothing
            ]

        immunizationUpdateFunc value form_ =
            { form_ | immunization = Just value }

        immunizationInput =
            [ viewQuestionLabel language <| translatePrenatalHealthEducationQuestion EducationImmunization
            , viewBoolInput
                language
                form.immunization
                (setBoolInputMsg immunizationUpdateFunc)
                "immunization"
                Nothing
            ]

        hygieneUpdateFunc value form_ =
            { form_ | hygiene = Just value }

        hygieneInput =
            [ viewQuestionLabel language <| translatePrenatalHealthEducationQuestion EducationHygiene
            , viewBoolInput
                language
                form.hygiene
                (setBoolInputMsg hygieneUpdateFunc)
                "hygiene"
                Nothing
            ]

        ( inputsFromFirst, tasksFromFirst ) =
            if healthEducationCompletedAtFirst || healthEducationCompletedAtSecond || healthEducationCompletedAtThird then
                ( [], [] )

            else
                ( firstEnconterInputs, firstEnconterTasks )

        healthEducationCompletedAtFirst =
            healthEducationCompletedAtEncounter ChwFirstEncounter

        healthEducationCompletedAtSecond =
            healthEducationCompletedAtEncounter ChwSecondEncounter

        healthEducationCompletedAtThird =
            healthEducationCompletedAtEncounter ChwThirdPlusEncounter
    in
    -- For all encounter types but postpartum, if Health
    -- education was not completed at previous encounter,
    -- its inputs are added to next encounter.
    case assembled.encounter.encounterType of
        ChwFirstEncounter ->
            ( List.concat firstEnconterInputs
            , firstEnconterTasks
            )

        ChwSecondEncounter ->
            ( List.concat <| inputsFromFirst ++ secondEnconterInputs
            , tasksFromFirst ++ secondEnconterTasks
            )

        ChwThirdPlusEncounter ->
            -- Second encounter tasks reappear at third encounter anyway,
            -- so, we do not need to add them explicitly.
            ( List.concat <| inputsFromFirst ++ thirdEnconterInputs
            , tasksFromFirst ++ thirdEnconterTasks
            )

        ChwPostpartumEncounter ->
            ( List.concat postpartumEnconterInputs
            , postpartumEnconterTasks
            )

        -- We should never get here, as function is only for CHW.
        NurseEncounter ->
            ( [], [] )

        -- We should never get here, as function is only for CHW.
        NursePostpartumEncounter ->
            ( [], [] )


nextStepsTasksCompletedFromTotal :
    Language
    -> NominalDate
    -> Bool
    -> AssembledData
    -> NextStepsData
    -> NextStepsTask
    -> ( Int, Int )
nextStepsTasksCompletedFromTotal language currentDate isChw assembled data task =
    case task of
        NextStepsAppointmentConfirmation ->
            let
                form =
                    assembled.measurements.appointmentConfirmation
                        |> getMeasurementValueFunc
                        |> appointmentConfirmationFormWithDefault data.appointmentConfirmationForm
            in
            ( taskCompleted form.appointmentDate
            , 1
            )

        NextStepsFollowUp ->
            let
                form =
                    assembled.measurements.followUp
                        |> getMeasurementValueFunc
                        |> followUpFormWithDefault data.followUpForm
            in
            ( taskCompleted form.option
            , 1
            )

        NextStepsSendToHC ->
            let
                form =
                    assembled.measurements.sendToHC
                        |> getMeasurementValueFunc
                        |> referralFormWithDefault data.referralForm

                ( _, tasks ) =
                    case assembled.encounter.encounterType of
                        NurseEncounter ->
                            tasksForNurse

                        NursePostpartumEncounter ->
                            tasksForNurse

                        _ ->
                            resolveReferralInputsAndTasksForCHW language currentDate assembled form

                tasksForNurse =
                    resolveReferralInputsAndTasksForNurse language
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

        NextStepsHealthEducation ->
            let
                form =
                    getMeasurementValueFunc assembled.measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm

                ( _, tasks ) =
                    healthEducationFormInputsAndTasks language assembled form
            in
            ( Maybe.Extra.values tasks
                |> List.length
            , List.length tasks
            )

        NextStepsNewbornEnrolment ->
            ( taskCompleted assembled.participant.newborn
            , 1
            )

        NextStepsMedicationDistribution ->
            let
                form =
                    getMeasurementValueFunc assembled.measurements.medicationDistribution
                        |> medicationDistributionFormWithDefaultInitialPhase data.medicationDistributionForm

                ( _, completed, total ) =
                    resolveMedicationDistributionInputsAndTasks language
                        currentDate
                        PrenatalEncounterPhaseInitial
                        assembled
                        SetMedicationDistributionBoolInput
                        SetMedicationDistributionAdministrationNote
                        SetRecommendedTreatmentSign
                        SetAvoidingGuidanceReason
                        form
            in
            ( completed, total )

        NextStepsWait ->
            let
                completed =
                    if nextStepsTaskCompleted currentDate assembled NextStepsWait then
                        1

                    else
                        0
            in
            ( completed
            , 1
            )


resolvePreviousValue : AssembledData -> (PrenatalMeasurements -> Maybe ( id, PrenatalMeasurement a )) -> (a -> b) -> Maybe b
resolvePreviousValue assembled measurementFunc valueFunc =
    assembled.nursePreviousEncountersData
        |> List.filterMap (.measurements >> measurementFunc >> Maybe.map (Tuple.second >> .value >> valueFunc))
        |> List.reverse
        |> List.head


resolvePreviousMaybeValue : AssembledData -> (PrenatalMeasurements -> Maybe ( id, PrenatalMeasurement a )) -> (a -> Maybe b) -> Maybe b
resolvePreviousMaybeValue assembled measurementFunc valueFunc =
    assembled.nursePreviousEncountersData
        |> List.filterMap (.measurements >> measurementFunc >> Maybe.andThen (Tuple.second >> .value >> valueFunc))
        |> List.reverse
        |> List.head


breastExamFormWithDefault : BreastExamForm -> Maybe BreastExamValue -> BreastExamForm
breastExamFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { breast = or form.breast (value.exam |> EverySet.toList |> Just)
                , dischargeType =
                    maybeValueConsideringIsDirtyField form.dischargeTypeDirty
                        form.dischargeType
                        value.dischargeType
                , dischargeTypeDirty = form.dischargeTypeDirty
                , selfGuidance = or form.selfGuidance (Just value.selfGuidance)
                }
            )


toBreastExamValueWithDefault : Maybe BreastExamValue -> BreastExamForm -> Maybe BreastExamValue
toBreastExamValueWithDefault saved form =
    breastExamFormWithDefault form saved
        |> toBreastExamValue


toBreastExamValue : BreastExamForm -> Maybe BreastExamValue
toBreastExamValue form =
    Maybe.map BreastExamValue (Maybe.map EverySet.fromList form.breast)
        |> andMap (Just form.dischargeType)
        |> andMap form.selfGuidance


fromDangerSignsValue : Maybe DangerSignsValue -> DangerSignsForm
fromDangerSignsValue saved =
    { signs = Maybe.map (.signs >> EverySet.toList) saved
    , postpartumMother = Maybe.map (.postpartumMother >> EverySet.toList) saved
    , postpartumChild = Maybe.map (.postpartumChild >> EverySet.toList) saved
    }


dangerSignsFormWithDefault : DangerSignsForm -> Maybe DangerSignsValue -> DangerSignsForm
dangerSignsFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value.signs |> Just)
                , postpartumMother = or form.postpartumMother (EverySet.toList value.postpartumMother |> Just)
                , postpartumChild = or form.postpartumChild (EverySet.toList value.postpartumChild |> Just)
                }
            )


toDangerSignsValueWithDefault : Maybe DangerSignsValue -> DangerSignsForm -> Maybe DangerSignsValue
toDangerSignsValueWithDefault saved form =
    dangerSignsFormWithDefault form saved
        |> toDangerSignsValue


toDangerSignsValue : DangerSignsForm -> Maybe DangerSignsValue
toDangerSignsValue form =
    let
        signs =
            form.signs
                |> Maybe.withDefault [ NoDangerSign ]
                |> EverySet.fromList

        postpartumMother =
            form.postpartumMother
                |> Maybe.withDefault [ NoPostpartumMotherDangerSigns ]
                |> EverySet.fromList

        postpartumChild =
            form.postpartumChild
                |> Maybe.withDefault [ NoPostpartumChildDangerSigns ]
                |> EverySet.fromList
    in
    Just <| DangerSignsValue signs postpartumMother postpartumChild


lastMenstrualPeriodFormWithDefault : PregnancyDatingForm -> Maybe LastMenstrualPeriodValue -> PregnancyDatingForm
lastMenstrualPeriodFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { lmpRange = or form.lmpRange (Just SixMonth)
                , lmpDate = or form.lmpDate (Just value.date)
                , lmpDateConfident = or form.lmpDateConfident (Just value.confident)
                , lmpDateNotConfidentReason = or form.lmpDateNotConfidentReason value.notConfidentReason
                , chwLmpConfirmation = or form.chwLmpConfirmation (Just value.confirmation)
                , dateSelectorPopupState = form.dateSelectorPopupState
                }
            )


toLastMenstrualPeriodValueWithDefault : Maybe LastMenstrualPeriodValue -> PregnancyDatingForm -> Maybe LastMenstrualPeriodValue
toLastMenstrualPeriodValueWithDefault saved form =
    lastMenstrualPeriodFormWithDefault form saved
        |> toLastMenstrualPeriodValue


toLastMenstrualPeriodValue : PregnancyDatingForm -> Maybe LastMenstrualPeriodValue
toLastMenstrualPeriodValue form =
    let
        chwLmpConfirmation =
            Maybe.withDefault False form.chwLmpConfirmation
    in
    Maybe.map LastMenstrualPeriodValue form.lmpDate
        |> andMap form.lmpDateConfident
        |> andMap (Just form.lmpDateNotConfidentReason)
        |> andMap (Just chwLmpConfirmation)


fromMedicalHistoryValue : Maybe (EverySet MedicalHistorySign) -> MedicalHistoryForm
fromMedicalHistoryValue saved =
    { uterineMyoma = Maybe.map (EverySet.member UterineMyoma) saved
    , diabetes = Maybe.map (EverySet.member Diabetes) saved
    , cardiacDisease = Maybe.map (EverySet.member CardiacDisease) saved
    , renalDisease = Maybe.map (EverySet.member RenalDisease) saved
    , hypertensionBeforePregnancy = Maybe.map (EverySet.member HypertensionBeforePregnancy) saved
    , tuberculosisPast = Maybe.map (EverySet.member TuberculosisPast) saved
    , tuberculosisPresent = Maybe.map (EverySet.member TuberculosisPresent) saved
    , asthma = Maybe.map (EverySet.member Asthma) saved
    , bowedLegs = Maybe.map (EverySet.member BowedLegs) saved
    , hiv = Maybe.map (EverySet.member HIV) saved
    , mentalHealthHistory = Maybe.map (EverySet.member MentalHealthHistory) saved
    }


medicalHistoryFormWithDefault : MedicalHistoryForm -> Maybe (EverySet MedicalHistorySign) -> MedicalHistoryForm
medicalHistoryFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { uterineMyoma = or form.uterineMyoma (EverySet.member UterineMyoma value |> Just)
                , diabetes = or form.diabetes (EverySet.member Diabetes value |> Just)
                , cardiacDisease = or form.cardiacDisease (EverySet.member CardiacDisease value |> Just)
                , renalDisease = or form.renalDisease (EverySet.member RenalDisease value |> Just)
                , hypertensionBeforePregnancy = or form.hypertensionBeforePregnancy (EverySet.member HypertensionBeforePregnancy value |> Just)
                , tuberculosisPast = or form.tuberculosisPast (EverySet.member TuberculosisPast value |> Just)
                , tuberculosisPresent = or form.tuberculosisPresent (EverySet.member TuberculosisPresent value |> Just)
                , asthma = or form.asthma (EverySet.member Asthma value |> Just)
                , bowedLegs = or form.bowedLegs (EverySet.member BowedLegs value |> Just)
                , hiv = or form.hiv (EverySet.member HIV value |> Just)
                , mentalHealthHistory = or form.mentalHealthHistory (EverySet.member MentalHealthHistory value |> Just)
                }
            )


toMedicalHistoryValueWithDefault : Maybe (EverySet MedicalHistorySign) -> MedicalHistoryForm -> Maybe (EverySet MedicalHistorySign)
toMedicalHistoryValueWithDefault saved form =
    medicalHistoryFormWithDefault form saved
        |> toMedicalHistoryValue


toMedicalHistoryValue : MedicalHistoryForm -> Maybe (EverySet MedicalHistorySign)
toMedicalHistoryValue form =
    [ Maybe.map (ifTrue UterineMyoma) form.uterineMyoma
    , Maybe.map (ifTrue Diabetes) form.diabetes
    , Maybe.map (ifTrue CardiacDisease) form.cardiacDisease
    , Maybe.map (ifTrue HypertensionBeforePregnancy) form.hypertensionBeforePregnancy
    , Maybe.map (ifTrue TuberculosisPast) form.tuberculosisPast
    , Maybe.map (ifTrue TuberculosisPresent) form.tuberculosisPresent
    , Maybe.map (ifTrue Asthma) form.asthma
    , Maybe.map (ifTrue BowedLegs) form.bowedLegs
    , Maybe.map (ifTrue HIV) form.hiv
    , Maybe.map (ifTrue MentalHealthHistory) form.mentalHealthHistory
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoMedicalHistorySigns)


medicationFormWithDefault : MedicationForm -> Maybe MedicationValue -> MedicationForm
medicationFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    hivMedicationNotGivenReason =
                        Maybe.andThen
                            (EverySet.toList
                                >> List.filter (\sign -> List.member sign reasonsForNoMedicationByPMTCT)
                                >> List.head
                            )
                            value.hivTreatment

                    treatmentSignFromValue sign treatment =
                        Maybe.map (EverySet.member sign) treatment

                    treatmentStillTakingFromValue =
                        treatmentSignFromValue MedicationTreatmentStillTaking

                    treatmentMissedDosesFromValue =
                        treatmentSignFromValue MedicationTreatmentMissedDoses

                    treatmentAdverseEventsFromValue =
                        treatmentSignFromValue MedicationTreatmentAdverseEvents

                    treatmentAdverseEventsHospitalizationFromValue =
                        treatmentSignFromValue MedicationTreatmentAdverseEventsHospitalization
                in
                { receivedIronFolicAcid = or form.receivedIronFolicAcid (Maybe.map (EverySet.member IronAndFolicAcidSupplement) value.signs)
                , receivedDewormingPill = or form.receivedDewormingPill (Maybe.map (EverySet.member DewormingPill) value.signs)
                , receivedMebendazole = or form.receivedMebendazole (Maybe.map (EverySet.member Mebendazole) value.signs)
                , receivedFolicAcid = or form.receivedFolicAcid (Maybe.map (EverySet.member PostpartumFolicAcid) value.signs)
                , receivedVitaminA = or form.receivedVitaminA (Maybe.map (EverySet.member PostpartumVitaminA) value.signs)
                , hivMedicationByPMTCT = or form.hivMedicationByPMTCT (Maybe.map (EverySet.member HIVTreatmentMedicineByPMTCT) value.hivTreatment)
                , hivMedicationNotGivenReason =
                    maybeValueConsideringIsDirtyField form.hivMedicationNotGivenReasonDirty
                        form.hivMedicationNotGivenReason
                        hivMedicationNotGivenReason
                , hivMedicationNotGivenReasonDirty = form.hivMedicationNotGivenReasonDirty
                , hivStillTaking = or form.hivStillTaking (treatmentSignFromValue HIVTreatmentStillTaking value.hivTreatment)
                , hivMissedDoses = or form.hivMissedDoses (treatmentSignFromValue HIVTreatmentMissedDoses value.hivTreatment)
                , hivAdverseEvents = or form.hivAdverseEvents (treatmentSignFromValue HIVTreatmentAdverseEvents value.hivTreatment)
                , hivAdverseEventsHospitalization =
                    maybeValueConsideringIsDirtyField form.hivAdverseEventsHospitalizationDirty
                        form.hivAdverseEventsHospitalization
                        (treatmentSignFromValue HIVTreatmentAdverseEventsHospitalization value.hivTreatment)
                , hivAdverseEventsHospitalizationDirty = form.hivAdverseEventsHospitalizationDirty
                , hypertensionStillTaking = or form.hypertensionStillTaking (treatmentStillTakingFromValue value.hypertensionTreatment)
                , hypertensionMissedDoses = or form.hypertensionMissedDoses (treatmentMissedDosesFromValue value.hypertensionTreatment)
                , hypertensionAdverseEvents = or form.hypertensionAdverseEvents (treatmentAdverseEventsFromValue value.hypertensionTreatment)
                , hypertensionAdverseEventsHospitalization =
                    maybeValueConsideringIsDirtyField form.hypertensionAdverseEventsHospitalizationDirty
                        form.hypertensionAdverseEventsHospitalization
                        (treatmentAdverseEventsHospitalizationFromValue value.hypertensionTreatment)
                , hypertensionAdverseEventsHospitalizationDirty = form.hypertensionAdverseEventsHospitalizationDirty
                , malariaStillTaking = or form.malariaStillTaking (treatmentStillTakingFromValue value.malariaTreatment)
                , malariaMissedDoses = or form.malariaMissedDoses (treatmentMissedDosesFromValue value.malariaTreatment)
                , malariaAdverseEvents = or form.malariaAdverseEvents (treatmentAdverseEventsFromValue value.malariaTreatment)
                , malariaAdverseEventsHospitalization =
                    maybeValueConsideringIsDirtyField form.malariaAdverseEventsHospitalizationDirty
                        form.malariaAdverseEventsHospitalization
                        (treatmentAdverseEventsHospitalizationFromValue value.malariaTreatment)
                , malariaAdverseEventsHospitalizationDirty = form.malariaAdverseEventsHospitalizationDirty
                , anemiaStillTaking = or form.anemiaStillTaking (treatmentStillTakingFromValue value.anemiaTreatment)
                , anemiaMissedDoses = or form.anemiaMissedDoses (treatmentMissedDosesFromValue value.anemiaTreatment)
                , anemiaAdverseEvents = or form.anemiaAdverseEvents (treatmentAdverseEventsFromValue value.anemiaTreatment)
                , anemiaAdverseEventsHospitalization =
                    maybeValueConsideringIsDirtyField form.anemiaAdverseEventsHospitalizationDirty
                        form.anemiaAdverseEventsHospitalization
                        (treatmentAdverseEventsHospitalizationFromValue value.anemiaTreatment)
                , anemiaAdverseEventsHospitalizationDirty = form.anemiaAdverseEventsHospitalizationDirty
                , syphilisStillTaking = or form.syphilisStillTaking (treatmentStillTakingFromValue value.syphilisTreatment)
                , syphilisMissedDoses = or form.syphilisMissedDoses (treatmentMissedDosesFromValue value.syphilisTreatment)
                , syphilisAdverseEvents = or form.syphilisAdverseEvents (treatmentAdverseEventsFromValue value.syphilisTreatment)
                , syphilisAdverseEventsHospitalization =
                    maybeValueConsideringIsDirtyField form.syphilisAdverseEventsHospitalizationDirty
                        form.syphilisAdverseEventsHospitalization
                        (treatmentAdverseEventsHospitalizationFromValue value.syphilisTreatment)
                , syphilisAdverseEventsHospitalizationDirty = form.syphilisAdverseEventsHospitalizationDirty
                }
            )


toMedicationValueWithDefault : Maybe MedicationValue -> MedicationForm -> Maybe MedicationValue
toMedicationValueWithDefault saved form =
    medicationFormWithDefault form saved
        |> toMedicationValue


toMedicationValue : MedicationForm -> Maybe MedicationValue
toMedicationValue form =
    let
        signs =
            if
                List.all isNothing
                    [ form.receivedIronFolicAcid
                    , form.receivedDewormingPill
                    , form.receivedMebendazole
                    , form.receivedFolicAcid
                    , form.receivedVitaminA
                    ]
            then
                Nothing

            else
                [ ifNullableTrue IronAndFolicAcidSupplement form.receivedIronFolicAcid
                , ifNullableTrue DewormingPill form.receivedDewormingPill
                , ifNullableTrue Mebendazole form.receivedMebendazole
                , ifNullableTrue PostpartumFolicAcid form.receivedFolicAcid
                , ifNullableTrue PostpartumVitaminA form.receivedVitaminA
                ]
                    |> Maybe.Extra.combine
                    |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoMedication)

        hivTreatment =
            if
                isNothing form.hivMedicationNotGivenReason
                    && List.all isNothing
                        [ form.hivMedicationByPMTCT
                        , form.hivStillTaking
                        , form.hivMissedDoses
                        , form.hivAdverseEvents
                        , form.hivAdverseEventsHospitalization
                        ]
            then
                Nothing

            else
                [ ifNullableTrue HIVTreatmentMedicineByPMTCT form.hivMedicationByPMTCT
                , ifNullableTrue HIVTreatmentStillTaking form.hivStillTaking
                , ifNullableTrue HIVTreatmentMissedDoses form.hivMissedDoses
                , ifNullableTrue HIVTreatmentAdverseEvents form.hivAdverseEvents
                , ifNullableTrue HIVTreatmentAdverseEventsHospitalization form.hivAdverseEventsHospitalization
                ]
                    ++ [ Maybe.map (EverySet.singleton >> Just) form.hivMedicationNotGivenReason
                            |> Maybe.withDefault (Just EverySet.empty)
                       ]
                    |> Maybe.Extra.combine
                    |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoHIVTreatment)

        hypertensionTreatment =
            if
                List.all isNothing
                    [ form.hypertensionStillTaking
                    , form.hypertensionMissedDoses
                    , form.hypertensionAdverseEvents
                    , form.hypertensionAdverseEventsHospitalization
                    ]
            then
                Nothing

            else
                [ ifNullableTrue MedicationTreatmentStillTaking form.hypertensionStillTaking
                , ifNullableTrue MedicationTreatmentMissedDoses form.hypertensionMissedDoses
                , ifNullableTrue MedicationTreatmentAdverseEvents form.hypertensionAdverseEvents
                , ifNullableTrue MedicationTreatmentAdverseEventsHospitalization form.hypertensionAdverseEventsHospitalization
                ]
                    |> Maybe.Extra.combine
                    |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoMedicationTreatment)

        malariaTreatment =
            if
                List.all isNothing
                    [ form.malariaStillTaking
                    , form.malariaMissedDoses
                    , form.malariaAdverseEvents
                    , form.malariaAdverseEventsHospitalization
                    ]
            then
                Nothing

            else
                [ ifNullableTrue MedicationTreatmentStillTaking form.malariaStillTaking
                , ifNullableTrue MedicationTreatmentMissedDoses form.malariaMissedDoses
                , ifNullableTrue MedicationTreatmentAdverseEvents form.malariaAdverseEvents
                , ifNullableTrue MedicationTreatmentAdverseEventsHospitalization form.malariaAdverseEventsHospitalization
                ]
                    |> Maybe.Extra.combine
                    |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoMedicationTreatment)

        anemiaTreatment =
            if
                List.all isNothing
                    [ form.anemiaStillTaking
                    , form.anemiaMissedDoses
                    , form.anemiaAdverseEvents
                    , form.anemiaAdverseEventsHospitalization
                    ]
            then
                Nothing

            else
                [ ifNullableTrue MedicationTreatmentStillTaking form.anemiaStillTaking
                , ifNullableTrue MedicationTreatmentMissedDoses form.anemiaMissedDoses
                , ifNullableTrue MedicationTreatmentAdverseEvents form.anemiaAdverseEvents
                , ifNullableTrue MedicationTreatmentAdverseEventsHospitalization form.anemiaAdverseEventsHospitalization
                ]
                    |> Maybe.Extra.combine
                    |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoMedicationTreatment)

        syphilisTreatment =
            if List.all isNothing [ form.syphilisStillTaking, form.syphilisMissedDoses, form.syphilisAdverseEvents ] then
                Nothing

            else
                [ ifNullableTrue MedicationTreatmentStillTaking form.syphilisStillTaking
                , ifNullableTrue MedicationTreatmentMissedDoses form.syphilisMissedDoses
                , ifNullableTrue MedicationTreatmentAdverseEvents form.syphilisAdverseEvents
                , ifNullableTrue MedicationTreatmentAdverseEventsHospitalization form.syphilisAdverseEventsHospitalization
                ]
                    |> Maybe.Extra.combine
                    |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoMedicationTreatment)
    in
    if
        isNothing signs
            && isNothing hivTreatment
            && List.all isNothing [ hypertensionTreatment, malariaTreatment, anemiaTreatment, syphilisTreatment ]
    then
        Nothing

    else
        Just
            { signs = signs
            , hivTreatment = hivTreatment
            , hypertensionTreatment = hypertensionTreatment
            , malariaTreatment = malariaTreatment
            , anemiaTreatment = anemiaTreatment
            , syphilisTreatment = syphilisTreatment
            }


treatmentReviewTasksCompletedFromTotal :
    Language
    -> NominalDate
    -> AssembledData
    -> TreatmentReviewData
    -> TreatmentReviewTask
    -> ( Int, Int )
treatmentReviewTasksCompletedFromTotal language currentDate assembled data task =
    let
        form =
            assembled.measurements.medication
                |> getMeasurementValueFunc
                |> medicationFormWithDefault data.medicationForm
    in
    case task of
        TreatmentReviewPrenatalMedication ->
            let
                ( _, tasks ) =
                    resolvePrenatalMedicationFormInputsAndTasks language
                        currentDate
                        SetMedicationSubActivityBoolInput
                        assembled
                        form
            in
            ( Maybe.Extra.values tasks
                |> List.length
            , List.length tasks
            )

        _ ->
            let
                ( _, tasks ) =
                    resolveMedicationTreatmentFormInputsAndTasks language
                        currentDate
                        SetMedicationSubActivityBoolInput
                        assembled
                        form
                        task
            in
            ( Maybe.Extra.values tasks
                |> List.length
            , List.length tasks
            )


resolvePrenatalMedicationFormInputsAndTasks :
    Language
    -> NominalDate
    -> ((Bool -> MedicationForm -> MedicationForm) -> Bool -> Msg)
    -> AssembledData
    -> MedicationForm
    -> ( List (Html Msg), List (Maybe Bool) )
resolvePrenatalMedicationFormInputsAndTasks language currentDate setBoolInputMsg assembled form =
    let
        receivedIronFolicAcidUpdateFunc value form_ =
            { form_ | receivedIronFolicAcid = Just value }

        receivedIronFolicAcidInput =
            [ viewQuestionLabel language Translate.ReceivedIronFolicAcid
            , viewBoolInput
                language
                form.receivedIronFolicAcid
                (setBoolInputMsg receivedIronFolicAcidUpdateFunc)
                "iron-folic-acid"
                Nothing
            ]

        ( receivedMebendazoleInput, receivedMebendazoleTask ) =
            if showMebendazoleQuestion currentDate assembled then
                let
                    receivedMebendazoleUpdateFunc value form_ =
                        { form_ | receivedMebendazole = Just value }
                in
                ( [ viewQuestionLabel language Translate.ReceivedMebendazole
                  , viewBoolInput
                        language
                        form.receivedMebendazole
                        (setBoolInputMsg receivedMebendazoleUpdateFunc)
                        "mebendezole"
                        Nothing
                  ]
                , [ form.receivedMebendazole ]
                )

            else
                ( [], [] )
    in
    ( receivedIronFolicAcidInput ++ receivedMebendazoleInput
    , form.receivedIronFolicAcid :: receivedMebendazoleTask
    )


resolveMedicationTreatmentFormInputsAndTasks :
    Language
    -> NominalDate
    -> ((Bool -> MedicationForm -> MedicationForm) -> Bool -> Msg)
    -> AssembledData
    -> MedicationForm
    -> TreatmentReviewTask
    -> ( List (Html Msg), List (Maybe Bool) )
resolveMedicationTreatmentFormInputsAndTasks language currentDate setBoolInputMsg assembled form task =
    case task of
        TreatmentReviewHIV ->
            let
                recievedHIVMedicationAtHC =
                    latestMedicationTreatmentForHIV assembled
                        |> isJust

                referredToHIVProgram =
                    referredToHIVProgramPreviously assembled
            in
            -- There's a scenario when there's HIV program at HC,
            -- patient is referred there, but did not get medication.
            -- In this case we'll find this out at Treatment review, and
            -- prescribe medication at HC, essentially, moving that patient
            -- from PMTCT to our care.
            -- Therefore, we take HIV program path, only if patient was
            -- referred to HIV program, and also was not medicated for
            -- HIV at HC.
            if referredToHIVProgramPreviously assembled && not recievedHIVMedicationAtHC then
                let
                    updateFunc =
                        \value form_ ->
                            { form_
                                | hivMedicationByPMTCT = Just value
                                , hivMedicationNotGivenReason = Nothing
                                , hivMedicationNotGivenReasonDirty = True
                            }

                    ( derivedSection, derivedTasks ) =
                        Maybe.map
                            (\gotMedicine ->
                                if gotMedicine then
                                    ( [], [] )

                                else
                                    ( [ div [ class "why-not" ]
                                            [ viewQuestionLabel language Translate.WhyNot
                                            , viewCheckBoxSelectInput language
                                                reasonsForNoMedicationByPMTCT
                                                []
                                                form.hivMedicationNotGivenReason
                                                SetHIVMedicationNotGivenReason
                                                Translate.HIVTreatmentSign
                                            ]
                                      ]
                                    , [ maybeToBoolTask form.hivMedicationNotGivenReason ]
                                    )
                            )
                            form.hivMedicationByPMTCT
                            |> Maybe.withDefault ( [], [] )
                in
                ( [ viewQuestionLabel language Translate.TreatmentReviewQuestionMedicationByPMTCT
                  , viewBoolInput
                        language
                        form.hivMedicationByPMTCT
                        (setBoolInputMsg updateFunc)
                        "hiv-medication-by-pmtct"
                        Nothing
                  ]
                    ++ derivedSection
                , form.hivMedicationByPMTCT :: derivedTasks
                )

            else
                -- No HIV program at heath center => patient was supposed to get medication.
                resolveMedicationTreatmentFormInputsAndTasksCommon language currentDate setBoolInputMsg assembled form task

        _ ->
            resolveMedicationTreatmentFormInputsAndTasksCommon language currentDate setBoolInputMsg assembled form task


reasonsForNoMedicationByPMTCT : List HIVTreatmentSign
reasonsForNoMedicationByPMTCT =
    [ HIVTreatmentNoMedicineNotSeenAtPMTCT
    , HIVTreatmentNoMedicineOutOfStock
    , HIVTreatmentNoMedicinePatientRefused
    , HIVTreatmentNoMedicineOther
    ]


resolveMedicationTreatmentFormInputsAndTasksCommon :
    Language
    -> NominalDate
    -> ((Bool -> MedicationForm -> MedicationForm) -> Bool -> Msg)
    -> AssembledData
    -> MedicationForm
    -> TreatmentReviewTask
    -> ( List (Html Msg), List (Maybe Bool) )
resolveMedicationTreatmentFormInputsAndTasksCommon language currentDate setBoolInputMsg assembled form task =
    let
        configForTask =
            case task of
                TreatmentReviewPrenatalMedication ->
                    Nothing

                TreatmentReviewHIV ->
                    Just
                        { latestMedicationTreatment = latestMedicationTreatmentForHIV assembled
                        , stillTakingFormValue = form.hivStillTaking
                        , missedDosesFormValue = form.hivMissedDoses
                        , adverseEventsFormValue = form.hivAdverseEvents
                        , adverseEventsHospitalizationFormValue = form.hivAdverseEventsHospitalization
                        , stillTakingUpdateFunc = \value form_ -> { form_ | hivStillTaking = Just value }
                        , missedDosesUpdateFunc = \value form_ -> { form_ | hivMissedDoses = Just value }
                        , adverseEventsUpdateFunc =
                            \value form_ ->
                                { form_
                                    | hivAdverseEvents = Just value
                                    , hivAdverseEventsHospitalization = Nothing
                                    , hivAdverseEventsHospitalizationDirty = True
                                }
                        , adverseEventsHospitalizationUpdateFunc =
                            \value form_ ->
                                { form_
                                    | hivAdverseEventsHospitalization = Just value
                                    , hivAdverseEventsHospitalizationDirty = True
                                }
                        }

                TreatmentReviewHypertension ->
                    Just
                        { latestMedicationTreatment = latestMedicationTreatmentForHypertension assembled
                        , stillTakingFormValue = form.hypertensionStillTaking
                        , missedDosesFormValue = form.hypertensionMissedDoses
                        , adverseEventsFormValue = form.hypertensionAdverseEvents
                        , adverseEventsHospitalizationFormValue = form.hypertensionAdverseEventsHospitalization
                        , stillTakingUpdateFunc = \value form_ -> { form_ | hypertensionStillTaking = Just value }
                        , missedDosesUpdateFunc = \value form_ -> { form_ | hypertensionMissedDoses = Just value }
                        , adverseEventsUpdateFunc =
                            \value form_ ->
                                { form_
                                    | hypertensionAdverseEvents = Just value
                                    , hypertensionAdverseEventsHospitalization = Nothing
                                    , hypertensionAdverseEventsHospitalizationDirty = True
                                }
                        , adverseEventsHospitalizationUpdateFunc =
                            \value form_ ->
                                { form_
                                    | hypertensionAdverseEventsHospitalization = Just value
                                    , hypertensionAdverseEventsHospitalizationDirty = True
                                }
                        }

                TreatmentReviewMalaria ->
                    Just
                        { latestMedicationTreatment = latestMedicationTreatmentForMalaria assembled
                        , stillTakingFormValue = form.malariaStillTaking
                        , missedDosesFormValue = form.malariaMissedDoses
                        , adverseEventsFormValue = form.malariaAdverseEvents
                        , adverseEventsHospitalizationFormValue = form.malariaAdverseEventsHospitalization
                        , stillTakingUpdateFunc = \value form_ -> { form_ | malariaStillTaking = Just value }
                        , missedDosesUpdateFunc = \value form_ -> { form_ | malariaMissedDoses = Just value }
                        , adverseEventsUpdateFunc =
                            \value form_ ->
                                { form_
                                    | malariaAdverseEvents = Just value
                                    , malariaAdverseEventsHospitalization = Nothing
                                    , malariaAdverseEventsHospitalizationDirty = True
                                }
                        , adverseEventsHospitalizationUpdateFunc =
                            \value form_ ->
                                { form_
                                    | malariaAdverseEventsHospitalization = Just value
                                    , malariaAdverseEventsHospitalizationDirty = True
                                }
                        }

                TreatmentReviewAnemia ->
                    Just
                        { latestMedicationTreatment = latestMedicationTreatmentForAnemia assembled
                        , stillTakingFormValue = form.anemiaStillTaking
                        , missedDosesFormValue = form.anemiaMissedDoses
                        , adverseEventsFormValue = form.anemiaAdverseEvents
                        , adverseEventsHospitalizationFormValue = form.anemiaAdverseEventsHospitalization
                        , stillTakingUpdateFunc = \value form_ -> { form_ | anemiaStillTaking = Just value }
                        , missedDosesUpdateFunc = \value form_ -> { form_ | anemiaMissedDoses = Just value }
                        , adverseEventsUpdateFunc =
                            \value form_ ->
                                { form_
                                    | anemiaAdverseEvents = Just value
                                    , anemiaAdverseEventsHospitalization = Nothing
                                    , anemiaAdverseEventsHospitalizationDirty = True
                                }
                        , adverseEventsHospitalizationUpdateFunc =
                            \value form_ ->
                                { form_
                                    | anemiaAdverseEventsHospitalization = Just value
                                    , anemiaAdverseEventsHospitalizationDirty = True
                                }
                        }

                TreatmentReviewSyphilis ->
                    Just
                        { latestMedicationTreatment = latestMedicationTreatmentForSyphilis assembled
                        , stillTakingFormValue = form.syphilisStillTaking
                        , missedDosesFormValue = form.syphilisMissedDoses
                        , adverseEventsFormValue = form.syphilisAdverseEvents
                        , adverseEventsHospitalizationFormValue = form.syphilisAdverseEventsHospitalization
                        , stillTakingUpdateFunc = \value form_ -> { form_ | syphilisStillTaking = Just value }
                        , missedDosesUpdateFunc = \value form_ -> { form_ | syphilisMissedDoses = Just value }
                        , adverseEventsUpdateFunc =
                            \value form_ ->
                                { form_
                                    | syphilisAdverseEvents = Just value
                                    , syphilisAdverseEventsHospitalization = Nothing
                                    , syphilisAdverseEventsHospitalizationDirty = True
                                }
                        , adverseEventsHospitalizationUpdateFunc =
                            \value form_ ->
                                { form_
                                    | syphilisAdverseEventsHospitalization = Just value
                                    , syphilisAdverseEventsHospitalizationDirty = True
                                }
                        }
    in
    Maybe.map
        (\config ->
            let
                header =
                    Maybe.map
                        (\transId ->
                            viewCustomLabel language transId "" "label header"
                        )
                        config.latestMedicationTreatment
                        |> Maybe.withDefault emptyNode

                ( derivedInput, derivedTask ) =
                    if config.adverseEventsFormValue == Just True then
                        ( [ viewQuestionLabel language Translate.TreatmentReviewQuestionAdverseEventsHospitalization
                          , viewBoolInput
                                language
                                config.adverseEventsHospitalizationFormValue
                                (setBoolInputMsg config.adverseEventsHospitalizationUpdateFunc)
                                "adverse-events-hospitalization"
                                Nothing
                          ]
                        , [ config.adverseEventsHospitalizationFormValue ]
                        )

                    else
                        ( [], [] )

                stillTakingTranslate =
                    if task == TreatmentReviewHIV then
                        Translate.TreatmentReviewQuestionStillTakingForHIV

                    else
                        Translate.TreatmentReviewQuestionStillTaking
            in
            ( [ header
              , viewQuestionLabel language stillTakingTranslate
              , viewBoolInput
                    language
                    config.stillTakingFormValue
                    (setBoolInputMsg config.stillTakingUpdateFunc)
                    "still-taking"
                    Nothing
              , viewQuestionLabel language Translate.TreatmentReviewQuestionMissedDoses
              , viewBoolInput
                    language
                    config.missedDosesFormValue
                    (setBoolInputMsg config.missedDosesUpdateFunc)
                    "missed-doses"
                    Nothing
              , viewQuestionLabel language Translate.TreatmentReviewQuestionAdverseEvents
              , viewBoolInput
                    language
                    config.adverseEventsFormValue
                    (setBoolInputMsg config.adverseEventsUpdateFunc)
                    "adverse-events"
                    Nothing
              ]
                ++ derivedInput
            , [ config.stillTakingFormValue
              , config.missedDosesFormValue
              , config.adverseEventsFormValue
              ]
                ++ derivedTask
            )
        )
        configForTask
        |> Maybe.withDefault ( [], [] )


obstetricalExamFormWithDefault : ObstetricalExamForm -> Maybe ObstetricalExamValue -> ObstetricalExamForm
obstetricalExamFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { fundalPalpable = or form.fundalPalpable (Just value.fundalPalpable)
                , fundalHeight =
                    maybeValueConsideringIsDirtyField form.fundalHeightDirty
                        form.fundalHeight
                        (Maybe.map getHeightValue value.fundalHeight)
                , fundalHeightDirty = form.fundalHeightDirty
                , fetalPresentation = or form.fetalPresentation (Just value.fetalPresentation)
                , fetalMovement = or form.fetalMovement (Just value.fetalMovement)
                , fetalHeartRate = valueConsideringIsDirtyField form.fetalHeartRateDirty form.fetalHeartRate value.fetalHeartRate
                , fetalHeartRateDirty = form.fetalHeartRateDirty
                , cSectionScar = or form.cSectionScar (Just value.cSectionScar)
                , displayFundalPalpablePopup = form.displayFundalPalpablePopup
                }
            )


toObstetricalExamValueWithDefault : Maybe ObstetricalExamValue -> ObstetricalExamForm -> Maybe ObstetricalExamValue
toObstetricalExamValueWithDefault saved form =
    obstetricalExamFormWithDefault form saved
        |> toObstetricalExamValue


toObstetricalExamValue : ObstetricalExamForm -> Maybe ObstetricalExamValue
toObstetricalExamValue form =
    Maybe.map ObstetricalExamValue form.fundalPalpable
        |> andMap (Just <| Maybe.map HeightInCm form.fundalHeight)
        |> andMap form.fetalPresentation
        |> andMap form.fetalMovement
        |> andMap form.fetalHeartRate
        |> andMap form.cSectionScar


fromObstetricHistoryValue : Maybe ObstetricHistoryValue -> ObstetricFormFirstStep
fromObstetricHistoryValue saved =
    { currentlyPregnant = Maybe.map .currentlyPregnant saved
    , termPregnancy = Maybe.map .termPregnancy saved
    , termPregnancyDirty = False
    , preTermPregnancy = Maybe.map .preTermPregnancy saved
    , preTermPregnancyDirty = False
    , stillbirthsAtTerm = Maybe.map .stillbirthsAtTerm saved
    , stillbirthsAtTermDirty = False
    , stillbirthsPreTerm = Maybe.map .stillbirthsPreTerm saved
    , stillbirthsPreTermDirty = False
    , abortions = Maybe.map .abortions saved
    , abortionsDirty = False
    , liveChildren = Maybe.map .liveChildren saved
    , liveChildrenDirty = False
    }


obstetricHistoryFormWithDefault : ObstetricFormFirstStep -> Maybe ObstetricHistoryValue -> ObstetricFormFirstStep
obstetricHistoryFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { currentlyPregnant = or form.currentlyPregnant (Just value.currentlyPregnant)
                , termPregnancy = valueConsideringIsDirtyField form.termPregnancyDirty form.termPregnancy value.termPregnancy
                , termPregnancyDirty = form.termPregnancyDirty
                , preTermPregnancy = valueConsideringIsDirtyField form.preTermPregnancyDirty form.preTermPregnancy value.preTermPregnancy
                , preTermPregnancyDirty = form.preTermPregnancyDirty
                , stillbirthsAtTerm = valueConsideringIsDirtyField form.stillbirthsAtTermDirty form.stillbirthsAtTerm value.stillbirthsAtTerm
                , stillbirthsAtTermDirty = form.stillbirthsAtTermDirty
                , stillbirthsPreTerm = valueConsideringIsDirtyField form.stillbirthsPreTermDirty form.stillbirthsPreTerm value.stillbirthsPreTerm
                , stillbirthsPreTermDirty = form.stillbirthsPreTermDirty
                , abortions = valueConsideringIsDirtyField form.abortionsDirty form.abortions value.abortions
                , abortionsDirty = form.abortionsDirty
                , liveChildren = valueConsideringIsDirtyField form.liveChildrenDirty form.liveChildren value.liveChildren
                , liveChildrenDirty = form.liveChildrenDirty
                }
            )


toObstetricHistoryValueWithDefault : Maybe ObstetricHistoryValue -> ObstetricFormFirstStep -> Maybe ObstetricHistoryValue
toObstetricHistoryValueWithDefault saved form =
    obstetricHistoryFormWithDefault form saved
        |> toObstetricHistoryValue


toObstetricHistoryValue : ObstetricFormFirstStep -> Maybe ObstetricHistoryValue
toObstetricHistoryValue form =
    Maybe.map ObstetricHistoryValue form.currentlyPregnant
        |> andMap form.termPregnancy
        |> andMap form.preTermPregnancy
        |> andMap form.stillbirthsAtTerm
        |> andMap form.stillbirthsPreTerm
        |> andMap form.abortions
        |> andMap form.liveChildren


obstetricHistoryStep2FormWithDefault : ObstetricFormSecondStep -> Maybe ObstetricHistoryStep2Value -> ObstetricFormSecondStep
obstetricHistoryStep2FormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { cSections = valueConsideringIsDirtyField form.cSectionsDirty form.cSections value.cSections
                , cSectionsDirty = form.cSectionsDirty
                , cSectionInPreviousDelivery =
                    maybeValueConsideringIsDirtyField form.cSectionInPreviousDeliveryDirty
                        form.cSectionInPreviousDelivery
                        (EverySet.member CSectionInPreviousDelivery value.previousDelivery |> Just)
                , cSectionInPreviousDeliveryDirty = form.cSectionInPreviousDeliveryDirty
                , cSectionReason =
                    maybeValueConsideringIsDirtyField form.cSectionReasonDirty
                        form.cSectionReason
                        (Maybe.map EverySet.toList value.cSectionReason |> Maybe.andThen List.head)
                , cSectionReasonDirty = form.cSectionReasonDirty
                , previousDeliveryPeriod = or form.previousDeliveryPeriod (value.previousDeliveryPeriod |> EverySet.toList |> List.head)
                , successiveAbortions = or form.successiveAbortions (EverySet.member SuccessiveAbortions value.obstetricHistory |> Just)
                , successivePrematureDeliveries = or form.successivePrematureDeliveries (EverySet.member SuccessivePrematureDeliveries value.obstetricHistory |> Just)
                , stillbornPreviousDelivery = or form.stillbornPreviousDelivery (EverySet.member StillbornPreviousDelivery value.previousDelivery |> Just)
                , babyDiedOnDayOfBirthPreviousDelivery = or form.babyDiedOnDayOfBirthPreviousDelivery (EverySet.member BabyDiedOnDayOfBirthPreviousDelivery value.previousDelivery |> Just)
                , partialPlacentaPreviousDelivery = or form.partialPlacentaPreviousDelivery (EverySet.member PartialPlacentaPreviousDelivery value.previousDelivery |> Just)
                , severeHemorrhagingPreviousDelivery = or form.severeHemorrhagingPreviousDelivery (EverySet.member SevereHemorrhagingPreviousDelivery value.previousDelivery |> Just)
                , preeclampsiaPreviousPregnancy = or form.preeclampsiaPreviousPregnancy (EverySet.member PreeclampsiaPreviousPregnancy value.obstetricHistory |> Just)
                , convulsionsPreviousDelivery = or form.convulsionsPreviousDelivery (EverySet.member ConvulsionsPreviousDelivery value.previousDelivery |> Just)
                , convulsionsAndUnconsciousPreviousDelivery = or form.convulsionsAndUnconsciousPreviousDelivery (EverySet.member ConvulsionsAndUnconsciousPreviousDelivery value.previousDelivery |> Just)
                , gestationalDiabetesPreviousPregnancy = or form.gestationalDiabetesPreviousPregnancy (EverySet.member GestationalDiabetesPreviousPregnancy value.obstetricHistory |> Just)
                , incompleteCervixPreviousPregnancy = or form.incompleteCervixPreviousPregnancy (EverySet.member IncompleteCervixPreviousPregnancy value.obstetricHistory |> Just)
                , rhNegative = or form.rhNegative (EverySet.member RhNegative value.obstetricHistory |> Just)
                }
            )


toObstetricHistoryStep2ValueWithDefault : Maybe ObstetricHistoryStep2Value -> ObstetricFormSecondStep -> Maybe ObstetricHistoryStep2Value
toObstetricHistoryStep2ValueWithDefault saved form =
    obstetricHistoryStep2FormWithDefault form saved
        |> toObstetricHistoryStep2Value


toObstetricHistoryStep2Value : ObstetricFormSecondStep -> Maybe ObstetricHistoryStep2Value
toObstetricHistoryStep2Value form =
    let
        previousDeliverySet =
            [ ifNullableTrue CSectionInPreviousDelivery form.cSectionInPreviousDelivery
            , Maybe.map (ifTrue StillbornPreviousDelivery) form.stillbornPreviousDelivery
            , Maybe.map (ifTrue BabyDiedOnDayOfBirthPreviousDelivery) form.babyDiedOnDayOfBirthPreviousDelivery
            , Maybe.map (ifTrue PartialPlacentaPreviousDelivery) form.partialPlacentaPreviousDelivery
            , Maybe.map (ifTrue SevereHemorrhagingPreviousDelivery) form.severeHemorrhagingPreviousDelivery
            , Maybe.map (ifTrue ConvulsionsPreviousDelivery) form.convulsionsPreviousDelivery
            , Maybe.map (ifTrue ConvulsionsAndUnconsciousPreviousDelivery) form.convulsionsAndUnconsciousPreviousDelivery
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoPreviousDeliverySign)

        obstetricHistorySet =
            [ Maybe.map (ifTrue SuccessiveAbortions) form.successiveAbortions
            , Maybe.map (ifTrue SuccessivePrematureDeliveries) form.successivePrematureDeliveries
            , Maybe.map (ifTrue PreeclampsiaPreviousPregnancy) form.preeclampsiaPreviousPregnancy
            , Maybe.map (ifTrue GestationalDiabetesPreviousPregnancy) form.gestationalDiabetesPreviousPregnancy
            , Maybe.map (ifTrue IncompleteCervixPreviousPregnancy) form.incompleteCervixPreviousPregnancy
            , Maybe.map (ifTrue RhNegative) form.rhNegative
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoObstetricHistorySign)
    in
    Maybe.map ObstetricHistoryStep2Value form.cSections
        |> andMap (Just <| Maybe.map EverySet.singleton form.cSectionReason)
        |> andMap previousDeliverySet
        |> andMap (Maybe.map EverySet.singleton form.previousDeliveryPeriod)
        |> andMap obstetricHistorySet


fromPrenatalNutritionValue : Maybe PrenatalNutritionValue -> NutritionAssessmentForm
fromPrenatalNutritionValue saved =
    { height = Maybe.map (.height >> getHeightValue) saved
    , heightDirty = False
    , weight = Maybe.map (.weight >> weightValueFunc) saved
    , weightDirty = False
    , muac = Maybe.map (.muac >> muacValueFunc) saved
    , muacDirty = False
    }


prenatalNutritionFormWithDefault : NutritionAssessmentForm -> Maybe PrenatalNutritionValue -> NutritionAssessmentForm
prenatalNutritionFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { height = valueConsideringIsDirtyField form.heightDirty form.height (getHeightValue value.height)
                , heightDirty = form.heightDirty
                , weight = valueConsideringIsDirtyField form.weightDirty form.weight (weightValueFunc value.weight)
                , weightDirty = form.weightDirty
                , muac = valueConsideringIsDirtyField form.muacDirty form.muac (muacValueFunc value.muac)
                , muacDirty = form.muacDirty
                }
            )


toPrenatalNutritionValueWithDefault : Maybe PrenatalNutritionValue -> NutritionAssessmentForm -> Maybe PrenatalNutritionValue
toPrenatalNutritionValueWithDefault saved form =
    prenatalNutritionFormWithDefault form saved
        |> toPrenatalNutritionValue


toPrenatalNutritionValue : NutritionAssessmentForm -> Maybe PrenatalNutritionValue
toPrenatalNutritionValue form =
    Maybe.map PrenatalNutritionValue (Maybe.map HeightInCm form.height)
        |> andMap (Maybe.map WeightInKg form.weight)
        |> andMap (Maybe.map MuacInCm form.muac)


socialHistoryFormWithDefault : SocialHistoryForm -> Maybe SocialHistoryValue -> SocialHistoryForm
socialHistoryFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { accompaniedByPartner = or form.accompaniedByPartner (EverySet.member AccompaniedByPartner value |> Just)
                , partnerReceivedCounseling = or form.partnerReceivedCounseling (EverySet.member PartnerHivCounseling value |> Just)
                }
            )


toSocialHistoryValueWithDefault : Maybe SocialHistoryValue -> SocialHistoryForm -> Maybe SocialHistoryValue
toSocialHistoryValueWithDefault saved form =
    socialHistoryFormWithDefault form saved
        |> toSocialHistoryValue


toSocialHistoryValue : SocialHistoryForm -> Maybe SocialHistoryValue
toSocialHistoryValue form =
    [ Maybe.map (ifTrue AccompaniedByPartner) form.accompaniedByPartner
    , ifNullableTrue PartnerHivCounseling form.partnerReceivedCounseling
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoSocialHistorySign)


fromPregnancyTestValue : Maybe PregnancyTestResult -> PregnancyTestForm
fromPregnancyTestValue saved =
    { pregnancyTestResult = saved }


pregnancyTestFormWithDefault : PregnancyTestForm -> Maybe PregnancyTestResult -> PregnancyTestForm
pregnancyTestFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    formWithDefault =
                        fromPregnancyTestValue saved
                in
                { pregnancyTestResult = or form.pregnancyTestResult formWithDefault.pregnancyTestResult
                }
            )


toPregnancyTestValueWithDefault : Maybe PregnancyTestResult -> PregnancyTestForm -> Maybe PregnancyTestResult
toPregnancyTestValueWithDefault saved form =
    pregnancyTestFormWithDefault form saved
        |> (\form_ ->
                form_
           )
        |> toPregnancyTestValue


toPregnancyTestValue : PregnancyTestForm -> Maybe PregnancyTestResult
toPregnancyTestValue form =
    form.pregnancyTestResult


examinationTasksCompletedFromTotal : AssembledData -> ExaminationData -> ExaminationTask -> ( Int, Int )
examinationTasksCompletedFromTotal assembled data task =
    case task of
        Vitals ->
            let
                form =
                    assembled.measurements.vitals
                        |> getMeasurementValueFunc
                        |> vitalsFormWithDefault data.vitalsForm
            in
            ( taskAllCompleted [ form.sysBloodPressure, form.diaBloodPressure ]
                + ([ Maybe.map (always ()) form.heartRate
                   , Maybe.map (always ()) form.respiratoryRate
                   , Maybe.map (always ()) form.bodyTemperature
                   ]
                    |> List.map taskCompleted
                    |> List.sum
                  )
            , 4
            )

        NutritionAssessment ->
            let
                measuredHeight =
                    resolveMeasuredHeight assembled

                hideHeightInput =
                    isJust measuredHeight

                form_ =
                    assembled.measurements.nutrition
                        |> getMeasurementValueFunc
                        |> prenatalNutritionFormWithDefault data.nutritionAssessmentForm

                form =
                    if hideHeightInput then
                        Maybe.map (\(HeightInCm height) -> { form_ | height = Just height }) measuredHeight
                            |> Maybe.withDefault form_

                    else
                        form_

                tasks_ =
                    if hideHeightInput then
                        [ form.weight, form.muac ]

                    else
                        [ form.height, form.weight, form.muac ]

                tasksForBmi =
                    if hideHeightInput then
                        [ form.weight ]

                    else
                        [ form.height, form.weight ]
            in
            ( (List.map taskCompleted tasks_ |> List.sum)
                -- This is for BMI task, which is considered as completed
                -- when both height and weight are set.
                + taskAllCompleted tasksForBmi
            , List.length tasks_ + 1
            )

        CorePhysicalExam ->
            let
                form =
                    assembled.measurements.corePhysicalExam
                        |> getMeasurementValueFunc
                        |> corePhysicalExamFormWithDefault data.corePhysicalExamForm

                extremitiesTaskCompleted =
                    if isJust form.hands && isJust form.legs then
                        1

                    else
                        0
            in
            ( extremitiesTaskCompleted
                + taskCompleted form.neck
                + taskCompleted form.lungs
                + taskCompleted form.abdomen
                + taskCompleted form.heart
                + ([ form.brittleHair
                   , form.paleConjuctiva
                   ]
                    |> List.map taskCompleted
                    |> List.sum
                  )
            , 7
            )

        ObstetricalExam ->
            -- This is not in use, because ObstetricalExam task got
            -- special treatment at viewExaminationContent().
            ( 0, 0 )

        BreastExam ->
            -- This is not in use, because BreastExam task got
            -- special treatment at viewExaminationContent().
            ( 0, 0 )

        GUExam ->
            let
                form =
                    getMeasurementValueFunc assembled.measurements.guExam
                        |> guExamFormWithDefault data.guExamForm

                ( _, tasks ) =
                    guExamFormInputsAndTasks English assembled form
            in
            ( Maybe.Extra.values tasks
                |> List.length
            , List.length tasks
            )


fromBirthPlanValue : Maybe BirthPlanValue -> BirthPlanForm
fromBirthPlanValue saved =
    { haveInsurance = Maybe.map (.signs >> EverySet.member Insurance) saved
    , boughtClothes = Maybe.map (.signs >> EverySet.member BoughtClothes) saved
    , caregiverAccompany = Maybe.map (.signs >> EverySet.member CaregiverAccompany) saved
    , savedMoney = Maybe.map (.signs >> EverySet.member SavedMoney) saved
    , haveTransportation = Maybe.map (.signs >> EverySet.member Transportation) saved
    , familyPlanning = Maybe.map (.familyPlanning >> EverySet.toList) saved
    }


birthPlanFormWithDefault : BirthPlanForm -> Maybe BirthPlanValue -> BirthPlanForm
birthPlanFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { haveInsurance = or form.haveInsurance (EverySet.member Insurance value.signs |> Just)
                , boughtClothes = or form.boughtClothes (EverySet.member BoughtClothes value.signs |> Just)
                , caregiverAccompany = or form.caregiverAccompany (EverySet.member CaregiverAccompany value.signs |> Just)
                , savedMoney = or form.savedMoney (EverySet.member SavedMoney value.signs |> Just)
                , haveTransportation = or form.haveTransportation (EverySet.member Transportation value.signs |> Just)
                , familyPlanning = or form.familyPlanning (value.familyPlanning |> EverySet.toList |> Just)
                }
            )


toBirthPlanValueWithDefault : Maybe BirthPlanValue -> BirthPlanForm -> Maybe BirthPlanValue
toBirthPlanValueWithDefault saved form =
    birthPlanFormWithDefault form saved
        |> toBirthPlanValue


toBirthPlanValue : BirthPlanForm -> Maybe BirthPlanValue
toBirthPlanValue form =
    let
        signs =
            [ Maybe.map (ifTrue Insurance) form.haveInsurance
            , Maybe.map (ifTrue BoughtClothes) form.boughtClothes
            , Maybe.map (ifTrue CaregiverAccompany) form.caregiverAccompany
            , Maybe.map (ifTrue SavedMoney) form.savedMoney
            , Maybe.map (ifTrue Transportation) form.haveTransportation
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoBirthPlan)
    in
    Maybe.map BirthPlanValue signs
        |> andMap (Maybe.map EverySet.fromList form.familyPlanning)


fromFollowUpValue : Maybe PrenatalFollowUpValue -> FollowUpForm
fromFollowUpValue saved =
    { option = Maybe.andThen (.options >> EverySet.toList >> List.head) saved
    , assesment = Maybe.map .assesment saved
    , resolutionDate = Maybe.andThen .resolutionDate saved
    }


followUpFormWithDefault : FollowUpForm -> Maybe PrenatalFollowUpValue -> FollowUpForm
followUpFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { option = or form.option (EverySet.toList value.options |> List.head)
                , assesment = or form.assesment (Just value.assesment)
                , resolutionDate = or form.resolutionDate value.resolutionDate
                }
            )


toFollowUpValueWithDefault : Maybe PrenatalFollowUpValue -> FollowUpForm -> Maybe PrenatalFollowUpValue
toFollowUpValueWithDefault saved form =
    followUpFormWithDefault form saved
        |> toFollowUpValue


toFollowUpValue : FollowUpForm -> Maybe PrenatalFollowUpValue
toFollowUpValue form =
    Maybe.map2
        (\options assesment ->
            PrenatalFollowUpValue options assesment form.resolutionDate
        )
        (Maybe.map (List.singleton >> EverySet.fromList) form.option)
        form.assesment


fromAppointmentConfirmationValue : Maybe PrenatalAppointmentConfirmationValue -> AppointmentConfirmationForm
fromAppointmentConfirmationValue saved =
    { appointmentDate = Maybe.map .date saved
    , dateSelectorPopupState = Nothing
    }


appointmentConfirmationFormWithDefault : AppointmentConfirmationForm -> Maybe PrenatalAppointmentConfirmationValue -> AppointmentConfirmationForm
appointmentConfirmationFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { appointmentDate = or form.appointmentDate (Just value.date)
                , dateSelectorPopupState = form.dateSelectorPopupState
                }
            )


toAppointmentConfirmationValueWithDefault : Maybe PrenatalAppointmentConfirmationValue -> AppointmentConfirmationForm -> Maybe PrenatalAppointmentConfirmationValue
toAppointmentConfirmationValueWithDefault saved form =
    let
        form_ =
            appointmentConfirmationFormWithDefault form saved
    in
    toAppointmentConfirmationValue form_


toAppointmentConfirmationValue : AppointmentConfirmationForm -> Maybe PrenatalAppointmentConfirmationValue
toAppointmentConfirmationValue form =
    Maybe.map PrenatalAppointmentConfirmationValue form.appointmentDate


laboratoryTasks : List LaboratoryTask
laboratoryTasks =
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
    , TaskCompletePreviousTests
    ]


laboratoryTaskCompleted : NominalDate -> AssembledData -> LaboratoryTask -> Bool
laboratoryTaskCompleted currentDate assembled task =
    let
        measurements =
            assembled.measurements

        taskExpected =
            expectLaboratoryTask currentDate assembled
    in
    case task of
        TaskHIVTest ->
            (not <| taskExpected TaskHIVTest) || isJust measurements.hivTest

        TaskSyphilisTest ->
            (not <| taskExpected TaskSyphilisTest) || isJust measurements.syphilisTest

        TaskHepatitisBTest ->
            (not <| taskExpected TaskHepatitisBTest) || isJust measurements.hepatitisBTest

        TaskMalariaTest ->
            (not <| taskExpected TaskMalariaTest) || isJust measurements.malariaTest

        TaskBloodGpRsTest ->
            (not <| taskExpected TaskBloodGpRsTest) || isJust measurements.bloodGpRsTest

        TaskUrineDipstickTest ->
            (not <| taskExpected TaskUrineDipstickTest) || isJust measurements.urineDipstickTest

        TaskHemoglobinTest ->
            (not <| taskExpected TaskHemoglobinTest) || isJust measurements.hemoglobinTest

        TaskRandomBloodSugarTest ->
            (not <| taskExpected TaskRandomBloodSugarTest) || isJust measurements.randomBloodSugarTest

        TaskHIVPCRTest ->
            (not <| taskExpected TaskHIVPCRTest) || isJust measurements.hivPCRTest

        TaskPartnerHIVTest ->
            (not <| taskExpected TaskPartnerHIVTest) || isJust measurements.partnerHIVTest

        TaskCompletePreviousTests ->
            not <| taskExpected TaskCompletePreviousTests

        -- Others are not in use at Prenatal.
        _ ->
            False


expectLaboratoryTask : NominalDate -> AssembledData -> LaboratoryTask -> Bool
expectLaboratoryTask currentDate assembled task =
    if assembled.encounter.encounterType /= NurseEncounter then
        False

    else
        let
            pendingLabs =
                generatePendingLabsFromPreviousEncounters assembled
        in
        if
            -- No pending tests left, or, nurse has indicated that there're no
            -- additional results record.
            List.isEmpty pendingLabs
                || EverySet.member IndicatorHistoryLabsCompleted assembled.encounter.indicators
        then
            let
                testsDates =
                    generatePreviousLaboratoryTestsDatesDict currentDate assembled

                isInitialTest test =
                    Dict.get test testsDates
                        |> Maybe.map List.isEmpty
                        |> Maybe.withDefault True

                isRepeatingTestOnWeek week test =
                    Maybe.map
                        (\lmpDate ->
                            if diffWeeks lmpDate currentDate < week then
                                isInitialTest test

                            else
                                let
                                    lastTestWeek =
                                        Dict.get test testsDates
                                            |> Maybe.map (List.map (\testsDate -> diffWeeks lmpDate testsDate))
                                            |> Maybe.withDefault []
                                            |> List.sort
                                            |> List.reverse
                                            |> List.head
                                in
                                Maybe.map (\testWeek -> testWeek < week) lastTestWeek
                                    |> Maybe.withDefault True
                        )
                        assembled.globalLmpDate
                        |> Maybe.withDefault (isInitialTest test)

                -- This function checks if patient has reported of having a disease.
                -- HIV and Hepatitis B are considered chronic diseases.
                -- If patient declared to have one of them, there's no point
                -- in testing for it.
                isKnownAsPositive getMeasurementFunc =
                    List.filter
                        (.measurements
                            >> getMeasurementFunc
                            >> getMeasurementValueFunc
                            >> Maybe.map (.executionNote >> (==) TestNoteKnownAsPositive)
                            >> Maybe.withDefault False
                        )
                        assembled.nursePreviousEncountersData
                        |> List.isEmpty
                        |> not
            in
            case task of
                TaskHIVTest ->
                    (not <| isKnownAsPositive .hivTest)
                        && isInitialTest TaskHIVTest

                TaskSyphilisTest ->
                    List.all (\diagnosis -> not <| EverySet.member diagnosis assembled.encounter.pastDiagnoses)
                        syphilisDiagnosesIncludingNeurosyphilis
                        && isRepeatingTestOnWeek 38 TaskSyphilisTest

                TaskHepatitisBTest ->
                    (not <| isKnownAsPositive .hepatitisBTest)
                        && (not <| EverySet.member DiagnosisHepatitisB assembled.encounter.pastDiagnoses)
                        && isInitialTest TaskHepatitisBTest

                TaskMalariaTest ->
                    True

                TaskBloodGpRsTest ->
                    isInitialTest TaskBloodGpRsTest

                TaskUrineDipstickTest ->
                    True

                TaskHemoglobinTest ->
                    True

                TaskRandomBloodSugarTest ->
                    List.all (\diagnosis -> not <| EverySet.member diagnosis assembled.encounter.pastDiagnoses)
                        diabetesDiagnoses

                TaskHIVPCRTest ->
                    isKnownAsPositive .hivTest || diagnosedPreviously DiagnosisHIV assembled

                TaskPartnerHIVTest ->
                    isInitialTest TaskPartnerHIVTest

                TaskCompletePreviousTests ->
                    -- If we got this far, history task was completed.
                    False

                -- Others are not in use at Prenatal.
                _ ->
                    False

        else
            task == TaskCompletePreviousTests


generatePendingLabsFromPreviousEncounters : AssembledData -> List ( NominalDate, PrenatalEncounterId, List LaboratoryTest )
generatePendingLabsFromPreviousEncounters assembled =
    List.filterMap
        (\data ->
            getMeasurementValueFunc data.measurements.labsResults
                |> Maybe.andThen
                    (\value ->
                        let
                            encounterId =
                                Maybe.andThen (Tuple.second >> .encounterId) data.measurements.labsResults

                            pendingTests =
                                EverySet.diff value.performedTests value.completedTests
                                    |> EverySet.toList
                                    |> -- Vitals recheck should ne completed on same day
                                       -- it was scheduled, and therefore we're not
                                       -- catching up with it.
                                       List.filter ((/=) TestVitalsRecheck)
                        in
                        if List.isEmpty pendingTests then
                            Nothing

                        else
                            Maybe.map (\id -> ( data.startDate, id, pendingTests )) encounterId
                    )
        )
        assembled.nursePreviousEncountersData


generatePreviousLaboratoryTestsDatesDict : NominalDate -> AssembledData -> Dict LaboratoryTask (List NominalDate)
generatePreviousLaboratoryTestsDatesDict currentDate assembled =
    let
        generateTestDates getMeasurementFunc resultsExistFunc resultsValidFunc =
            List.filterMap
                (.measurements
                    >> getMeasurementFunc
                    >> resolveLabTestDate currentDate resultsExistFunc resultsValidFunc
                )
                assembled.nursePreviousEncountersData
    in
    [ ( TaskHIVTest, generateTestDates .hivTest (always True) isTestResultValid )
    , ( TaskSyphilisTest, generateTestDates .syphilisTest (.testResult >> isJust) isTestResultValid )
    , ( TaskHepatitisBTest, generateTestDates .hepatitisBTest (.testResult >> isJust) isTestResultValid )
    , ( TaskMalariaTest, generateTestDates .malariaTest (always True) isTestResultValid )
    , ( TaskBloodGpRsTest, generateTestDates .bloodGpRsTest (.bloodGroup >> isJust) (always True) )
    , ( TaskUrineDipstickTest, generateTestDates .urineDipstickTest (.protein >> isJust) (always True) )
    , ( TaskHemoglobinTest, generateTestDates .hemoglobinTest (.hemoglobinCount >> isJust) (always True) )
    , ( TaskRandomBloodSugarTest, generateTestDates .randomBloodSugarTest (.sugarCount >> isJust) (always True) )
    , ( TaskPartnerHIVTest, generateTestDates .partnerHIVTest (always True) isTestResultValid )
    ]
        |> Dict.fromList


symptomReviewFormWithDefault : SymptomReviewForm -> Maybe PrenatalSymptomReviewValue -> SymptomReviewForm
symptomReviewFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    symptoms =
                        or form.symptoms (EverySet.toList value.symptoms |> Just)

                    resolveFromValue question =
                        EverySet.member question value.symptomQuestions |> Just
                in
                { symptoms = symptoms
                , dizziness = or form.dizziness (resolveFromValue SymptomQuestionDizziness)
                , lowUrineOutput = or form.lowUrineOutput (resolveFromValue SymptomQuestionLowUrineOutput)
                , darkUrine = or form.darkUrine (resolveFromValue SymptomQuestionDarkUrine)
                , pelvicPainHospitalization = or form.pelvicPainHospitalization (resolveFromValue SymptomQuestionPelvicPainHospitalization)
                , problemLeftLeg = or form.problemLeftLeg (resolveFromValue SymptomQuestionLegPainRednessLeft)
                , legPainful = or form.legPainful (resolveFromValue SymptomQuestionLegPainful)
                , legWarm = or form.legWarm (resolveFromValue SymptomQuestionLegWarm)
                , legSwollen = or form.legSwollen (resolveFromValue SymptomQuestionLegSwollen)
                , nightSweats = or form.nightSweats (resolveFromValue SymptomQuestionNightSweats)
                , bloodInSputum = or form.bloodInSputum (resolveFromValue SymptomQuestionBloodInSputum)
                , weightLoss = or form.weightLoss (resolveFromValue SymptomQuestionWeightLoss)
                , severeFatigue = or form.severeFatigue (resolveFromValue SymptomQuestionSevereFatigue)
                , vaginalDischarge = or form.vaginalDischarge (resolveFromValue SymptomQuestionVaginalDischarge)
                , frequentUrination = or form.frequentUrination (resolveFromValue SymptomQuestionFrequentUrination)
                , vaginalItching = or form.vaginalItching (resolveFromValue SymptomQuestionVaginalItching)
                , partnerUrethralDischarge = or form.partnerUrethralDischarge (resolveFromValue SymptomQuestionPartnerUrethralDischarge)
                , flankPainSign = or form.flankPainSign value.flankPainSign
                }
            )


toSymptomReviewValueWithDefault : Maybe PrenatalSymptomReviewValue -> SymptomReviewForm -> Maybe PrenatalSymptomReviewValue
toSymptomReviewValueWithDefault saved form =
    symptomReviewFormWithDefault form saved
        |> toSymptomReviewValue


toSymptomReviewValue : SymptomReviewForm -> Maybe PrenatalSymptomReviewValue
toSymptomReviewValue form =
    let
        symptoms =
            Maybe.map (EverySet.fromList >> ifEverySetEmpty NoPrenatalSymptoms) form.symptoms

        symptomQuestions =
            [ ifNullableTrue SymptomQuestionDizziness form.dizziness
            , ifNullableTrue SymptomQuestionLowUrineOutput form.lowUrineOutput
            , ifNullableTrue SymptomQuestionDarkUrine form.darkUrine
            , ifNullableTrue SymptomQuestionPelvicPainHospitalization form.pelvicPainHospitalization
            , ifNullableTrue SymptomQuestionLegPainRednessLeft form.problemLeftLeg
            , ifNullableTrue SymptomQuestionLegPainful form.legPainful
            , ifNullableTrue SymptomQuestionLegSwollen form.legSwollen
            , ifNullableTrue SymptomQuestionLegWarm form.legWarm
            , ifNullableTrue SymptomQuestionNightSweats form.nightSweats
            , ifNullableTrue SymptomQuestionBloodInSputum form.bloodInSputum
            , ifNullableTrue SymptomQuestionWeightLoss form.weightLoss
            , ifNullableTrue SymptomQuestionSevereFatigue form.severeFatigue
            , ifNullableTrue SymptomQuestionVaginalDischarge form.vaginalDischarge
            , ifNullableTrue SymptomQuestionFrequentUrination form.frequentUrination
            , ifNullableTrue SymptomQuestionVaginalItching form.vaginalItching
            , ifNullableTrue SymptomQuestionPartnerUrethralDischarge form.partnerUrethralDischarge
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoSymptomQuestions)
    in
    Maybe.map PrenatalSymptomReviewValue symptoms
        |> andMap symptomQuestions
        |> andMap (Just form.flankPainSign)


updateSymptomReviewFormWithSymptoms : SymptomReviewForm -> List PrenatalSymptom -> SymptomReviewForm
updateSymptomReviewFormWithSymptoms form symptoms =
    let
        updateFromValue formValue question =
            if expectPrenatalSymptomQuestion symptoms question then
                formValue

            else
                Nothing
    in
    { symptoms = Just symptoms
    , dizziness = updateFromValue form.dizziness SymptomQuestionDizziness
    , lowUrineOutput = updateFromValue form.lowUrineOutput SymptomQuestionLowUrineOutput
    , darkUrine = updateFromValue form.darkUrine SymptomQuestionDarkUrine
    , pelvicPainHospitalization = updateFromValue form.pelvicPainHospitalization SymptomQuestionPelvicPainHospitalization
    , problemLeftLeg = updateFromValue form.problemLeftLeg SymptomQuestionLegPainRednessLeft
    , legPainful = updateFromValue form.legPainful SymptomQuestionLegPainful
    , legWarm = updateFromValue form.legWarm SymptomQuestionLegWarm
    , legSwollen = updateFromValue form.legSwollen SymptomQuestionLegSwollen
    , nightSweats = updateFromValue form.nightSweats SymptomQuestionNightSweats
    , bloodInSputum = updateFromValue form.bloodInSputum SymptomQuestionBloodInSputum
    , weightLoss = updateFromValue form.weightLoss SymptomQuestionWeightLoss
    , severeFatigue = updateFromValue form.severeFatigue SymptomQuestionSevereFatigue
    , vaginalDischarge = updateFromValue form.vaginalDischarge SymptomQuestionVaginalDischarge
    , frequentUrination = updateFromValue form.frequentUrination SymptomQuestionFrequentUrination
    , vaginalItching = updateFromValue form.vaginalItching SymptomQuestionVaginalItching
    , partnerUrethralDischarge = updateFromValue form.partnerUrethralDischarge SymptomQuestionPartnerUrethralDischarge
    , flankPainSign = updateFromValue form.flankPainSign SymptomQuestionFlankPain
    }


symptomReviewFormInputsAndTasks : Language -> PrenatalEncounterType -> SymptomReviewStep -> SymptomReviewForm -> ( List (Html Msg), Int, Int )
symptomReviewFormInputsAndTasks language encounterType step form =
    case step of
        SymptomReviewStepSymptoms ->
            symptomReviewFormInputsAndTasksSymptoms language encounterType form

        SymptomReviewStepQuestions ->
            symptomReviewFormInputsAndTasksQuestions language form


symptomReviewFormInputsAndTasksSymptoms : Language -> PrenatalEncounterType -> SymptomReviewForm -> ( List (Html Msg), Int, Int )
symptomReviewFormInputsAndTasksSymptoms language encounterType form =
    let
        ( symptomsLeft, symptomsRight ) =
            case encounterType of
                NurseEncounter ->
                    ( [ BurningWithUrination, AbnormalVaginalDischarge, NauseaAndVomiting, Heartburn, LegCramps, LowBackPain ]
                    , [ CoughContinuous, PelvicPain, Constipation, VaricoseVeins, LegPainRedness ]
                    )

                NursePostpartumEncounter ->
                    ( [ BurningWithUrination
                      , AbnormalVaginalDischarge
                      , NauseaAndVomiting
                      , Heartburn
                      , LegCramps
                      , LowBackPain
                      , CoughContinuous
                      , PostpartumAbdominalPain
                      , PostpartumUrinaryIncontinence
                      ]
                    , [ PelvicPain
                      , Constipation
                      , VaricoseVeins
                      , PostpartumHeadache
                      , PostpartumFatigue
                      , PostpartumFever
                      , PostpartumPerinealPainOrDischarge
                      , LegPainRedness
                      ]
                    )

                -- We should never get here, as these are CHW encounter types.
                _ ->
                    ( [], [] )
    in
    ( [ div [ class "ui form symptom-review" ]
            [ viewLabel language Translate.SelectIllnessSymptoms
            , viewCheckBoxMultipleSelectInput language
                symptomsLeft
                symptomsRight
                (form.symptoms |> Maybe.withDefault [])
                (Just NoPrenatalSymptoms)
                SetPrenatalSymptom
                Translate.PrenatalSymptom
            ]
      ]
    , taskCompleted form.symptoms
    , 1
    )


symptomReviewFormInputsAndTasksQuestions : Language -> SymptomReviewForm -> ( List (Html Msg), Int, Int )
symptomReviewFormInputsAndTasksQuestions language form =
    Maybe.map
        (\symptoms ->
            let
                inputsWithState =
                    List.filter (expectPrenatalSymptomQuestion symptoms) prenatalSymptomQuestion
                        |> List.map (prenatalSymptomQuestionInputAndState language form)

                inputs =
                    List.map Tuple.first inputsWithState
            in
            ( List.concat inputs
            , List.map Tuple.second inputsWithState |> List.sum
            , List.length inputs
            )
        )
        form.symptoms
        |> Maybe.withDefault ( [], 0, 0 )


prenatalSymptomQuestion : List PrenatalSymptomQuestion
prenatalSymptomQuestion =
    [ SymptomQuestionDizziness
    , SymptomQuestionLowUrineOutput
    , SymptomQuestionDarkUrine
    , SymptomQuestionPelvicPainHospitalization
    , SymptomQuestionLegPainRednessLeft
    , SymptomQuestionLegPainful
    , SymptomQuestionLegSwollen
    , SymptomQuestionLegWarm
    , SymptomQuestionNightSweats
    , SymptomQuestionBloodInSputum
    , SymptomQuestionWeightLoss
    , SymptomQuestionSevereFatigue
    , SymptomQuestionVaginalItching
    , SymptomQuestionVaginalDischarge
    , SymptomQuestionFrequentUrination
    , SymptomQuestionFlankPain
    , SymptomQuestionPartnerUrethralDischarge
    ]


expectPrenatalSymptomQuestion : List PrenatalSymptom -> PrenatalSymptomQuestion -> Bool
expectPrenatalSymptomQuestion symptoms question =
    let
        symptomReported symptom =
            List.member symptom symptoms
    in
    case question of
        SymptomQuestionDizziness ->
            symptomReported NauseaAndVomiting

        SymptomQuestionLowUrineOutput ->
            symptomReported NauseaAndVomiting

        SymptomQuestionDarkUrine ->
            symptomReported NauseaAndVomiting

        SymptomQuestionPelvicPainHospitalization ->
            symptomReported PelvicPain

        SymptomQuestionLegPainRednessLeft ->
            symptomReported LegPainRedness

        SymptomQuestionLegPainful ->
            symptomReported LegPainRedness

        SymptomQuestionLegSwollen ->
            symptomReported LegPainRedness

        SymptomQuestionLegWarm ->
            symptomReported LegPainRedness

        SymptomQuestionNightSweats ->
            symptomReported CoughContinuous

        SymptomQuestionBloodInSputum ->
            symptomReported CoughContinuous

        SymptomQuestionWeightLoss ->
            symptomReported CoughContinuous

        SymptomQuestionSevereFatigue ->
            symptomReported CoughContinuous

        SymptomQuestionVaginalDischarge ->
            symptomReported BurningWithUrination

        SymptomQuestionFrequentUrination ->
            symptomReported BurningWithUrination

        SymptomQuestionFlankPain ->
            symptomReported BurningWithUrination

        SymptomQuestionVaginalItching ->
            symptomReported BurningWithUrination
                || symptomReported AbnormalVaginalDischarge

        SymptomQuestionPartnerUrethralDischarge ->
            symptomReported AbnormalVaginalDischarge

        NoSymptomQuestions ->
            False


prenatalSymptomQuestionInputAndState : Language -> SymptomReviewForm -> PrenatalSymptomQuestion -> ( List (Html Msg), Int )
prenatalSymptomQuestionInputAndState language form question =
    let
        viewQuestion field updateFunc =
            [ viewQuestionLabel language (Translate.PrenatalSymptomQuestion question)
            , viewBoolInput
                language
                field
                (SetPrenatalSymptomQuestionBoolInput updateFunc)
                "symptom-question"
                Nothing
            ]
    in
    case question of
        SymptomQuestionDizziness ->
            ( viewQuestion form.dizziness (\value form_ -> { form_ | dizziness = Just value })
            , taskCompleted form.dizziness
            )

        SymptomQuestionLowUrineOutput ->
            ( viewQuestion form.lowUrineOutput (\value form_ -> { form_ | lowUrineOutput = Just value })
            , taskCompleted form.lowUrineOutput
            )

        SymptomQuestionDarkUrine ->
            ( viewQuestion form.darkUrine (\value form_ -> { form_ | darkUrine = Just value })
            , taskCompleted form.darkUrine
            )

        SymptomQuestionPelvicPainHospitalization ->
            ( viewQuestion form.pelvicPainHospitalization
                (\value form_ -> { form_ | pelvicPainHospitalization = Just value })
            , taskCompleted form.pelvicPainHospitalization
            )

        SymptomQuestionLegPainRednessLeft ->
            ( [ viewQuestionLabel language (Translate.PrenatalSymptomQuestion SymptomQuestionLegPainRednessLeft)
              , viewCustomBoolInput
                    language
                    form.problemLeftLeg
                    (SetPrenatalSymptomQuestionBoolInput (\value form_ -> { form_ | problemLeftLeg = Just value }))
                    "symptom-question"
                    ( Translate.Left, Translate.Right )
                    "four"
              ]
            , taskCompleted form.problemLeftLeg
            )

        SymptomQuestionLegPainful ->
            ( viewQuestion form.legPainful (\value form_ -> { form_ | legPainful = Just value })
            , taskCompleted form.legPainful
            )

        SymptomQuestionLegSwollen ->
            ( viewQuestion form.legSwollen (\value form_ -> { form_ | legSwollen = Just value })
            , taskCompleted form.legSwollen
            )

        SymptomQuestionLegWarm ->
            ( viewQuestion form.legWarm (\value form_ -> { form_ | legWarm = Just value })
            , taskCompleted form.legWarm
            )

        SymptomQuestionNightSweats ->
            ( viewQuestion form.nightSweats (\value form_ -> { form_ | nightSweats = Just value })
            , taskCompleted form.nightSweats
            )

        SymptomQuestionBloodInSputum ->
            ( viewQuestion form.bloodInSputum (\value form_ -> { form_ | bloodInSputum = Just value })
            , taskCompleted form.bloodInSputum
            )

        SymptomQuestionWeightLoss ->
            ( viewQuestion form.weightLoss (\value form_ -> { form_ | weightLoss = Just value })
            , taskCompleted form.weightLoss
            )

        SymptomQuestionSevereFatigue ->
            ( viewQuestion form.severeFatigue (\value form_ -> { form_ | severeFatigue = Just value })
            , taskCompleted form.severeFatigue
            )

        SymptomQuestionVaginalDischarge ->
            ( viewQuestion form.vaginalDischarge (\value form_ -> { form_ | vaginalDischarge = Just value })
            , taskCompleted form.vaginalDischarge
            )

        SymptomQuestionFrequentUrination ->
            ( viewQuestion form.frequentUrination (\value form_ -> { form_ | frequentUrination = Just value })
            , taskCompleted form.frequentUrination
            )

        SymptomQuestionFlankPain ->
            ( [ viewQuestionLabel language (Translate.PrenatalSymptomQuestion SymptomQuestionFlankPain)
              , viewCheckBoxSelectInput language
                    [ NoFlankPain, FlankPainLeftSide ]
                    [ FlankPainRightSide, FlankPainBothSides ]
                    form.flankPainSign
                    SetFlankPainSign
                    Translate.PrenatalFlankPainSign
              ]
            , taskCompleted form.flankPainSign
            )

        SymptomQuestionVaginalItching ->
            ( viewQuestion form.vaginalItching (\value form_ -> { form_ | vaginalItching = Just value })
            , taskCompleted form.vaginalItching
            )

        SymptomQuestionPartnerUrethralDischarge ->
            ( viewQuestion form.partnerUrethralDischarge (\value form_ -> { form_ | partnerUrethralDischarge = Just value })
            , taskCompleted form.partnerUrethralDischarge
            )

        NoSymptomQuestions ->
            -- We should never get here.
            ( [], 0 )


mentalHealthFormWithDefault : MentalHealthForm -> Maybe PrenatalMentalHealthValue -> MentalHealthForm
mentalHealthFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (Just value.signs)
                , specialistAtHC = or form.specialistAtHC (Just value.specialistAtHC)
                , step = form.step
                }
            )


toPrenatalMentalHealthValueWithDefault : Maybe PrenatalMentalHealthValue -> MentalHealthForm -> Maybe PrenatalMentalHealthValue
toPrenatalMentalHealthValueWithDefault saved form =
    mentalHealthFormWithDefault form saved
        |> toPrenatalMentalHealthValue


toPrenatalMentalHealthValue : MentalHealthForm -> Maybe PrenatalMentalHealthValue
toPrenatalMentalHealthValue form =
    Maybe.map2
        (\signs specialistAtHC ->
            { signs = signs
            , specialistAtHC = specialistAtHC
            }
        )
        form.signs
        form.specialistAtHC


immunisationTaskCompleted : NominalDate -> AssembledData -> ImmunisationTask -> Bool
immunisationTaskCompleted currentDate data task =
    let
        measurements =
            data.measurements

        taskExpected =
            expectImmunisationTask currentDate data
    in
    case task of
        TaskTetanus ->
            (not <| taskExpected TaskTetanus) || isJust measurements.tetanusImmunisation


expectImmunisationTask : NominalDate -> AssembledData -> ImmunisationTask -> Bool
expectImmunisationTask currentDate assembled task =
    let
        futureVaccinations =
            generateFutureVaccinationsData currentDate assembled
                |> Dict.fromList

        isTaskExpected vaccineType =
            Dict.get vaccineType futureVaccinations
                |> Maybe.Extra.join
                |> Maybe.map
                    (\( dose, date ) ->
                        not <| Date.compare date currentDate == GT
                    )
                |> Maybe.withDefault False
    in
    immunisationTaskToVaccineType task
        |> isTaskExpected


{-| For each type of vaccine, we generate next dose and administration date.
If there's no need for future vaccination, Nothing is returned.
-}
generateFutureVaccinationsData : NominalDate -> AssembledData -> List ( PrenatalVaccineType, Maybe ( VaccineDose, NominalDate ) )
generateFutureVaccinationsData currentDate assembled =
    Maybe.map
        (\lmpDate ->
            let
                egaInWeeks =
                    calculateEGAWeeks currentDate lmpDate
            in
            List.map
                (\vaccineType ->
                    let
                        nextVaccinationData =
                            case latestVaccinationDataForVaccine assembled.vaccinationHistory vaccineType of
                                Just ( lastDoseAdministered, lastDoseDate ) ->
                                    nextVaccinationDataForVaccine currentDate egaInWeeks vaccineType lastDoseDate lastDoseAdministered

                                Nothing ->
                                    -- There were no vaccination so far, so
                                    -- we offer first dose for today.
                                    Just ( VaccineDoseFirst, currentDate )
                    in
                    -- Getting Nothing at nextVaccinationData indicates that
                    -- vacination cycle is completed for this vaccine.
                    ( vaccineType, nextVaccinationData )
                )
                allVaccineTypes
        )
        assembled.globalLmpDate
        |> Maybe.withDefault []


immunisationTaskToVaccineType : ImmunisationTask -> PrenatalVaccineType
immunisationTaskToVaccineType task =
    case task of
        TaskTetanus ->
            VaccineTetanus


immunisationTasks : List ImmunisationTask
immunisationTasks =
    [ TaskTetanus ]


generateSuggestedVaccinations : NominalDate -> Int -> AssembledData -> List ( PrenatalVaccineType, VaccineDose )
generateSuggestedVaccinations currentDate egaInWeeks assembled =
    List.filterMap
        (\vaccineType ->
            let
                suggestedDose =
                    case latestVaccinationDataForVaccine assembled.vaccinationHistory vaccineType of
                        Just ( lastDoseAdministered, lastDoseDate ) ->
                            nextDoseForVaccine currentDate egaInWeeks vaccineType lastDoseDate lastDoseAdministered

                        Nothing ->
                            Just VaccineDoseFirst
            in
            Maybe.map (\nextDose -> ( vaccineType, nextDose )) suggestedDose
        )
        allVaccineTypes


allVaccineTypes : List PrenatalVaccineType
allVaccineTypes =
    [ VaccineTetanus ]


latestVaccinationDataForVaccine : VaccinationProgressDict -> PrenatalVaccineType -> Maybe ( VaccineDose, NominalDate )
latestVaccinationDataForVaccine vaccinationHistory vaccineType =
    Dict.get vaccineType vaccinationHistory
        |> Maybe.andThen
            (Dict.toList
                >> List.sortBy (Tuple.first >> vaccineDoseToComparable)
                >> List.reverse
                >> List.head
            )


nextDoseForVaccine : NominalDate -> Int -> PrenatalVaccineType -> NominalDate -> VaccineDose -> Maybe VaccineDose
nextDoseForVaccine currentDate egaInWeeks vaccineType lastDoseDate lastDoseAdministered =
    nextVaccinationDataForVaccine currentDate egaInWeeks vaccineType lastDoseDate lastDoseAdministered
        |> Maybe.andThen
            (\( dose, dueDate ) ->
                if Date.compare dueDate currentDate == GT then
                    Nothing

                else
                    Just dose
            )


nextVaccinationDataForVaccine : NominalDate -> Int -> PrenatalVaccineType -> NominalDate -> VaccineDose -> Maybe ( VaccineDose, NominalDate )
nextVaccinationDataForVaccine currentDate egaInWeeks vaccineType lastDoseDate lastDoseAdministered =
    if getLastDoseForVaccine vaccineType == lastDoseAdministered then
        Nothing

    else
        getNextVaccineDose lastDoseAdministered
            |> Maybe.map
                (\dose ->
                    let
                        ( interval, unit ) =
                            getIntervalForVaccine vaccineType lastDoseAdministered

                        nextVaccinationDateByInterval =
                            Date.add unit interval lastDoseDate

                        nextVaccinationDate =
                            -- Per requirements, if patient got less than 5 doses,
                            -- and did not recieve a dose during current pregnancy, we
                            -- need to give that dose right away.
                            -- Therefore, in case next shot date per standard interval
                            -- is in future, but last shot was given before current
                            -- pregnancy started, we suggest next shot to be given today.
                            if
                                (Date.compare currentDate nextVaccinationDateByInterval == LT)
                                    && (Date.diff Weeks lastDoseDate currentDate > egaInWeeks)
                            then
                                currentDate

                            else
                                nextVaccinationDateByInterval
                    in
                    ( dose, nextVaccinationDate )
                )


getLastDoseForVaccine : PrenatalVaccineType -> VaccineDose
getLastDoseForVaccine vaccineType =
    case vaccineType of
        VaccineTetanus ->
            VaccineDoseFifth


getIntervalForVaccine : PrenatalVaccineType -> VaccineDose -> ( Int, Unit )
getIntervalForVaccine vaccineType lastDoseAdministered =
    case vaccineType of
        VaccineTetanus ->
            case lastDoseAdministered of
                VaccineDoseFirst ->
                    ( 4, Weeks )

                VaccineDoseSecond ->
                    ( 6, Months )

                VaccineDoseThird ->
                    ( 1, Years )

                VaccineDoseFourth ->
                    ( 1, Years )

                VaccineDoseFifth ->
                    -- We should never get here.
                    ( 99, Years )


immunisationTasksCompletedFromTotal : Language -> NominalDate -> AssembledData -> ImmunisationData -> Pages.Prenatal.Activity.Types.ImmunisationTask -> ( Int, Int )
immunisationTasksCompletedFromTotal language currentDate assembled data task =
    let
        vaccineType =
            immunisationTaskToVaccineType task

        form =
            case vaccineType of
                VaccineTetanus ->
                    assembled.measurements.tetanusImmunisation
                        |> getMeasurementValueFunc
                        |> vaccinationFormWithDefault data.tetanusForm

        ( _, tasksActive, tasksCompleted ) =
            vaccinationFormDynamicContentAndTasks language currentDate assembled vaccineType form
    in
    ( tasksActive, tasksCompleted )


vaccinationFormDynamicContentAndTasks :
    Language
    -> NominalDate
    -> AssembledData
    -> PrenatalVaccineType
    -> PrenatalVaccinationForm
    -> ( List (Html Msg), Int, Int )
vaccinationFormDynamicContentAndTasks language currentDate assembled vaccineType form =
    Maybe.map2
        (\birthDate lmpDate ->
            let
                config =
                    { birthDate = birthDate
                    , expectedDoses = expectedDoses
                    , dosesFromPreviousEncountersData = dosesFromPreviousEncountersData
                    , dosesFromCurrentEncounterData = dosesFromCurrentEncounterData
                    , setVaccinationFormViewModeMsg = SetVaccinationFormViewMode vaccineType
                    , setUpdatePreviousVaccinesMsg = SetUpdatePreviousVaccines vaccineType
                    , setWillReceiveVaccineTodayMsg = SetWillReceiveVaccineToday vaccineType
                    , setAdministrationNoteMsg = SetAdministrationNote vaccineType
                    , setVaccinationUpdateDateSelectorStateMsg = SetVaccinationUpdateDateSelectorState vaccineType
                    , setVaccinationUpdateDateMsg = SetVaccinationUpdateDate vaccineType
                    , saveVaccinationUpdateDateMsg = SaveVaccinationUpdateDate vaccineType
                    , deleteVaccinationUpdateDateMsg = DeleteVaccinationUpdateDate vaccineType
                    , nextVaccinationDataForVaccine = nextVaccinationDataForVaccine currentDate egaInWeeks vaccineType
                    , getIntervalForVaccine = getIntervalForVaccine vaccineType
                    , firstDoseExpectedFrom = birthDate
                    }

                egaInWeeks =
                    calculateEGAWeeks currentDate lmpDate

                expectedDoses =
                    getAllDosesForVaccine vaccineType

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
            in
            Measurement.Utils.vaccinationFormDynamicContentAndTasks language currentDate config (PrenatalVaccine vaccineType) form
        )
        assembled.person.birthDate
        assembled.globalLmpDate
        |> Maybe.withDefault ( [], 0, 1 )


getAllDosesForVaccine : PrenatalVaccineType -> List VaccineDose
getAllDosesForVaccine vaccineType =
    let
        lastDose =
            getLastDoseForVaccine vaccineType
    in
    List.filterMap
        (\dose ->
            if vaccineDoseToComparable dose <= vaccineDoseToComparable lastDose then
                Just dose

            else
                Nothing
        )
        allVaccineDoses


allVaccineDoses : List VaccineDose
allVaccineDoses =
    [ VaccineDoseFirst, VaccineDoseSecond, VaccineDoseThird, VaccineDoseFourth, VaccineDoseFifth ]


getFormByVaccineTypeFunc : PrenatalVaccineType -> (ImmunisationData -> PrenatalVaccinationForm)
getFormByVaccineTypeFunc vaccineType =
    case vaccineType of
        VaccineTetanus ->
            .tetanusForm


updateVaccinationFormByVaccineType : PrenatalVaccineType -> PrenatalVaccinationForm -> ImmunisationData -> ImmunisationData
updateVaccinationFormByVaccineType vaccineType form data =
    case vaccineType of
        VaccineTetanus ->
            { data | tetanusForm = form }


getMeasurementByVaccineTypeFunc : PrenatalVaccineType -> PrenatalMeasurements -> Maybe VaccinationValue
getMeasurementByVaccineTypeFunc vaccineType measurements =
    case vaccineType of
        VaccineTetanus ->
            getMeasurementValueFunc measurements.tetanusImmunisation


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
                { expectations = or form.expectations (EverySet.member EducationExpectations value.signs |> Just)
                , visitsReview = or form.visitsReview (EverySet.member EducationVisitsReview value.signs |> Just)
                , warningSigns = or form.warningSigns (EverySet.member EducationWarningSigns value.signs |> Just)
                , hemorrhaging = or form.hemorrhaging (EverySet.member EducationHemorrhaging value.signs |> Just)
                , familyPlanning = or form.familyPlanning (EverySet.member EducationFamilyPlanning value.signs |> Just)
                , breastfeeding = or form.breastfeeding (EverySet.member EducationBreastfeeding value.signs |> Just)
                , immunization = or form.immunization (EverySet.member EducationImmunization value.signs |> Just)
                , hygiene = or form.hygiene (EverySet.member EducationHygiene value.signs |> Just)
                , positiveHIV = or form.positiveHIV (EverySet.member EducationPositiveHIV value.signs |> Just)
                , saferSexHIV = or form.saferSexHIV (EverySet.member EducationSaferSexHIV value.signs |> Just)
                , partnerTesting = or form.partnerTesting (EverySet.member EducationPartnerTesting value.signs |> Just)
                , nauseaVomiting = or form.nauseaVomiting (EverySet.member EducationNauseaVomiting value.signs |> Just)
                , legCramps = or form.legCramps (EverySet.member EducationLegCramps value.signs |> Just)
                , lowBackPain = or form.lowBackPain (EverySet.member EducationLowBackPain value.signs |> Just)
                , constipation = or form.constipation (EverySet.member EducationConstipation value.signs |> Just)
                , heartburn = or form.heartburn (EverySet.member EducationHeartburn value.signs |> Just)
                , varicoseVeins = or form.varicoseVeins (EverySet.member EducationVaricoseVeins value.signs |> Just)
                , legPainRedness = or form.legPainRedness (EverySet.member EducationLegPainRedness value.signs |> Just)
                , pelvicPain = or form.pelvicPain (EverySet.member EducationPelvicPain value.signs |> Just)
                , saferSex = or form.saferSex (EverySet.member EducationSaferSex value.signs |> Just)
                , mentalHealth = or form.mentalHealth (EverySet.member EducationMentalHealth value.signs |> Just)
                , earlyMastitisOrEngorgment = or form.earlyMastitisOrEngorgment (EverySet.member EducationEarlyMastitisOrEngorgment value.signs |> Just)
                , mastitis = or form.mastitis (EverySet.member EducationMastitis value.signs |> Just)

                -- Signs that do not participate at initial phase. Resolved directly from value.
                , hivDetectableViralLoad = Maybe.map (EverySet.member EducationHIVDetectableViralLoad) value.signsPhase2
                , diabetes = Maybe.map (EverySet.member EducationDiabetes) value.signsPhase2
                }
            )


toHealthEducationValue : Maybe PrenatalHealthEducationValue -> HealthEducationForm -> Maybe PrenatalHealthEducationValue
toHealthEducationValue saved form =
    [ ifNullableTrue EducationExpectations form.expectations
    , ifNullableTrue EducationVisitsReview form.visitsReview
    , ifNullableTrue EducationWarningSigns form.warningSigns
    , ifNullableTrue EducationHemorrhaging form.hemorrhaging
    , ifNullableTrue EducationFamilyPlanning form.familyPlanning
    , ifNullableTrue EducationBreastfeeding form.breastfeeding
    , ifNullableTrue EducationImmunization form.immunization
    , ifNullableTrue EducationHygiene form.hygiene
    , ifNullableTrue EducationPositiveHIV form.positiveHIV
    , ifNullableTrue EducationSaferSexHIV form.saferSexHIV
    , ifNullableTrue EducationPartnerTesting form.partnerTesting
    , ifNullableTrue EducationNauseaVomiting form.nauseaVomiting
    , ifNullableTrue EducationLegCramps form.legCramps
    , ifNullableTrue EducationLowBackPain form.lowBackPain
    , ifNullableTrue EducationConstipation form.constipation
    , ifNullableTrue EducationHeartburn form.heartburn
    , ifNullableTrue EducationVaricoseVeins form.varicoseVeins
    , ifNullableTrue EducationLegPainRedness form.legPainRedness
    , ifNullableTrue EducationPelvicPain form.pelvicPain
    , ifNullableTrue EducationSaferSex form.saferSex
    , ifNullableTrue EducationMentalHealth form.mentalHealth
    , ifNullableTrue EducationEarlyMastitisOrEngorgment form.earlyMastitisOrEngorgment
    , ifNullableTrue EducationMastitis form.mastitis
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoPrenatalHealthEducationSigns)
        |> Maybe.map
            (\signs ->
                { signs = signs
                , signsPhase2 = Maybe.andThen .signsPhase2 saved
                }
            )


breastfeedingFormWithDefault : BreastfeedingForm -> Maybe BreastfeedingValue -> BreastfeedingForm
breastfeedingFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    reasonForNotBreastfeedingFromValue =
                        EverySet.toList value
                            |> List.filter (\sign -> List.member sign reasonsForNotBreastfeeding)
                            |> List.head
                in
                { isBreastfeeding = or form.isBreastfeeding (EverySet.member IsBreastfeeding value |> Just)
                , reasonForNotBreastfeeding =
                    maybeValueConsideringIsDirtyField form.reasonForNotBreastfeedingDirty
                        form.reasonForNotBreastfeeding
                        reasonForNotBreastfeedingFromValue
                , reasonForNotBreastfeedingDirty = form.reasonForNotBreastfeedingDirty
                , breastPain = maybeValueConsideringIsDirtyField form.breastPainDirty form.breastPain (EverySet.member BreastPain value |> Just)
                , breastPainDirty = form.breastPainDirty
                , breastRedness = maybeValueConsideringIsDirtyField form.breastRednessDirty form.breastRedness (EverySet.member BreastRedness value |> Just)
                , breastRednessDirty = form.breastRednessDirty
                , enoughMilk = maybeValueConsideringIsDirtyField form.enoughMilkDirty form.enoughMilk (EverySet.member EnoughMilk value |> Just)
                , enoughMilkDirty = form.enoughMilkDirty
                , latchingWell = maybeValueConsideringIsDirtyField form.latchingWellDirty form.latchingWell (EverySet.member LatchingWell value |> Just)
                , latchingWellDirty = form.latchingWellDirty
                }
            )


toBreastfeedingValueWithDefault : Maybe BreastfeedingValue -> BreastfeedingForm -> Maybe BreastfeedingValue
toBreastfeedingValueWithDefault saved form =
    breastfeedingFormWithDefault form saved
        |> toBreastfeedingValue


toBreastfeedingValue : BreastfeedingForm -> Maybe BreastfeedingValue
toBreastfeedingValue form =
    [ Maybe.map (ifTrue IsBreastfeeding) form.isBreastfeeding
    , ifNullableTrue BreastPain form.breastPain
    , ifNullableTrue BreastRedness form.breastRedness
    , ifNullableTrue EnoughMilk form.enoughMilk
    , ifNullableTrue LatchingWell form.latchingWell
    ]
        ++ [ Maybe.map (EverySet.singleton >> Just) form.reasonForNotBreastfeeding
                |> Maybe.withDefault (Just EverySet.empty)
           ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoBreastfeedingSigns)


reasonsForNotBreastfeeding : List BreastfeedingSign
reasonsForNotBreastfeeding =
    reasonsForNotBreastfeedingLeft ++ reasonsForNotBreastfeedingRight


reasonsForNotBreastfeedingLeft : List BreastfeedingSign
reasonsForNotBreastfeedingLeft =
    [ NotBreastfeedingBreastPain
    , NotBreastfeedingBreastRedness
    , NotBreastfeedingLowMilkProduction
    , NotBreastfeedingProblemsLatching
    ]


reasonsForNotBreastfeedingRight : List BreastfeedingSign
reasonsForNotBreastfeedingRight =
    [ NotBreastfeedingMedicalProblems
    , NotBreastfeedingPersonalChoice
    , NotBreastfeedingOther
    ]


guExamFormWithDefault : GUExamForm -> Maybe GUExamValue -> GUExamForm
guExamFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    postpartumHealingProblemsFromValue =
                        Maybe.andThen
                            (\problems ->
                                case EverySet.toList problems of
                                    [ NormalPostpartumHealing ] ->
                                        Nothing

                                    _ ->
                                        Just <| EverySet.toList problems
                            )
                            value.postpartumHealingProblems
                in
                { vaginalExamSigns = or form.vaginalExamSigns (EverySet.toList value.vaginalExamSigns |> Just)
                , episiotomyOrPerinealTear = or form.episiotomyOrPerinealTear (EverySet.member EpisiotomyOrPerinealTear value.guExamSigns |> Just)
                , healingNormally =
                    maybeValueConsideringIsDirtyField
                        form.healingNormallyDirty
                        form.healingNormally
                        (Maybe.map (EverySet.member NormalPostpartumHealing) value.postpartumHealingProblems)
                , healingNormallyDirty = form.healingNormallyDirty
                , postpartumHealingProblems =
                    maybeValueConsideringIsDirtyField
                        form.postpartumHealingProblemsDirty
                        form.postpartumHealingProblems
                        postpartumHealingProblemsFromValue
                , postpartumHealingProblemsDirty = form.postpartumHealingProblemsDirty
                , rectalHemorrhoids = or form.rectalHemorrhoids (EverySet.member RectalHemorrhoids value.guExamSigns |> Just)
                }
            )


toGUExamValueWithDefault : Maybe GUExamValue -> GUExamForm -> Maybe GUExamValue
toGUExamValueWithDefault saved form =
    guExamFormWithDefault form saved
        |> toGUExamValue


toGUExamValue : GUExamForm -> Maybe GUExamValue
toGUExamValue form =
    let
        maybeGUExamSigns =
            [ Maybe.map (ifTrue EpisiotomyOrPerinealTear) form.episiotomyOrPerinealTear
            , Maybe.map (ifTrue RectalHemorrhoids) form.rectalHemorrhoids
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoGUExamSigns)
    in
    Maybe.map2
        (\vaginalExamSigns guExamSigns ->
            let
                postpartumHealingProblems =
                    Maybe.andThen
                        (\healingNormally ->
                            if healingNormally then
                                Just <| EverySet.singleton NormalPostpartumHealing

                            else
                                Maybe.map EverySet.fromList form.postpartumHealingProblems
                        )
                        form.healingNormally
            in
            { vaginalExamSigns = EverySet.fromList vaginalExamSigns
            , guExamSigns = guExamSigns
            , postpartumHealingProblems = postpartumHealingProblems
            }
        )
        form.vaginalExamSigns
        maybeGUExamSigns


guExamFormInputsAndTasks : Language -> AssembledData -> GUExamForm -> ( List (Html Msg), List (Maybe Bool) )
guExamFormInputsAndTasks language assembled form =
    let
        ( initialSection, initialTasks ) =
            let
                episiotomyOrPerinealTeareUpdateFunc value form_ =
                    { form_
                        | episiotomyOrPerinealTear = Just value
                        , healingNormally = Nothing
                        , healingNormallyDirty = True
                        , postpartumHealingProblems = Nothing
                        , postpartumHealingProblemsDirty = True
                    }
            in
            ( [ div [ class "ui grid" ]
                    [ div [ class "twelve wide column" ]
                        [ viewLabel language Translate.VaginalExamination ]
                    ]
              , viewCheckBoxMultipleSelectInput language
                    [ FoulSmellingLochia, ExcessiveVaginalBleeding ]
                    [ NormalVaginalExam ]
                    (form.vaginalExamSigns |> Maybe.withDefault [])
                    Nothing
                    SetVaginalExamSign
                    Translate.VaginalExamSign
              , div [ class "separator double" ] []
              , viewCustomLabel language Translate.EpisiotomyOrPerinealTearQuestion "?" "label question"
              , viewBoolInput
                    language
                    form.episiotomyOrPerinealTear
                    (SetGUExamBoolInput episiotomyOrPerinealTeareUpdateFunc)
                    "episiotomy"
                    Nothing
              ]
            , [ maybeToBoolTask form.vaginalExamSigns
              , form.episiotomyOrPerinealTear
              ]
            )

        ( derivedSection, derivedTasks ) =
            if isNothing form.episiotomyOrPerinealTear then
                ( [], [] )

            else if form.episiotomyOrPerinealTear == Just True then
                let
                    ( healingProblemsSection, healingProblemsTasks ) =
                        if form.healingNormally == Just False then
                            ( [ viewCustomLabel language Translate.PostpartumHealingProblemQuestion "?" "label question"
                              , viewCheckBoxMultipleSelectInput language
                                    [ HealingProblemSwelling, HealingProblemDischarge, HealingProblemReleaseOfSutures ]
                                    [ HealingProblemHematoma, HealingProblemBruising ]
                                    (form.postpartumHealingProblems |> Maybe.withDefault [])
                                    Nothing
                                    SetPostpartumHealingProblem
                                    Translate.PostpartumHealingProblem
                              , div [ class "separator double" ] []
                              ]
                            , [ maybeToBoolTask form.postpartumHealingProblems ]
                            )

                        else
                            ( [], [] )

                    healingNormallyUpdateFunc value form_ =
                        { form_
                            | healingNormally = Just value
                            , healingNormallyDirty = True
                            , postpartumHealingProblems = Nothing
                            , postpartumHealingProblemsDirty = True
                        }
                in
                ( [ viewCustomLabel language Translate.EpisiotomyOrPerinealTearHealingQuestion "?" "label question"
                  , viewBoolInput
                        language
                        form.healingNormally
                        (SetGUExamBoolInput healingNormallyUpdateFunc)
                        "healing-normally"
                        Nothing
                  ]
                    ++ healingProblemsSection
                    ++ rectalHemorrhoidsSection
                , [ form.healingNormally ] ++ healingProblemsTasks ++ rectalHemorrhoidsTasks
                )

            else
                ( rectalHemorrhoidsSection, rectalHemorrhoidsTasks )

        ( rectalHemorrhoidsSection, rectalHemorrhoidsTasks ) =
            let
                rectalHemorrhoidsUpdateFunc value form_ =
                    { form_ | rectalHemorrhoids = Just value }
            in
            ( [ viewCustomLabel language Translate.RectalHemorrhoids "?" "label question"
              , viewBoolInput
                    language
                    form.rectalHemorrhoids
                    (SetGUExamBoolInput rectalHemorrhoidsUpdateFunc)
                    "rectal-hemorrhoids"
                    Nothing
              ]
            , [ form.rectalHemorrhoids ]
            )
    in
    ( initialSection ++ derivedSection, initialTasks ++ derivedTasks )


resolveReferralInputsAndTasksForCHW :
    Language
    -> NominalDate
    -> AssembledData
    -> ReferralForm
    -> ( List (Html Msg), List (Maybe Bool) )
resolveReferralInputsAndTasksForCHW language currentDate assembled form =
    let
        ( derivedSection, derivedTasks ) =
            Maybe.map
                (\referToHealthCenter ->
                    if referToHealthCenter then
                        ( [ viewQuestionLabel language Translate.HandedReferralFormQuestion
                          , viewBoolInput
                                language
                                form.handReferralForm
                                (SetReferralBoolInput
                                    (\value form_ ->
                                        { form_ | handReferralForm = Just value }
                                    )
                                )
                                "hand-referral-form"
                                Nothing
                          , viewQuestionLabel language <| Translate.AccompanyToFacilityQuestion FacilityHealthCenter
                          , viewBoolInput
                                language
                                form.accompanyToHealthCenter
                                (SetReferralBoolInput
                                    (\value form_ ->
                                        { form_ | accompanyToHealthCenter = Just value }
                                    )
                                )
                                "accompany-to-hc"
                                Nothing
                          ]
                        , [ form.handReferralForm, form.accompanyToHealthCenter ]
                        )

                    else
                        ( [ div [ class "why-not" ]
                                [ viewQuestionLabel language Translate.WhyNot
                                , viewCheckBoxSelectInput language
                                    [ ClientRefused
                                    , NoAmbulance
                                    , ClientUnableToAffordFees
                                    , ReasonForNonReferralNotIndicated
                                    , ReasonForNonReferralOther
                                    ]
                                    []
                                    form.reasonForNotSendingToHC
                                    SetHealthCenterNonReferralReason
                                    Translate.ReasonForNonReferral
                                ]
                          ]
                        , [ maybeToBoolTask form.reasonForNotSendingToHC ]
                        )
                )
                form.referToHealthCenter
                |> Maybe.withDefault ( [], [] )
    in
    ( [ h2 [] [ text <| translate language Translate.ActionsToTake ++ ":" ]
      , div [ class "instructions" ]
            [ viewActionTakenLabel language (Translate.CompleteFacilityReferralForm FacilityHealthCenter) "icon-forms" Nothing
            , viewActionTakenLabel language (Translate.SendPatientToFacility FacilityHealthCenter) "icon-shuttle" Nothing
            ]
      , viewQuestionLabel language <| Translate.ReferredPatientToFacilityQuestion FacilityHealthCenter
      , viewBoolInput
            language
            form.referToHealthCenter
            (SetReferralBoolInput
                (\value form_ ->
                    { form_ | referToHealthCenter = Just value, reasonForNotSendingToHC = Nothing }
                )
            )
            "refer-to-hc"
            Nothing
      ]
        ++ derivedSection
    , [ form.referToHealthCenter ] ++ derivedTasks
    )


resolveReferralInputsAndTasksForNurse :
    Language
    -> NominalDate
    -> AssembledData
    -> ((Bool -> ReferralForm -> ReferralForm) -> Bool -> msg)
    -> (Maybe ReasonForNonReferral -> ReferralFacility -> ReasonForNonReferral -> msg)
    -> ReferralForm
    -> ( List (Html msg), List (Maybe Bool) )
resolveReferralInputsAndTasksForNurse language currentDate assembled setReferralBoolInputMsg setNonReferralReasonMsg form =
    let
        foldResults =
            List.foldr
                (\( inputs, tasks ) ( accumInputs, accumTasks ) ->
                    ( inputs ++ accumInputs, tasks ++ accumTasks )
                )
                ( [], [] )
    in
    resolveRequiredReferralFacilities assembled
        |> List.map (resolveReferralToFacilityInputsAndTasks language currentDate PrenatalEncounterPhaseInitial assembled setReferralBoolInputMsg setNonReferralReasonMsg form)
        |> foldResults


resolveRequiredReferralFacilities : AssembledData -> List ReferralFacility
resolveRequiredReferralFacilities assembled =
    List.filter (matchRequiredReferralFacility assembled) referralFacilities


matchRequiredReferralFacility : AssembledData -> ReferralFacility -> Bool
matchRequiredReferralFacility assembled facility =
    case facility of
        FacilityHospital ->
            referToHospital assembled

        FacilityMentalHealthSpecialist ->
            referToMentalHealthSpecialist assembled

        FacilityARVProgram ->
            referToARVProgram assembled

        FacilityNCDProgram ->
            referredToSpecialityCareProgram EnrolledToNCDProgram assembled

        FacilityANCServices ->
            -- Explicit NCD facility.
            False

        FacilityUltrasound ->
            referToUltrasound assembled

        FacilityHealthCenter ->
            -- We should never get here. HC inputs are resolved
            -- with resolveReferralInputsAndTasksForCHW.
            False


referralFacilities : List ReferralFacility
referralFacilities =
    [ FacilityHospital
    , FacilityMentalHealthSpecialist
    , FacilityARVProgram
    , FacilityNCDProgram
    , FacilityUltrasound
    ]


specialityCareFormWithDefault : SpecialityCareForm -> Maybe SpecialityCareValue -> SpecialityCareForm
specialityCareFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { enrolledToARVProgram = or form.enrolledToARVProgram (EverySet.member EnrolledToARVProgram value |> Just)
                , enrolledToNCDProgram = or form.enrolledToNCDProgram (EverySet.member EnrolledToNCDProgram value |> Just)
                }
            )


toSpecialityCareValueWithDefault : Maybe SpecialityCareValue -> SpecialityCareForm -> Maybe SpecialityCareValue
toSpecialityCareValueWithDefault saved form =
    specialityCareFormWithDefault form saved
        |> toSpecialityCareValue


toSpecialityCareValue : SpecialityCareForm -> Maybe SpecialityCareValue
toSpecialityCareValue form =
    [ ifNullableTrue EnrolledToARVProgram form.enrolledToARVProgram
    , ifNullableTrue EnrolledToNCDProgram form.enrolledToNCDProgram
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoSpecialityCareSigns)


lmpRangeToString : LmpRange -> String
lmpRangeToString range =
    case range of
        OneMonth ->
            "one-month"

        ThreeMonth ->
            "three-month"

        SixMonth ->
            "six-month"


lmpRangeFromString : String -> Maybe LmpRange
lmpRangeFromString s =
    case s of
        "one-month" ->
            Just OneMonth

        "three-month" ->
            Just ThreeMonth

        "six-month" ->
            Just SixMonth

        _ ->
            Nothing
