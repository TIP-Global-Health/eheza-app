module Pages.Prenatal.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (PrenatalEncounterId)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils
    exposing
        ( diabetesBySugarCount
        , diabetesByUrineGlucose
        , getHeightValue
        , getMeasurementValueFunc
        , muacValueFunc
        , weightValueFunc
        )
import Backend.Person.Model exposing (Person)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType(..), PrenatalIndicator(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Backend.PrenatalEncounter.Utils exposing (isNurseEncounter)
import Date exposing (Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, diffDays, diffWeeks, diffYears, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List.Extra
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model
    exposing
        ( InvokationModule(..)
        , LaboratoryTask(..)
        , MedicationAdministrationFormConfig
        , VitalsFormConfig
        , VitalsFormMode(..)
        )
import Measurement.Utils
    exposing
        ( corePhysicalExamFormWithDefault
        , getNextVaccineDose
        , isTestResultValid
        , medicationAdministrationFormInputsAndTasks
        , medicationAdministrationFormWithDefault
        , resolveLabTestDate
        , testPerformedByExecutionNote
        , vaccinationFormWithDefault
        , vaccineDoseToComparable
        , vitalsFormWithDefault
        )
import Measurement.View exposing (viewActionTakenLabel, vitalsFormInputsAndTasks)
import Pages.Prenatal.Activity.Model exposing (..)
import Pages.Prenatal.Activity.Types exposing (..)
import Pages.Prenatal.Encounter.Utils exposing (calculateBmi, emergencyReferalRequired, generateGravida, generatePara, getAllActivities)
import Pages.Prenatal.Model exposing (AssembledData, HealthEducationForm, PrenatalEncounterPhase(..), ReferralForm, VaccinationProgressDict)
import Pages.Prenatal.Utils exposing (..)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeToBoolTask
        , maybeValueConsideringIsDirtyField
        , resolveTasksCompletedFromTotal
        , taskAllCompleted
        , taskCompleted
        , valueConsideringIsDirtyField
        , viewBoolInput
        , viewCheckBoxMultipleSelectInput
        , viewCheckBoxSelectInput
        , viewCustomBoolInput
        , viewCustomLabel
        , viewLabel
        , viewQuestionLabel
        )
import SyncManager.Model exposing (Site)
import Translate exposing (translate)
import Translate.Model exposing (Language(..))
import Utils.Html exposing (viewModal)
import ZScore.Model
import ZScore.Utils exposing (viewZScore, zScoreBmiForAge)


expectActivity : NominalDate -> Site -> AssembledData -> PrenatalActivity -> Bool
expectActivity currentDate site assembled activity =
    let
        noNurseEncounters =
            nurseEncounterNotPerformed assembled
    in
    case assembled.encounter.encounterType of
        -- Note that for nurse it's always used after
        -- Pages.Prenatal.Encounter.Utils.getAllActivities, which supplies
        -- different activities, depending if nurse encounter was performed
        -- previously, or not.
        NurseEncounter ->
            case activity of
                PregnancyDating ->
                    noNurseEncounters

                History ->
                    resolveHistoryTasks assembled
                        |> List.isEmpty
                        |> not

                Examination ->
                    True

                FamilyPlanning ->
                    True

                Backend.PrenatalActivity.Model.MalariaPrevention ->
                    expectMalariaPreventionActivity PhaseInitial assembled

                Backend.PrenatalActivity.Model.Medication ->
                    resolveMedicationTasks currentDate assembled
                        |> List.isEmpty
                        |> not

                DangerSigns ->
                    True

                Laboratory ->
                    -- Always True, as there are sub activities that should be
                    -- presented on every encounter.
                    True

                PrenatalPhoto ->
                    expectPrenatalPhoto currentDate assembled

                NextSteps ->
                    mandatoryActivitiesForNextStepsCompleted currentDate site assembled
                        && (resolveNextStepsTasks currentDate assembled
                                |> List.isEmpty
                                |> not
                           )

                SymptomReview ->
                    True

                PrenatalTreatmentReview ->
                    not noNurseEncounters

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
                    mandatoryActivitiesForNextStepsCompleted currentDate site assembled
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
                    noNurseEncounters

                Laboratory ->
                    -- Do not show, if patient already visited health center.
                    noNurseEncounters

                DangerSigns ->
                    True

                Backend.PrenatalActivity.Model.HealthEducation ->
                    activityCompleted currentDate site assembled DangerSigns
                        && noDangerSigns assembled

                NextSteps ->
                    mandatoryActivitiesForNextStepsCompleted currentDate site assembled

                -- Activities that do not participate at CHW encounter 1.
                _ ->
                    False

        ChwSecondEncounter ->
            case activity of
                DangerSigns ->
                    True

                BirthPlan ->
                    activityCompleted currentDate site assembled DangerSigns
                        && noDangerSigns assembled

                Backend.PrenatalActivity.Model.HealthEducation ->
                    activityCompleted currentDate site assembled DangerSigns
                        && noDangerSigns assembled

                NextSteps ->
                    mandatoryActivitiesForNextStepsCompleted currentDate site assembled

                -- Activities that do not participate at CHW encounter 2.
                _ ->
                    False

        ChwThirdPlusEncounter ->
            case activity of
                DangerSigns ->
                    True

                Backend.PrenatalActivity.Model.HealthEducation ->
                    activityCompleted currentDate site assembled DangerSigns
                        && noDangerSigns assembled

                NextSteps ->
                    mandatoryActivitiesForNextStepsCompleted currentDate site assembled

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
                    mandatoryActivitiesForNextStepsCompleted currentDate site assembled

                -- Activities that do not participate at CHW Postpartum encounter.
                _ ->
                    False


activityCompleted : NominalDate -> Site -> AssembledData -> PrenatalActivity -> Bool
activityCompleted currentDate site assembled activity =
    case activity of
        PregnancyDating ->
            isJust assembled.measurements.lastMenstrualPeriod

        History ->
            resolveHistoryTasks assembled
                |> List.all (historyTaskCompleted assembled)

        Examination ->
            resolveExaminationTasks assembled
                |> List.all (examinationTaskCompleted assembled)

        FamilyPlanning ->
            isJust assembled.measurements.familyPlanning

        Backend.PrenatalActivity.Model.MalariaPrevention ->
            isJust assembled.measurements.malariaPrevention

        Backend.PrenatalActivity.Model.Medication ->
            resolveMedicationTasks currentDate assembled
                |> List.all (medicationTaskCompleted assembled)

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
            (not <| expectActivity currentDate site assembled PrenatalImmunisation)
                || List.all (immunisationTaskCompleted currentDate assembled) immunisationVaccinationTasks

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
                    [ NextStepsHealthEducation, NextStepsMedicationDistribution, NextStepsSendToHC, NextStepsNextVisit, NextStepsWait ]

                NursePostpartumEncounter ->
                    [ NextStepsHealthEducation, NextStepsMedicationDistribution, NextStepsSendToHC ]

                _ ->
                    -- The order is important. Do not change.
                    [ NextStepsAppointmentConfirmation, NextStepsSendToHC, NextStepsFollowUp, NextStepsHealthEducation, NextStepsNextVisit, NextStepsNewbornEnrolment ]
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
                    let
                        triggeringDiagnoses =
                            [ DiagnosisHeartburn
                            , DiagnosisCandidiasis
                            , DiagnosisGonorrhea
                            , DiagnosisTrichomonasOrBacterialVaginosis
                            , DiagnosisHIVDetectableViralLoadInitialPhase
                            ]
                                ++ diabetesDiagnosesInitialPhase
                    in
                    -- Emergency referral is not required.
                    (not <| emergencyReferalRequired assembled)
                        && (provideHIVEducation PrenatalEncounterPhaseInitial assembled.measurements
                                || provideHIVPartnerPresenceEducation assembled.measurements
                                || provideNauseaAndVomitingEducation assembled
                                || List.any (symptomRecorded assembled.measurements)
                                    [ LegCramps, LowBackPain, Constipation, VaricoseVeins ]
                                || provideLegPainRednessEducation assembled
                                || providePelvicPainEducation assembled
                                || diagnosedAnyOf triggeringDiagnoses assembled
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
                           )

                ChwPostpartumEncounter ->
                    True

                _ ->
                    False

        -- Exclusive CHW task.
        NextStepsNewbornEnrolment ->
            (assembled.encounter.encounterType == ChwPostpartumEncounter)
                && liveChildBorn assembled.participant.outcome

        -- Exclusive task for Nurse.
        NextStepsMedicationDistribution ->
            case assembled.encounter.encounterType of
                NurseEncounter ->
                    -- Emergency referral is not required.
                    (not <| emergencyReferalRequired assembled)
                        && ((resolveRequiredMedicationsSet English currentDate PrenatalEncounterPhaseInitial assembled
                                |> List.isEmpty
                                |> not
                            )
                                || (diagnosedMalariaByPhase PrenatalEncounterPhaseInitial assembled
                                        && (not <| referToHospitalDueToAdverseEventForMalariaTreatment assembled)
                                   )
                                || diagnosedSyphilisByPhase PrenatalEncounterPhaseInitial assembled
                                || diagnosedHypertension PrenatalEncounterPhaseInitial assembled
                                || diagnosedAnyOf
                                    [ DiagnosisHeartburn
                                    , DiagnosisUrinaryTractInfection
                                    , DiagnosisCandidiasis
                                    , DiagnosisGonorrhea
                                    , DiagnosisTrichomonasOrBacterialVaginosis
                                    , DiagnosisModerateAnemiaInitialPhase
                                    ]
                                    assembled
                                || continuousHypertensionTreatmentRequired assembled
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
                   -- test that was performed, or, 2 hours waiting is required
                   -- for blood pressure recheck during initial nurse encounter.
                   (getMeasurementValueFunc assembled.measurements.labsResults
                        |> Maybe.map
                            (\value ->
                                EverySet.diff value.performedTests value.completedTests
                                    |> EverySet.isEmpty
                                    |> not
                            )
                        |> Maybe.withDefault False
                   )

        NextStepsNextVisit ->
            List.member assembled.encounter.encounterType
                [ NurseEncounter
                , ChwFirstEncounter
                , ChwSecondEncounter
                , ChwThirdPlusEncounter
                ]


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
                medicationDistributionCompleted =
                    let
                        medicationDistributionRequired =
                            resolveRequiredMedicationsSet English currentDate PrenatalEncounterPhaseInitial assembled
                                |> List.isEmpty
                                |> not
                    in
                    if medicationDistributionRequired then
                        let
                            allowedSigns =
                                NoMedicationDistributionSignsInitialPhase :: medicationsInitialPhase
                        in
                        medicationDistributionMeasurementTaken allowedSigns assembled.measurements

                    else
                        True

                malariaTreatmentCompleted =
                    if diagnosedMalariaByPhase PrenatalEncounterPhaseInitial assembled then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForMalaria assembled.measurements

                    else
                        True

                syphilisTreatmentCompleted =
                    if diagnosedSyphilisByPhase PrenatalEncounterPhaseInitial assembled then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForSyphilis assembled.measurements

                    else
                        True

                heartburnTreatmentCompleted =
                    if diagnosed DiagnosisHeartburn assembled then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForHeartburn assembled.measurements

                    else
                        True

                hypertensionTreatmentCompleted =
                    if
                        -- Hypertension diagnosed at current encounter.
                        diagnosedHypertension PrenatalEncounterPhaseInitial assembled
                            || -- Hypertension diagnosed previously and we
                               -- need to continue treatment.
                               continuousHypertensionTreatmentRequired assembled
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

                anemiaTreatmentCompleted =
                    if diagnosed DiagnosisModerateAnemiaInitialPhase assembled then
                        reinforceTreatmentSignsCompleted assembled.measurements

                    else
                        True
            in
            medicationDistributionCompleted
                && malariaTreatmentCompleted
                && syphilisTreatmentCompleted
                && hypertensionTreatmentCompleted
                && heartburnTreatmentCompleted
                && candidiasisTreatmentCompleted
                && urinaryTractInfectionTreatmentCompleted
                && mastitisTreatmentCompleted
                && anemiaTreatmentCompleted

        NextStepsWait ->
            getMeasurementValueFunc assembled.measurements.labsResults
                |> Maybe.map .patientNotified
                |> Maybe.withDefault False

        NextStepsNextVisit ->
            isJust assembled.encounter.nextVisitDate


continuousHypertensionTreatmentRequired : AssembledData -> Bool
continuousHypertensionTreatmentRequired assembled =
    (-- Given treatment to Hypertension / Moderate Preeclampsia, which needs updating.
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
    )
        && (-- If Preeclampsia was diagnosed at current
            -- encounter, there's no need to medicate, because
            -- patient is sent to hospital anyway.
            not <|
                diagnosedAnyOf
                    [ DiagnosisModeratePreeclampsiaInitialPhase
                    , DiagnosisSeverePreeclampsiaInitialPhase
                    ]
                    assembled
           )


resolveMedicationTasks : NominalDate -> AssembledData -> List MedicationTask
resolveMedicationTasks currentDate assembled =
    List.filter (expectMedicationTask currentDate assembled)
        [ TaskMMS, TaskFefol, TaskFolate, TaskIron, TaskCalcium, TaskMebendazole ]


expectMedicationTask : NominalDate -> AssembledData -> MedicationTask -> Bool
expectMedicationTask currentDate assembled task =
    let
        egaAboveOrEqualWeek week =
            Maybe.map
                (\lmpDate ->
                    let
                        egaInWeeks =
                            calculateEGAWeeks currentDate lmpDate
                    in
                    egaInWeeks >= week
                )
                assembled.globalLmpDate
                |> Maybe.withDefault False

        medicationNotAdministeredToday getMeasurementFunc =
            getMeasurementFunc assembled.measurements
                |> getMeasurementValueFunc
                |> Maybe.map ((/=) AdministeredToday)
                |> Maybe.withDefault True

        medicationNeverAdministeredPreviously getMeasurementFunc =
            List.filter
                (.measurements
                    >> getMeasurementFunc
                    >> getMeasurementValueFunc
                    >> Maybe.map ((==) AdministeredToday)
                    >> Maybe.withDefault False
                )
                assembled.nursePreviousEncountersData
                |> List.isEmpty
    in
    case task of
        TaskCalcium ->
            medicationNeverAdministeredPreviously .calcium
                && egaAboveOrEqualWeek 14

        TaskFefol ->
            medicationNeverAdministeredPreviously .fefol
                && medicationNeverAdministeredPreviously .iron
                && medicationNeverAdministeredPreviously .folate
                && medicationNotAdministeredToday .iron
                && medicationNotAdministeredToday .folate

        TaskFolate ->
            medicationNeverAdministeredPreviously .folate
                && medicationNeverAdministeredPreviously .fefol
                && medicationNotAdministeredToday .fefol

        TaskIron ->
            medicationNeverAdministeredPreviously .iron
                && medicationNeverAdministeredPreviously .fefol
                && medicationNotAdministeredToday .fefol

        TaskMMS ->
            medicationNeverAdministeredPreviously .mms

        TaskMebendazole ->
            medicationNeverAdministeredPreviously .mebendazole
                && egaAboveOrEqualWeek 24


medicationTaskCompleted : AssembledData -> MedicationTask -> Bool
medicationTaskCompleted assembled task =
    case task of
        TaskCalcium ->
            isJust assembled.measurements.calcium

        TaskFefol ->
            isJust assembled.measurements.fefol

        TaskFolate ->
            isJust assembled.measurements.folate

        TaskIron ->
            isJust assembled.measurements.iron

        TaskMMS ->
            isJust assembled.measurements.mms

        TaskMebendazole ->
            isJust assembled.measurements.mebendazole


resolveTreatmentReviewTasks : AssembledData -> List TreatmentReviewTask
resolveTreatmentReviewTasks assembled =
    List.filter (expectTreatmentReviewTask assembled)
        [ TreatmentReviewPrenatalMedication
        , TreatmentReviewHIV
        , TreatmentReviewHypertension
        , TreatmentReviewMalaria
        , TreatmentReviewAnemia
        , TreatmentReviewSyphilis
        ]


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
            -- Requirements are not to present this task anymore.
            -- See https://github.com/TIP-Global-Health/eheza-app/issues/1323.
            False

        OutsideCare ->
            not firstEnconter


historyTaskCompleted : AssembledData -> HistoryTask -> Bool
historyTaskCompleted assembled task =
    case task of
        Obstetric ->
            let
                obstetricHistoryValue =
                    getMeasurementValueFunc assembled.measurements.obstetricHistory
            in
            if skipObstetricHistorySecondStep obstetricHistoryValue then
                isJust assembled.measurements.obstetricHistory

            else
                isJust assembled.measurements.obstetricHistory
                    && isJust assembled.measurements.obstetricHistoryStep2

        Medical ->
            isJust assembled.measurements.medicalHistory

        Social ->
            isJust assembled.measurements.socialHistory

        OutsideCare ->
            isJust assembled.measurements.outsideCare


skipObstetricHistorySecondStep : Maybe ObstetricHistoryValue -> Bool
skipObstetricHistorySecondStep obstetricHistoryValue =
    let
        gravida =
            Maybe.map generateGravida obstetricHistoryValue
                |> Maybe.withDefault ""

        para =
            Maybe.map generatePara obstetricHistoryValue
                |> Maybe.withDefault ""
    in
    gravida == "01" && para == "0000"


resolveExaminationTasks : AssembledData -> List ExaminationTask
resolveExaminationTasks assembled =
    List.filter (expectExaminationTask assembled)
        [ Vitals, NutritionAssessment, CorePhysicalExam, ObstetricalExam, BreastExam, GUExam ]


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
            if
                List.any (\diagnosis -> EverySet.member diagnosis data.diagnoses)
                    [ DiagnosisHIVInitialPhase, DiagnosisHIVRecurrentPhase ]
            then
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


referToMentalHealthSpecialist : AssembledData -> Bool
referToMentalHealthSpecialist assembled =
    mentalHealthSpecialistAtHC assembled && diagnosedAnyOf mentalHealthDiagnosesRequiringTreatment assembled


referToARVProgram : AssembledData -> Bool
referToARVProgram assembled =
    (diagnosed DiagnosisHIVInitialPhase assembled && hivProgramAtHC assembled.measurements)
        || (expectSpecialityCareSignSection assembled EnrolledToARVProgram
                && referredToSpecialityCareProgram EnrolledToARVProgram assembled
           )


referToUltrasound : AssembledData -> Bool
referToUltrasound assembled =
    getMeasurementValueFunc assembled.measurements.lastMenstrualPeriod
        |> Maybe.map
            (\value ->
                (value.confident == False)
                    && isJust value.notConfidentReason
            )
        |> Maybe.withDefault False


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


mandatoryActivitiesForAssessmentCompleted : NominalDate -> Site -> AssembledData -> Bool
mandatoryActivitiesForAssessmentCompleted currentDate site assembled =
    case assembled.encounter.encounterType of
        NurseEncounter ->
            activityCompleted currentDate site assembled DangerSigns

        NursePostpartumEncounter ->
            True

        _ ->
            mandatoryActivitiesForNextStepsCompleted currentDate site assembled


mandatoryActivitiesForNextStepsCompleted : NominalDate -> Site -> AssembledData -> Bool
mandatoryActivitiesForNextStepsCompleted currentDate site assembled =
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
                |> List.filter (expectActivity currentDate site assembled)
                |> List.all (activityCompleted currentDate site assembled)
    in
    case assembled.encounter.encounterType of
        NurseEncounter ->
            -- If we have emergency diagnosis that require immediate referral,
            -- we allow displaying Next steps right away.
            diagnosedAnyOf emergencyReferralDiagnoses assembled
                || -- Otherwise, we need all activities that will appear at
                   -- current encounter completed, besides Photo
                   -- and Next Steps itself.
                   mandatoryActivitiesForNurseCompleted

        NursePostpartumEncounter ->
            mandatoryActivitiesForNurseCompleted

        ChwFirstEncounter ->
            let
                commonMandatoryActivitiesCompleted =
                    ((not <| expectActivity currentDate site assembled PregnancyDating)
                        || activityCompleted currentDate site assembled PregnancyDating
                    )
                        && ((not <| expectActivity currentDate site assembled Laboratory)
                                || activityCompleted currentDate site assembled Laboratory
                           )
                        && activityCompleted currentDate site assembled DangerSigns
            in
            if dangerSignsPresent assembled then
                commonMandatoryActivitiesCompleted

            else
                commonMandatoryActivitiesCompleted
                    && activityCompleted currentDate site assembled Backend.PrenatalActivity.Model.HealthEducation

        ChwSecondEncounter ->
            let
                commonMandatoryActivitiesCompleted =
                    activityCompleted currentDate site assembled DangerSigns
            in
            if dangerSignsPresent assembled then
                commonMandatoryActivitiesCompleted

            else
                commonMandatoryActivitiesCompleted
                    && activityCompleted currentDate site assembled BirthPlan
                    && activityCompleted currentDate site assembled Backend.PrenatalActivity.Model.HealthEducation

        ChwThirdPlusEncounter ->
            let
                commonMandatoryActivitiesCompleted =
                    activityCompleted currentDate site assembled DangerSigns
            in
            if dangerSignsPresent assembled then
                commonMandatoryActivitiesCompleted

            else
                commonMandatoryActivitiesCompleted
                    && activityCompleted currentDate site assembled Backend.PrenatalActivity.Model.HealthEducation

        ChwPostpartumEncounter ->
            activityCompleted currentDate site assembled PregnancyOutcome
                && activityCompleted currentDate site assembled DangerSigns


expectPrenatalPhoto : NominalDate -> AssembledData -> Bool
expectPrenatalPhoto currentDate assembled =
    assembled.globalLmpDate
        |> Maybe.map
            (\lmpDate ->
                let
                    periods =
                        -- Periods, where we want to have 1 photo:
                        --  1. 12 weeks, or less.
                        --  2. Between week 13 and week 27.
                        --  3. Week 28, or more.
                        [ [ (>) 13 ], [ (>) 28, (<=) 13 ], [ (<=) 28 ] ]

                    currentWeek =
                        calculateEGAWeeks currentDate lmpDate

                    conditionsForCurrentWeek =
                        periods
                            |> List.filter
                                (\periodConditions ->
                                    List.all (\condition -> condition currentWeek) periodConditions
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
                                            List.all (\condition -> condition encounterWeek) conditions
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
        byCurrent =
            getMeasurementValueFunc assembled.measurements.nutrition
                |> Maybe.map .height
    in
    Maybe.Extra.or byCurrent (resolvePreviouslyMeasuredHeight assembled)


resolvePreviouslyMeasuredHeight : AssembledData -> Maybe HeightInCm
resolvePreviouslyMeasuredHeight assembled =
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


resolvePrePregnancyWeight : AssembledData -> Maybe WeightInKg
resolvePrePregnancyWeight assembled =
    let
        resolveWeight measurements =
            getMeasurementValueFunc measurements.lastMenstrualPeriod
                |> Maybe.andThen .prePregnancyWeight

        byCurrent =
            resolveWeight assembled.measurements

        byNurse =
            List.filterMap (.measurements >> resolveWeight)
                assembled.nursePreviousEncountersData
                |> List.head

        byCHW =
            List.filterMap
                (\( _, _, measurements ) ->
                    resolveWeight measurements
                )
                assembled.chwPreviousMeasurementsWithDates
                |> List.head
    in
    Maybe.Extra.or byNurse byCHW
        |> Maybe.Extra.or byCurrent


{-| Used for patients bellow 19 years of age.
-}
zscoreToPrePregnancyClassification : Float -> PrePregnancyClassification
zscoreToPrePregnancyClassification zscore =
    if zscore < -2 then
        PrePregnancyUnderWeight

    else if zscore <= 1 then
        PrePregnancyNormal

    else if zscore <= 2 then
        PrePregnancyOverweight

    else
        PrePregnancyObesity


{-| Used for patients of 19 years of age and above.
-}
bmiToPrePregnancyClassification : Float -> PrePregnancyClassification
bmiToPrePregnancyClassification bmi =
    if bmi < 18.5 then
        PrePregnancyUnderWeight

    else if bmi < 25 then
        PrePregnancyNormal

    else if bmi < 30 then
        PrePregnancyOverweight

    else
        PrePregnancyObesity


resolvePrePregnancyClassification : ZScore.Model.Model -> AssembledData -> Maybe Float -> Maybe PrePregnancyClassification
resolvePrePregnancyClassification zscores assembled prePregnancyBmi =
    Maybe.Extra.andThen3
        (\bmi lmpDate birthDate ->
            let
                ageInYearsOnLMP =
                    diffYears birthDate lmpDate
            in
            if ageInYearsOnLMP < 19 then
                let
                    ageInDaysOnLMP =
                        diffDays birthDate lmpDate
                in
                zScoreBmiForAge zscores (ZScore.Model.Days ageInDaysOnLMP) assembled.person.gender (ZScore.Model.BMI bmi)
                    |> Maybe.andThen (viewZScore >> String.toFloat)
                    |> Maybe.map zscoreToPrePregnancyClassification

            else
                Just <| bmiToPrePregnancyClassification bmi
        )
        prePregnancyBmi
        assembled.globalLmpDate
        assembled.person.birthDate


resolveGWGClassification : NominalDate -> PrePregnancyClassification -> Float -> Float -> AssembledData -> Maybe GWGClassification
resolveGWGClassification currentDate prePregnancyClassification prePregnancyWeight currentWeight assembled =
    Maybe.map
        (\lmpDate ->
            let
                egaInWeeks =
                    calculateEGAWeeks currentDate lmpDate

                actualWeightGain =
                    currentWeight - prePregnancyWeight

                expectedWeightGain =
                    let
                        weeksAfterFirstTrimester =
                            egaInWeeks - 12

                        ( forFirstTrimester, perWeek ) =
                            weightGainStandardsPerPrePregnancyClassification prePregnancyClassification
                    in
                    if weeksAfterFirstTrimester <= 0 then
                        forFirstTrimester

                    else
                        forFirstTrimester + toFloat weeksAfterFirstTrimester * perWeek

                relation =
                    actualWeightGain / expectedWeightGain
            in
            if relation < 0.7 then
                GWGSeverelyInadequate

            else if relation < 0.9 then
                GWGInadequate

            else if relation <= 1.25 then
                GWGAdequate

            else
                GWGExcessive
        )
        assembled.globalLmpDate


weightGainStandardsPerPrePregnancyClassification : PrePregnancyClassification -> ( Float, Float )
weightGainStandardsPerPrePregnancyClassification prePregnancyClassification =
    case prePregnancyClassification of
        PrePregnancyUnderWeight ->
            ( 2, 0.51 )

        PrePregnancyNormal ->
            ( 1, 0.42 )

        PrePregnancyOverweight ->
            ( 1, 0.28 )

        PrePregnancyObesity ->
            ( 0.5, 0.22 )


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
            if assembled.encounter.encounterType == NursePostpartumEncounter then
                -- There are not emergency diagnoses during Postpartum encounter.
                EverySet.empty

            else
                List.filter
                    (matchEmergencyReferalPrenatalDiagnosis
                        egaInWeeks
                        dangerSignsList
                        assembled
                    )
                    emergencyReferralDiagnoses
                    |> EverySet.fromList

        diagnosesByLabResultsAndExamination =
            resolveLabResultsAndExaminationDiagnoses currentDate assembled
                |> List.filter (matchLabResultsAndExaminationPrenatalDiagnosis egaInWeeks dangerSignsList assembled)
                |> EverySet.fromList

        diagnosesBySymptoms =
            resolveSymptomsDiagnoses assembled.encounter.encounterType
                |> List.filter (matchSymptomsPrenatalDiagnosis egaInWeeks assembled)
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

        diagnosedAtInitalPhase =
            matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs assembled
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
                            && moderatePreeclampsiaByMeasurementsInitialPhase measurements
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
                    not <| diagnosedAtInitalPhase DiagnosisModeratePreeclampsiaInitialPhaseEGA37Plus
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
                        && severePreeclampsiaInitialPhase signs measurements
                )

        DiagnosisSeverePreeclampsiaRecurrentPhaseEGA37Plus ->
            (-- If diagnosed Severe Preeclampsia at initial stage, we do not
             -- need to diagnose again.
             not <| diagnosedAtInitalPhase DiagnosisSeverePreeclampsiaInitialPhaseEGA37Plus
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

        DiagnosisSevereAnemiaWithComplicationsInitialPhase ->
            severeAnemiaWithComplicationsDiagnosed signs assembled.measurements
                && labTestWithImmediateResult .hemoglobinTest assembled.measurements

        DiagnosisSevereAnemiaWithComplicationsRecurrentPhase ->
            (not <| diagnosedAtInitalPhase DiagnosisSevereAnemiaWithComplicationsInitialPhase)
                && severeAnemiaWithComplicationsDiagnosed signs assembled.measurements

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

        immediateResult getMeasurementFunc =
            labTestWithImmediateResult getMeasurementFunc measurements

        hemoglobinCount =
            resolveHemoglobinCount measurements

        malariaConditionsMatch =
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

        malariaWithAnemiaConditionsMatch =
            positiveMalariaTest
                && (-- Hemoglobin test was performed, and,
                    -- hemoglobin count indicates mild to moderate anemia.
                    Maybe.map (\count -> count >= 7 && count < 11) hemoglobinCount
                        |> Maybe.withDefault False
                   )

        diabetesDiagnosedInitialPhase =
            getMeasurementValueFunc measurements.randomBloodSugarTest
                |> Maybe.map
                    (\value ->
                        let
                            bySugarCount =
                                diabetesBySugarCount value
                                    && immediateResult .randomBloodSugarTest

                            byUrineGlucose =
                                if testPerformedByExecutionNote value.executionNote then
                                    -- If random blood sugar test was perfomed, we determine by its results.
                                    False

                                else
                                    -- If random blood sugar test was not perfomed, we determine by
                                    -- glucose level at urine dipstick test.
                                    (getMeasurementValueFunc measurements.urineDipstickTest
                                        |> Maybe.map diabetesByUrineGlucose
                                        |> Maybe.withDefault False
                                    )
                                        && immediateResult .urineDipstickTest
                        in
                        bySugarCount || byUrineGlucose
                    )
                |> Maybe.withDefault False

        diabetesDiagnosedAnyPhase =
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

        hivDetectableViralLoadDiagnosed =
            getMeasurementValueFunc measurements.hivPCRTest
                |> Maybe.andThen
                    (.hivViralLoad >> Maybe.map (\viralLoad -> viralLoad >= 20))
                |> Maybe.withDefault False

        discordantPartnershipDiagnosed =
            let
                byHIVTest =
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

                byPartnerHIVTest =
                    let
                        patientHIVNegative =
                            getMeasurementValueFunc measurements.hivTest
                                |> Maybe.map
                                    (\value ->
                                        List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]
                                            && (value.testResult == Just TestNegative)
                                    )
                                |> Maybe.withDefault False
                    in
                    patientHIVNegative
                        && (getMeasurementValueFunc measurements.partnerHIVTest
                                |> Maybe.map
                                    (\value ->
                                        if
                                            (value.executionNote == TestNoteKnownAsPositive)
                                                || (List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]
                                                        && (value.testResult == Just TestPositive)
                                                   )
                                        then
                                            Maybe.map
                                                (\hivSigns ->
                                                    (-- Partner is not taking ARVs.
                                                     (not <| EverySet.member PartnerTakingARV hivSigns)
                                                        || -- Partner is taking ARVs, but did not
                                                           -- reach surpressed viral load.
                                                           (EverySet.member PartnerTakingARV hivSigns
                                                                && (not <| EverySet.member PartnerSurpressedViralLoad hivSigns)
                                                           )
                                                    )
                                                )
                                                value.hivSigns
                                                |> Maybe.withDefault False

                                        else
                                            False
                                    )
                                |> Maybe.withDefault False
                           )
            in
            byPartnerHIVTest || byHIVTest

        syphilisDiagnosed =
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

        syphilisWithComplicationDiagnosed =
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

        neurosyphilisDiagnosed =
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

        malariaDiagnosed =
            malariaConditionsMatch
                && ((isNothing <| latestMedicationTreatmentForMalaria assembled)
                        || (not <| diagnosedPreviouslyAnyOf [ DiagnosisMalariaInitialPhase, DiagnosisMalariaRecurrentPhase ] assembled)
                   )

        malariaMedicatedContinuedDiagnosed =
            malariaConditionsMatch
                && (isJust <| latestMedicationTreatmentForMalaria assembled)
                && diagnosedPreviouslyAnyOf [ DiagnosisMalariaInitialPhase, DiagnosisMalariaRecurrentPhase ] assembled

        malariaWithAnemiaDiagnosed =
            malariaWithAnemiaConditionsMatch
                && ((isNothing <| latestMedicationTreatmentForMalaria assembled)
                        || (not <|
                                diagnosedPreviouslyAnyOf
                                    [ DiagnosisMalariaWithAnemiaInitialPhase
                                    , DiagnosisMalariaWithAnemiaRecurrentPhase
                                    ]
                                    assembled
                           )
                   )

        malariaWithAnemiaMedicatedContinuedDiagnosed =
            malariaWithAnemiaConditionsMatch
                && (isJust <| latestMedicationTreatmentForMalaria assembled)
                && diagnosedPreviouslyAnyOf
                    [ DiagnosisMalariaWithAnemiaInitialPhase
                    , DiagnosisMalariaWithAnemiaRecurrentPhase
                    , DiagnosisMalariaWithSevereAnemiaInitialPhase
                    , DiagnosisMalariaWithSevereAnemiaRecurrentPhase
                    ]
                    assembled

        malariaWithSevereAnemiaDiagnosed =
            positiveMalariaTest
                && (-- Hemoglobin test was performed, and,
                    -- hemoglobin count indicates severe anemia.
                    Maybe.map (\count -> count < 7) hemoglobinCount
                        |> Maybe.withDefault False
                   )

        moderateAnemiaDiagnosed =
            not positiveMalariaTest
                && (-- No indication for being positive for malaria,
                    -- Hemoglobin test was performed, and, hemoglobin
                    -- count indicates mild to moderate anemia.
                    Maybe.map (\count -> count >= 7 && count < 11) hemoglobinCount
                        |> Maybe.withDefault False
                   )

        severeAnemiaDiagnosed =
            not positiveMalariaTest
                && (-- No indication for being positive for malaria,
                    -- Hemoglobin test was performed, and, hemoglobin
                    -- count indicates severe anemia.
                    Maybe.map (\count -> count < 7) hemoglobinCount
                        |> Maybe.withDefault False
                   )
                && (not <| anemiaComplicationSignsPresent dangerSigns measurements)

        rhesusNegativeDiagnosed =
            getMeasurementValueFunc measurements.bloodGpRsTest
                |> Maybe.andThen .rhesus
                |> Maybe.map ((==) RhesusNegative)
                |> Maybe.withDefault False

        moderateRiskPreeclampsiaDiagnosed =
            let
                byAgeAndFirstPregnancy =
                    let
                        ageInYears =
                            Maybe.map2
                                (Date.diff Years)
                                assembled.person.birthDate
                                assembled.globalLmpDate

                        totalPregnancies =
                            List.filterMap (.obstetricHistory >> getMeasurementValueFunc)
                                allMeasurements
                                |> List.head
                                |> Maybe.map
                                    (\value ->
                                        value.termPregnancy + value.preTermPregnancy
                                    )
                    in
                    Maybe.map2
                        (\ageYears pregnanciesCount ->
                            (pregnanciesCount == 0)
                                && (ageYears < 18 || ageYears >= 35)
                        )
                        ageInYears
                        totalPregnancies
                        |> Maybe.withDefault False

                byBMI =
                    calculateBmi
                        (resolveMeasuredHeight assembled
                            |> Maybe.map getHeightValue
                        )
                        (getMeasurementValueFunc assembled.measurements.nutrition
                            |> Maybe.map (.weight >> weightValueFunc)
                        )
                        |> Maybe.map (\bmi -> bmi >= 30)
                        |> Maybe.withDefault False

                byPreeclampsiaInFamily =
                    List.filterMap (.medicalHistory >> getMeasurementValueFunc)
                        allMeasurements
                        |> List.head
                        |> Maybe.map (.preeclampsiaInFamily >> (==) DoesOccur)
                        |> Maybe.withDefault False

                byStillbornChildren =
                    List.filterMap (.obstetricHistory >> getMeasurementValueFunc)
                        allMeasurements
                        |> List.head
                        |> Maybe.map
                            (\value ->
                                value.stillbirthsAtTerm + value.stillbirthsPreTerm > 0
                            )
                        |> Maybe.withDefault False

                byObstetricHistorySigns =
                    List.filterMap (.obstetricHistoryStep2 >> getMeasurementValueFunc)
                        allMeasurements
                        |> List.head
                        |> Maybe.map
                            (\value ->
                                let
                                    byPeriodFromPreviousPregnancy =
                                        EverySet.member MoreThan10Years value.previousDeliveryPeriod

                                    bySigns =
                                        List.any
                                            (\sign ->
                                                EverySet.member sign value.signs
                                            )
                                            [ ObstetricHistoryPlacentaAbruptionPreviousDelivery
                                            , ObstetricHistoryChildWithLowBirthweightPreviousDelivery
                                            , ObstetricHistorySmallForGestationalAgePreviousDelivery
                                            , ObstetricHistoryIntraUterineDeathPreviousDelivery
                                            ]
                                in
                                byPeriodFromPreviousPregnancy || bySigns
                            )
                        |> Maybe.withDefault False
            in
            byAgeAndFirstPregnancy
                || byBMI
                || byPreeclampsiaInFamily
                || byStillbornChildren
                || byObstetricHistorySigns

        highRiskPreeclampsiaDiagnosedBySignsAndPreviousDiagnoses =
            let
                byPreviousPreeclampsia =
                    List.filterMap (.obstetricHistoryStep2 >> getMeasurementValueFunc)
                        allMeasurements
                        |> List.head
                        |> Maybe.map
                            (\value ->
                                EverySet.member ObstetricHistoryPreeclampsiaPreviousPregnancy value.signs
                            )
                        |> Maybe.withDefault False

                byFetalPresentation =
                    List.filterMap (.obstetricalExam >> getMeasurementValueFunc)
                        allMeasurements
                        |> List.head
                        |> Maybe.map (.fetalPresentation >> (==) Twins)
                        |> Maybe.withDefault False

                byMedicalHistorySigns =
                    List.filterMap (.medicalHistory >> getMeasurementValueFunc)
                        allMeasurements
                        |> List.head
                        |> Maybe.map
                            (\value ->
                                List.any
                                    (\sign ->
                                        EverySet.member sign value.signs
                                    )
                                    [ HypertensionBeforePregnancy
                                    , Diabetes
                                    , RenalDisease
                                    , AutoimmuneDisease
                                    ]
                            )
                        |> Maybe.withDefault False

                byPreviousHypertensionDiagnoses =
                    diagnosedHypertensionPrevoiusly assembled

                byPreviousDiabetesDiagnoses =
                    diagnosedDiabetesPrevoiusly assembled
            in
            byPreviousPreeclampsia
                || byFetalPresentation
                || byMedicalHistorySigns
                || byPreviousHypertensionDiagnoses
                || byPreviousDiabetesDiagnoses

        allMeasurements =
            assembled.measurements
                :: (List.map .measurements assembled.nursePreviousEncountersData |> List.reverse)

        resolveEGAWeeksAndThen func =
            resolveEGAInWeeksAndThen func egaInWeeks

        diagnosedAtInitalPhase =
            matchLabResultsAndExaminationPrenatalDiagnosis egaInWeeks dangerSigns assembled
    in
    case diagnosis of
        DiagnosisModerateRiskOfPreeclampsia ->
            moderateRiskPreeclampsiaDiagnosed
                && (not <| diagnosedRiskOfPreeclampsiaPrevoiusly assembled)
                -- We don't diagnose Preeclampsia RISK, if any kind of
                -- Preeclampsia was diagnosed previously, or at current encounter.
                && (not <|
                        diagnosedPreviouslyAnyOf
                            (DiagnosisEclampsia :: preeclampsiaDiagnoses)
                            assembled
                   )
                && (not <|
                        List.any (matchLabResultsAndExaminationPrenatalDiagnosis egaInWeeks dangerSigns assembled)
                            preeclampsiaDiagnoses
                   )

        DiagnosisHighRiskOfPreeclampsiaInitialPhase ->
            (highRiskPreeclampsiaDiagnosedBySignsAndPreviousDiagnoses
                || -- Hypertension or Diabetes diagnoses were diagnosed during current encounter.
                   List.any (matchLabResultsAndExaminationPrenatalDiagnosis egaInWeeks dangerSigns assembled)
                    (hypertensionDiagnoses ++ diabetesDiagnoses)
            )
                && (not <| diagnosedHighRiskOfPreeclampsiaPrevoiusly assembled)
                -- We don't diagnose Preeclampsia RISK, if any kind of
                -- Preeclampsia was diagnosed previously, or at current encounter.
                && (not <|
                        diagnosedPreviouslyAnyOf
                            (DiagnosisEclampsia :: preeclampsiaDiagnoses)
                            assembled
                   )
                && (not <|
                        List.any (matchLabResultsAndExaminationPrenatalDiagnosis egaInWeeks dangerSigns assembled)
                            preeclampsiaDiagnoses
                   )

        DiagnosisHighRiskOfPreeclampsiaRecurrentPhase ->
            (not <| diagnosedAtInitalPhase DiagnosisHighRiskOfPreeclampsiaInitialPhase)
                && (highRiskPreeclampsiaDiagnosedBySignsAndPreviousDiagnoses
                        || -- Hypertension or Diabetes diagnoses were diagnosed during current encounter.
                           List.any (matchLabResultsAndExaminationPrenatalDiagnosis egaInWeeks dangerSigns assembled)
                            (hypertensionDiagnoses ++ diabetesDiagnoses)
                   )
                -- We don't diagnose Preeclampsia RISK, if any kind of
                -- Preeclampsia was diagnosed previously, or at current encounter.
                && (not <| diagnosedHighRiskOfPreeclampsiaPrevoiusly assembled)
                && (not <|
                        diagnosedPreviouslyAnyOf
                            (DiagnosisEclampsia :: preeclampsiaDiagnoses)
                            assembled
                   )
                && (not <|
                        List.any (matchLabResultsAndExaminationPrenatalDiagnosis egaInWeeks dangerSigns assembled)
                            preeclampsiaDiagnoses
                   )

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
                            && moderatePreeclampsiaByMeasurementsInitialPhase measurements
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
                   (not <| diagnosedAtInitalPhase DiagnosisModeratePreeclampsiaInitialPhase)
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
                        && severePreeclampsiaInitialPhase dangerSigns measurements
                )

        DiagnosisSeverePreeclampsiaRecurrentPhase ->
            (-- If diagnosed Severe Preeclampsia at initial stage, we do not
             -- need to diagnose again.
             not <| diagnosedAtInitalPhase DiagnosisSeverePreeclampsiaInitialPhase
            )
                && resolveEGAWeeksAndThen
                    (\egaWeeks ->
                        (egaWeeks < 37)
                            && severePreeclampsiaRecurrentPhase dangerSigns measurements
                    )

        DiagnosisHIVInitialPhase ->
            testedPositiveAt .hivTest && immediateResult .hivTest

        DiagnosisHIVRecurrentPhase ->
            testedPositiveAt .hivTest
                && (not <| diagnosedAtInitalPhase DiagnosisHIVInitialPhase)

        DiagnosisHIVDetectableViralLoadInitialPhase ->
            hivDetectableViralLoadDiagnosed && immediateResult .hivPCRTest

        DiagnosisHIVDetectableViralLoadRecurrentPhase ->
            hivDetectableViralLoadDiagnosed
                && (not <| diagnosedAtInitalPhase DiagnosisHIVDetectableViralLoadInitialPhase)

        DiagnosisDiscordantPartnershipInitialPhase ->
            discordantPartnershipDiagnosed && immediateResult .hivTest

        DiagnosisDiscordantPartnershipRecurrentPhase ->
            discordantPartnershipDiagnosed
                && (not <| diagnosedAtInitalPhase DiagnosisDiscordantPartnershipInitialPhase)

        DiagnosisSyphilisInitialPhase ->
            syphilisDiagnosed && immediateResult .syphilisTest

        DiagnosisSyphilisRecurrentPhase ->
            syphilisDiagnosed
                && (not <| diagnosedAtInitalPhase DiagnosisSyphilisInitialPhase)

        DiagnosisSyphilisWithComplicationsInitialPhase ->
            syphilisWithComplicationDiagnosed && immediateResult .syphilisTest

        DiagnosisSyphilisWithComplicationsRecurrentPhase ->
            syphilisWithComplicationDiagnosed
                && (not <| diagnosedAtInitalPhase DiagnosisSyphilisWithComplicationsInitialPhase)

        DiagnosisNeurosyphilisInitialPhase ->
            neurosyphilisDiagnosed && immediateResult .syphilisTest

        DiagnosisNeurosyphilisRecurrentPhase ->
            neurosyphilisDiagnosed
                && (not <| diagnosedAtInitalPhase DiagnosisNeurosyphilisInitialPhase)

        DiagnosisHepatitisBInitialPhase ->
            testedPositiveAt .hepatitisBTest && immediateResult .hepatitisBTest

        DiagnosisHepatitisBRecurrentPhase ->
            testedPositiveAt .hepatitisBTest
                && (not <| diagnosedAtInitalPhase DiagnosisHepatitisBInitialPhase)

        DiagnosisMalariaInitialPhase ->
            malariaDiagnosed
                && immediateResult .malariaTest

        DiagnosisMalariaRecurrentPhase ->
            malariaDiagnosed
                && (not <| diagnosedAtInitalPhase DiagnosisMalariaInitialPhase)

        DiagnosisMalariaMedicatedContinuedInitialPhase ->
            malariaMedicatedContinuedDiagnosed && immediateResult .malariaTest

        DiagnosisMalariaMedicatedContinuedRecurrentPhase ->
            malariaMedicatedContinuedDiagnosed
                && (not <| diagnosedAtInitalPhase DiagnosisMalariaMedicatedContinuedInitialPhase)

        DiagnosisMalariaWithAnemiaInitialPhase ->
            malariaWithAnemiaDiagnosed
                && immediateResult .malariaTest
                && immediateResult .hemoglobinTest

        DiagnosisMalariaWithAnemiaRecurrentPhase ->
            malariaWithAnemiaDiagnosed
                && (not <| diagnosedAtInitalPhase DiagnosisMalariaWithAnemiaInitialPhase)

        DiagnosisMalariaWithAnemiaMedicatedContinuedInitialPhase ->
            malariaWithAnemiaMedicatedContinuedDiagnosed
                && immediateResult .malariaTest
                && immediateResult .hemoglobinTest

        DiagnosisMalariaWithAnemiaMedicatedContinuedRecurrentPhase ->
            malariaWithAnemiaMedicatedContinuedDiagnosed
                && (not <| diagnosedAtInitalPhase DiagnosisMalariaWithAnemiaMedicatedContinuedInitialPhase)

        DiagnosisMalariaWithSevereAnemiaInitialPhase ->
            malariaWithSevereAnemiaDiagnosed
                && immediateResult .malariaTest
                && immediateResult .hemoglobinTest

        DiagnosisMalariaWithSevereAnemiaRecurrentPhase ->
            malariaWithSevereAnemiaDiagnosed
                && (not <| diagnosedAtInitalPhase DiagnosisMalariaWithSevereAnemiaInitialPhase)

        DiagnosisModerateAnemiaInitialPhase ->
            moderateAnemiaDiagnosed && immediateResult .hemoglobinTest

        DiagnosisModerateAnemiaRecurrentPhase ->
            moderateAnemiaDiagnosed
                && (not <| diagnosedAtInitalPhase DiagnosisModerateAnemiaInitialPhase)

        DiagnosisSevereAnemiaInitialPhase ->
            severeAnemiaDiagnosed && immediateResult .hemoglobinTest

        DiagnosisSevereAnemiaRecurrentPhase ->
            severeAnemiaDiagnosed
                && (not <| diagnosedAtInitalPhase DiagnosisSevereAnemiaInitialPhase)

        Backend.PrenatalEncounter.Types.DiagnosisDiabetesInitialPhase ->
            (not <| diagnosedPreviouslyAnyOf diabetesDiagnoses assembled)
                && resolveEGAWeeksAndThen
                    (\egaWeeks ->
                        egaWeeks <= 20 && diabetesDiagnosedInitialPhase
                    )

        Backend.PrenatalEncounter.Types.DiagnosisDiabetesRecurrentPhase ->
            (not <| diagnosedAtInitalPhase Backend.PrenatalEncounter.Types.DiagnosisDiabetesInitialPhase)
                && (not <| diagnosedPreviouslyAnyOf diabetesDiagnoses assembled)
                && resolveEGAWeeksAndThen
                    (\egaWeeks ->
                        egaWeeks <= 20 && diabetesDiagnosedAnyPhase
                    )

        Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetesInitialPhase ->
            (not <| diagnosedPreviouslyAnyOf diabetesDiagnoses assembled)
                && resolveEGAWeeksAndThen
                    (\egaWeeks ->
                        egaWeeks > 20 && diabetesDiagnosedInitialPhase
                    )

        Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetesRecurrentPhase ->
            (not <| diagnosedAtInitalPhase Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetesInitialPhase)
                && (not <| diagnosedPreviouslyAnyOf diabetesDiagnoses assembled)
                && resolveEGAWeeksAndThen
                    (\egaWeeks ->
                        egaWeeks > 20 && diabetesDiagnosedAnyPhase
                    )

        DiagnosisRhesusNegativeInitialPhase ->
            rhesusNegativeDiagnosed && immediateResult .bloodGpRsTest

        DiagnosisRhesusNegativeRecurrentPhase ->
            rhesusNegativeDiagnosed
                && (not <| diagnosedAtInitalPhase DiagnosisRhesusNegativeInitialPhase)

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


moderatePreeclampsiaByMeasurementsInitialPhase : PrenatalMeasurements -> Bool
moderatePreeclampsiaByMeasurementsInitialPhase measurements =
    let
        highProtein =
            highUrineProteinInitialPhase measurements
    in
    highBloodPressure measurements
        && (highProtein || edemaOnHandOrLegs measurements)


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


severePreeclampsiaInitialPhase : List DangerSign -> PrenatalMeasurements -> Bool
severePreeclampsiaInitialPhase dangerSigns measurements =
    let
        byBloodPressure =
            getMeasurementValueFunc measurements.vitals
                |> Maybe.andThen
                    (\value ->
                        Maybe.map2
                            (\dia sys -> dia >= 110 && sys >= 160)
                            value.dia
                            value.sys
                    )
                |> Maybe.withDefault False
    in
    List.member HeadacheBlurredVision dangerSigns
        || (byBloodPressure
                && highUrineProtein measurements
                && severePreeclampsiaSigns measurements
           )


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


highUrineProteinInitialPhase : PrenatalMeasurements -> Bool
highUrineProteinInitialPhase measurements =
    getMeasurementValueFunc measurements.urineDipstickTest
        |> Maybe.andThen
            (\value ->
                let
                    highProtein =
                        Maybe.map
                            (\protein ->
                                List.member protein
                                    [ ProteinPlus1
                                    , ProteinPlus2
                                    , ProteinPlus3
                                    , ProteinPlus4
                                    ]
                            )
                            value.protein

                    immediateResult =
                        Maybe.map (EverySet.member PrerequisiteImmediateResult)
                            value.testPrerequisites
                in
                Maybe.map2 (&&)
                    highProtein
                    immediateResult
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


emergencyObstetricCareServicesDiagnoses : List PrenatalDiagnosis
emergencyObstetricCareServicesDiagnoses =
    [ DiagnosisEclampsia
    , DiagnosisMiscarriage
    , DiagnosisMolarPregnancy
    , DiagnosisPlacentaPrevia
    , DiagnosisPlacentalAbruption
    , DiagnosisUterineRupture
    , DiagnosisObstructedLabor
    , DiagnosisPostAbortionSepsis
    , DiagnosisEctopicPregnancy
    , DiagnosisPPROM
    , DiagnosisHyperemesisGravidum
    , DiagnosisMaternalComplications

    -- Infection diagnosis will be available at latter phase.
    -- , DiagnosisInfection
    ]


resolveLabResultsAndExaminationDiagnoses : NominalDate -> AssembledData -> List PrenatalDiagnosis
resolveLabResultsAndExaminationDiagnoses currentDate assembled =
    case assembled.encounter.encounterType of
        NursePostpartumEncounter ->
            [ DiagnosisPostpartumEarlyMastitisOrEngorgment
            , DiagnosisPostpartumMastitis
            , DiagnosisPostpartumInfection
            , DiagnosisPostpartumExcessiveBleeding
            ]

        _ ->
            let
                preeclampsiaRiskDiagnoses =
                    Maybe.map
                        (\lmpDate ->
                            let
                                egaInWeeks =
                                    calculateEGAWeeks currentDate lmpDate
                            in
                            if egaInWeeks >= 12 then
                                -- Preeclampsia risk diagnoses appear from EGA week 12.
                                [ DiagnosisModerateRiskOfPreeclampsia
                                , DiagnosisHighRiskOfPreeclampsiaInitialPhase
                                , DiagnosisHighRiskOfPreeclampsiaRecurrentPhase
                                ]

                            else
                                []
                        )
                        assembled.globalLmpDate
                        |> Maybe.withDefault []
            in
            preeclampsiaRiskDiagnoses
                ++ [ DiagnosisChronicHypertensionImmediate
                   , DiagnosisChronicHypertensionAfterRecheck
                   , DiagnosisGestationalHypertensionImmediate
                   , DiagnosisGestationalHypertensionAfterRecheck
                   , DiagnosisModeratePreeclampsiaInitialPhase
                   , DiagnosisModeratePreeclampsiaRecurrentPhase
                   , DiagnosisSeverePreeclampsiaInitialPhase
                   , DiagnosisSeverePreeclampsiaRecurrentPhase
                   , DiagnosisHIVInitialPhase
                   , DiagnosisHIVRecurrentPhase
                   , DiagnosisHIVDetectableViralLoadInitialPhase
                   , DiagnosisHIVDetectableViralLoadRecurrentPhase
                   , DiagnosisDiscordantPartnershipInitialPhase
                   , DiagnosisDiscordantPartnershipRecurrentPhase
                   , DiagnosisSyphilisInitialPhase
                   , DiagnosisSyphilisRecurrentPhase
                   , DiagnosisSyphilisWithComplicationsInitialPhase
                   , DiagnosisSyphilisWithComplicationsRecurrentPhase
                   , DiagnosisNeurosyphilisInitialPhase
                   , DiagnosisNeurosyphilisRecurrentPhase
                   , DiagnosisHepatitisBInitialPhase
                   , DiagnosisHepatitisBRecurrentPhase
                   , DiagnosisMalariaInitialPhase
                   , DiagnosisMalariaRecurrentPhase
                   , DiagnosisMalariaMedicatedContinuedInitialPhase
                   , DiagnosisMalariaMedicatedContinuedRecurrentPhase
                   , DiagnosisMalariaWithAnemiaInitialPhase
                   , DiagnosisMalariaWithAnemiaRecurrentPhase
                   , DiagnosisMalariaWithAnemiaMedicatedContinuedInitialPhase
                   , DiagnosisMalariaWithAnemiaMedicatedContinuedRecurrentPhase
                   , DiagnosisMalariaWithSevereAnemiaInitialPhase
                   , DiagnosisMalariaWithSevereAnemiaRecurrentPhase
                   , DiagnosisModerateAnemiaInitialPhase
                   , DiagnosisModerateAnemiaRecurrentPhase
                   , DiagnosisSevereAnemiaInitialPhase
                   , DiagnosisSevereAnemiaRecurrentPhase
                   , DiagnosisSevereAnemiaWithComplicationsInitialPhase
                   , DiagnosisSevereAnemiaWithComplicationsRecurrentPhase
                   , Backend.PrenatalEncounter.Types.DiagnosisDiabetesInitialPhase
                   , Backend.PrenatalEncounter.Types.DiagnosisDiabetesRecurrentPhase
                   , Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetesInitialPhase
                   , Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetesRecurrentPhase
                   , DiagnosisRhesusNegativeInitialPhase
                   , DiagnosisRhesusNegativeRecurrentPhase
                   ]


resolveSymptomsDiagnoses : PrenatalEncounterType -> List PrenatalDiagnosis
resolveSymptomsDiagnoses encounterType =
    case encounterType of
        NursePostpartumEncounter ->
            [ DiagnosisPostpartumAbdominalPain
            , DiagnosisPostpartumUrinaryIncontinence
            , DiagnosisPostpartumHeadache
            , DiagnosisPostpartumFatigue
            , DiagnosisPostpartumFever
            , DiagnosisPostpartumPerinealPainOrDischarge
            ]

        _ ->
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
            ]


mentalHealthDiagnoses : List PrenatalDiagnosis
mentalHealthDiagnoses =
    DiagnosisDepressionNotLikely :: mentalHealthDiagnosesRequiringTreatment


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
                ( _, tasks ) =
                    getMeasurementValueFunc assembled.measurements.appointmentConfirmation
                        |> appointmentConfirmationFormWithDefault data.appointmentConfirmationForm
                        |> appointmentConfirmationFormInutsAndTasks language currentDate
            in
            resolveTasksCompletedFromTotal tasks

        NextStepsFollowUp ->
            let
                ( _, tasks ) =
                    getMeasurementValueFunc assembled.measurements.followUp
                        |> followUpFormWithDefault data.followUpForm
                        |> followUpFormInutsAndTasks language currentDate
            in
            resolveTasksCompletedFromTotal tasks

        NextStepsSendToHC ->
            let
                form =
                    getMeasurementValueFunc assembled.measurements.sendToHC
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
            resolveTasksCompletedFromTotal tasks

        NextStepsHealthEducation ->
            let
                ( _, tasks ) =
                    getMeasurementValueFunc assembled.measurements.healthEducation
                        |> healthEducationFormWithDefault data.healthEducationForm
                        |> healthEducationFormInputsAndTasks language assembled
            in
            resolveTasksCompletedFromTotal tasks

        NextStepsNewbornEnrolment ->
            ( taskCompleted assembled.participant.newborn
            , 1
            )

        NextStepsMedicationDistribution ->
            let
                ( _, completed, total ) =
                    getMeasurementValueFunc assembled.measurements.medicationDistribution
                        |> medicationDistributionFormWithDefaultInitialPhase data.medicationDistributionForm
                        |> resolveMedicationDistributionInputsAndTasks language
                            currentDate
                            PrenatalEncounterPhaseInitial
                            assembled
                            SetMedicationDistributionBoolInput
                            SetMedicationDistributionAdministrationNote
                            SetRecommendedTreatmentSign
                            SetAvoidingGuidanceReason
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

        NextStepsNextVisit ->
            let
                completed =
                    if nextStepsTaskCompleted currentDate assembled NextStepsNextVisit then
                        1

                    else
                        0
            in
            ( completed
            , 1
            )


appointmentConfirmationFormInutsAndTasks :
    Language
    -> NominalDate
    -> AppointmentConfirmationForm
    -> ( List (Html Msg), List (Maybe Bool) )
appointmentConfirmationFormInutsAndTasks language currentDate form =
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
    ( [ viewLabel language Translate.AppointmentConfirmationInstrunction
      , div
            [ class "form-input date"
            , onClick <| SetAppointmentDateSelectorState (Just dateSelectorConfig)
            ]
            [ text appointmentDateForView ]
      , viewModal <| viewCalendarPopup language form.dateSelectorPopupState form.appointmentDate
      ]
    , [ maybeToBoolTask form.appointmentDate ]
    )


followUpFormInutsAndTasks : Language -> NominalDate -> FollowUpForm -> ( List (Html Msg), List (Maybe Bool) )
followUpFormInutsAndTasks language currentDate form =
    ( [ viewLabel language Translate.FollowUpWithMotherLabel
      , viewCheckBoxSelectInput language
            [ ThreeDays
            , Backend.Measurement.Model.OneMonth
            , TwoMonths
            , Backend.Measurement.Model.ThreeMonths
            , FollowUpNotNeeded
            ]
            []
            form.option
            SetFollowUpOption
            Translate.FollowUpOption
      ]
    , [ maybeToBoolTask form.option ]
    )


healthEducationFormInputsAndTasks : Language -> AssembledData -> HealthEducationForm -> ( List (Html Msg), List (Maybe Bool) )
healthEducationFormInputsAndTasks language assembled healthEducationForm =
    let
        form =
            getMeasurementValueFunc assembled.measurements.healthEducation
                |> healthEducationFormWithDefault healthEducationForm
    in
    case assembled.encounter.encounterType of
        NurseEncounter ->
            healthEducationFormInputsAndTasksForNurse language
                PrenatalEncounterPhaseInitial
                SetHealthEducationSubActivityBoolInput
                assembled
                form

        NursePostpartumEncounter ->
            healthEducationFormInputsAndTasksForNurse language
                PrenatalEncounterPhaseInitial
                SetHealthEducationSubActivityBoolInput
                assembled
                form

        _ ->
            healthEducationFormInputsAndTasksForChw language assembled form


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
            let
                secondEnconterInputs =
                    [ hemorrhagingInput ]

                secondEnconterTasks =
                    [ form.hemorrhaging ]
            in
            ( List.concat <| inputsFromFirst ++ secondEnconterInputs
            , tasksFromFirst ++ secondEnconterTasks
            )

        ChwThirdPlusEncounter ->
            -- Second encounter tasks reappear at third encounter anyway,
            -- so, we do not need to add them explicitly.
            let
                familyPlanningInput =
                    healthEducationFormFamilyPlanningInput language SetHealthEducationBoolInput True form

                thirdEnconterInputs =
                    [ hemorrhagingInput, familyPlanningInput, breastfeedingInput ]

                thirdEnconterTasks =
                    [ form.hemorrhaging, form.familyPlanning, form.breastfeeding ]
            in
            ( List.concat <| inputsFromFirst ++ thirdEnconterInputs
            , tasksFromFirst ++ thirdEnconterTasks
            )

        ChwPostpartumEncounter ->
            let
                ( outcomeDependantInputs, outcomeDependantTasks ) =
                    if liveChildBorn assembled.participant.outcome then
                        let
                            immunizationInput =
                                [ viewQuestionLabel language <| translatePrenatalHealthEducationQuestion EducationImmunization
                                , viewBoolInput
                                    language
                                    form.immunization
                                    (setBoolInputMsg immunizationUpdateFunc)
                                    "immunization"
                                    Nothing
                                ]

                            immunizationUpdateFunc value form_ =
                                { form_ | immunization = Just value }
                        in
                        ( [ breastfeedingInput, immunizationInput ]
                        , [ form.breastfeeding, form.immunization ]
                        )

                    else
                        let
                            griefUpdateFunc value form_ =
                                { form_ | grief = Just value }
                        in
                        ( [ [ viewQuestionLabel language <| translatePrenatalHealthEducationQuestion EducationGrief
                            , viewBoolInput
                                language
                                form.grief
                                (setBoolInputMsg griefUpdateFunc)
                                "grief"
                                Nothing
                            ]
                          ]
                        , [ form.grief ]
                        )

                hygieneInput =
                    [ viewQuestionLabel language <| translatePrenatalHealthEducationQuestion EducationHygiene
                    , viewBoolInput
                        language
                        form.hygiene
                        (setBoolInputMsg hygieneUpdateFunc)
                        "hygiene"
                        Nothing
                    ]

                hygieneUpdateFunc value form_ =
                    { form_ | hygiene = Just value }

                postpartumEnconterInputs =
                    outcomeDependantInputs ++ [ hygieneInput ]

                postpartumEnconterTasks =
                    outcomeDependantTasks ++ [ form.hygiene ]
            in
            ( List.concat postpartumEnconterInputs
            , postpartumEnconterTasks
            )

        -- We should never get here, as function is only for CHW.
        NurseEncounter ->
            ( [], [] )

        -- We should never get here, as function is only for CHW.
        NursePostpartumEncounter ->
            ( [], [] )


liveChildBorn : Maybe Backend.IndividualEncounterParticipant.Model.IndividualEncounterParticipantOutcome -> Bool
liveChildBorn =
    Maybe.map
        (\outcome ->
            case outcome of
                Backend.IndividualEncounterParticipant.Model.Pregnancy pregnancyOutcome ->
                    List.member pregnancyOutcome
                        [ Backend.IndividualEncounterParticipant.Model.OutcomeLiveAtTerm
                        , Backend.IndividualEncounterParticipant.Model.OutcomeLivePreTerm
                        ]

                _ ->
                    False
        )
        >> Maybe.withDefault False


resolveLastRecordedValue : AssembledData -> (PrenatalMeasurements -> Maybe ( id, PrenatalMeasurement a )) -> (a -> b) -> Maybe b
resolveLastRecordedValue assembled measurementFunc valueFunc =
    (assembled.measurements
        :: (List.map .measurements assembled.nursePreviousEncountersData |> List.reverse)
    )
        |> List.filterMap (measurementFunc >> Maybe.map (Tuple.second >> .value >> valueFunc))
        |> List.head


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
                { lmpDate = or form.lmpDate (Just value.date)
                , prePregnancyWeight =
                    maybeValueConsideringIsDirtyField form.prePregnancyWeightDirty
                        form.prePregnancyWeight
                        (Maybe.map weightValueFunc value.prePregnancyWeight)
                , prePregnancyWeightDirty = form.prePregnancyWeightDirty
                , lmpDateConfident = or form.lmpDateConfident (Just value.confident)
                , lmpDateNotConfidentReason = or form.lmpDateNotConfidentReason value.notConfidentReason
                , lateFirstVisitReason = or form.lateFirstVisitReason value.lateFirstVisitReason
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
        |> andMap (Just <| Maybe.map WeightInKg form.prePregnancyWeight)
        |> andMap form.lmpDateConfident
        |> andMap (Just form.lmpDateNotConfidentReason)
        |> andMap (Just form.lateFirstVisitReason)
        |> andMap (Just chwLmpConfirmation)


medicalHistoryFormWithDefault : MedicalHistoryForm -> Maybe MedicalHistoryValue -> MedicalHistoryForm
medicalHistoryFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (Just <| EverySet.toList value.signs)
                , physicalConditions = or form.physicalConditions (Just <| EverySet.toList value.physicalConditions)
                , infectiousDiseases = or form.infectiousDiseases (Just <| EverySet.toList value.infectiousDiseases)
                , mentalHealthIssues = or form.mentalHealthIssues (Just <| EverySet.toList value.mentalHealthIssues)
                , preeclampsiaInFamily = or form.preeclampsiaInFamily (Just value.preeclampsiaInFamily)
                }
            )


toMedicalHistoryValueWithDefault : Maybe MedicalHistoryValue -> MedicalHistoryForm -> Maybe MedicalHistoryValue
toMedicalHistoryValueWithDefault saved form =
    medicalHistoryFormWithDefault form saved
        |> toMedicalHistoryValue


toMedicalHistoryValue : MedicalHistoryForm -> Maybe MedicalHistoryValue
toMedicalHistoryValue form =
    Maybe.map5 MedicalHistoryValue
        (Maybe.map EverySet.fromList form.signs)
        (Maybe.map EverySet.fromList form.physicalConditions)
        (Maybe.map EverySet.fromList form.infectiousDiseases)
        (Maybe.map EverySet.fromList form.mentalHealthIssues)
        form.preeclampsiaInFamily


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
                , Maybe.map (EverySet.singleton >> Just) form.hivMedicationNotGivenReason
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
            getMeasurementValueFunc assembled.measurements.medication
                |> medicationFormWithDefault data.medicationForm

        ( _, tasks ) =
            case task of
                TreatmentReviewPrenatalMedication ->
                    resolvePrenatalMedicationFormInputsAndTasks language
                        currentDate
                        SetMedicationSubActivityBoolInput
                        assembled
                        form

                _ ->
                    resolveMedicationTreatmentFormInputsAndTasks language
                        currentDate
                        SetMedicationSubActivityBoolInput
                        assembled
                        form
                        task
    in
    resolveTasksCompletedFromTotal tasks


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
                let
                    fetalHeartRate =
                        valueConsideringIsDirtyField form.fetalHeartRateDirty form.fetalHeartRate value.fetalHeartRate

                    fetalHeartRateNotAudible =
                        if fetalHeartRate == Just 0 then
                            Just True

                        else
                            Just False
                in
                { fundalPalpable = or form.fundalPalpable (Just value.fundalPalpable)
                , fundalHeight =
                    maybeValueConsideringIsDirtyField form.fundalHeightDirty
                        form.fundalHeight
                        (Maybe.map getHeightValue value.fundalHeight)
                , fundalHeightDirty = form.fundalHeightDirty
                , fetalPresentation = or form.fetalPresentation (Just value.fetalPresentation)
                , fetalMovement = or form.fetalMovement (Just value.fetalMovement)
                , fetalHeartRate = fetalHeartRate
                , fetalHeartRateDirty = form.fetalHeartRateDirty
                , fetalHeartRateNotAudible = or form.fetalHeartRateNotAudible fetalHeartRateNotAudible
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
                let
                    cSectionInPastFromValue =
                        if EverySet.member CSectionInPast value.previousDelivery then
                            True

                        else
                            -- This comes for intermediate period, where devices may
                            -- have content to upload where number of c-section was recorded.
                            -- In new version, this value is set to -1, so result will
                            -- always be false here.
                            value.cSections > 0
                in
                { cSectionInPast = or form.cSectionInPast (Just cSectionInPastFromValue)
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
                , signs = or form.signs (Just <| EverySet.toList value.signs)
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
            [ Maybe.map (ifTrue CSectionInPast) form.cSectionInPast
            , ifNullableTrue CSectionInPreviousDelivery form.cSectionInPreviousDelivery
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoPreviousDeliverySign)
    in
    -- Number of C-sections field is oboslete. Since we still need to
    -- keep this info as part of the value, to support exisitng measurements,
    -- we default it to -1.
    Maybe.map ObstetricHistoryStep2Value (Just -1)
        |> andMap (Just <| Maybe.map EverySet.singleton form.cSectionReason)
        |> andMap previousDeliverySet
        |> andMap (Maybe.map EverySet.singleton form.previousDeliveryPeriod)
        |> andMap (Just <| EverySet.singleton NoObstetricHistorySign)
        |> andMap (Maybe.map EverySet.fromList form.signs)


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
            (\_ ->
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


examinationTasksCompletedFromTotal : NominalDate -> AssembledData -> ExaminationData -> ExaminationTask -> ( Int, Int )
examinationTasksCompletedFromTotal currentDate assembled data task =
    case task of
        Vitals ->
            let
                formConfig =
                    generateVitalsFormConfig assembled

                ( _, tasks ) =
                    getMeasurementValueFunc assembled.measurements.vitals
                        |> vitalsFormWithDefault data.vitalsForm
                        |> vitalsFormInputsAndTasks English currentDate formConfig
            in
            resolveTasksCompletedFromTotal tasks

        NutritionAssessment ->
            let
                measuredHeight =
                    resolvePreviouslyMeasuredHeight assembled

                hideHeightInput =
                    isJust measuredHeight

                form_ =
                    getMeasurementValueFunc assembled.measurements.nutrition
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
                    getMeasurementValueFunc assembled.measurements.corePhysicalExam
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
                ( _, tasks ) =
                    getMeasurementValueFunc assembled.measurements.guExam
                        |> guExamFormWithDefault data.guExamForm
                        |> guExamFormInputsAndTasks English assembled
            in
            resolveTasksCompletedFromTotal tasks


generateVitalsFormConfig : AssembledData -> VitalsFormConfig Msg
generateVitalsFormConfig assembled =
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
            PrenatalFollowUpValue options form.resolutionDate assesment
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
                        && (not <| EverySet.member DiagnosisHepatitisBInitialPhase assembled.encounter.pastDiagnoses)
                        && (not <| EverySet.member DiagnosisHepatitisBRecurrentPhase assembled.encounter.pastDiagnoses)
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
                    isKnownAsPositive .hivTest
                        || diagnosedPreviouslyAnyOf
                            [ DiagnosisHIVInitialPhase
                            , DiagnosisHIVRecurrentPhase
                            ]
                            assembled

                TaskPartnerHIVTest ->
                    (not <| isKnownAsPositive .partnerHIVTest)
                        && isInitialTest TaskPartnerHIVTest

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
                            pendingTests =
                                EverySet.diff value.performedTests value.completedTests
                                    |> EverySet.toList
                                    |> -- Vitals recheck should be completed on same day
                                       -- it was scheduled, and therefore we're not
                                       -- catching up with it.
                                       List.filter ((/=) TestVitalsRecheck)
                        in
                        if List.isEmpty pendingTests then
                            Nothing

                        else
                            let
                                encounterId =
                                    Maybe.andThen (Tuple.second >> .encounterId) data.measurements.labsResults
                            in
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
                    False
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

        TaskOverview ->
            not <| taskExpected TaskOverview


expectImmunisationTask : NominalDate -> AssembledData -> ImmunisationTask -> Bool
expectImmunisationTask currentDate assembled task =
    let
        futureVaccinations =
            generateFutureVaccinationsDataByHistory currentDate assembled
                |> Dict.fromList

        isTaskExpected vaccineType =
            Dict.get vaccineType futureVaccinations
                |> Maybe.Extra.join
                |> Maybe.map
                    (\( _, date ) ->
                        not <| Date.compare date currentDate == GT
                    )
                |> Maybe.withDefault False
    in
    immunisationTaskToVaccineType task
        |> Maybe.map isTaskExpected
        -- Only task that is not converted to vaccine type
        -- is 'Overview', which we always show.
        |> Maybe.withDefault True


generateFutureVaccinationsDataByHistory : NominalDate -> AssembledData -> List ( PrenatalVaccineType, Maybe ( VaccineDose, NominalDate ) )
generateFutureVaccinationsDataByHistory currentDate assembled =
    generateFutureVaccinationsData currentDate assembled.globalLmpDate assembled.vaccinationHistory


generateFutureVaccinationsDataByProgress : NominalDate -> AssembledData -> List ( PrenatalVaccineType, Maybe ( VaccineDose, NominalDate ) )
generateFutureVaccinationsDataByProgress currentDate assembled =
    generateFutureVaccinationsData currentDate assembled.globalLmpDate assembled.vaccinationProgress


{-| For each type of vaccine, we generate next dose and administration date.
If there's no need for future vaccination, Nothing is returned.
-}
generateFutureVaccinationsData :
    NominalDate
    -> Maybe NominalDate
    -> VaccinationProgressDict
    -> List ( PrenatalVaccineType, Maybe ( VaccineDose, NominalDate ) )
generateFutureVaccinationsData currentDate globalLmpDate vaccinationDict =
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
                            case latestVaccinationDataForVaccine vaccinationDict vaccineType of
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
        globalLmpDate
        |> Maybe.withDefault []


immunisationTaskToVaccineType : ImmunisationTask -> Maybe PrenatalVaccineType
immunisationTaskToVaccineType task =
    case task of
        TaskTetanus ->
            Just VaccineTetanus

        TaskOverview ->
            Nothing


immunisationVaccinationTasks : List ImmunisationTask
immunisationVaccinationTasks =
    [ TaskTetanus ]


immunisationTasks : List ImmunisationTask
immunisationTasks =
    immunisationVaccinationTasks ++ [ TaskOverview ]


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


immunisationTasksCompletedFromTotal :
    Language
    -> NominalDate
    -> Site
    -> AssembledData
    -> ImmunisationData
    -> Pages.Prenatal.Activity.Types.ImmunisationTask
    -> ( Int, Int )
immunisationTasksCompletedFromTotal language currentDate site assembled data task =
    immunisationTaskToVaccineType task
        |> Maybe.map
            (\vaccineType ->
                let
                    form =
                        case vaccineType of
                            VaccineTetanus ->
                                getMeasurementValueFunc assembled.measurements.tetanusImmunisation
                                    |> vaccinationFormWithDefault data.tetanusForm

                    ( _, tasksActive, tasksCompleted ) =
                        vaccinationFormDynamicContentAndTasks language currentDate site assembled vaccineType form
                in
                ( tasksActive, tasksCompleted )
            )
        |> Maybe.withDefault ( 0, 0 )


vaccinationFormDynamicContentAndTasks :
    Language
    -> NominalDate
    -> Site
    -> AssembledData
    -> PrenatalVaccineType
    -> PrenatalVaccinationForm
    -> ( List (Html Msg), Int, Int )
vaccinationFormDynamicContentAndTasks language currentDate site assembled vaccineType form =
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
                    , suggestDoseToday = True
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
            Measurement.Utils.vaccinationFormDynamicContentAndTasks language
                currentDate
                site
                config
                (PrenatalVaccine vaccineType)
                form
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
                , hivDetectableViralLoad = or form.hivDetectableViralLoad (EverySet.member EducationHIVDetectableViralLoad value.signs |> Just)
                , diabetes = or form.diabetes (EverySet.member EducationDiabetes value.signs |> Just)
                , grief = or form.grief (EverySet.member EducationGrief value.signs |> Just)
                , hivPartnerPresence = or form.hivPartnerPresence (EverySet.member EducationHIVPartnerPresence value.signs |> Just)
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
    , ifNullableTrue EducationHIVDetectableViralLoad form.hivDetectableViralLoad
    , ifNullableTrue EducationDiabetes form.diabetes
    , ifNullableTrue EducationGrief form.grief
    , ifNullableTrue EducationHIVPartnerPresence form.hivPartnerPresence
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
    , Maybe.map (EverySet.singleton >> Just) form.reasonForNotBreastfeeding
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
                , form.healingNormally :: healingProblemsTasks ++ rectalHemorrhoidsTasks
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
    , form.referToHealthCenter :: derivedTasks
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
            diagnosesCausingHospitalReferralByPhase PrenatalEncounterPhaseInitial assembled
                |> EverySet.isEmpty
                |> not

        FacilityMentalHealthSpecialist ->
            referToMentalHealthSpecialist assembled

        FacilityARVProgram ->
            referToARVProgram assembled

        FacilityNCDProgram ->
            expectSpecialityCareSignSection assembled EnrolledToNCDProgram
                && referredToSpecialityCareProgram EnrolledToNCDProgram assembled

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


resolveWarningPopupContentForUrgentDiagnoses : Language -> List PrenatalDiagnosis -> ( String, String )
resolveWarningPopupContentForUrgentDiagnoses language urgentDiagnoses =
    let
        signs =
            List.map (Translate.PrenatalDiagnosis >> translate language) urgentDiagnoses
                |> String.join ", "
    in
    ( translate language Translate.DangerSignsLabelForNurse ++ " " ++ signs
    , if
        List.any
            (\immediateDeliveryDiagnosis ->
                List.member immediateDeliveryDiagnosis urgentDiagnoses
            )
            immediateDeliveryDiagnoses
      then
        translate language Translate.EmergencyReferralHelperReferToHospitalForImmediateDelivery

      else if
        List.any
            (\maternityWardDiagnosis ->
                List.member maternityWardDiagnosis urgentDiagnoses
            )
            maternityWardDiagnoses
      then
        translate language Translate.EmergencyReferralHelperReferToMaternityWard

      else if
        List.any
            (\emergencyObstetricCareServicesDiagnosis ->
                List.member emergencyObstetricCareServicesDiagnosis urgentDiagnoses
            )
            emergencyObstetricCareServicesDiagnoses
      then
        translate language Translate.EmergencyReferralHelperReferToEmergencyObstetricCareServices

      else
        translate language Translate.EmergencyReferralHelperReferToHospitalImmediately
    )


medicationTasksCompletedFromTotal : NominalDate -> AssembledData -> MedicationData -> MedicationTask -> ( Int, Int )
medicationTasksCompletedFromTotal currentDate assembled data task =
    let
        measurements =
            assembled.measurements

        ( _, tasks ) =
            case task of
                TaskCalcium ->
                    getMeasurementValueFunc measurements.calcium
                        |> medicationAdministrationFormWithDefault data.calciumForm
                        |> medicationAdministrationFormInputsAndTasks English
                            currentDate
                            assembled.person
                            calciumAdministrationFormConfig

                TaskFefol ->
                    getMeasurementValueFunc measurements.fefol
                        |> medicationAdministrationFormWithDefault data.fefolForm
                        |> medicationAdministrationFormInputsAndTasks English
                            currentDate
                            assembled.person
                            fefolAdministrationFormConfig

                TaskFolate ->
                    getMeasurementValueFunc measurements.folate
                        |> medicationAdministrationFormWithDefault data.folateForm
                        |> medicationAdministrationFormInputsAndTasks English
                            currentDate
                            assembled.person
                            folateAdministrationFormConfig

                TaskIron ->
                    getMeasurementValueFunc measurements.iron
                        |> medicationAdministrationFormWithDefault data.ironForm
                        |> medicationAdministrationFormInputsAndTasks English
                            currentDate
                            assembled.person
                            ironAdministrationFormConfig

                TaskMMS ->
                    getMeasurementValueFunc measurements.mms
                        |> medicationAdministrationFormWithDefault data.mmsForm
                        |> medicationAdministrationFormInputsAndTasks English
                            currentDate
                            assembled.person
                            mmsAdministrationFormConfig

                TaskMebendazole ->
                    getMeasurementValueFunc measurements.mebendazole
                        |> medicationAdministrationFormWithDefault data.mebendazoleForm
                        |> medicationAdministrationFormInputsAndTasks English
                            currentDate
                            assembled.person
                            mebendazoleAdministrationFormConfig
    in
    resolveTasksCompletedFromTotal tasks


calciumAdministrationFormConfig : MedicationAdministrationFormConfig Msg
calciumAdministrationFormConfig =
    { medication = Calcium
    , setMedicationAdministeredMsg = SetCalciumAdministered
    , setReasonForNonAdministration = SetCalciumReasonForNonAdministration
    , resolveDosageAndIconFunc = resolveCalciumDosageAndIcon
    }


resolveCalciumDosageAndIcon : Language -> NominalDate -> Person -> Maybe ( String, String, String )
resolveCalciumDosageAndIcon language currentDate person =
    Just ( "500 mg", "icon-pills", translate language Translate.AdministerCalciumHelper )


fefolAdministrationFormConfig : MedicationAdministrationFormConfig Msg
fefolAdministrationFormConfig =
    { medication = Fefol
    , setMedicationAdministeredMsg = SetFefolAdministered
    , setReasonForNonAdministration = SetFefolReasonForNonAdministration
    , resolveDosageAndIconFunc = resolveFefolDosageAndIcon
    }


folateAdministrationFormConfig : MedicationAdministrationFormConfig Msg
folateAdministrationFormConfig =
    { medication = FolicAcid
    , setMedicationAdministeredMsg = SetFolateAdministered
    , setReasonForNonAdministration = SetFolateReasonForNonAdministration
    , resolveDosageAndIconFunc = resolveFolicAcidDosageAndIcon
    }


resolveFolicAcidDosageAndIcon : Language -> NominalDate -> Person -> Maybe ( String, String, String )
resolveFolicAcidDosageAndIcon language currentDate person =
    Just ( "400 UI", "icon-pills", translate language Translate.AdministerFolicAcidHelper )


ironAdministrationFormConfig : MedicationAdministrationFormConfig Msg
ironAdministrationFormConfig =
    { medication = Iron
    , setMedicationAdministeredMsg = SetIronAdministered
    , setReasonForNonAdministration = SetIronReasonForNonAdministration
    , resolveDosageAndIconFunc = resolveIronDosageAndIcon
    }


resolveIronDosageAndIcon : Language -> NominalDate -> Person -> Maybe ( String, String, String )
resolveIronDosageAndIcon language currentDate person =
    Just ( "120 mg", "icon-pills", translate language Translate.AdministerIronHelper )


mmsAdministrationFormConfig : MedicationAdministrationFormConfig Msg
mmsAdministrationFormConfig =
    { medication = MMS
    , setMedicationAdministeredMsg = SetMMSAdministered
    , setReasonForNonAdministration = SetMMSReasonForNonAdministration
    , resolveDosageAndIconFunc = resolveMMSDosageAndIcon
    }


mebendazoleAdministrationFormConfig : MedicationAdministrationFormConfig Msg
mebendazoleAdministrationFormConfig =
    { medication = Mebendezole
    , setMedicationAdministeredMsg = SetMebendazoleAdministered
    , setReasonForNonAdministration = SetMebendazoleReasonForNonAdministration
    , resolveDosageAndIconFunc = resolveMebendezoleDosageAndIcon
    }


resolveMebendezoleDosageAndIcon : Language -> NominalDate -> Person -> Maybe ( String, String, String )
resolveMebendezoleDosageAndIcon language currentDate person =
    Just ( "500 mg", "icon-pills", translate language Translate.AdministerPrenatalMebendezoleHelper )
