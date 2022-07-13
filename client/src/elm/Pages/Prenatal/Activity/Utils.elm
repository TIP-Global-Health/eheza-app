module Pages.Prenatal.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (PrenatalEncounterId)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getHeightValue, getMeasurementValueFunc, muacIndication, muacValueFunc, prenatalLabExpirationPeriod, weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounterType(..), PrenatalIndicator(..))
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
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
import Measurement.Model exposing (VaccinationFormViewMode(..), VitalsForm)
import Measurement.Utils
    exposing
        ( getNextVaccineDose
        , sendToHCFormWithDefault
        , vaccinationFormWithDefault
        , vaccineDoseToComparable
        , vitalsFormWithDefault
        )
import Pages.AcuteIllness.Activity.Utils exposing (getCurrentReasonForMedicationNonAdministration, nonAdministrationReasonToSign)
import Pages.AcuteIllness.Activity.View exposing (viewAdministeredMedicationCustomLabel, viewAdministeredMedicationLabel, viewAdministeredMedicationQuestion)
import Pages.Prenatal.Activity.Model exposing (..)
import Pages.Prenatal.Activity.Types exposing (..)
import Pages.Prenatal.Encounter.Utils exposing (diagnosisRequiresEmergencyReferal, emergencyReferalRequired)
import Pages.Prenatal.Model exposing (AssembledData, HealthEducationForm, PrenatalEncounterPhase(..), VaccinationProgressDict)
import Pages.Prenatal.Utils exposing (..)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
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
                    assembled.nursePreviousMeasurementsWithDates
                        |> List.filter
                            (\( _, _, measurements ) ->
                                measurements.malariaPrevention
                                    |> Maybe.map (Tuple.second >> .value >> EverySet.member MosquitoNet)
                                    |> Maybe.withDefault False
                            )
                        |> List.isEmpty

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
                                |> List.filter (expectNextStepsTask currentDate assembled)
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
                                    assembled.nursePreviousMeasurementsWithDates
                                        |> List.filter
                                            (\( _, _, measurements ) ->
                                                isJust measurements.mentalHealth
                                            )
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

                -- Unique Chw activities.
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

                -- Unique nurse activities.
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

                -- Unique nurse activities.
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

                -- Unique nurse activities.
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

                -- Unique nurse activities.
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
            isJust assembled.measurements.vitals
                && isJust assembled.measurements.nutrition
                && isJust assembled.measurements.corePhysicalExam
                && isJust assembled.measurements.obstetricalExam
                && isJust assembled.measurements.breastExam

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
            if assembled.encounter.encounterType == NurseEncounter then
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
                |> List.all (nextStepsMeasurementTaken assembled)

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


resolveNextStepsTasks : NominalDate -> AssembledData -> List NextStepsTask
resolveNextStepsTasks currentDate assembled =
    let
        tasks =
            case assembled.encounter.encounterType of
                NurseEncounter ->
                    -- The order is important. Do not change.
                    [ NextStepsHealthEducation, NextStepsMedicationDistribution, NextStepsSendToHC, NextStepsWait ]

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
                    referToHospitalForNonHIVDiagnosis assembled
                        || referToHospitalDueToAdverseEvent assembled
                        || referToHospitalDueToPastDiagnosis assembled
                        || referToHIVProgram assembled
                        || referToMentalHealthSpecialist assembled

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

                ChwPostpartumEncounter ->
                    True

                _ ->
                    False

        -- Exclusive CHW task.
        NextStepsNewbornEnrolment ->
            assembled.encounter.encounterType == ChwPostpartumEncounter

        -- Exclusive Nurse task.
        NextStepsMedicationDistribution ->
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
                        || (updateHypertensionTreatmentWithMedication assembled
                                && (not <| referToHospitalDueToAdverseEventForHypertensionTreatment assembled)
                           )
                   )

        NextStepsWait ->
            -- If we refer patients somewhere, there's no need to wait.
            (not <| expectNextStepsTask currentDate assembled NextStepsSendToHC)
                && -- We show Wait activity when there's at least one
                   -- test that was performed, or, 2 hours waiting is
                   -- required for blood preasure recheck.
                   (getMeasurementValueFunc assembled.measurements.labsResults
                        |> Maybe.map (.performedTests >> EverySet.isEmpty >> not)
                        |> Maybe.withDefault False
                   )


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


referredToHIVProgramPreviously : AssembledData -> Bool
referredToHIVProgramPreviously assembled =
    List.filterMap
        (\( _, diagnoses, measurements ) ->
            if EverySet.member DiagnosisHIV diagnoses then
                Just measurements

            else
                Nothing
        )
        assembled.nursePreviousMeasurementsWithDates
        |> List.head
        |> Maybe.map hivProgramAtHC
        |> Maybe.withDefault False


latestMedicationTreatmentForHIV : AssembledData -> Maybe Translate.TranslationId
latestMedicationTreatmentForHIV assembled =
    let
        prescribedMedications =
            List.reverse assembled.nursePreviousMeasurementsWithDates
                |> List.filterMap
                    (\( _, _, measurements ) ->
                        getMeasurementValueFunc measurements.medicationDistribution
                            |> Maybe.andThen
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


latestMedicationTreatmentForHypertension : AssembledData -> Maybe Translate.TranslationId
latestMedicationTreatmentForHypertension assembled =
    getLatestTreatmentByTreatmentOptions recommendedTreatmentSignsForHypertension assembled
        |> Maybe.map Translate.TreatmentDetailsHypertension


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
            List.reverse assembled.nursePreviousMeasurementsWithDates
                |> List.filter
                    (\( _, _, measurements ) ->
                        getMeasurementValueFunc measurements.medicationDistribution
                            |> Maybe.map
                                (\value ->
                                    List.any (\sign -> EverySet.member sign value.distributionSigns)
                                        [ Iron
                                        , FolicAcid
                                        ]
                                )
                            |> Maybe.withDefault False
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


diagnosesCausingHospitalReferral : AssembledData -> EverySet PrenatalDiagnosis
diagnosesCausingHospitalReferral assembled =
    let
        nonHIV =
            nonHIVDiagnosesCausingHospitalReferral assembled

        byAdverseEvent =
            diagnosesCausingHospitalReferralByAdverseEventForTreatment assembled

        byPastDiagnoses =
            diagnosesCausingHospitalReferralByPastDiagnoses assembled

        overall =
            nonHIV
                ++ byAdverseEvent
                ++ byPastDiagnoses

        additional =
            -- There are 2 cases where patient is referred to hospital, skipping
            -- referals to other facilities:
            -- 1. HIV-positive patients, while there's an HIV program, at the health center.
            -- 2. Patient with a mental health diagnosis, and available mental health
            --   specialist at helath center.
            -- In both cases, since we have a need to refer to the hospital, it must
            -- be more urgent, and therefore, treatment for (HIV/Mental health) will
            -- be given at the hospital.
            if not <| List.isEmpty overall then
                let
                    hiv =
                        if diagnosed DiagnosisHIV assembled && hivProgramAtHC assembled.measurements then
                            [ DiagnosisHIV ]

                        else
                            []

                    mentalHealth =
                        if mentalHealthSpecialistAtHC assembled then
                            List.filter (\diagnosis -> diagnosed diagnosis assembled) mentalHealthDiagnosesRequiringTreatment

                        else
                            []
                in
                hiv

            else
                []
    in
    overall
        ++ additional
        |> EverySet.fromList


referToHospitalForNonHIVDiagnosis : AssembledData -> Bool
referToHospitalForNonHIVDiagnosis =
    nonHIVDiagnosesCausingHospitalReferral >> List.isEmpty >> not


nonHIVDiagnosesCausingHospitalReferral : AssembledData -> List PrenatalDiagnosis
nonHIVDiagnosesCausingHospitalReferral assembled =
    diagnosesCausingHospitalReferralByImmediateDiagnoses assembled
        ++ diagnosesCausingHospitalReferralByMentalHealth assembled
        ++ diagnosesCausingHospitalReferralByOtherReasons assembled


diagnosesCausingHospitalReferralByImmediateDiagnoses : AssembledData -> List PrenatalDiagnosis
diagnosesCausingHospitalReferralByImmediateDiagnoses assembled =
    let
        immediateReferralDiagnoses =
            emergencyReferralDiagnosesInitial
                ++ [ DiagnosisModeratePreeclampsiaImmediate
                   , DiagnosisHeartburnPersistent
                   , DiagnosisDeepVeinThrombosis
                   , DiagnosisPelvicPainIntense
                   , DiagnosisPelvicPainContinued
                   , DiagnosisPyelonephritis
                   , DiagnosisMalariaMedicatedContinued
                   , DiagnosisMalariaWithAnemiaMedicatedContinued
                   , DiagnosisUrinaryTractInfectionContinued
                   , DiagnosisCandidiasisContinued
                   , DiagnosisGonorrheaContinued
                   , DiagnosisTrichomonasOrBacterialVaginosisContinued
                   ]
    in
    List.filter (\diagnosis -> diagnosed diagnosis assembled)
        immediateReferralDiagnoses


diagnosesCausingHospitalReferralByMentalHealth : AssembledData -> List PrenatalDiagnosis
diagnosesCausingHospitalReferralByMentalHealth assembled =
    if mentalHealthSpecialistAtHC assembled then
        []

    else
        List.filter (\diagnosis -> diagnosed diagnosis assembled) mentalHealthDiagnosesRequiringTreatment


diagnosesCausingHospitalReferralByOtherReasons : AssembledData -> List PrenatalDiagnosis
diagnosesCausingHospitalReferralByOtherReasons assembled =
    let
        severeMalariaTreatment =
            getMeasurementValueFunc assembled.measurements.medicationDistribution
                |> Maybe.andThen (.recommendedTreatmentSigns >> Maybe.map (EverySet.member TreatmentReferToHospital))
                |> Maybe.withDefault False

        malaria =
            if diagnosedMalaria assembled && severeMalariaTreatment then
                [ DiagnosisMalaria ]

            else
                []

        hypertension =
            if updateHypertensionTreatmentWithHospitalization assembled then
                [ DiagnosisChronicHypertensionImmediate ]

            else
                []
    in
    malaria ++ hypertension


referToHospitalForMentalHealthDiagnosis : AssembledData -> Bool
referToHospitalForMentalHealthDiagnosis =
    diagnosesCausingHospitalReferralByMentalHealth >> List.isEmpty >> not


referToMentalHealthSpecialist : AssembledData -> Bool
referToMentalHealthSpecialist assembled =
    mentalHealthSpecialistAtHC assembled && diagnosedAnyOf mentalHealthDiagnosesRequiringTreatment assembled


mentalHealthSpecialistAtHC : AssembledData -> Bool
mentalHealthSpecialistAtHC assembled =
    getMeasurementValueFunc assembled.measurements.mentalHealth
        |> Maybe.map .specialistAtHC
        |> Maybe.withDefault False


referToHIVProgram : AssembledData -> Bool
referToHIVProgram assembled =
    diagnosed DiagnosisHIV assembled && hivProgramAtHC assembled.measurements


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
    isJust assembled.measurements.mentalHealth
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
    assembled.nursePreviousMeasurementsWithDates
        |> List.filter
            (\( _, _, measurements ) ->
                symptomRecorded measurements symptom
            )
        |> List.isEmpty
        |> not


nextStepsMeasurementTaken : AssembledData -> NextStepsTask -> Bool
nextStepsMeasurementTaken assembled task =
    case task of
        NextStepsAppointmentConfirmation ->
            isJust assembled.measurements.appointmentConfirmation

        NextStepsFollowUp ->
            isJust assembled.measurements.followUp

        NextStepsSendToHC ->
            isJust assembled.measurements.sendToHC

        NextStepsHealthEducation ->
            isJust assembled.measurements.healthEducation

        NextStepsNewbornEnrolment ->
            isJust assembled.participant.newborn

        NextStepsMedicationDistribution ->
            let
                allowedSigns =
                    NoMedicationDistributionSignsInitialPhase :: medicationsInitialPhase

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
                    if diagnosedHypertension PrenatalEncounterPhaseInitial assembled then
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
            in
            medicationDistributionMeasurementTaken allowedSigns assembled.measurements
                && malariaTreatmentCompleted
                && heartburnTreatmentCompleted
                && hypertensionTreatmentCompleted
                && candidiasisTreatmentCompleted
                && urinaryTractInfectionTreatmentCompleted

        NextStepsWait ->
            getMeasurementValueFunc assembled.measurements.labsResults
                |> Maybe.map .patientNotified
                |> Maybe.withDefault False


mandatoryActivitiesForNextStepsCompleted : NominalDate -> AssembledData -> Bool
mandatoryActivitiesForNextStepsCompleted currentDate assembled =
    case assembled.encounter.encounterType of
        NurseEncounter ->
            activityCompleted currentDate assembled DangerSigns

        ChwFirstEncounter ->
            let
                commonMandatoryActivitiesCompleted =
                    ((not <| expectActivity currentDate assembled PregnancyDating) || activityCompleted currentDate assembled PregnancyDating)
                        && ((not <| expectActivity currentDate assembled Laboratory) || activityCompleted currentDate assembled Laboratory)
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
            List.map (\( _, _, measurements ) -> measurements) assembled.nursePreviousMeasurementsWithDates
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
                            assembled.nursePreviousMeasurementsWithDates
                                |> List.filterMap
                                    (\( encounterDate, _, measurements ) ->
                                        let
                                            encounterWeek =
                                                diffDays lmpDate encounterDate // 7
                                        in
                                        -- Encounter is within dates range, and it's has a photo measurement.
                                        if
                                            List.all (\condition -> condition encounterWeek == True) conditions
                                                && isJust measurements.prenatalPhoto
                                        then
                                            Just encounterDate

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

        _ ->
            []


generateDangerSignsListForChw : Language -> AssembledData -> List String
generateDangerSignsListForChw language assembled =
    case assembled.encounter.encounterType of
        NurseEncounter ->
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
            List.filterMap
                (\( _, _, measurements ) ->
                    resolveHeight measurements
                )
                assembled.nursePreviousMeasurementsWithDates
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

        diagnosesByLabResults =
            List.filter (matchLabResultsPrenatalDiagnosis egaInWeeks dangerSignsList assembled)
                labResultsDiagnoses
                |> EverySet.fromList

        dangerSignsList =
            generateDangerSignsListForNurse assembled

        emergencyReferalDiagnoses =
            List.filter
                (matchEmergencyReferalPrenatalDiagnosis
                    egaInWeeks
                    dangerSignsList
                    assembled
                )
                emergencyReferralDiagnosesInitial
                |> EverySet.fromList

        diagnosesByExamination =
            Maybe.map
                (\egaWeeks ->
                    List.filter (matchExaminationPrenatalDiagnosis egaWeeks assembled)
                        examinationDiagnoses
                        |> EverySet.fromList
                )
                egaInWeeks
                |> Maybe.withDefault EverySet.empty

        diagnosesBySymptoms =
            List.filter (matchSymptomsPrenatalDiagnosis egaInWeeks assembled)
                symptomsDiagnoses
                |> EverySet.fromList

        diagnosesByMentalHealth =
            List.filter (matchMentalHealthPrenatalDiagnosis assembled)
                mentalHealthDiagnoses
                |> EverySet.fromList
    in
    EverySet.union diagnosesByLabResults emergencyReferalDiagnoses
        |> EverySet.union diagnosesByExamination
        |> EverySet.union diagnosesBySymptoms
        |> EverySet.union diagnosesByMentalHealth


matchEmergencyReferalPrenatalDiagnosis : Maybe Int -> List DangerSign -> AssembledData -> PrenatalDiagnosis -> Bool
matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs assembled diagnosis =
    case diagnosis of
        DiagnosisSeverePreeclampsiaImmediate ->
            List.member HeadacheBlurredVision signs

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

        DiagnosisInfection ->
            List.member Fever signs
                && (List.member ExtremeWeakness signs || respiratoryRateElevated assembled.measurements)

        DiagnosisImminentDelivery ->
            List.member ImminentDelivery signs

        DiagnosisLaborAndDelivery ->
            List.member Labor signs

        -- Non Emergency Referral diagnoses.
        _ ->
            False


matchLabResultsPrenatalDiagnosis : Maybe Int -> List DangerSign -> AssembledData -> PrenatalDiagnosis -> Bool
matchLabResultsPrenatalDiagnosis egaInWeeks dangerSigns assembled diagnosis =
    let
        measurements =
            assembled.measurements

        positiveMalariaTest =
            testedPositiveAt .malariaTest

        positiveSyphilisTest =
            testedPositiveAt .syphilisTest

        testedPositiveAt getMeasurementFunc =
            getMeasurementFunc measurements
                |> getMeasurementValueFunc
                |> Maybe.andThen (.testResult >> Maybe.map ((==) PrenatalTestPositive))
                |> Maybe.withDefault False

        hemoglobinCount =
            getMeasurementValueFunc measurements.hemoglobinTest
                |> Maybe.andThen .hemoglobinCount

        anemiaComplicationSignsPresent =
            respiratoryRateElevated measurements
                || List.member DifficultyBreathing dangerSigns
                || anemiaComplicationSignsByExamination

        anemiaComplicationSignsByExamination =
            getMeasurementValueFunc measurements.corePhysicalExam
                |> Maybe.map
                    (\exam ->
                        EverySet.member PallorHands exam.hands || EverySet.member PaleConjuctiva exam.eyes
                    )
                |> Maybe.withDefault False

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
                           severeAnemiaWithComplicationsDiagnosed
                   )

        malariaWithAnemiaDiagnosed =
            positiveMalariaTest
                && (-- Hemoglobin test was performed, and,
                    -- hemoglobin count indicates mild to moderate anemia.
                    Maybe.map (\count -> count >= 7 && count < 11) hemoglobinCount
                        |> Maybe.withDefault False
                   )

        severeAnemiaWithComplicationsDiagnosed =
            (-- No indication for being positive for malaria,
             -- Hemoglobin test was performed, and, hemoglobin
             -- count indicates severe anemia.
             Maybe.map (\count -> count < 7) hemoglobinCount
                |> Maybe.withDefault False
            )
                && anemiaComplicationSignsPresent
    in
    case diagnosis of
        DiagnosisSeverePreeclampsiaAfterRecheck ->
            Maybe.map
                (\egaWeeks ->
                    (egaWeeks >= 20)
                        && (highBloodPressure measurements
                                || repeatedHighBloodPressure measurements
                           )
                        && highUrineProtein measurements
                        && severePreeclampsiaSigns measurements
                )
                egaInWeeks
                |> Maybe.withDefault False

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
                && not anemiaComplicationSignsPresent

        DiagnosisSevereAnemiaWithComplications ->
            severeAnemiaWithComplicationsDiagnosed

        -- Non Lab Results diagnoses.
        _ ->
            False


matchExaminationPrenatalDiagnosis : Int -> AssembledData -> PrenatalDiagnosis -> Bool
matchExaminationPrenatalDiagnosis egaInWeeks assembled diagnosis =
    let
        measurements =
            assembled.measurements
    in
    case diagnosis of
        DiagnosisChronicHypertensionImmediate ->
            -- Hypertension is a chronic diagnosis for whole duration
            -- of pregnancy. Therefore, if diagnosed once, we do not need
            -- to diagnose it again.
            (not <| diagnosedHypertensionPrevoiusly assembled)
                && (egaInWeeks < 20 && immediateHypertensionByMeasurements measurements)

        DiagnosisChronicHypertensionAfterRecheck ->
            (not <| diagnosedHypertensionPrevoiusly assembled)
                && (egaInWeeks < 20 && recheckedHypertensionByMeasurements measurements)

        DiagnosisGestationalHypertensionImmediate ->
            (not <| diagnosedHypertensionPrevoiusly assembled)
                && (egaInWeeks >= 20 && immediateHypertensionByMeasurements measurements)

        DiagnosisGestationalHypertensionAfterRecheck ->
            (not <| diagnosedHypertensionPrevoiusly assembled)
                && (egaInWeeks >= 20 && recheckedHypertensionByMeasurements measurements)

        DiagnosisModeratePreeclampsiaImmediate ->
            egaInWeeks >= 20 && immediatePreeclampsiaByMeasurements measurements

        DiagnosisModeratePreeclampsiaAfterRecheck ->
            egaInWeeks >= 20 && recheckedPreeclampsiaByMeasurements measurements

        -- Non Examination diagnoses.
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


immediateHypertensionByMeasurements : PrenatalMeasurements -> Bool
immediateHypertensionByMeasurements =
    highBloodPressure


recheckedHypertensionByMeasurements : PrenatalMeasurements -> Bool
recheckedHypertensionByMeasurements =
    repeatedTestForMarginalBloodPressure


immediatePreeclampsiaByMeasurements : PrenatalMeasurements -> Bool
immediatePreeclampsiaByMeasurements measurements =
    highBloodPressure measurements
        && edemaOnHandOrLegs measurements


recheckedPreeclampsiaByMeasurements : PrenatalMeasurements -> Bool
recheckedPreeclampsiaByMeasurements measurements =
    repeatedTestForMarginalBloodPressure measurements
        && ((highUrineProtein measurements
                && -- Adding this, so we would not have both Moderate and
                   -- Severe Preeclapsia diagnoses.
                   (not <| severePreeclampsiaSigns measurements)
            )
                || edemaOnHandOrLegs measurements
           )


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


{-| We measure BP again when we suspect Hypertension or Preeclamsia
(dia between 90 and 110, and dia between 140 and 160).
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


highBloodPressureCondition : Float -> Float -> Bool
highBloodPressureCondition dia sys =
    dia >= 110 || sys >= 160


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


labResultsDiagnoses : List PrenatalDiagnosis
labResultsDiagnoses =
    [ DiagnosisSeverePreeclampsiaAfterRecheck
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
    ]


examinationDiagnoses : List PrenatalDiagnosis
examinationDiagnoses =
    [ DiagnosisChronicHypertensionImmediate
    , DiagnosisChronicHypertensionAfterRecheck
    , DiagnosisGestationalHypertensionImmediate
    , DiagnosisGestationalHypertensionAfterRecheck
    , DiagnosisModeratePreeclampsiaImmediate
    , DiagnosisModeratePreeclampsiaAfterRecheck
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
    ]


mentalHealthDiagnoses : List PrenatalDiagnosis
mentalHealthDiagnoses =
    DiagnosisDepressionNotLikely :: mentalHealthDiagnosesRequiringTreatment


mentalHealthDiagnosesRequiringTreatment : List PrenatalDiagnosis
mentalHealthDiagnosesRequiringTreatment =
    [ DiagnosisDepressionPossible
    , DiagnosisDepressionHighlyPossible
    , DiagnosisDepressionProbable
    , DiagnosisSuicideRisk
    ]


healthEducationFormInputsAndTasks : Language -> AssembledData -> HealthEducationForm -> ( List (Html Msg), List (Maybe Bool) )
healthEducationFormInputsAndTasks language assembled healthEducationForm =
    let
        form =
            assembled.measurements.healthEducation
                |> getMeasurementValueFunc
                |> healthEducationFormWithDefaultInitialPhase healthEducationForm
    in
    case assembled.encounter.encounterType of
        NurseEncounter ->
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
                -- 'Third' enciunter, therefore, the check
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

        -- We should never get here, as health
        -- education is presented only for CHW.
        NurseEncounter ->
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
                        |> prenatalSendToHCFormWithDefault data.sendToHCForm

                ( reasonForNotSentCompleted, reasonForNotSentActive ) =
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

                ( accompanyToHealthCenterCompleted, accompanyToHealthCenterActive ) =
                    if isChw then
                        ( taskCompleted form.accompanyToHealthCenter, 1 )

                    else if
                        referToHospitalForNonHIVDiagnosis assembled
                            || referToHospitalDueToAdverseEvent assembled
                            || referToHospitalDueToPastDiagnosis assembled
                            || referToMentalHealthSpecialist assembled
                    then
                        ( 0, 0 )

                    else
                        ( taskCompleted form.accompanyToHealthCenter, 1 )
            in
            ( taskCompleted form.handReferralForm + reasonForNotSentCompleted + accompanyToHealthCenterCompleted
            , 1 + reasonForNotSentActive + accompanyToHealthCenterActive
            )

        NextStepsHealthEducation ->
            let
                form =
                    assembled.measurements.healthEducation
                        |> getMeasurementValueFunc
                        |> healthEducationFormWithDefaultInitialPhase data.healthEducationForm

                ( _, tasks ) =
                    healthEducationFormInputsAndTasks language assembled form
            in
            ( List.map taskCompleted tasks
                |> List.sum
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
                    if nextStepsMeasurementTaken assembled NextStepsWait then
                        1

                    else
                        0
            in
            ( completed
            , 1
            )


{-| This is a convenience for cases where the form values ought to be redefined
to allow multiple values. So, it should go away eventually.
-}
toEverySet : a -> a -> Bool -> EverySet a
toEverySet presentValue absentValue present =
    if present then
        EverySet.singleton presentValue

    else
        EverySet.singleton absentValue


resolvePreviousValue : AssembledData -> (PrenatalMeasurements -> Maybe ( id, PrenatalMeasurement a )) -> (a -> b) -> Maybe b
resolvePreviousValue assembled measurementFunc valueFunc =
    assembled.nursePreviousMeasurementsWithDates
        |> List.filterMap
            (\( _, _, measurements ) ->
                measurementFunc measurements
                    |> Maybe.map (Tuple.second >> .value >> valueFunc)
            )
        |> List.reverse
        |> List.head


resolvePreviousMaybeValue : AssembledData -> (PrenatalMeasurements -> Maybe ( id, PrenatalMeasurement a )) -> (a -> Maybe b) -> Maybe b
resolvePreviousMaybeValue assembled measurementFunc valueFunc =
    assembled.nursePreviousMeasurementsWithDates
        |> List.filterMap
            (\( _, _, measurements ) ->
                measurementFunc measurements
                    |> Maybe.andThen (Tuple.second >> .value >> valueFunc)
            )
        |> List.reverse
        |> List.head


fromBreastExamValue : Maybe BreastExamValue -> BreastExamForm
fromBreastExamValue saved =
    { breast = Maybe.map (.exam >> EverySet.toList) saved
    , selfGuidance = Maybe.map .selfGuidance saved
    }


breastExamFormWithDefault : BreastExamForm -> Maybe BreastExamValue -> BreastExamForm
breastExamFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { breast = or form.breast (value.exam |> EverySet.toList |> Just)
                , selfGuidance = or form.selfGuidance (Just value.selfGuidance)
                }
            )


toBreastExamValueWithDefault : Maybe BreastExamValue -> BreastExamForm -> Maybe BreastExamValue
toBreastExamValueWithDefault saved form =
    breastExamFormWithDefault form saved
        |> toBreastExamValue


toBreastExamValue : BreastExamForm -> Maybe BreastExamValue
toBreastExamValue form =
    -- The `EverySet.singleton` is temporary, until BresatExamForm is
    -- redefined to allow more than one.
    Maybe.map BreastExamValue (Maybe.map EverySet.fromList form.breast)
        |> andMap form.selfGuidance


fromCorePhysicalExamValue : Maybe CorePhysicalExamValue -> CorePhysicalExamForm
fromCorePhysicalExamValue saved =
    { brittleHair = Maybe.map (.hairHead >> EverySet.member BrittleHairCPE) saved
    , paleConjuctiva = Maybe.map (.eyes >> EverySet.member PaleConjuctiva) saved
    , neck = Maybe.map (.neck >> EverySet.toList) saved
    , heart = Maybe.andThen (.heart >> EverySet.toList >> List.head) saved
    , heartMurmur = Maybe.map .heartMurmur saved
    , lungs = Maybe.map (.lungs >> EverySet.toList) saved
    , abdomen = Maybe.map (.abdomen >> EverySet.toList) saved
    , hands = Maybe.map (.hands >> EverySet.toList) saved
    , legs = Maybe.map (.legs >> EverySet.toList) saved
    }


corePhysicalExamFormWithDefault : CorePhysicalExamForm -> Maybe CorePhysicalExamValue -> CorePhysicalExamForm
corePhysicalExamFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { brittleHair = or form.brittleHair (value.hairHead |> EverySet.member BrittleHairCPE |> Just)
                , paleConjuctiva = or form.paleConjuctiva (value.eyes |> EverySet.member PaleConjuctiva |> Just)
                , neck = or form.neck (value.neck |> EverySet.toList |> Just)
                , heart = or form.heart (value.heart |> EverySet.toList |> List.head)
                , heartMurmur = or form.heartMurmur (Just value.heartMurmur)
                , lungs = or form.lungs (value.lungs |> EverySet.toList |> Just)
                , abdomen = or form.abdomen (value.abdomen |> EverySet.toList |> Just)
                , hands = or form.hands (value.hands |> EverySet.toList |> Just)
                , legs = or form.legs (value.legs |> EverySet.toList |> Just)
                }
            )


toCorePhysicalExamValueWithDefault : Maybe CorePhysicalExamValue -> CorePhysicalExamForm -> Maybe CorePhysicalExamValue
toCorePhysicalExamValueWithDefault saved form =
    corePhysicalExamFormWithDefault form saved
        |> toCorePhysicalExamValue


toCorePhysicalExamValue : CorePhysicalExamForm -> Maybe CorePhysicalExamValue
toCorePhysicalExamValue form =
    Maybe.map CorePhysicalExamValue (Maybe.map (toEverySet BrittleHairCPE NormalHairHead) form.brittleHair)
        |> andMap (Maybe.map (toEverySet PaleConjuctiva NormalEyes) form.paleConjuctiva)
        |> andMap (Maybe.map EverySet.singleton form.heart)
        |> andMap form.heartMurmur
        |> andMap (Maybe.map EverySet.fromList form.neck)
        |> andMap (Maybe.map EverySet.fromList form.lungs)
        |> andMap (Maybe.map EverySet.fromList form.abdomen)
        |> andMap (Maybe.map EverySet.fromList form.hands)
        |> andMap (Maybe.map EverySet.fromList form.legs)


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


fromLastMenstrualPeriodValue : Maybe LastMenstrualPeriodValue -> PregnancyDatingForm
fromLastMenstrualPeriodValue saved =
    { lmpRange = Nothing
    , lmpDate = Maybe.map .date saved
    , lmpDateConfident = Maybe.map .confident saved
    , chwLmpConfirmation = Maybe.map .confirmation saved
    , dateSelectorPopupState = Nothing
    }


lastMenstrualPeriodFormWithDefault : PregnancyDatingForm -> Maybe LastMenstrualPeriodValue -> PregnancyDatingForm
lastMenstrualPeriodFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { lmpRange = or form.lmpRange (Just SixMonth)
                , lmpDate = or form.lmpDate (Just value.date)
                , lmpDateConfident = or form.lmpDateConfident (Just value.confident)
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
            if List.all isNothing [ form.receivedIronFolicAcid, form.receivedDewormingPill, form.receivedMebendazole ] then
                Nothing

            else
                [ ifNullableTrue IronAndFolicAcidSupplement form.receivedIronFolicAcid
                , ifNullableTrue DewormingPill form.receivedDewormingPill
                , ifNullableTrue Mebendazole form.receivedMebendazole
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
                                    , [ if isJust form.hivMedicationNotGivenReason then
                                            Just True

                                        else
                                            Nothing
                                      ]
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

                ( derrivedInput, derrivedTask ) =
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
            in
            ( [ header
              , viewQuestionLabel language Translate.TreatmentReviewQuestionStillTaking
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
                ++ derrivedInput
            , [ config.stillTakingFormValue
              , config.missedDosesFormValue
              , config.adverseEventsFormValue
              ]
                ++ derrivedTask
            )
        )
        configForTask
        |> Maybe.withDefault ( [], [] )


fromObstetricalExamValue : Maybe ObstetricalExamValue -> ObstetricalExamForm
fromObstetricalExamValue saved =
    { fundalHeight = Maybe.map (.fundalHeight >> getHeightValue) saved
    , fundalHeightDirty = False
    , fetalPresentation = Maybe.map .fetalPresentation saved
    , fetalMovement = Maybe.map .fetalMovement saved
    , fetalHeartRate = Maybe.map .fetalHeartRate saved
    , fetalHeartRateDirty = False
    , cSectionScar = Maybe.map .cSectionScar saved
    }


obstetricalExamFormWithDefault : ObstetricalExamForm -> Maybe ObstetricalExamValue -> ObstetricalExamForm
obstetricalExamFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { fundalHeight = valueConsideringIsDirtyField form.fundalHeightDirty form.fundalHeight (getHeightValue value.fundalHeight)
                , fundalHeightDirty = form.fundalHeightDirty
                , fetalPresentation = or form.fetalPresentation (Just value.fetalPresentation)
                , fetalMovement = or form.fetalMovement (Just value.fetalMovement)
                , fetalHeartRate = valueConsideringIsDirtyField form.fetalHeartRateDirty form.fetalHeartRate value.fetalHeartRate
                , fetalHeartRateDirty = form.fetalHeartRateDirty
                , cSectionScar = or form.cSectionScar (Just value.cSectionScar)
                }
            )


toObstetricalExamValueWithDefault : Maybe ObstetricalExamValue -> ObstetricalExamForm -> Maybe ObstetricalExamValue
toObstetricalExamValueWithDefault saved form =
    obstetricalExamFormWithDefault form saved
        |> toObstetricalExamValue


toObstetricalExamValue : ObstetricalExamForm -> Maybe ObstetricalExamValue
toObstetricalExamValue form =
    Maybe.map ObstetricalExamValue (Maybe.map HeightInCm form.fundalHeight)
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
                , cSectionInPreviousDelivery = or form.cSectionInPreviousDelivery (EverySet.member CSectionInPreviousDelivery value.previousDelivery |> Just)
                , cSectionReason = or form.cSectionReason (value.cSectionReason |> EverySet.toList |> List.head)
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
            [ Maybe.map (ifTrue CSectionInPreviousDelivery) form.cSectionInPreviousDelivery
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
        |> andMap (Maybe.map EverySet.singleton form.cSectionReason)
        |> andMap previousDeliverySet
        |> andMap (Maybe.map EverySet.singleton form.previousDeliveryPeriod)
        |> andMap obstetricHistorySet


fromFamilyPlanningValue : Maybe (EverySet FamilyPlanningSign) -> FamilyPlanningForm
fromFamilyPlanningValue saved =
    { signs = Maybe.map EverySet.toList saved }


familyPlanningFormWithDefault : FamilyPlanningForm -> Maybe (EverySet FamilyPlanningSign) -> FamilyPlanningForm
familyPlanningFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { signs = or form.signs (EverySet.toList value |> Just) }
            )


toFamilyPlanningValueWithDefault : Maybe (EverySet FamilyPlanningSign) -> FamilyPlanningForm -> Maybe (EverySet FamilyPlanningSign)
toFamilyPlanningValueWithDefault saved form =
    familyPlanningFormWithDefault form saved
        |> toFamilyPlanningValue


toFamilyPlanningValue : FamilyPlanningForm -> Maybe (EverySet FamilyPlanningSign)
toFamilyPlanningValue form =
    Maybe.map (EverySet.fromList >> ifEverySetEmpty NoFamilyPlanning) form.signs


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


malariaPreventionFormWithDefault : MalariaPreventionForm -> Maybe (EverySet MalariaPreventionSign) -> MalariaPreventionForm
malariaPreventionFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { receivedMosquitoNet = or form.receivedMosquitoNet (EverySet.member MosquitoNet value |> Just)
                }
            )


toMalariaPreventionValueWithDefault : Maybe (EverySet MalariaPreventionSign) -> MalariaPreventionForm -> Maybe (EverySet MalariaPreventionSign)
toMalariaPreventionValueWithDefault saved form =
    malariaPreventionFormWithDefault form saved
        |> toMalariaPreventionValue


toMalariaPreventionValue : MalariaPreventionForm -> Maybe (EverySet MalariaPreventionSign)
toMalariaPreventionValue form =
    Maybe.map (toEverySet MosquitoNet NoMalariaPreventionSigns) form.receivedMosquitoNet


fromSocialHistoryValue : Maybe SocialHistoryValue -> SocialHistoryForm
fromSocialHistoryValue saved =
    { accompaniedByPartner = Maybe.map (.socialHistory >> EverySet.member AccompaniedByPartner) saved
    , partnerReceivedCounseling = Maybe.map (.socialHistory >> EverySet.member PartnerHivCounseling) saved
    , partnerReceivedTesting = Maybe.map (.hivTestingResult >> (==) NoHivTesting >> not) saved
    , partnerTestingResult = Maybe.map .hivTestingResult saved
    }


socialHistoryFormWithDefault : SocialHistoryForm -> Maybe SocialHistoryValue -> SocialHistoryForm
socialHistoryFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { accompaniedByPartner = or form.accompaniedByPartner (EverySet.member AccompaniedByPartner value.socialHistory |> Just)
                , partnerReceivedCounseling = or form.partnerReceivedCounseling (EverySet.member PartnerHivCounseling value.socialHistory |> Just)
                , partnerReceivedTesting = or form.partnerReceivedTesting (value.hivTestingResult == NoHivTesting |> not |> Just)
                , partnerTestingResult = or form.partnerTestingResult (Just value.hivTestingResult)
                }
            )


toSocialHistoryValueWithDefault : Maybe SocialHistoryValue -> SocialHistoryForm -> Maybe SocialHistoryValue
toSocialHistoryValueWithDefault saved form =
    socialHistoryFormWithDefault form saved
        |> toSocialHistoryValue


toSocialHistoryValue : SocialHistoryForm -> Maybe SocialHistoryValue
toSocialHistoryValue form =
    let
        socialHistory =
            [ Maybe.map (ifTrue AccompaniedByPartner) form.accompaniedByPartner
            , ifNullableTrue PartnerHivCounseling form.partnerReceivedCounseling
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoSocialHistorySign)
    in
    Maybe.map SocialHistoryValue socialHistory
        |> andMap form.partnerTestingResult


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


historyTasksCompletedFromTotal : AssembledData -> HistoryData -> HistoryTask -> ( Int, Int )
historyTasksCompletedFromTotal assembled data task =
    case task of
        Obstetric ->
            case data.obstetricHistoryStep of
                ObstetricHistoryFirstStep ->
                    let
                        formStep1_ =
                            assembled.measurements.obstetricHistory
                                |> getMeasurementValueFunc
                                |> obstetricHistoryFormWithDefault data.obstetricFormFirstStep

                        intInputs =
                            [ formStep1_.termPregnancy
                            , formStep1_.preTermPregnancy
                            , formStep1_.stillbirthsAtTerm
                            , formStep1_.stillbirthsPreTerm
                            , formStep1_.abortions
                            , formStep1_.liveChildren
                            ]
                    in
                    ( (intInputs
                        |> List.map taskCompleted
                        |> List.sum
                      )
                        + taskCompleted formStep1_.currentlyPregnant
                    , List.length intInputs + 1
                    )

                ObstetricHistorySecondStep ->
                    let
                        formStep2_ =
                            assembled.measurements.obstetricHistoryStep2
                                |> getMeasurementValueFunc
                                |> obstetricHistoryStep2FormWithDefault data.obstetricFormSecondStep

                        boolInputs =
                            [ formStep2_.cSectionInPreviousDelivery
                            , formStep2_.successiveAbortions
                            , formStep2_.successivePrematureDeliveries
                            , formStep2_.stillbornPreviousDelivery
                            , formStep2_.babyDiedOnDayOfBirthPreviousDelivery
                            , formStep2_.partialPlacentaPreviousDelivery
                            , formStep2_.severeHemorrhagingPreviousDelivery
                            , formStep2_.preeclampsiaPreviousPregnancy
                            , formStep2_.convulsionsPreviousDelivery
                            , formStep2_.convulsionsAndUnconsciousPreviousDelivery
                            , formStep2_.gestationalDiabetesPreviousPregnancy
                            , formStep2_.incompleteCervixPreviousPregnancy
                            , formStep2_.rhNegative
                            ]
                    in
                    ( (boolInputs
                        |> List.map taskCompleted
                        |> List.sum
                      )
                        + taskCompleted formStep2_.cSections
                        + taskCompleted formStep2_.cSectionReason
                        + taskCompleted formStep2_.previousDeliveryPeriod
                    , List.length boolInputs + 3
                    )

        Medical ->
            let
                medicalForm =
                    assembled.measurements.medicalHistory
                        |> getMeasurementValueFunc
                        |> medicalHistoryFormWithDefault data.medicalForm

                boolInputs =
                    [ medicalForm.uterineMyoma
                    , medicalForm.diabetes
                    , medicalForm.cardiacDisease
                    , medicalForm.renalDisease
                    , medicalForm.hypertensionBeforePregnancy
                    , medicalForm.tuberculosisPast
                    , medicalForm.tuberculosisPresent
                    , medicalForm.asthma
                    , medicalForm.bowedLegs
                    , medicalForm.hiv
                    ]
            in
            ( boolInputs
                |> List.map taskCompleted
                |> List.sum
            , List.length boolInputs
            )

        Social ->
            let
                socialForm =
                    assembled.measurements.socialHistory
                        |> getMeasurementValueFunc
                        |> socialHistoryFormWithDefault data.socialForm

                showCounselingQuestion =
                    assembled.nursePreviousMeasurementsWithDates
                        |> List.filter
                            (\( _, _, measurements ) ->
                                measurements.socialHistory
                                    |> Maybe.map (Tuple.second >> .value >> .socialHistory >> EverySet.member PartnerHivCounseling)
                                    |> Maybe.withDefault False
                            )
                        |> List.isEmpty

                partnerReceivedCounselingInput =
                    if showCounselingQuestion then
                        [ socialForm.partnerReceivedCounseling ]

                    else
                        []

                showTestingQuestions =
                    assembled.nursePreviousMeasurementsWithDates
                        |> List.filter
                            (\( _, _, measurements ) ->
                                measurements.socialHistory
                                    |> Maybe.map
                                        (\socialHistory ->
                                            let
                                                value =
                                                    Tuple.second socialHistory |> .value
                                            in
                                            (value.hivTestingResult == ResultHivPositive)
                                                || (value.hivTestingResult == ResultHivNegative)
                                        )
                                    |> Maybe.withDefault False
                            )
                        |> List.isEmpty

                partnerReceivedTestingInput =
                    if showTestingQuestions then
                        [ socialForm.partnerReceivedTesting ]

                    else
                        []

                boolInputs =
                    (socialForm.accompaniedByPartner
                        :: partnerReceivedCounselingInput
                    )
                        ++ partnerReceivedTestingInput

                listInputs =
                    if socialForm.partnerReceivedTesting == Just True then
                        [ socialForm.partnerTestingResult ]

                    else
                        []
            in
            ( (boolInputs |> List.map taskCompleted |> List.sum)
                + (listInputs |> List.map taskCompleted |> List.sum)
            , List.length boolInputs + List.length listInputs
            )

        OutsideCare ->
            -- This is not in use, because OutsideCare task got
            -- special treatment at viewHistoryContent().
            ( 0, 0 )


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
            let
                form =
                    assembled.measurements.obstetricalExam
                        |> getMeasurementValueFunc
                        |> obstetricalExamFormWithDefault data.obstetricalExamForm
            in
            ( taskCompleted form.fetalPresentation
                + taskCompleted form.fetalMovement
                + taskCompleted form.cSectionScar
                + ([ Maybe.map (always ()) form.fundalHeight, Maybe.map (always ()) form.fetalHeartRate ]
                    |> List.map taskCompleted
                    |> List.sum
                  )
            , 5
            )

        BreastExam ->
            let
                form =
                    assembled.measurements.breastExam
                        |> getMeasurementValueFunc
                        |> breastExamFormWithDefault data.breastExamForm
            in
            ( taskCompleted form.breast + taskCompleted form.selfGuidance
            , 2
            )


socialHistoryHivTestingResultFromString : String -> Maybe SocialHistoryHivTestingResult
socialHistoryHivTestingResultFromString result =
    case result of
        "positive" ->
            Just ResultHivPositive

        "negative" ->
            Just ResultHivNegative

        "indeterminate" ->
            Just ResultHivIndeterminate

        "none" ->
            Just NoHivTesting

        _ ->
            Nothing


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


prenatalMalariaTestFormWithDefault : PrenatalMalariaTestForm -> Maybe PrenatalMalariaTestValue -> PrenatalMalariaTestForm
prenatalMalariaTestFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    testPerformedValue =
                        List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]

                    testPerformedTodayFromValue =
                        value.executionNote == TestNoteRunToday
                in
                { testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , testPerformedToday = valueConsideringIsDirtyField form.testPerformedTodayDirty form.testPerformedToday testPerformedTodayFromValue
                , testPerformedTodayDirty = form.testPerformedTodayDirty
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , testResult = or form.testResult value.testResult
                , dateSelectorPopupState = form.dateSelectorPopupState
                }
            )


toPrenatalMalariaTestValueWithDefault : Maybe PrenatalMalariaTestValue -> PrenatalMalariaTestForm -> Maybe PrenatalMalariaTestValue
toPrenatalMalariaTestValueWithDefault saved form =
    prenatalMalariaTestFormWithDefault form saved
        |> toPrenatalMalariaTestValue


toPrenatalMalariaTestValue : PrenatalMalariaTestForm -> Maybe PrenatalMalariaTestValue
toPrenatalMalariaTestValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testResult = form.testResult
            }
        )
        form.executionNote


prenatalHIVTestFormWithDefault : PrenatalHIVTestForm -> Maybe PrenatalHIVTestValue -> PrenatalHIVTestForm
prenatalHIVTestFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    knownAsPositiveValue =
                        List.member value.executionNote [ TestNoteKnownAsPositive ]

                    testPerformedValue =
                        List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]

                    testPerformedTodayFromValue =
                        value.executionNote == TestNoteRunToday

                    hivProgramHCValue =
                        Maybe.map (EverySet.member HIVProgramHC)
                            value.hivSigns
                            |> Maybe.withDefault False

                    partnerHIVPositiveValue =
                        Maybe.map (EverySet.member PartnerHIVPositive)
                            value.hivSigns
                            |> Maybe.withDefault False

                    partnerTakingARVValue =
                        Maybe.map (EverySet.member PartnerTakingARV)
                            value.hivSigns
                            |> Maybe.withDefault False

                    partnerSurpressedViralLoadValue =
                        Maybe.map (EverySet.member PartnerSurpressedViralLoad)
                            value.hivSigns
                            |> Maybe.withDefault False
                in
                { knownAsPositive = or form.knownAsPositive (Just knownAsPositiveValue)
                , testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , testPerformedToday = valueConsideringIsDirtyField form.testPerformedTodayDirty form.testPerformedToday testPerformedTodayFromValue
                , testPerformedTodayDirty = form.testPerformedTodayDirty
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , testResult = or form.testResult value.testResult
                , hivProgramHC = valueConsideringIsDirtyField form.hivProgramHCDirty form.hivProgramHC hivProgramHCValue
                , hivProgramHCDirty = form.hivProgramHCDirty
                , partnerHIVPositive = valueConsideringIsDirtyField form.partnerHIVPositiveDirty form.partnerHIVPositive partnerHIVPositiveValue
                , partnerHIVPositiveDirty = form.partnerHIVPositiveDirty
                , partnerTakingARV = valueConsideringIsDirtyField form.partnerTakingARVDirty form.partnerTakingARV partnerTakingARVValue
                , partnerTakingARVDirty = form.partnerTakingARVDirty
                , partnerSurpressedViralLoad = valueConsideringIsDirtyField form.partnerSurpressedViralLoadDirty form.partnerSurpressedViralLoad partnerSurpressedViralLoadValue
                , partnerSurpressedViralLoadDirty = form.partnerSurpressedViralLoadDirty
                , dateSelectorPopupState = form.dateSelectorPopupState
                }
            )


toPrenatalHIVTestValueWithDefault : Maybe PrenatalHIVTestValue -> PrenatalHIVTestForm -> Maybe PrenatalHIVTestValue
toPrenatalHIVTestValueWithDefault saved form =
    prenatalHIVTestFormWithDefault form saved
        |> toPrenatalHIVTestValue


toPrenatalHIVTestValue : PrenatalHIVTestForm -> Maybe PrenatalHIVTestValue
toPrenatalHIVTestValue form =
    Maybe.map
        (\executionNote ->
            let
                hivSigns =
                    [ ifNullableTrue HIVProgramHC form.hivProgramHC
                    , ifNullableTrue PartnerHIVPositive form.partnerHIVPositive
                    , ifNullableTrue PartnerTakingARV form.partnerTakingARV
                    , ifNullableTrue PartnerSurpressedViralLoad form.partnerSurpressedViralLoad
                    ]
                        |> Maybe.Extra.combine
                        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoPrenatalHIVSign)
            in
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testResult = form.testResult
            , hivSigns = hivSigns
            }
        )
        form.executionNote


prenatalUrineDipstickFormWithDefault : PrenatalUrineDipstickForm -> Maybe PrenatalUrineDipstickTestValue -> PrenatalUrineDipstickForm
prenatalUrineDipstickFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    testPerformedValue =
                        List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]

                    testPerformedTodayFromValue =
                        value.executionNote == TestNoteRunToday
                in
                { testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , testPerformedToday = valueConsideringIsDirtyField form.testPerformedTodayDirty form.testPerformedToday testPerformedTodayFromValue
                , testPerformedTodayDirty = form.testPerformedTodayDirty
                , testVariant = or form.testVariant value.testVariant
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , dateSelectorPopupState = form.dateSelectorPopupState
                }
            )


toPrenatalUrineDipstickTestValueWithDefault : Maybe PrenatalUrineDipstickTestValue -> PrenatalUrineDipstickForm -> Maybe PrenatalUrineDipstickTestValue
toPrenatalUrineDipstickTestValueWithDefault saved form =
    prenatalUrineDipstickFormWithDefault form saved
        |> toPrenatalUrineDipstickTestValue


toPrenatalUrineDipstickTestValue : PrenatalUrineDipstickForm -> Maybe PrenatalUrineDipstickTestValue
toPrenatalUrineDipstickTestValue form =
    Maybe.map
        (\executionNote ->
            { testVariant = form.testVariant
            , executionNote = executionNote
            , executionDate = form.executionDate
            , protein = Nothing
            , ph = Nothing
            , glucose = Nothing
            , leukocytes = Nothing
            , nitrite = Nothing
            , urobilinogen = Nothing
            , haemoglobin = Nothing
            , ketone = Nothing
            , bilirubin = Nothing
            }
        )
        form.executionNote


prenatalRandomBloodSugarFormWithDefault : PrenatalRandomBloodSugarForm -> Maybe PrenatalRandomBloodSugarTestValue -> PrenatalRandomBloodSugarForm
prenatalRandomBloodSugarFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    testPerformedValue =
                        List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]

                    testPerformedTodayFromValue =
                        value.executionNote == TestNoteRunToday

                    patientFastedValue =
                        Maybe.map (EverySet.member PrerequisiteFastFor12h)
                            value.testPrerequisites
                in
                { testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , patientFasted = or form.patientFasted patientFastedValue
                , testPerformedToday = valueConsideringIsDirtyField form.testPerformedTodayDirty form.testPerformedToday testPerformedTodayFromValue
                , testPerformedTodayDirty = form.testPerformedTodayDirty
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , dateSelectorPopupState = form.dateSelectorPopupState
                }
            )


toPrenatalRandomBloodSugarValueWithDefault : Maybe PrenatalRandomBloodSugarTestValue -> PrenatalRandomBloodSugarForm -> Maybe PrenatalRandomBloodSugarTestValue
toPrenatalRandomBloodSugarValueWithDefault saved form =
    prenatalRandomBloodSugarFormWithDefault form saved
        |> toPrenatalRandomBloodSugarTestValue


toPrenatalRandomBloodSugarTestValue : PrenatalRandomBloodSugarForm -> Maybe PrenatalRandomBloodSugarTestValue
toPrenatalRandomBloodSugarTestValue form =
    Maybe.map
        (\executionNote ->
            let
                testPrerequisites =
                    Maybe.map
                        (\patientFasted ->
                            if patientFasted then
                                EverySet.singleton PrerequisiteFastFor12h

                            else
                                EverySet.singleton NoTestPrerequisites
                        )
                        form.patientFasted
            in
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testPrerequisites = testPrerequisites
            , sugarCount = Nothing
            }
        )
        form.executionNote


prenatalNonRDTFormWithDefault :
    PrenatalLabsNonRDTForm
    -> Maybe { value | executionNote : PrenatalTestExecutionNote, executionDate : Maybe NominalDate }
    -> PrenatalLabsNonRDTForm
prenatalNonRDTFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    knownAsPositiveValue =
                        List.member value.executionNote [ TestNoteKnownAsPositive ]

                    testPerformedValue =
                        List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]

                    testPerformedTodayFromValue =
                        value.executionNote == TestNoteRunToday
                in
                { knownAsPositive = or form.knownAsPositive (Just knownAsPositiveValue)
                , testPerformed = valueConsideringIsDirtyField form.testPerformedDirty form.testPerformed testPerformedValue
                , testPerformedDirty = form.testPerformedDirty
                , testPerformedToday = valueConsideringIsDirtyField form.testPerformedTodayDirty form.testPerformedToday testPerformedTodayFromValue
                , testPerformedTodayDirty = form.testPerformedTodayDirty
                , executionNote = valueConsideringIsDirtyField form.executionNoteDirty form.executionNote value.executionNote
                , executionNoteDirty = form.executionNoteDirty
                , executionDate = maybeValueConsideringIsDirtyField form.executionDateDirty form.executionDate value.executionDate
                , executionDateDirty = form.executionDateDirty
                , dateSelectorPopupState = form.dateSelectorPopupState
                }
            )


toPrenatalNonRDTValueWithDefault :
    Maybe { value | executionNote : PrenatalTestExecutionNote, executionDate : Maybe NominalDate }
    -> (PrenatalTestExecutionNote -> Maybe NominalDate -> { value | executionNote : PrenatalTestExecutionNote, executionDate : Maybe NominalDate })
    -> PrenatalLabsNonRDTForm
    -> Maybe { value | executionNote : PrenatalTestExecutionNote, executionDate : Maybe NominalDate }
toPrenatalNonRDTValueWithDefault saved withEmptyResultsFunc form =
    let
        formWithDefault =
            prenatalNonRDTFormWithDefault form saved
    in
    Maybe.map (\executionNote -> withEmptyResultsFunc executionNote formWithDefault.executionDate)
        formWithDefault.executionNote


toHepatitisBTestValueWithEmptyResults : PrenatalTestExecutionNote -> Maybe NominalDate -> PrenatalHepatitisBTestValue
toHepatitisBTestValueWithEmptyResults note date =
    PrenatalHepatitisBTestValue note date Nothing Nothing


toSyphilisTestValueWithEmptyResults : PrenatalTestExecutionNote -> Maybe NominalDate -> PrenatalSyphilisTestValue
toSyphilisTestValueWithEmptyResults note date =
    PrenatalSyphilisTestValue note date Nothing Nothing Nothing


toHemoglobinTestValueWithEmptyResults : PrenatalTestExecutionNote -> Maybe NominalDate -> PrenatalHemoglobinTestValue
toHemoglobinTestValueWithEmptyResults note date =
    PrenatalHemoglobinTestValue note date Nothing


toBloodGpRsTestValueWithEmptyResults : PrenatalTestExecutionNote -> Maybe NominalDate -> PrenatalBloodGpRsTestValue
toBloodGpRsTestValueWithEmptyResults note date =
    PrenatalBloodGpRsTestValue note date Nothing Nothing


toHIVPCRTestValueWithEmptyResults : PrenatalTestExecutionNote -> Maybe NominalDate -> PrenatalHIVPCRTestValue
toHIVPCRTestValueWithEmptyResults note date =
    PrenatalHIVPCRTestValue note date Nothing Nothing


laboratoryTasks : List LaboratoryTask
laboratoryTasks =
    [ TaskHIVTest
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

        TaskCompletePreviousTests ->
            not <| taskExpected TaskCompletePreviousTests


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
                -- HIV and Hepatitis B are considered chronical diseases.
                -- If patient declared to have one of them, there's no point
                -- in testing for it.
                isKnownAsPositive getMeasurementFunc =
                    List.filter
                        (\( _, _, measurements ) ->
                            getMeasurementFunc measurements
                                |> getMeasurementValueFunc
                                |> Maybe.map (.executionNote >> (==) TestNoteKnownAsPositive)
                                |> Maybe.withDefault False
                        )
                        assembled.nursePreviousMeasurementsWithDates
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
                    True

                TaskHIVPCRTest ->
                    isKnownAsPositive .hivTest || diagnosedPreviously DiagnosisHIV assembled

                TaskCompletePreviousTests ->
                    -- If we got this far, history task was completed.
                    False

        else
            task == TaskCompletePreviousTests


generatePendingLabsFromPreviousEncounters : AssembledData -> List ( NominalDate, PrenatalEncounterId, List PrenatalLaboratoryTest )
generatePendingLabsFromPreviousEncounters assembled =
    List.filterMap
        (\( date, _, measurements ) ->
            getMeasurementValueFunc measurements.labsResults
                |> Maybe.andThen
                    (\value ->
                        let
                            encounterId =
                                Maybe.andThen (Tuple.second >> .encounterId) measurements.labsResults

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
                            Maybe.map (\id -> ( date, id, pendingTests )) encounterId
                    )
        )
        assembled.nursePreviousMeasurementsWithDates


generatePreviousLaboratoryTestsDatesDict : NominalDate -> AssembledData -> Dict LaboratoryTask (List NominalDate)
generatePreviousLaboratoryTestsDatesDict currentDate assembled =
    let
        generateTestDates getMeasurementFunc resultsExistFunc resultsValidFunc =
            List.filterMap
                (\( _, _, measurements ) ->
                    let
                        measurement =
                            getMeasurementFunc measurements

                        dateMeasured =
                            -- Date on which test was recorded.
                            -- Note that this is not the date when test was performed,
                            -- because it's possible to set past date for that.
                            -- We need the recorded date, because the logic says that
                            -- test that will not have results set for over 14 days is expired.
                            -- Can default to current date, because we use it only when there's
                            -- measurement value, and this means that there must be dateMeasured set.
                            Maybe.map (Tuple.second >> .dateMeasured) measurement
                                |> Maybe.withDefault currentDate
                    in
                    getMeasurementValueFunc measurement
                        |> Maybe.andThen
                            (\value ->
                                if List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ] then
                                    if resultsExistFunc value && (not <| resultsValidFunc value) then
                                        -- Entered result is not valid, therefore,
                                        -- we treat the test as if it was not performed.
                                        Nothing

                                    else if (not <| resultsExistFunc value) && (Date.diff Days dateMeasured currentDate > prenatalLabExpirationPeriod) then
                                        -- No results were entered for more than 14 days since the
                                        -- day on which measurement was taken.
                                        -- Test is considered expired, and is being ignored
                                        -- (as if it was never performed).
                                        Nothing

                                    else
                                        value.executionDate

                                else
                                    Nothing
                            )
                )
                assembled.nursePreviousMeasurementsWithDates

        isTestResultValid =
            .testResult
                >> Maybe.map ((/=) PrenatalTestIndeterminate)
                >> -- In case test result was not set yet, we consider
                   -- it to be valid, because results for some test are
                   -- updated after few hours, or even days.
                   Maybe.withDefault True
    in
    [ ( TaskHIVTest, generateTestDates .hivTest (always True) isTestResultValid )
    , ( TaskSyphilisTest, generateTestDates .syphilisTest (.testResult >> isJust) isTestResultValid )
    , ( TaskHepatitisBTest, generateTestDates .hepatitisBTest (.testResult >> isJust) isTestResultValid )
    , ( TaskMalariaTest, generateTestDates .malariaTest (always True) isTestResultValid )
    , ( TaskBloodGpRsTest, generateTestDates .bloodGpRsTest (.bloodGroup >> isJust) (always True) )
    , ( TaskUrineDipstickTest, generateTestDates .urineDipstickTest (.protein >> isJust) (always True) )
    , ( TaskHemoglobinTest, generateTestDates .hemoglobinTest (.hemoglobinCount >> isJust) (always True) )
    , ( TaskRandomBloodSugarTest, generateTestDates .randomBloodSugarTest (.sugarCount >> isJust) (always True) )
    ]
        |> Dict.fromList


laboratoryTaskIconClass : LaboratoryTask -> String
laboratoryTaskIconClass task =
    case task of
        TaskHIVTest ->
            "laboratory-hiv"

        TaskSyphilisTest ->
            "laboratory-syphilis"

        TaskHepatitisBTest ->
            "laboratory-hepatitis-b"

        TaskMalariaTest ->
            "laboratory-malaria-testing"

        TaskBloodGpRsTest ->
            "laboratory-blood-group"

        TaskUrineDipstickTest ->
            "laboratory-urine-dipstick"

        TaskHemoglobinTest ->
            "laboratory-hemoglobin"

        TaskRandomBloodSugarTest ->
            "laboratory-blood-sugar"

        TaskHIVPCRTest ->
            "laboratory-hiv"

        TaskCompletePreviousTests ->
            "laboratory-history"


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


symptomReviewFormInputsAndTasks : Language -> SymptomReviewStep -> SymptomReviewForm -> ( List (Html Msg), Int, Int )
symptomReviewFormInputsAndTasks language step form =
    case step of
        SymptomReviewStepSymptoms ->
            symptomReviewFormInputsAndTasksSymptoms language form

        SymptomReviewStepQuestions ->
            symptomReviewFormInputsAndTasksQuestions language form


symptomReviewFormInputsAndTasksSymptoms : Language -> SymptomReviewForm -> ( List (Html Msg), Int, Int )
symptomReviewFormInputsAndTasksSymptoms language form =
    ( [ div [ class "ui form symptom-review" ]
            [ viewLabel language Translate.SelectIllnessSymptoms
            , viewCheckBoxMultipleSelectInput language
                [ BurningWithUrination, AbnormalVaginalDischarge, NauseaAndVomiting, Heartburn, LegCramps, LowBackPain ]
                [ CoughContinuous, PelvicPain, Constipation, VaricoseVeins, LegPainRedness ]
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


outsideCareFormWithDefault : OutsideCareForm -> Maybe PrenatalOutsideCareValue -> OutsideCareForm
outsideCareFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                let
                    malariaMedications =
                        filterIllnessOptions outsideCareMedicationOptionsMalaria

                    hypertensionMedications =
                        filterIllnessOptions outsideCareMedicationOptionsHypertension

                    syphilisMedications =
                        filterIllnessOptions outsideCareMedicationOptionsSyphilis

                    anemiaMedications =
                        filterIllnessOptions outsideCareMedicationOptionsAnemia

                    hivMedications =
                        filterIllnessOptions outsideCareMedicationOptionsHIV

                    filterIllnessOptions options =
                        Maybe.map
                            (EverySet.toList
                                >> List.filter
                                    (\medication ->
                                        List.member medication options
                                    )
                            )
                            value.medications
                in
                { seenAtAnotherFacility = or form.seenAtAnotherFacility (EverySet.member SeenAtAnotherFacility value.signs |> Just)
                , givenNewDiagnosis = or form.givenNewDiagnosis (EverySet.member GivenNewDiagnoses value.signs |> Just)
                , givenMedicine = or form.givenMedicine (EverySet.member GivenMedicine value.signs |> Just)
                , diagnoses = maybeValueConsideringIsDirtyField form.diagnosesDirty form.diagnoses (value.diagnoses |> Maybe.map EverySet.toList)
                , diagnosesDirty = form.diagnosesDirty
                , malariaMedications = or form.malariaMedications malariaMedications
                , hypertensionMedications = or form.hypertensionMedications hypertensionMedications
                , syphilisMedications = or form.syphilisMedications syphilisMedications
                , hivMedications = or form.hivMedications hivMedications
                , anemiaMedications = or form.anemiaMedications anemiaMedications
                }
            )


toPrenatalOutsideCareValueWithDefault : Maybe PrenatalOutsideCareValue -> OutsideCareForm -> Maybe PrenatalOutsideCareValue
toPrenatalOutsideCareValueWithDefault saved form =
    outsideCareFormWithDefault form saved
        |> toPrenatalOutsideCareValue


toPrenatalOutsideCareValue : OutsideCareForm -> Maybe PrenatalOutsideCareValue
toPrenatalOutsideCareValue form =
    let
        maybeSigns =
            [ Maybe.map (ifTrue SeenAtAnotherFacility) form.seenAtAnotherFacility
            , ifNullableTrue GivenNewDiagnoses form.givenNewDiagnosis
            , ifNullableTrue GivenMedicine form.givenMedicine
            ]
                |> Maybe.Extra.combine
                |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoPrenatalOutsideCareSigns)
    in
    Maybe.map
        (\signs ->
            let
                diagnoses =
                    Maybe.map (EverySet.fromList >> ifEverySetEmpty NoPrenatalDiagnosis) form.diagnoses

                mapMedications illnessMedications =
                    [ Maybe.map (EverySet.fromList >> Just) illnessMedications
                        |> Maybe.withDefault (Just EverySet.empty)
                    ]

                medications =
                    mapMedications form.malariaMedications
                        ++ mapMedications form.hypertensionMedications
                        ++ mapMedications form.syphilisMedications
                        ++ mapMedications form.anemiaMedications
                        ++ mapMedications form.hivMedications
                        |> Maybe.Extra.combine
                        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoPrenatalOutsideCareMedications)
            in
            { signs = signs
            , diagnoses = diagnoses
            , medications = medications
            }
        )
        maybeSigns


outsideCareFormInputsAndTasks : Language -> OutsideCareStep -> OutsideCareForm -> ( List (Html Msg), List (Maybe Bool) )
outsideCareFormInputsAndTasks language step form =
    case step of
        OutsideCareStepDiagnoses ->
            outsideCareFormInputsAndTasksDiagnoses language form

        OutsideCareStepMedications ->
            outsideCareFormInputsAndTasksMedications language form


outsideCareFormInputsAndTasksDiagnoses : Language -> OutsideCareForm -> ( List (Html Msg), List (Maybe Bool) )
outsideCareFormInputsAndTasksDiagnoses language form =
    let
        ( givenNewDiagnosisSection, givenNewDiagnosisTasks ) =
            if form.seenAtAnotherFacility == Just True then
                let
                    ( newDiagnosisSection, newDiagnosisTasks ) =
                        if form.givenNewDiagnosis == Just True then
                            let
                                ( givenMedicineSection, givenMedicineTasks ) =
                                    Maybe.map
                                        (\diagnoses ->
                                            if
                                                List.any (\diagnosis -> List.member diagnosis diagnoses)
                                                    outsideCareDiagnosesWithPossibleMedication
                                            then
                                                ( [ viewQuestionLabel language <| Translate.PrenatalOutsideCareSignQuestion GivenMedicine
                                                  , viewBoolInput
                                                        language
                                                        form.givenMedicine
                                                        (SetOutsideCareSignBoolInput
                                                            (\value form_ ->
                                                                { form_
                                                                    | givenMedicine = Just value
                                                                    , malariaMedications = Nothing
                                                                    , hypertensionMedications = Nothing
                                                                    , syphilisMedications = Nothing
                                                                    , hivMedications = Nothing
                                                                    , anemiaMedications = Nothing
                                                                }
                                                            )
                                                        )
                                                        "given-medicine"
                                                        Nothing
                                                  ]
                                                , [ form.givenMedicine ]
                                                )

                                            else
                                                ( [], [] )
                                        )
                                        form.diagnoses
                                        |> Maybe.withDefault ( [], [] )
                            in
                            ( [ viewLabel language Translate.SelectAllDiagnoses
                              , viewCheckBoxMultipleSelectInput language
                                    outsideCareDiagnosesLeftColumn
                                    outsideCareDiagnosesRightColumn
                                    (form.diagnoses |> Maybe.withDefault [])
                                    (Just DiagnosisOther)
                                    SetOutsideCareDiagnosis
                                    Translate.PrenatalDiagnosis
                              ]
                                ++ givenMedicineSection
                            , [ if isJust form.diagnoses then
                                    Just True

                                else
                                    Nothing
                              ]
                                ++ givenMedicineTasks
                            )

                        else
                            ( [], [] )
                in
                ( [ viewQuestionLabel language <| Translate.PrenatalOutsideCareSignQuestion GivenNewDiagnoses
                  , viewBoolInput
                        language
                        form.givenNewDiagnosis
                        (SetOutsideCareSignBoolInput
                            (\value form_ ->
                                { form_
                                    | givenNewDiagnosis = Just value
                                    , givenMedicine = Nothing
                                    , diagnoses = Nothing
                                    , diagnosesDirty = True
                                }
                            )
                        )
                        "given-new-diagnosis"
                        Nothing
                  ]
                    ++ newDiagnosisSection
                , [ form.givenNewDiagnosis ] ++ newDiagnosisTasks
                )

            else
                ( [], [] )
    in
    ( [ viewQuestionLabel language <| Translate.PrenatalOutsideCareSignQuestion SeenAtAnotherFacility
      , viewBoolInput
            language
            form.seenAtAnotherFacility
            (SetOutsideCareSignBoolInput
                (\value form_ ->
                    { form_
                        | seenAtAnotherFacility = Just value
                        , givenNewDiagnosis = Nothing
                        , givenMedicine = Nothing
                        , diagnoses = Nothing
                        , diagnosesDirty = True
                    }
                )
            )
            "seen-at-another-facility"
            Nothing
      ]
        ++ givenNewDiagnosisSection
    , [ form.seenAtAnotherFacility ] ++ givenNewDiagnosisTasks
    )


outsideCareFormInputsAndTasksMedications : Language -> OutsideCareForm -> ( List (Html Msg), List (Maybe Bool) )
outsideCareFormInputsAndTasksMedications language form =
    if form.givenMedicine == Just True then
        Maybe.map
            (\diagnoses ->
                let
                    ( malariaInputs, malariaTasks ) =
                        if List.member DiagnosisMalaria diagnoses then
                            ( [ viewHeader <| Translate.PrenatalDiagnosis DiagnosisMalaria
                              , selectTreatmentOptionsInput outsideCareMedicationOptionsMalaria
                                    NoOutsideCareMedicationForMalaria
                                    form.malariaMedications
                                    SetOutsideCareMalariaMedication
                              ]
                            , [ if isJust form.malariaMedications then
                                    Just True

                                else
                                    Nothing
                              ]
                            )

                        else
                            ( [], [] )

                    ( hypertensionInputs, hypertensionTasks ) =
                        if
                            List.any (\diagnosis -> List.member diagnosis diagnoses)
                                [ DiagnosisGestationalHypertensionImmediate
                                , DiagnosisChronicHypertensionImmediate
                                ]
                        then
                            ( [ viewHeader Translate.Hypertension
                              , selectTreatmentOptionsInput outsideCareMedicationOptionsHypertension
                                    NoOutsideCareMedicationForHypertension
                                    form.hypertensionMedications
                                    SetOutsideCareHypertensionMedication
                              ]
                            , [ if isJust form.hypertensionMedications then
                                    Just True

                                else
                                    Nothing
                              ]
                            )

                        else
                            ( [], [] )

                    ( syphilisInputs, syphilisTasks ) =
                        if List.member DiagnosisSyphilis diagnoses then
                            ( [ viewHeader <| Translate.PrenatalDiagnosis DiagnosisSyphilis
                              , selectTreatmentOptionsInput outsideCareMedicationOptionsSyphilis
                                    NoOutsideCareMedicationForSyphilis
                                    form.syphilisMedications
                                    SetOutsideCareSyphilisMedication
                              ]
                            , [ if isJust form.syphilisMedications then
                                    Just True

                                else
                                    Nothing
                              ]
                            )

                        else
                            ( [], [] )

                    ( anemiaInputs, anemiaTasks ) =
                        if List.member DiagnosisModerateAnemia diagnoses then
                            ( [ viewHeader <| Translate.PrenatalDiagnosis DiagnosisModerateAnemia
                              , selectTreatmentOptionsInput outsideCareMedicationOptionsAnemia
                                    NoOutsideCareMedicationForAnemia
                                    form.anemiaMedications
                                    SetOutsideCareAnemiaMedication
                              ]
                            , [ if isJust form.anemiaMedications then
                                    Just True

                                else
                                    Nothing
                              ]
                            )

                        else
                            ( [], [] )

                    ( hivInputs, hivTasks ) =
                        if List.member DiagnosisHIV diagnoses then
                            ( [ viewHeader <| Translate.PrenatalDiagnosis DiagnosisHIV
                              , selectTreatmentOptionsInput outsideCareMedicationOptionsHIV
                                    NoOutsideCareMedicationForHIV
                                    form.hivMedications
                                    SetOutsideCareHIVMedication
                              ]
                            , [ if isJust form.hivMedications then
                                    Just True

                                else
                                    Nothing
                              ]
                            )

                        else
                            ( [], [] )

                    selectTreatmentOptionsInput allOptions noneOption currentValue setMsg =
                        let
                            options =
                                List.filter ((/=) noneOption) allOptions
                        in
                        viewCheckBoxMultipleSelectCustomInput language
                            options
                            []
                            (Maybe.withDefault [] currentValue)
                            (Just noneOption)
                            setMsg
                            (viewOutsideCareMedicationOption language)

                    viewHeader diagnosisTransId =
                        div [ class "label" ]
                            [ span [] [ text <| translate language Translate.DiagnosedAtAnotherFacilityPrefix ]
                            , text " "
                            , span [ class "diagnosis" ] [ text <| translate language diagnosisTransId ]
                            , text " "
                            , span [] [ text <| translate language Translate.DiagnosedAtAnotherFacilitySuffix ]
                            ]
                in
                ( malariaInputs ++ hypertensionInputs ++ syphilisInputs ++ anemiaInputs ++ hivInputs
                , malariaTasks ++ hypertensionTasks ++ syphilisTasks ++ anemiaTasks ++ hivTasks
                )
            )
            form.diagnoses
            |> Maybe.withDefault ( [], [] )

    else
        ( [], [] )


viewOutsideCareMedicationOption : Language -> PrenatalOutsideCareMedication -> Html any
viewOutsideCareMedicationOption language medication =
    if List.member medication noOutsideCareMedicationOptions then
        label [] [ text <| translate language <| Translate.PrenatalOutsideCareMedicationLabel medication ]

    else
        viewOutsideCareMedicationOptionWithDosage language medication


outsideCareMedicationOptionsMalaria : List PrenatalOutsideCareMedication
outsideCareMedicationOptionsMalaria =
    [ OutsideCareMedicationQuinineSulphate
    , OutsideCareMedicationCoartem
    , NoOutsideCareMedicationForMalaria
    ]


outsideCareMedicationOptionsHypertension : List PrenatalOutsideCareMedication
outsideCareMedicationOptionsHypertension =
    [ OutsideCareMedicationMethyldopa2
    , OutsideCareMedicationMethyldopa3
    , OutsideCareMedicationMethyldopa4
    , OutsideCareMedicationCarvedilol
    , OutsideCareMedicationAmlodipine
    , NoOutsideCareMedicationForHypertension
    ]


outsideCareMedicationOptionsSyphilis : List PrenatalOutsideCareMedication
outsideCareMedicationOptionsSyphilis =
    [ OutsideCareMedicationPenecilin1
    , OutsideCareMedicationPenecilin3
    , OutsideCareMedicationErythromycin
    , OutsideCareMedicationAzithromycin
    , OutsideCareMedicationCeftriaxon
    , NoOutsideCareMedicationForSyphilis
    ]


outsideCareMedicationOptionsAnemia : List PrenatalOutsideCareMedication
outsideCareMedicationOptionsAnemia =
    [ OutsideCareMedicationIron1
    , OutsideCareMedicationIron2
    , OutsideCareMedicationFolicAcid
    , NoOutsideCareMedicationForAnemia
    ]


outsideCareMedicationOptionsHIV : List PrenatalOutsideCareMedication
outsideCareMedicationOptionsHIV =
    [ OutsideCareMedicationTDF3TC
    , OutsideCareMedicationDolutegravir
    , NoOutsideCareMedicationForHIV
    ]


noOutsideCareMedicationOptions : List PrenatalOutsideCareMedication
noOutsideCareMedicationOptions =
    [ NoOutsideCareMedicationForMalaria
    , NoOutsideCareMedicationForHypertension
    , NoOutsideCareMedicationForSyphilis
    , NoOutsideCareMedicationForAnemia
    , NoOutsideCareMedicationForHIV
    ]


viewOutsideCareMedicationOptionWithDosage : Language -> PrenatalOutsideCareMedication -> Html any
viewOutsideCareMedicationOptionWithDosage language medication =
    label []
        [ span [ class "treatment" ] [ text <| translate language <| Translate.PrenatalOutsideCareMedicationLabel medication ]
        , text ": "
        , span [ class "dosage" ] [ text <| translate language <| Translate.PrenatalOutsideCareMedicationDosage medication ]
        ]


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
