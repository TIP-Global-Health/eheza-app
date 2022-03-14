module Pages.Prenatal.Activity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc, heightValueFunc, muacIndication, muacValueFunc, prenatalLabExpirationPeriod, weightValueFunc)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalEncounter.Model exposing (PrenatalDiagnosis(..), PrenatalEncounterType(..))
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, diffDays, diffWeeks)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (VitalsForm)
import Measurement.Utils exposing (sendToHCFormWithDefault, vitalsFormWithDefault)
import Pages.AcuteIllness.Activity.Utils exposing (getCurrentReasonForMedicationNonAdministration, nonAdministrationReasonToSign)
import Pages.AcuteIllness.Activity.View exposing (viewAdministeredMedicationCustomLabel, viewAdministeredMedicationLabel, viewAdministeredMedicationQuestion)
import Pages.Prenatal.Activity.Model exposing (..)
import Pages.Prenatal.Activity.Types exposing (..)
import Pages.Prenatal.Encounter.Utils exposing (isFirstEncounter)
import Pages.Prenatal.Model exposing (AssembledData)
import Pages.Prenatal.Utils exposing (..)
import Pages.Utils
    exposing
        ( ifEverySetEmpty
        , ifNullableTrue
        , ifTrue
        , maybeValueConsideringIsDirtyField
        , taskAllCompleted
        , taskCompleted
        , valueConsideringIsDirtyField
        , viewBoolInput
        , viewCheckBoxSelectInput
        , viewQuestionLabel
        )
import Translate exposing (Language, translate)


expectActivity : NominalDate -> AssembledData -> PrenatalActivity -> Bool
expectActivity currentDate assembled activity =
    case assembled.encounter.encounterType of
        NurseEncounter ->
            case activity of
                PregnancyDating ->
                    -- Only show on first encounter
                    isFirstEncounter assembled

                History ->
                    True

                Examination ->
                    True

                FamilyPlanning ->
                    True

                Backend.PrenatalActivity.Model.MalariaPrevention ->
                    assembled.nursePreviousMeasurementsWithDates
                        |> List.filter
                            (\( _, measurements ) ->
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
                    resolveNextStepsTasks currentDate assembled
                        |> List.filter (expectNextStepsTask currentDate assembled)
                        |> List.isEmpty
                        |> not

                -- Unique Chw activities.
                _ ->
                    False

        ChwFirstEncounter ->
            case activity of
                PregnancyDating ->
                    -- Do not show, if patient already visited health center.
                    isFirstEncounter assembled

                Laboratory ->
                    -- Do not show, if patient already visited health center.
                    isFirstEncounter assembled

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
            if isFirstEncounter assembled then
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


resolveNextStepsTasks : NominalDate -> AssembledData -> List NextStepsTask
resolveNextStepsTasks currentDate assembled =
    let
        tasks =
            case assembled.encounter.encounterType of
                NurseEncounter ->
                    -- The order is important. Do not change.
                    [ NextStepsHealthEducation, NextStepsMedicationDistribution, NextStepsRecommendedTreatment, NextStepsSendToHC ]

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
                    let
                        severeMalariaTreatment =
                            getMeasurementValueFunc assembled.measurements.recommendedTreatment
                                |> Maybe.andThen (.signs >> Maybe.map (EverySet.member TreatementReferToHospital))
                                |> Maybe.withDefault False
                    in
                    emergencyReferalRequired assembled
                        || (diagnosed DiagnosisHIV assembled && hivProgramAtHC assembled)
                        || (diagnosed DiagnosisMalaria assembled && severeMalariaTreatment)

                _ ->
                    dangerSigns

        -- Common task for nurse and CHW.
        NextStepsHealthEducation ->
            case assembled.encounter.encounterType of
                NurseEncounter ->
                    -- Emergency refferal is not required.
                    (not <| emergencyReferalRequired assembled)
                        && -- Appear whenever HIV test was performed.
                           isJust assembled.measurements.hivTest

                ChwPostpartumEncounter ->
                    True

                _ ->
                    False

        -- Exclusive CHW task.
        NextStepsNewbornEnrolment ->
            assembled.encounter.encounterType == ChwPostpartumEncounter

        -- Exclusive Nurse task.
        NextStepsMedicationDistribution ->
            -- Emergency refferal is not required.
            (not <| emergencyReferalRequired assembled)
                && -- We were asking if Mebendazole was given already.
                   showMebendazoleQuestion currentDate assembled
                && -- The answer was that Mebendazole was not given yet.
                   (getMeasurementValueFunc assembled.measurements.medication
                        |> Maybe.map (EverySet.member Mebendazole >> not)
                        |> Maybe.withDefault False
                   )

        -- Exclusive Nurse task.
        NextStepsRecommendedTreatment ->
            -- Emergency refferal is not required.
            (not <| emergencyReferalRequired assembled)
                && diagnosed DiagnosisMalaria assembled


emergencyReferalRequired : AssembledData -> Bool
emergencyReferalRequired assembled =
    EverySet.toList assembled.encounter.diagnoses
        |> List.filter diagnosisRequiresEmergencyReferal
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
            isJust assembled.measurements.medicationDistribution

        NextStepsRecommendedTreatment ->
            isJust assembled.measurements.recommendedTreatment


showMebendazoleQuestion : NominalDate -> AssembledData -> Bool
showMebendazoleQuestion currentDate assembled =
    assembled.globalLmpDate
        |> Maybe.map
            (\lmpDate ->
                let
                    egaInWeeks =
                        calculateEGAWeeks currentDate lmpDate

                    dewormingPillNotGiven =
                        List.filter
                            (\( _, measurements ) ->
                                measurements.medication
                                    |> Maybe.map (Tuple.second >> .value >> EverySet.member DewormingPill)
                                    |> Maybe.withDefault False
                            )
                            assembled.nursePreviousMeasurementsWithDates
                            |> List.isEmpty

                    mebenadazoleNotPrescribed =
                        List.filter
                            (\( _, measurements ) ->
                                measurements.medicationDistribution
                                    |> Maybe.map (Tuple.second >> .value >> .distributionSigns >> EverySet.member Mebendezole)
                                    |> Maybe.withDefault False
                            )
                            assembled.nursePreviousMeasurementsWithDates
                            |> List.isEmpty
                in
                -- Starting EGA week 24.
                (egaInWeeks >= 24)
                    && -- Previous variation had a question about deworming pill,
                       -- which is actually Menendazole, or something similar.
                       -- If somewhere during previous encounters patient stated that
                       -- deworming pill was given, we do not ask about Mebendazole.
                       dewormingPillNotGiven
                    && -- Mebendazole was not prescribed during the current pregnancy.
                       mebenadazoleNotPrescribed
            )
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
            List.map Tuple.second assembled.nursePreviousMeasurementsWithDates
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
                                    (\( encounterDate, measurements ) ->
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


getMotherHeightMeasurement : PrenatalMeasurements -> Maybe HeightInCm
getMotherHeightMeasurement measurements =
    getMeasurementValueFunc measurements.nutrition
        |> Maybe.map .height


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

        diagnosesByMedication =
            if showMebendazoleQuestion currentDate assembled then
                EverySet.singleton DiagnosisPrescribeMebendezole

            else
                EverySet.empty

        diagnisisByLabResults =
            List.filter (matchLabResultsPrenatalDiagnosis dangerSignsList assembled.measurements) labResultsDiagnoses
                |> EverySet.fromList

        dangerSignsList =
            generateDangerSignsListForNurse assembled

        diagnosesByDangerSigns =
            List.filter (matchEmergencyReferalPrenatalDiagnosis egaInWeeks dangerSignsList assembled.measurements) emergencyReferralDiagnosesInitial
                |> EverySet.fromList
    in
    EverySet.union diagnosesByMedication diagnisisByLabResults
        |> EverySet.union diagnosesByDangerSigns


matchEmergencyReferalPrenatalDiagnosis : Maybe Int -> List DangerSign -> PrenatalMeasurements -> PrenatalDiagnosis -> Bool
matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs measurements diagnosis =
    case diagnosis of
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
            matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs measurements DiagnosisMiscarriage

        DiagnosisPlacentaPrevia ->
            Maybe.map
                (\egaWeeks ->
                    (egaWeeks > 22) && List.member VaginalBleeding signs
                )
                egaInWeeks
                |> Maybe.withDefault False

        DiagnosisPlacentalAbruption ->
            matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs measurements DiagnosisPlacentaPrevia
                || matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs measurements DiagnosisObstructedLabor

        DiagnosisUterineRupture ->
            matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs measurements DiagnosisPlacentaPrevia
                || matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs measurements DiagnosisObstructedLabor

        DiagnosisObstructedLabor ->
            Maybe.map
                (\egaWeeks ->
                    (egaWeeks >= 22) && List.member AbdominalPain signs
                )
                egaInWeeks
                |> Maybe.withDefault False

        DiagnosisPostAbortionSepsis ->
            List.member AbdominalPain signs
                || matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs measurements DiagnosisMiscarriage

        DiagnosisEctopicPregnancy ->
            matchEmergencyReferalPrenatalDiagnosis egaInWeeks signs measurements DiagnosisMiscarriage
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
            List.member SevereVomiting signs

        DiagnosisMaternalComplications ->
            List.member ExtremeWeakness signs
                || List.member Unconscious signs
                || List.member LooksVeryIll signs

        DiagnosisInfection ->
            List.member Fever signs
                && (List.member ExtremeWeakness signs || respiratoryRateElevated measurements)

        DiagnosisImminentDelivery ->
            List.member ImminentDelivery signs

        DiagnosisLaborAndDelivery ->
            List.member Labor signs

        -- Non Emergency Referral diagnoses.
        _ ->
            False


matchLabResultsPrenatalDiagnosis : List DangerSign -> PrenatalMeasurements -> PrenatalDiagnosis -> Bool
matchLabResultsPrenatalDiagnosis dangerSigns measurements diagnosis =
    let
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
    in
    case diagnosis of
        DiagnosisHIV ->
            testedPositiveAt .hivTest

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
                                EverySet.member IllnessSymptomRash symptoms
                                    || EverySet.member IllnessSymptomPainlessUlcerMouth symptoms
                                    || EverySet.member IllnessSymptomPainlessUlcerGenitals symptoms
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
            positiveMalariaTest
                && ((-- Either hemoglobin test was not performed, or,
                     -- hemoglobin count is within normal ranges.
                     Maybe.map (\count -> count >= 11) hemoglobinCount
                        |> Maybe.withDefault True
                    )
                        || -- When severe Anemia with complications is diagnosed,
                           -- we view Malaia as separate diagnosis.
                           -- Therefore's not 'Malarial and Severe Anemia with
                           -- complications' diagnosis.
                           matchLabResultsPrenatalDiagnosis dangerSigns measurements DiagnosisSevereAnemiaWithComplications
                   )

        DiagnosisMalariaWithAnemia ->
            positiveMalariaTest
                && (-- Hemoglobin test was performed, and,
                    -- hemoglobin count indicates mild to moderate anemia.
                    Maybe.map (\count -> count >= 7 && count < 11) hemoglobinCount
                        |> Maybe.withDefault False
                   )

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
            (-- No indication for being positive for malaria,
             -- Hemoglobin test was performed, and, hemoglobin
             -- count indicates severe anemia.
             Maybe.map (\count -> count < 7) hemoglobinCount
                |> Maybe.withDefault False
            )
                && anemiaComplicationSignsPresent

        -- Non Lab Results diagnoses.
        _ ->
            False


respiratoryRateElevated : PrenatalMeasurements -> Bool
respiratoryRateElevated measurements =
    getMeasurementValueFunc measurements.vitals
        |> Maybe.map (\value -> value.respiratoryRate > 30)
        |> Maybe.withDefault False


diagnosisRequiresEmergencyReferal : PrenatalDiagnosis -> Bool
diagnosisRequiresEmergencyReferal diagnosis =
    List.member diagnosis emergencyReferralDiagnosesInitial


maternityWardDiagnoses : List PrenatalDiagnosis
maternityWardDiagnoses =
    [ DiagnosisImminentDelivery
    , DiagnosisLaborAndDelivery
    ]


labResultsDiagnoses : List PrenatalDiagnosis
labResultsDiagnoses =
    [ DiagnosisHIV
    , DiagnosisDiscordantPartnership
    , DiagnosisSyphilis
    , DiagnosisSyphilisWithComplications
    , DiagnosisNeurosyphilis
    , DiagnosisHepatitisB
    , DiagnosisMalaria
    , DiagnosisMalariaWithAnemia
    , DiagnosisMalariaWithSevereAnemia
    , DiagnosisModerateAnemia
    , DiagnosisSevereAnemia
    , DiagnosisSevereAnemiaWithComplications
    ]


healthEducationFormInputsAndTasks : Language -> AssembledData -> HealthEducationForm -> ( List (Html Msg), List (Maybe Bool) )
healthEducationFormInputsAndTasks language assembled healthEducationForm =
    case assembled.encounter.encounterType of
        NurseEncounter ->
            healthEducationFormInputsAndTasksForNurse language assembled healthEducationForm

        _ ->
            healthEducationFormInputsAndTasksForChw language assembled healthEducationForm


healthEducationFormInputsAndTasksForNurse : Language -> AssembledData -> HealthEducationForm -> ( List (Html Msg), List (Maybe Bool) )
healthEducationFormInputsAndTasksForNurse language assembled healthEducationForm =
    let
        form =
            assembled.measurements.healthEducation
                |> getMeasurementValueFunc
                |> healthEducationFormWithDefault healthEducationForm

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

        saferSexUpdateFunc value form_ =
            { form_ | saferSex = Just value }

        saferSexInput =
            [ viewQuestionLabel language <| translatePrenatalHealthEducationQuestion EducationSaferSex
            , viewBoolInput
                language
                form.saferSex
                (SetHealthEducationSubActivityBoolInput saferSexUpdateFunc)
                "safer-sex"
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
    in
    if
        List.any
            (\diagnosis ->
                diagnosed diagnosis assembled
            )
            [ DiagnosisHIV, DiagnosisDiscordantPartnership ]
    then
        ( positiveHIVInput ++ saferSexInput ++ partnerTestingInput ++ familyPlanningInput
        , [ form.positiveHIV, form.saferSex, form.partnerTesting, form.familyPlanning ]
        )

    else if partnerSurpressedViralLoad then
        ( saferSexInput
        , [ form.saferSex ]
        )

    else
        ( saferSexInput ++ partnerTestingInput
        , [ form.saferSex, form.partnerTesting ]
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
healthEducationFormInputsAndTasksForChw language assembled healthEducationForm =
    let
        form =
            assembled.measurements.healthEducation
                |> getMeasurementValueFunc
                |> healthEducationFormWithDefault healthEducationForm

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
                        |> sendToHCFormWithDefault data.sendToHCForm

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

                    else if emergencyReferalRequired assembled || diagnosed DiagnosisMalaria assembled then
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
                        |> healthEducationFormWithDefault data.healthEducationForm

                tasksCompleted =
                    List.map taskCompleted tasks
                        |> List.sum

                ( _, tasks ) =
                    healthEducationFormInputsAndTasks language assembled data.healthEducationForm
            in
            ( tasksCompleted
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
                        |> medicationDistributionFormWithDefault data.medicationDistributionForm

                ( _, completed, total ) =
                    resolveMedicationDistributionInputsAndTasks language
                        currentDate
                        assembled
                        SetMedicationDistributionBoolInput
                        SetMedicationDistributionAdministrationNote
                        form
            in
            ( completed, total )

        NextStepsRecommendedTreatment ->
            let
                form =
                    assembled.measurements.recommendedTreatment
                        |> getMeasurementValueFunc
                        |> recommendedTreatmentFormWithDefault data.recommendedTreatmentForm
            in
            ( taskCompleted form.signs
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
            (\( _, measurements ) ->
                measurementFunc measurements
                    |> Maybe.map (Tuple.second >> .value >> valueFunc)
            )
        |> List.reverse
        |> List.head


resolvePreviousMaybeValue : AssembledData -> (PrenatalMeasurements -> Maybe ( id, PrenatalMeasurement a )) -> (a -> Maybe b) -> Maybe b
resolvePreviousMaybeValue assembled measurementFunc valueFunc =
    assembled.nursePreviousMeasurementsWithDates
        |> List.filterMap
            (\( _, measurements ) ->
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


medicationFormWithDefault : MedicationForm -> Maybe (EverySet MedicationSign) -> MedicationForm
medicationFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { receivedIronFolicAcid = or form.receivedIronFolicAcid (EverySet.member IronAndFolicAcidSupplement value |> Just)
                , receivedDewormingPill = or form.receivedDewormingPill (EverySet.member DewormingPill value |> Just)
                , receivedMebendazole = or form.receivedMebendazole (EverySet.member Mebendazole value |> Just)
                }
            )


toMedicationValueWithDefault : Maybe (EverySet MedicationSign) -> MedicationForm -> Maybe (EverySet MedicationSign)
toMedicationValueWithDefault saved form =
    medicationFormWithDefault form saved
        |> toMedicationValue


toMedicationValue : MedicationForm -> Maybe (EverySet MedicationSign)
toMedicationValue form =
    [ Maybe.map (ifTrue IronAndFolicAcidSupplement) form.receivedIronFolicAcid
    , ifNullableTrue DewormingPill form.receivedDewormingPill
    , ifNullableTrue Mebendazole form.receivedMebendazole
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoMedication)


fromObstetricalExamValue : Maybe ObstetricalExamValue -> ObstetricalExamForm
fromObstetricalExamValue saved =
    { fundalHeight = Maybe.map (.fundalHeight >> heightValueFunc) saved
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
                { fundalHeight = valueConsideringIsDirtyField form.fundalHeightDirty form.fundalHeight (heightValueFunc value.fundalHeight)
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
    { height = Maybe.map (.height >> heightValueFunc) saved
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
                { height = valueConsideringIsDirtyField form.heightDirty form.height (heightValueFunc value.height)
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


fromMalariaPreventionValue : Maybe (EverySet MalariaPreventionSign) -> MalariaPreventionForm
fromMalariaPreventionValue saved =
    { receivedMosquitoNet = Maybe.map (EverySet.member MosquitoNet) saved
    }


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
                            (\( _, measurements ) ->
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
                            (\( _, measurements ) ->
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


examinationTasksCompletedFromTotal : AssembledData -> ExaminationData -> Bool -> ExaminationTask -> ( Int, Int )
examinationTasksCompletedFromTotal assembled data firstEncounter task =
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
                hideHeightInput =
                    not firstEncounter

                form_ =
                    assembled.measurements.nutrition
                        |> getMeasurementValueFunc
                        |> prenatalNutritionFormWithDefault data.nutritionAssessmentForm

                form =
                    if hideHeightInput then
                        assembled.nursePreviousMeasurementsWithDates
                            |> List.head
                            |> Maybe.andThen (Tuple.second >> getMotherHeightMeasurement)
                            |> Maybe.map (\(HeightInCm height) -> { form_ | height = Just height })
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


healthEducationFormWithDefault : HealthEducationForm -> Maybe (EverySet PrenatalHealthEducationSign) -> HealthEducationForm
healthEducationFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\signs ->
                { expectations = or form.expectations (EverySet.member EducationExpectations signs |> Just)
                , visitsReview = or form.visitsReview (EverySet.member EducationVisitsReview signs |> Just)
                , warningSigns = or form.warningSigns (EverySet.member EducationWarningSigns signs |> Just)
                , hemorrhaging = or form.hemorrhaging (EverySet.member EducationHemorrhaging signs |> Just)
                , familyPlanning = or form.familyPlanning (EverySet.member EducationFamilyPlanning signs |> Just)
                , breastfeeding = or form.breastfeeding (EverySet.member EducationBreastfeeding signs |> Just)
                , immunization = or form.immunization (EverySet.member EducationImmunization signs |> Just)
                , hygiene = or form.hygiene (EverySet.member EducationHygiene signs |> Just)
                , positiveHIV = or form.positiveHIV (EverySet.member EducationPositiveHIV signs |> Just)
                , saferSex = or form.saferSex (EverySet.member EducationSaferSex signs |> Just)
                , partnerTesting = or form.partnerTesting (EverySet.member EducationPartnerTesting signs |> Just)
                }
            )


toHealthEducationValueWithDefault : Maybe (EverySet PrenatalHealthEducationSign) -> HealthEducationForm -> Maybe (EverySet PrenatalHealthEducationSign)
toHealthEducationValueWithDefault saved form =
    healthEducationFormWithDefault form saved
        |> toHealthEducationValue


toHealthEducationValue : HealthEducationForm -> Maybe (EverySet PrenatalHealthEducationSign)
toHealthEducationValue form =
    [ ifNullableTrue EducationExpectations form.expectations
    , ifNullableTrue EducationVisitsReview form.visitsReview
    , ifNullableTrue EducationWarningSigns form.warningSigns
    , ifNullableTrue EducationHemorrhaging form.hemorrhaging
    , ifNullableTrue EducationFamilyPlanning form.familyPlanning
    , ifNullableTrue EducationBreastfeeding form.breastfeeding
    , ifNullableTrue EducationImmunization form.immunization
    , ifNullableTrue EducationHygiene form.hygiene
    , ifNullableTrue EducationPositiveHIV form.positiveHIV
    , ifNullableTrue EducationSaferSex form.saferSex
    , ifNullableTrue EducationPartnerTesting form.partnerTesting
    ]
        |> Maybe.Extra.combine
        |> Maybe.map (List.foldl EverySet.union EverySet.empty >> ifEverySetEmpty NoPrenatalHealthEducationSigns)


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
            , specificGravity = Nothing
            , ketone = Nothing
            , bilirubin = Nothing
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
    PrenatalHepatitisBTestValue note date Nothing


toSyphilisTestValueWithEmptyResults : PrenatalTestExecutionNote -> Maybe NominalDate -> PrenatalSyphilisTestValue
toSyphilisTestValueWithEmptyResults note date =
    PrenatalSyphilisTestValue note date Nothing Nothing


toHemoglobinTestValueWithEmptyResults : PrenatalTestExecutionNote -> Maybe NominalDate -> PrenatalHemoglobinTestValue
toHemoglobinTestValueWithEmptyResults note date =
    PrenatalHemoglobinTestValue note date Nothing


toRandomBloodSugarTestValueWithEmptyResults : PrenatalTestExecutionNote -> Maybe NominalDate -> PrenatalRandomBloodSugarTestValue
toRandomBloodSugarTestValueWithEmptyResults note date =
    PrenatalRandomBloodSugarTestValue note date Nothing


toBloodGpRsTestValueWithEmptyResults : PrenatalTestExecutionNote -> Maybe NominalDate -> PrenatalBloodGpRsTestValue
toBloodGpRsTestValueWithEmptyResults note date =
    PrenatalBloodGpRsTestValue note date Nothing Nothing


laboratoryTasks : List LaboratoryTask
laboratoryTasks =
    [ TaskHIVTest
    , TaskSyphilisTest
    , TaskHepatitisBTest
    , TaskMalariaTest
    , TaskBloodGpRsTest
    , TaskUrineDipstickTest
    , TaskHemoglobinTest
    , TaskRandomBloodSugarTest
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


expectLaboratoryTask : NominalDate -> AssembledData -> LaboratoryTask -> Bool
expectLaboratoryTask currentDate assembled task =
    if assembled.encounter.encounterType /= NurseEncounter then
        False

    else
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
                    (Tuple.second
                        >> getMeasurementFunc
                        >> getMeasurementValueFunc
                        >> Maybe.map (.executionNote >> (==) TestNoteKnownAsPositive)
                        >> Maybe.withDefault False
                    )
                    assembled.nursePreviousMeasurementsWithDates
                    |> List.isEmpty
                    |> not
        in
        case task of
            TaskHIVTest ->
                (not <| isKnownAsPositive .hivTest)
                    && isRepeatingTestOnWeek 38 TaskHIVTest

            TaskSyphilisTest ->
                isRepeatingTestOnWeek 38 TaskSyphilisTest

            TaskHepatitisBTest ->
                (not <| isKnownAsPositive .hepatitisBTest)
                    && isRepeatingTestOnWeek 34 TaskHepatitisBTest

            TaskMalariaTest ->
                True

            TaskBloodGpRsTest ->
                isInitialTest TaskBloodGpRsTest

            TaskUrineDipstickTest ->
                True

            TaskHemoglobinTest ->
                True

            TaskRandomBloodSugarTest ->
                isInitialTest TaskRandomBloodSugarTest


generatePreviousLaboratoryTestsDatesDict : NominalDate -> AssembledData -> Dict LaboratoryTask (List NominalDate)
generatePreviousLaboratoryTestsDatesDict currentDate assembled =
    let
        generateTestDates getMeasurementFunc resultsExistFunc resultsValidFunc =
            List.filterMap
                (\( _, measurements ) ->
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
