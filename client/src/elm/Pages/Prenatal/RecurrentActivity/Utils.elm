module Pages.Prenatal.RecurrentActivity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, diffWeeks)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Model exposing (LaboratoryTask(..))
import Measurement.Utils exposing (expectRandomBloodSugarResultTask, testPerformedByValue, vitalsFormWithDefault)
import Pages.Prenatal.Model exposing (AssembledData, HealthEducationForm, PrenatalEncounterPhase(..), ReferralForm)
import Pages.Prenatal.RecurrentActivity.Model exposing (..)
import Pages.Prenatal.RecurrentActivity.Types exposing (..)
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
        , viewCustomLabel
        , viewQuestionLabel
        )
import Translate exposing (Language, TranslationId, translate)
import Translate.Model exposing (Language(..))


expectActivity : NominalDate -> AssembledData -> PrenatalRecurrentActivity -> Bool
expectActivity currentDate assembled activity =
    case activity of
        LabResults ->
            resolveLaboratoryResultTask currentDate assembled
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


activityCompleted : NominalDate -> AssembledData -> PrenatalRecurrentActivity -> Bool
activityCompleted currentDate assembled activity =
    case activity of
        LabResults ->
            (not <| expectActivity currentDate assembled LabResults)
                || (resolveLaboratoryResultTask currentDate assembled
                        |> List.all (laboratoryResultTaskCompleted currentDate assembled)
                   )

        RecurrentNextSteps ->
            (not <| expectActivity currentDate assembled RecurrentNextSteps)
                || (resolveNextStepsTasks currentDate assembled
                        |> List.all (nextStepsTaskCompleted currentDate assembled)
                   )

        RecurrentExamination ->
            (not <| expectActivity currentDate assembled RecurrentExamination)
                || (resolveExaminationTasks currentDate assembled
                        |> List.all (examinationMeasurementTaken assembled)
                   )


laboratoryResultTasks : List LaboratoryTask
laboratoryResultTasks =
    [ TaskSyphilisTest
    , TaskHepatitisBTest
    , TaskBloodGpRsTest
    , TaskUrineDipstickTest
    , TaskHemoglobinTest
    , TaskRandomBloodSugarTest
    , TaskHIVPCRTest
    ]


resolveLaboratoryResultTask : NominalDate -> AssembledData -> List LaboratoryTask
resolveLaboratoryResultTask currentDate assembled =
    List.filter (expectLaboratoryResultTask currentDate assembled) laboratoryResultTasks


laboratoryResultTaskCompleted : NominalDate -> AssembledData -> LaboratoryTask -> Bool
laboratoryResultTaskCompleted currentDate assembled task =
    let
        taskExpected =
            expectLaboratoryResultTask currentDate assembled

        testResultsCompleted getMeasurementFunc getResultFieldFunc =
            getMeasurementFunc assembled.measurements
                |> getMeasurementValueFunc
                |> Maybe.andThen getResultFieldFunc
                |> isJust
    in
    case task of
        TaskHIVTest ->
            not <| taskExpected TaskHIVTest

        TaskSyphilisTest ->
            (not <| taskExpected TaskSyphilisTest) || testResultsCompleted .syphilisTest .testResult

        TaskHepatitisBTest ->
            (not <| taskExpected TaskHepatitisBTest) || testResultsCompleted .hepatitisBTest .testResult

        TaskMalariaTest ->
            not <| taskExpected TaskMalariaTest

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


expectLaboratoryResultTask : NominalDate -> AssembledData -> LaboratoryTask -> Bool
expectLaboratoryResultTask currentDate assembled task =
    let
        wasTestPerformed getMeasurementFunc =
            getMeasurementFunc assembled.measurements
                |> getMeasurementValueFunc
                |> testPerformedByValue
    in
    case task of
        TaskHIVTest ->
            False

        TaskSyphilisTest ->
            wasTestPerformed .syphilisTest

        TaskHepatitisBTest ->
            wasTestPerformed .hepatitisBTest

        TaskMalariaTest ->
            False

        TaskBloodGpRsTest ->
            wasTestPerformed .bloodGpRsTest

        TaskUrineDipstickTest ->
            wasTestPerformed .urineDipstickTest

        TaskHemoglobinTest ->
            wasTestPerformed .hemoglobinTest

        TaskRandomBloodSugarTest ->
            getMeasurementValueFunc assembled.measurements.randomBloodSugarTest
                |> Maybe.map expectRandomBloodSugarResultTask
                |> Maybe.withDefault False

        TaskHIVPCRTest ->
            wasTestPerformed .hivPCRTest

        TaskCompletePreviousTests ->
            False

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
                        || diagnosedSyphilis assembled
                        || diagnosedHypertension PrenatalEncounterPhaseRecurrent assembled
                   )

        NextStepsHealthEducation ->
            diagnosedAnyOf (DiagnosisHIVDetectableViralLoad :: diabetesDiagnoses) assembled


nextStepsTaskCompleted : NominalDate -> AssembledData -> NextStepsTask -> Bool
nextStepsTaskCompleted currentDate assembled task =
    case task of
        NextStepsSendToHC ->
            resolveRequiredReferralFacilities assembled
                |> List.all (referralToFacilityCompleted assembled)

        NextStepsMedicationDistribution ->
            let
                allowedSigns =
                    NoMedicationDistributionSignsRecurrentPhase :: medicationsRecurrentPhase

                medicationDistributionRequired =
                    resolveRequiredMedicationsSet English currentDate PrenatalEncounterPhaseRecurrent assembled
                        |> List.isEmpty
                        |> not

                medicationDistributionCompleted =
                    if medicationDistributionRequired then
                        medicationDistributionMeasurementTaken allowedSigns assembled.measurements

                    else
                        True

                syphilisTreatmentCompleted =
                    if diagnosedSyphilis assembled then
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
    -- The order is important. Do not change.
    [ ExaminationVitals ]
        |> List.filter (expectExaminationTask currentDate assembled)


expectExaminationTask : NominalDate -> AssembledData -> ExaminationTask -> Bool
expectExaminationTask currentDate assembled task =
    case task of
        ExaminationVitals ->
            -- Hypertension is a chronic diagnosis for whole duration
            -- of pregnancy. If diagnised, we do not need to recheck the BP.
            -- Measurement taken at initial phase of encounter is sufficient.
            (not <| diagnosedHypertensionPrevoiusly assembled)
                && (not <| diagnosedAnyOf hierarchalBloodPressureDiagnoses assembled)
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
    let
        detectableViralLoad =
            if diagnosed DiagnosisHIVDetectableViralLoad assembled then
                ( [ viewCustomLabel language Translate.DetectableViralLoad "" "label header"
                  , viewCustomLabel language Translate.PrenatalHealthEducationHivDetectableViralLoadInform "." "label paragraph"
                  , viewQuestionLabel language Translate.PrenatalHealthEducationAppropriateProvided
                  , viewBoolInput
                        language
                        form.hivDetectableViralLoad
                        (SetHealthEducationBoolInput (\value form_ -> { form_ | hivDetectableViralLoad = Just value }))
                        "hiv-detectable-viral-load"
                        Nothing
                  ]
                , Just form.hivDetectableViralLoad
                )

            else
                ( [], Nothing )

        diabetes =
            if diagnosedAnyOf diabetesDiagnoses assembled then
                let
                    header =
                        if diagnosed Backend.PrenatalEncounter.Types.DiagnosisDiabetes assembled then
                            Translate.PrenatalDiagnosis Backend.PrenatalEncounter.Types.DiagnosisDiabetes

                        else
                            Translate.PrenatalDiagnosis Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetes
                in
                ( [ viewCustomLabel language header "" "label header"
                  , viewCustomLabel language Translate.PrenatalHealthEducationDiabetesInform "." "label paragraph"
                  , viewQuestionLabel language Translate.PrenatalHealthEducationAppropriateProvided
                  , viewBoolInput
                        language
                        form.diabetes
                        (SetHealthEducationBoolInput (\value form_ -> { form_ | diabetes = Just value }))
                        "diabetes"
                        Nothing
                  ]
                , Just form.diabetes
                )

            else
                ( [], Nothing )

        inputsAndTasks =
            [ detectableViralLoad
            , diabetes
            ]
    in
    ( List.map Tuple.first inputsAndTasks
        |> List.concat
    , List.map Tuple.second inputsAndTasks
        |> Maybe.Extra.values
    )


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

                -- Signs that do not participate at recurrent phase. Resolved directly from value.
                , expectations = EverySet.member EducationExpectations value.signs |> Just
                , visitsReview = EverySet.member EducationVisitsReview value.signs |> Just
                , warningSigns = EverySet.member EducationWarningSigns value.signs |> Just
                , hemorrhaging = EverySet.member EducationHemorrhaging value.signs |> Just
                , familyPlanning = EverySet.member EducationFamilyPlanning value.signs |> Just
                , breastfeeding = EverySet.member EducationBreastfeeding value.signs |> Just
                , immunization = EverySet.member EducationImmunization value.signs |> Just
                , hygiene = EverySet.member EducationHygiene value.signs |> Just
                , positiveHIV = EverySet.member EducationPositiveHIV value.signs |> Just
                , saferSexHIV = EverySet.member EducationSaferSexHIV value.signs |> Just
                , partnerTesting = EverySet.member EducationPartnerTesting value.signs |> Just
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
    [ ifNullableTrue EducationHIVDetectableViralLoad form.hivDetectableViralLoad
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
            False

        FacilityNCDProgram ->
            False

        -- Explicit NCD facility.
        FacilityANCServices ->
            False

        FacilityHealthCenter ->
            -- We should never get here. HC inputs are resolved
            -- with resolveReferralInputsAndTasksForCHW.
            False


referralFacilities : List ReferralFacility
referralFacilities =
    [ FacilityHospital ]
