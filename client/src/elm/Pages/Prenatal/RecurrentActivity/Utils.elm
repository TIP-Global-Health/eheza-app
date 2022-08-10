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
import Measurement.Utils exposing (sendToHCFormWithDefault, vitalsFormWithDefault)
import Pages.Prenatal.Activity.Types exposing (LaboratoryTask(..))
import Pages.Prenatal.Model exposing (AssembledData, HealthEducationForm, PrenatalEncounterPhase(..))
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
                        |> List.all (nextStepsTaskCompleted assembled)
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


expectLaboratoryResultTask : NominalDate -> AssembledData -> LaboratoryTask -> Bool
expectLaboratoryResultTask currentDate assembled task =
    let
        wasTestPerformed getMeasurementFunc =
            getMeasurementFunc assembled.measurements
                |> getMeasurementValueFunc
                |> Maybe.map
                    (\value ->
                        List.member value.executionNote [ TestNoteRunToday, TestNoteRunPreviously ]
                    )
                |> Maybe.withDefault False
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
            wasTestPerformed .randomBloodSugarTest

        TaskHIVPCRTest ->
            wasTestPerformed .hivPCRTest

        TaskCompletePreviousTests ->
            False


hepatitisBFormWithDefault : HepatitisBResultForm -> Maybe PrenatalHepatitisBTestValue -> HepatitisBResultForm
hepatitisBFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , testResult = or form.testResult value.testResult
                , originatingEncounter = or form.originatingEncounter value.originatingEncounter
                }
            )


toHepatitisBValueWithDefault : Maybe PrenatalHepatitisBTestValue -> HepatitisBResultForm -> Maybe PrenatalHepatitisBTestValue
toHepatitisBValueWithDefault saved form =
    hepatitisBFormWithDefault form saved
        |> toHepatitisBValue


toHepatitisBValue : HepatitisBResultForm -> Maybe PrenatalHepatitisBTestValue
toHepatitisBValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testResult = form.testResult
            , originatingEncounter = form.originatingEncounter
            }
        )
        form.executionNote


syphilisResultFormWithDefault : SyphilisResultForm -> Maybe PrenatalSyphilisTestValue -> SyphilisResultForm
syphilisResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , testResult = or form.testResult value.testResult
                , symptoms = maybeValueConsideringIsDirtyField form.symptomsDirty form.symptoms (Maybe.map EverySet.toList value.symptoms)
                , symptomsDirty = form.symptomsDirty
                , originatingEncounter = or form.originatingEncounter value.originatingEncounter
                }
            )


toSyphilisResultValueWithDefault : Maybe PrenatalSyphilisTestValue -> SyphilisResultForm -> Maybe PrenatalSyphilisTestValue
toSyphilisResultValueWithDefault saved form =
    syphilisResultFormWithDefault form saved
        |> toSyphilisResultValue


toSyphilisResultValue : SyphilisResultForm -> Maybe PrenatalSyphilisTestValue
toSyphilisResultValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testResult = form.testResult
            , symptoms = Maybe.map EverySet.fromList form.symptoms
            , originatingEncounter = form.originatingEncounter
            }
        )
        form.executionNote


prenatalBloodGpRsResultFormWithDefault : PrenatalBloodGpRsResultForm -> Maybe PrenatalBloodGpRsTestValue -> PrenatalBloodGpRsResultForm
prenatalBloodGpRsResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , bloodGroup = or form.bloodGroup value.bloodGroup
                , rhesus = or form.rhesus value.rhesus
                , originatingEncounter = or form.originatingEncounter value.originatingEncounter
                }
            )


toPrenatalBloodGpRsResultsValueWithDefault : Maybe PrenatalBloodGpRsTestValue -> PrenatalBloodGpRsResultForm -> Maybe PrenatalBloodGpRsTestValue
toPrenatalBloodGpRsResultsValueWithDefault saved form =
    prenatalBloodGpRsResultFormWithDefault form saved
        |> toPrenatalBloodGpRsResultsValue


toPrenatalBloodGpRsResultsValue : PrenatalBloodGpRsResultForm -> Maybe PrenatalBloodGpRsTestValue
toPrenatalBloodGpRsResultsValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , bloodGroup = form.bloodGroup
            , rhesus = form.rhesus
            , originatingEncounter = form.originatingEncounter
            }
        )
        form.executionNote


prenatalHemoglobinResultFormWithDefault : PrenatalHemoglobinResultForm -> Maybe PrenatalHemoglobinTestValue -> PrenatalHemoglobinResultForm
prenatalHemoglobinResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , hemoglobinCount = or form.hemoglobinCount value.hemoglobinCount
                }
            )


toPrenatalHemoglobinResultsValueWithDefault : Maybe PrenatalHemoglobinTestValue -> PrenatalHemoglobinResultForm -> Maybe PrenatalHemoglobinTestValue
toPrenatalHemoglobinResultsValueWithDefault saved form =
    prenatalHemoglobinResultFormWithDefault form saved
        |> toPrenatalHemoglobinResultsValue


toPrenatalHemoglobinResultsValue : PrenatalHemoglobinResultForm -> Maybe PrenatalHemoglobinTestValue
toPrenatalHemoglobinResultsValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , hemoglobinCount = form.hemoglobinCount
            }
        )
        form.executionNote


prenatalRandomBloodSugarResultFormWithDefault : PrenatalRandomBloodSugarResultForm -> Maybe PrenatalRandomBloodSugarTestValue -> PrenatalRandomBloodSugarResultForm
prenatalRandomBloodSugarResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , testPrerequisites = or form.testPrerequisites value.testPrerequisites
                , sugarCount = or form.sugarCount value.sugarCount
                , originatingEncounter = or form.originatingEncounter value.originatingEncounter
                }
            )


toPrenatalRandomBloodSugarResultsValueWithDefault : Maybe PrenatalRandomBloodSugarTestValue -> PrenatalRandomBloodSugarResultForm -> Maybe PrenatalRandomBloodSugarTestValue
toPrenatalRandomBloodSugarResultsValueWithDefault saved form =
    prenatalRandomBloodSugarResultFormWithDefault form saved
        |> toPrenatalRandomBloodSugarResultsValue


toPrenatalRandomBloodSugarResultsValue : PrenatalRandomBloodSugarResultForm -> Maybe PrenatalRandomBloodSugarTestValue
toPrenatalRandomBloodSugarResultsValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , testPrerequisites = form.testPrerequisites
            , sugarCount = form.sugarCount
            , originatingEncounter = form.originatingEncounter
            }
        )
        form.executionNote


prenatalUrineDipstickResultFormWithDefault : PrenatalUrineDipstickResultForm -> Maybe PrenatalUrineDipstickTestValue -> PrenatalUrineDipstickResultForm
prenatalUrineDipstickResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { testVariant = or form.testVariant value.testVariant
                , executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , protein = or form.protein value.protein
                , ph = or form.ph value.ph
                , glucose = or form.glucose value.glucose
                , leukocytes = or form.leukocytes value.leukocytes
                , nitrite = or form.nitrite value.nitrite
                , urobilinogen = or form.urobilinogen value.urobilinogen
                , haemoglobin = or form.haemoglobin value.haemoglobin
                , ketone = or form.ketone value.ketone
                , bilirubin = or form.bilirubin value.bilirubin
                }
            )


toPrenatalUrineDipstickResultsValueWithDefault : Maybe PrenatalUrineDipstickTestValue -> PrenatalUrineDipstickResultForm -> Maybe PrenatalUrineDipstickTestValue
toPrenatalUrineDipstickResultsValueWithDefault saved form =
    prenatalUrineDipstickResultFormWithDefault form saved
        |> toPrenatalUrineDipstickResultsValue


toPrenatalUrineDipstickResultsValue : PrenatalUrineDipstickResultForm -> Maybe PrenatalUrineDipstickTestValue
toPrenatalUrineDipstickResultsValue form =
    Maybe.map
        (\executionNote ->
            { testVariant = form.testVariant
            , executionNote = executionNote
            , executionDate = form.executionDate
            , protein = form.protein
            , ph = form.ph
            , glucose = form.glucose
            , leukocytes = form.leukocytes
            , nitrite = form.nitrite
            , urobilinogen = form.urobilinogen
            , haemoglobin = form.haemoglobin
            , ketone = form.ketone
            , bilirubin = form.bilirubin
            }
        )
        form.executionNote


resolveNextStepsTasks : NominalDate -> AssembledData -> List NextStepsTask
resolveNextStepsTasks currentDate assembled =
    -- The order is important. Do not change.
    [ NextStepsHealthEducation, NextStepsMedicationDistribution, NextStepsSendToHC ]
        |> List.filter (expectNextStepsTask currentDate assembled)


expectNextStepsTask : NominalDate -> AssembledData -> NextStepsTask -> Bool
expectNextStepsTask currentDate assembled task =
    case task of
        NextStepsSendToHC ->
            diagnosesCausingHospitalReferralByImmediateDiagnoses assembled
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


diagnosesCausingHospitalReferralByImmediateDiagnoses : AssembledData -> List PrenatalDiagnosis
diagnosesCausingHospitalReferralByImmediateDiagnoses assembled =
    emergencyReferralDiagnosesRecurrent
        ++ [ DiagnosisHepatitisB
           , DiagnosisNeurosyphilis
           , DiagnosisMalariaWithSevereAnemia
           , DiagnosisSevereAnemia
           , DiagnosisModeratePreeclampsiaRecurrentPhase
           , Backend.PrenatalEncounter.Types.DiagnosisDiabetes
           , Backend.PrenatalEncounter.Types.DiagnosisGestationalDiabetes
           , DiagnosisRhesusNegative
           ]
        |> List.filter (\diagnosis -> diagnosed diagnosis assembled)


nextStepsTaskCompleted : AssembledData -> NextStepsTask -> Bool
nextStepsTaskCompleted assembled task =
    case task of
        NextStepsSendToHC ->
            isJust assembled.measurements.sendToHC

        NextStepsMedicationDistribution ->
            let
                allowedSigns =
                    NoMedicationDistributionSignsRecurrentPhase :: medicationsRecurrentPhase

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
            medicationDistributionMeasurementTaken allowedSigns assembled.measurements
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
            in
            ( taskCompleted form.handReferralForm + reasonForNotSentCompleted
            , 1 + reasonForNotSentActive
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
                && (not <| diagnosedAnyOf hierarchalBloodPreasureDiagnoses assembled)
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


prenatalHIVPCRResultFormWithDefault : PrenatalHIVPCRResultForm -> Maybe PrenatalHIVPCRTestValue -> PrenatalHIVPCRResultForm
prenatalHIVPCRResultFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , hivViralLoadStatus = or form.hivViralLoadStatus value.hivViralLoadStatus
                , hivViralLoad = or form.hivViralLoad value.hivViralLoad
                }
            )


toPrenatalHIVPCRResultsValueWithDefault : Maybe PrenatalHIVPCRTestValue -> PrenatalHIVPCRResultForm -> Maybe PrenatalHIVPCRTestValue
toPrenatalHIVPCRResultsValueWithDefault saved form =
    prenatalHIVPCRResultFormWithDefault form saved
        |> toPrenatalHIVPCRResultsValue


toPrenatalHIVPCRResultsValue : PrenatalHIVPCRResultForm -> Maybe PrenatalHIVPCRTestValue
toPrenatalHIVPCRResultsValue form =
    Maybe.map
        (\executionNote ->
            { executionNote = executionNote
            , executionDate = form.executionDate
            , hivViralLoadStatus = form.hivViralLoadStatus
            , hivViralLoad = form.hivViralLoad
            }
        )
        form.executionNote


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
