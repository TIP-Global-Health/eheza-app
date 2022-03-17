module Pages.Prenatal.RecurrentActivity.Utils exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (getMeasurementValueFunc)
import Backend.PrenatalActivity.Model exposing (..)
import Backend.PrenatalEncounter.Model exposing (PrenatalDiagnosis(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate, diffDays, diffWeeks)
import Html exposing (Html)
import Maybe.Extra exposing (andMap, isJust, isNothing, or, unwrap)
import Measurement.Utils exposing (sendToHCFormWithDefault)
import Pages.AcuteIllness.Activity.Utils exposing (nonAdministrationReasonToSign)
import Pages.Prenatal.Activity.Types exposing (LaboratoryTask(..))
import Pages.Prenatal.Model exposing (AssembledData)
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
        , viewQuestionLabel
        )
import Translate exposing (Language, TranslationId, translate)


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
                        |> List.all (nextStepsMeasurementTaken assembled)
                   )


laboratoryResultTasks : List LaboratoryTask
laboratoryResultTasks =
    [ TaskSyphilisTest
    , TaskHepatitisBTest
    , TaskBloodGpRsTest
    , TaskUrineDipstickTest
    , TaskHemoglobinTest
    , TaskRandomBloodSugarTest
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


hepatitisBFormWithDefault : HepatitisBResultForm -> Maybe PrenatalHepatitisBTestValue -> HepatitisBResultForm
hepatitisBFormWithDefault form saved =
    saved
        |> unwrap
            form
            (\value ->
                { executionNote = or form.executionNote (Just value.executionNote)
                , executionDate = or form.executionDate value.executionDate
                , testResult = or form.testResult value.testResult
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
                , sugarCount = or form.sugarCount value.sugarCount
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
            , sugarCount = form.sugarCount
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
                , specificGravity = or form.specificGravity value.specificGravity
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
            , specificGravity = form.specificGravity
            , ketone = form.ketone
            , bilirubin = form.bilirubin
            }
        )
        form.executionNote


resolveNextStepsTasks : NominalDate -> AssembledData -> List NextStepsTask
resolveNextStepsTasks currentDate assembled =
    -- The order is important. Do not change.
    [ NextStepsMedicationDistribution, NextStepsRecommendedTreatment, NextStepsSendToHC ]
        |> List.filter (expectNextStepsTask currentDate assembled)


expectNextStepsTask : NominalDate -> AssembledData -> NextStepsTask -> Bool
expectNextStepsTask currentDate assembled task =
    case task of
        NextStepsSendToHC ->
            diagnosedAnyOf
                [ DiagnosisHepatitisB
                , DiagnosisNeurosyphilis
                , DiagnosisMalariaWithSevereAnemia
                , DiagnosisSevereAnemia
                , DiagnosisSevereAnemiaWithComplications
                ]
                assembled

        NextStepsMedicationDistribution ->
            -- Emergency refferal is not required.
            (not <| emergencyReferalRequired assembled)
                && (resolveMedicationsByDiagnoses assembled medicationsRecurrentPhase
                        |> List.isEmpty
                        |> not
                   )

        -- Exclusive Nurse task.
        NextStepsRecommendedTreatment ->
            -- Emergency refferal is not required.
            (not <| emergencyReferalRequired assembled)
                && (diagnosedSyphilis assembled
                        || diagnosedHypertensionAfterRecheck assembled
                   )


diagnosedSyphilis : AssembledData -> Bool
diagnosedSyphilis =
    diagnosedAnyOf
        [ DiagnosisSyphilis
        , DiagnosisSyphilisWithComplications
        ]


diagnosedHypertensionAfterRecheck : AssembledData -> Bool
diagnosedHypertensionAfterRecheck =
    diagnosedAnyOf
        [ DiagnosisChronicHypertensionAfterRecheck
        , DiagnosisGestationalHypertensionAfterRecheck
        ]


nextStepsMeasurementTaken : AssembledData -> NextStepsTask -> Bool
nextStepsMeasurementTaken assembled task =
    case task of
        NextStepsSendToHC ->
            isJust assembled.measurements.sendToHC

        NextStepsMedicationDistribution ->
            let
                allowedSigns =
                    NoMedicationDistributionSignsRecurrentPhase :: medicationsRecurrentPhase
            in
            medicationDistributionMeasurementTaken allowedSigns assembled.measurements

        NextStepsRecommendedTreatment ->
            let
                syphilisTreatmentCompleted =
                    if diagnosedSyphilis assembled then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForSyphilis assembled.measurements

                    else
                        True

                hypertensionTreatmentCompleted =
                    if diagnosedHypertensionAfterRecheck assembled then
                        recommendedTreatmentMeasurementTaken recommendedTreatmentSignsForHypertension assembled.measurements

                    else
                        True
            in
            syphilisTreatmentCompleted && hypertensionTreatmentCompleted


recommendedTreatmentSignsForSyphilis : List RecommendedTreatmentSign
recommendedTreatmentSignsForSyphilis =
    [ TreatementPenecilin1
    , TreatementPenecilin3
    , TreatementErythromycin
    , TreatementAzithromycin
    , TreatementCeftriaxon
    ]


resolveAllowedRecommendedTreatmentSigns : AssembledData -> List RecommendedTreatmentSign
resolveAllowedRecommendedTreatmentSigns assembled =
    let
        hypertensionSigns =
            if diagnosedHypertensionAfterRecheck assembled then
                recommendedTreatmentSignsForHypertension

            else
                []
    in
    recommendedTreatmentSignsForSyphilis ++ hypertensionSigns


nextStepsTasksCompletedFromTotal : Language -> NominalDate -> AssembledData -> NextStepsData -> NextStepsTask -> ( Int, Int )
nextStepsTasksCompletedFromTotal language currentDate assembled data task =
    case task of
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
                        assembled
                        SetMedicationDistributionBoolInput
                        SetMedicationDistributionAdministrationNote
                        medicationsRecurrentPhase
                        form
            in
            ( completed, total )

        NextStepsRecommendedTreatment ->
            let
                form =
                    assembled.measurements.recommendedTreatment
                        |> getMeasurementValueFunc
                        |> recommendedTreatmentFormWithDefault data.recommendedTreatmentForm

                ( syphilisSectionCompleted, syphilisSectionActive ) =
                    resolveRecommendedTreatmentSectionState (diagnosedSyphilis assembled) recommendedTreatmentSignsForSyphilis form.signs

                ( hypertensionSectionCompleted, hypertensionSectionActive ) =
                    resolveRecommendedTreatmentSectionState (diagnosedHypertensionAfterRecheck assembled) recommendedTreatmentSignsForHypertension form.signs
            in
            ( syphilisSectionCompleted + hypertensionSectionCompleted
            , syphilisSectionActive + hypertensionSectionActive
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
