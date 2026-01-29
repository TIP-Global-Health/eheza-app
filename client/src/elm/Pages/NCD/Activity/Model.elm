module Pages.NCD.Activity.Model exposing (CoMorbiditiesForm, DangerSignsData, DangerSignsForm, ExaminationData, FamilyHistoryForm, FamilyPlanningData, HealthEducationForm, LaboratoryData, MedicalHistoryData, MedicationHistoryForm, Model, Msg(..), NextStepsData, OutsideCareData, SocialHistoryForm, SymptomReviewData, SymptomReviewForm, emptyCoMorbiditiesForm, emptyDangerSignsData, emptyDangerSignsForm, emptyExaminationData, emptyFamilyHistoryForm, emptyFamilyPlanningData, emptyHealthEducationForm, emptyLaboratoryData, emptyMedicalHistoryData, emptyMedicationHistoryForm, emptyModel, emptyNextStepsData, emptyOutsideCareData, emptySocialHistoryForm, emptySymptomReviewData, emptySymptomReviewForm)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import DateSelector.Model exposing (DateSelectorConfig)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model
    exposing
        ( CorePhysicalExamForm
        , FamilyPlanningForm
        , HIVTestForm
        , HbA1cTestForm
        , LaboratoryTask
        , NonRDTForm
        , OutsideCareForm
        , OutsideCareStep(..)
        , PregnancyTestForm
        , RandomBloodSugarTestForm
        , UrineDipstickTestForm
        , VitalsForm
        , emptyCorePhysicalExamForm
        , emptyFamilyPlanningForm
        , emptyHIVTestForm
        , emptyHbA1cTestForm
        , emptyNonRDTForm
        , emptyOutsideCareForm
        , emptyPregnancyTestForm
        , emptyRandomBloodSugarTestForm
        , emptyUrineDipstickTestForm
        , emptyVitalsForm
        )
import Pages.NCD.Activity.Types exposing (ExaminationTask, MedicalHistoryTask, NextStepsTask)
import Pages.NCD.Model exposing (MedicationDistributionForm, ReferralForm, emptyMedicationDistributionForm, emptyReferralForm)
import Pages.Page exposing (Page)


type Msg
    = NoOp
    | SetActivePage Page
      -- DangerSignsMsgs
    | SetDangerSign NCDDangerSign
    | SaveDangerSigns PersonId (Maybe ( NCDDangerSignsId, NCDDangerSigns ))
      -- SymptomReviewMsgs
    | SetGroup1Symptom NCDGroup1Symptom
    | SetGroup2Symptom NCDGroup2Symptom
    | SetPainSymptom NCDPainSymptom
    | SaveSymptomReview PersonId (Maybe ( NCDSymptomReviewId, NCDSymptomReview ))
      -- FamilyPlanningMsgs
    | SetFamilyPlanningSign FamilyPlanningSign
    | SaveFamilyPlanning PersonId (Maybe ( NCDFamilyPlanningId, NCDFamilyPlanning ))
      -- ExaminationMsgs
    | SetActiveExaminationTask ExaminationTask
      -- ExaminationMsgs, Vitals
    | SetVitalsIntInput (Maybe Int -> VitalsForm -> VitalsForm) String
    | SetVitalsFloatInput (Maybe Float -> VitalsForm -> VitalsForm) String
    | SaveVitals PersonId (Maybe ( NCDVitalsId, NCDVitals )) (Maybe ExaminationTask)
      -- ExaminationMsgs, Core Exam
    | SetCoreExamBoolInput (Bool -> CorePhysicalExamForm -> CorePhysicalExamForm) Bool
    | SetCoreExamHeart HeartCPESign
    | SetCoreExamNeck NeckCPESign
    | SetCoreExamLungs LungsCPESign
    | SetCoreExamAbdomen AbdomenCPESign
    | SetCoreExamHands HandsCPESign
    | SetCoreExamLegs LegsCPESign
    | SaveCoreExam PersonId (Maybe ( NCDCoreExamId, NCDCoreExam )) (Maybe ExaminationTask)
      -- MedicalHistoryMsgs
    | SetActiveMedicalHistoryTask MedicalHistoryTask
    | SetMedicalCondition MedicalCondition
    | SaveCoMorbidities PersonId (Maybe ( NCDCoMorbiditiesId, NCDCoMorbidities )) (Maybe MedicalHistoryTask)
    | SetMedicationCausingHypertension MedicationCausingHypertension
    | SetMedicationTreatingHypertension MedicationTreatingHypertension
    | SetMedicationTreatingDiabetes MedicationTreatingDiabetes
    | SaveMedicationHistory PersonId (Maybe ( NCDMedicationHistoryId, NCDMedicationHistory )) (Maybe MedicalHistoryTask)
    | SetSocialHistoryBoolInput (Bool -> SocialHistoryForm -> SocialHistoryForm) Bool
    | SetSocialHistoryIntInput (Maybe Int -> SocialHistoryForm -> SocialHistoryForm) String
    | SetFoodGroup FoodGroup
    | SaveSocialHistory PersonId (Maybe ( NCDSocialHistoryId, NCDSocialHistory )) (Maybe MedicalHistoryTask)
    | SetFamilyHistoryBoolInput (Bool -> FamilyHistoryForm -> FamilyHistoryForm) Bool
    | SetHypertensionPredecessor Predecessor
    | SetHeartProblemPredecessor Predecessor
    | SetDiabetesPredecessor Predecessor
    | SaveFamilyHistory PersonId (Maybe ( NCDFamilyHistoryId, NCDFamilyHistory )) (Maybe MedicalHistoryTask)
    | SetOutsideCareStep OutsideCareStep
    | SetOutsideCareSignBoolInput (Bool -> OutsideCareForm MedicalCondition -> OutsideCareForm MedicalCondition) Bool
    | SetOutsideCareDiagnosis MedicalCondition
    | SetOutsideCareMalariaMedication OutsideCareMedication
    | SetOutsideCareHypertensionMedication OutsideCareMedication
    | SetOutsideCareSyphilisMedication OutsideCareMedication
    | SetOutsideCareAnemiaMedication OutsideCareMedication
    | SetOutsideCareHIVMedication OutsideCareMedication
    | SaveOutsideCare PersonId (Maybe ( NCDOutsideCareId, NCDOutsideCare )) (Maybe MedicalHistoryTask)
      -- LABORATORYMsgs
    | SetActiveLaboratoryTask LaboratoryTask
    | SetHIVTestFormBoolInput (Bool -> HIVTestForm Msg -> HIVTestForm Msg) Bool
    | SetHIVTestExecutionNote TestExecutionNote
    | SetHIVTestExecutionDate NominalDate
    | SetHIVTestResult String
    | SetHIVTestDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SaveHIVTest PersonId (Maybe ( NCDHIVTestId, NCDHIVTest )) (Maybe LaboratoryTask)
    | SetUrineDipstickTestFormBoolInput (Bool -> UrineDipstickTestForm Msg -> UrineDipstickTestForm Msg) Bool
    | SetUrineDipstickTestExecutionNote TestExecutionNote
    | SetUrineDipstickTestVariant TestVariant
    | SetUrineDipstickTestExecutionDate NominalDate
    | SetUrineDipstickTestDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SaveUrineDipstickTest PersonId (Maybe ( NCDUrineDipstickTestId, NCDUrineDipstickTest )) (Maybe LaboratoryTask)
    | SetRandomBloodSugarTestFormBoolInput (Bool -> RandomBloodSugarTestForm Msg -> RandomBloodSugarTestForm Msg) Bool
    | SetRandomBloodSugarTestExecutionNote TestExecutionNote
    | SetRandomBloodSugarTestExecutionDate NominalDate
    | SetRandomBloodSugarTestDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SetRandomBloodSugarResult String
    | SaveRandomBloodSugarTest PersonId (Maybe ( NCDRandomBloodSugarTestId, NCDRandomBloodSugarTest )) (Maybe LaboratoryTask)
    | SetPregnancyTestFormBoolInput (Bool -> PregnancyTestForm Msg -> PregnancyTestForm Msg) Bool
    | SetPregnancyTestExecutionNote TestExecutionNote
    | SetPregnancyTestExecutionDate NominalDate
    | SetPregnancyTestResult String
    | SetPregnancyTestDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SavePregnancyTest PersonId (Maybe ( NCDPregnancyTestId, NCDPregnancyTest )) (Maybe LaboratoryTask)
    | SetCreatinineTestFormBoolInput (Bool -> NonRDTForm Msg -> NonRDTForm Msg) Bool
    | SetCreatinineTestExecutionNote TestExecutionNote
    | SetCreatinineTestExecutionDate NominalDate
    | SetCreatinineTestDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SaveCreatinineTest PersonId (Maybe ( NCDCreatinineTestId, NCDCreatinineTest )) (Maybe LaboratoryTask)
    | SetLiverFunctionTestFormBoolInput (Bool -> NonRDTForm Msg -> NonRDTForm Msg) Bool
    | SetLiverFunctionTestExecutionNote TestExecutionNote
    | SetLiverFunctionTestExecutionDate NominalDate
    | SetLiverFunctionTestDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SaveLiverFunctionTest PersonId (Maybe ( NCDLiverFunctionTestId, NCDLiverFunctionTest )) (Maybe LaboratoryTask)
    | SetLipidPanelTestFormBoolInput (Bool -> NonRDTForm Msg -> NonRDTForm Msg) Bool
    | SetLipidPanelTestExecutionNote TestExecutionNote
    | SetLipidPanelTestExecutionDate NominalDate
    | SetLipidPanelTestDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SaveLipidPanelTest PersonId (Maybe ( NCDLipidPanelTestId, NCDLipidPanelTest )) (Maybe LaboratoryTask)
    | SetHbA1cTestFormBoolInput (Bool -> HbA1cTestForm Msg -> HbA1cTestForm Msg) Bool
    | SetHbA1cTestExecutionDate NominalDate
    | SetHbA1cTestDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SetHbA1cTestResult String
    | SaveHbA1cTest PersonId (Maybe ( NCDHbA1cTestId, NCDHbA1cTest )) (Maybe LaboratoryTask)
      -- NextStepsMsgs
    | SetActiveNextStepsTask NextStepsTask
    | SetHealthEducationBoolInput (Bool -> HealthEducationForm -> HealthEducationForm) Bool
    | SaveHealthEducation PersonId (Maybe ( NCDHealthEducationId, NCDHealthEducation )) (Maybe NextStepsTask)
    | SetRecommendedTreatmentSignSingle (List RecommendedTreatmentSign) RecommendedTreatmentSign
    | SetRecommendedTreatmentSignMultiple (List RecommendedTreatmentSign) RecommendedTreatmentSign RecommendedTreatmentSign
    | SetMedicationDistributionBoolInput (Bool -> MedicationDistributionForm -> MedicationDistributionForm) Bool
    | SaveMedicationDistribution PersonId (Maybe ( NCDMedicationDistributionId, NCDMedicationDistribution )) (Maybe NextStepsTask)
    | SetReferralBoolInput (Bool -> ReferralForm -> ReferralForm) Bool
    | SetFacilityNonReferralReason (Maybe ReasonForNonReferral) ReferralFacility ReasonForNonReferral
    | SaveReferral PersonId (Maybe ( NCDReferralId, NCDReferral )) (Maybe NextStepsTask)


type alias Model =
    { dangerSignsData : DangerSignsData
    , symptomReviewData : SymptomReviewData
    , examinationData : ExaminationData
    , familyPlanningData : FamilyPlanningData
    , medicalHistoryData : MedicalHistoryData
    , laboratoryData : LaboratoryData
    , outsideCareData : OutsideCareData
    , nextStepsData : NextStepsData
    }


emptyModel : Model
emptyModel =
    { dangerSignsData = emptyDangerSignsData
    , symptomReviewData = emptySymptomReviewData
    , examinationData = emptyExaminationData
    , familyPlanningData = emptyFamilyPlanningData
    , medicalHistoryData = emptyMedicalHistoryData
    , laboratoryData = emptyLaboratoryData
    , outsideCareData = emptyOutsideCareData
    , nextStepsData = emptyNextStepsData
    }


type alias DangerSignsData =
    { form : DangerSignsForm
    }


emptyDangerSignsData : DangerSignsData
emptyDangerSignsData =
    { form = emptyDangerSignsForm
    }


type alias DangerSignsForm =
    { signs : Maybe (List NCDDangerSign)
    }


emptyDangerSignsForm : DangerSignsForm
emptyDangerSignsForm =
    DangerSignsForm Nothing


type alias SymptomReviewData =
    { form : SymptomReviewForm
    }


emptySymptomReviewData : SymptomReviewData
emptySymptomReviewData =
    { form = emptySymptomReviewForm
    }


type alias SymptomReviewForm =
    { group1Symptoms : Maybe (List NCDGroup1Symptom)
    , group2Symptoms : Maybe (List NCDGroup2Symptom)
    , painSymptoms : Maybe (List NCDPainSymptom)
    }


emptySymptomReviewForm : SymptomReviewForm
emptySymptomReviewForm =
    SymptomReviewForm Nothing Nothing Nothing


type alias ExaminationData =
    { vitalsForm : VitalsForm
    , coreExamForm : CorePhysicalExamForm
    , activeTask : Maybe ExaminationTask
    }


emptyExaminationData : ExaminationData
emptyExaminationData =
    { vitalsForm = emptyVitalsForm
    , coreExamForm = emptyCorePhysicalExamForm
    , activeTask = Nothing
    }


type alias FamilyPlanningData =
    { form : FamilyPlanningForm
    }


emptyFamilyPlanningData : FamilyPlanningData
emptyFamilyPlanningData =
    { form = emptyFamilyPlanningForm
    }


type alias MedicalHistoryData =
    { coMorbiditiesForm : CoMorbiditiesForm
    , familyHistoryForm : FamilyHistoryForm
    , medicationHistoryForm : MedicationHistoryForm
    , socialHistoryForm : SocialHistoryForm
    , outsideCareForm : OutsideCareForm MedicalCondition
    , activeTask : Maybe MedicalHistoryTask
    }


emptyMedicalHistoryData : MedicalHistoryData
emptyMedicalHistoryData =
    { coMorbiditiesForm = emptyCoMorbiditiesForm
    , familyHistoryForm = emptyFamilyHistoryForm
    , medicationHistoryForm = emptyMedicationHistoryForm
    , socialHistoryForm = emptySocialHistoryForm
    , outsideCareForm = emptyOutsideCareForm
    , activeTask = Nothing
    }


type alias CoMorbiditiesForm =
    { conditions : Maybe (List MedicalCondition)
    }


emptyCoMorbiditiesForm : CoMorbiditiesForm
emptyCoMorbiditiesForm =
    CoMorbiditiesForm Nothing


type alias FamilyHistoryForm =
    { hypertensionInFamily : Maybe Bool
    , heartProblemInFamily : Maybe Bool
    , diabetesInFamily : Maybe Bool
    , hypertensionPredecessors : Maybe (List Predecessor)
    , hypertensionPredecessorsDirty : Bool
    , heartProblemPredecessors : Maybe (List Predecessor)
    , heartProblemPredecessorsDirty : Bool
    , diabetesPredecessors : Maybe (List Predecessor)
    , diabetesPredecessorsDirty : Bool
    }


emptyFamilyHistoryForm : FamilyHistoryForm
emptyFamilyHistoryForm =
    { hypertensionInFamily = Nothing
    , heartProblemInFamily = Nothing
    , diabetesInFamily = Nothing
    , hypertensionPredecessors = Nothing
    , hypertensionPredecessorsDirty = False
    , heartProblemPredecessors = Nothing
    , heartProblemPredecessorsDirty = False
    , diabetesPredecessors = Nothing
    , diabetesPredecessorsDirty = False
    }


type alias MedicationHistoryForm =
    { medicationsCausingHypertension : Maybe (List MedicationCausingHypertension)
    , medicationsTreatingHypertension : Maybe (List MedicationTreatingHypertension)
    , medicationsTreatingDiabetes : Maybe (List MedicationTreatingDiabetes)
    }


emptyMedicationHistoryForm : MedicationHistoryForm
emptyMedicationHistoryForm =
    MedicationHistoryForm Nothing Nothing Nothing


type alias SocialHistoryForm =
    { alcohol : Maybe Bool
    , cigarettes : Maybe Bool
    , salt : Maybe Bool
    , difficult4Times : Maybe Bool
    , helpAtHome : Maybe Bool
    , foodGroup : Maybe FoodGroup
    , beveragesPerWeek : Maybe Int
    , beveragesPerWeekDirty : Bool
    , cigarettesPerWeek : Maybe Int
    , cigarettesPerWeekDirty : Bool
    }


emptySocialHistoryForm : SocialHistoryForm
emptySocialHistoryForm =
    { alcohol = Nothing
    , cigarettes = Nothing
    , salt = Nothing
    , difficult4Times = Nothing
    , helpAtHome = Nothing
    , foodGroup = Nothing
    , beveragesPerWeek = Nothing
    , beveragesPerWeekDirty = False
    , cigarettesPerWeek = Nothing
    , cigarettesPerWeekDirty = False
    }


type alias LaboratoryData =
    { hivTestForm : HIVTestForm Msg
    , urineDipstickTestForm : UrineDipstickTestForm Msg
    , randomBloodSugarTestForm : RandomBloodSugarTestForm Msg
    , pregnancyTestForm : PregnancyTestForm Msg
    , creatinineTestForm : NonRDTForm Msg
    , liverFunctionTestForm : NonRDTForm Msg
    , lipidPanelTestForm : NonRDTForm Msg
    , hba1cTestForm : HbA1cTestForm Msg
    , activeTask : Maybe LaboratoryTask
    }


emptyLaboratoryData : LaboratoryData
emptyLaboratoryData =
    { hivTestForm = emptyHIVTestForm
    , urineDipstickTestForm = emptyUrineDipstickTestForm
    , randomBloodSugarTestForm = emptyRandomBloodSugarTestForm
    , pregnancyTestForm = emptyPregnancyTestForm
    , creatinineTestForm = emptyNonRDTForm
    , liverFunctionTestForm = emptyNonRDTForm
    , lipidPanelTestForm = emptyNonRDTForm
    , hba1cTestForm = emptyHbA1cTestForm
    , activeTask = Nothing
    }


type alias NextStepsData =
    { referralForm : ReferralForm
    , healthEducationForm : HealthEducationForm
    , medicationDistributionForm : MedicationDistributionForm
    , activeTask : Maybe NextStepsTask
    }


emptyNextStepsData : NextStepsData
emptyNextStepsData =
    { referralForm = emptyReferralForm
    , healthEducationForm = emptyHealthEducationForm
    , medicationDistributionForm = emptyMedicationDistributionForm
    , activeTask = Nothing
    }


type alias HealthEducationForm =
    { hypertension : Maybe Bool }


emptyHealthEducationForm : HealthEducationForm
emptyHealthEducationForm =
    { hypertension = Nothing }


type alias OutsideCareData =
    { form : OutsideCareForm MedicalCondition
    , step : OutsideCareStep
    }


emptyOutsideCareData : OutsideCareData
emptyOutsideCareData =
    { form = emptyOutsideCareForm
    , step = OutsideCareStepDiagnoses
    }
