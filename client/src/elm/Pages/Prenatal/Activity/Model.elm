module Pages.Prenatal.Activity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis)
import Date exposing (Date)
import DateSelector.SelectorPopup exposing (DateSelectorConfig)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (DropZoneFile, SendToHCForm, VitalsForm, emptySendToHCForm, emptyVitalsForm)
import Pages.Page exposing (Page)
import Pages.Prenatal.Activity.Types exposing (..)
import Pages.Prenatal.Model exposing (..)


type Msg
    = NoOp
    | DropZoneComplete DropZoneFile
    | SetActivePage Page
    | SetAlertsDialogState Bool
    | SetWarningPopupState (Maybe ( String, String ))
    | ViewWarningPopupForNonUrgentDiagnoses
      -- PregnancyDatingMsgs
    | SetLmpDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SetConfirmLmpDate NominalDate Bool
    | SetLmpDate Date
    | SetLmpDateConfident Bool
    | SetLmpRange String
    | SavePregnancyDating IndividualEncounterParticipantId PersonId (Maybe ( LastMenstrualPeriodId, LastMenstrualPeriod ))
      -- HistoryMsgs
    | SetActiveHistoryTask HistoryTask
      -- HistoryMsgs, OB, Step 1
    | SetCurrentlyPregnant Bool
    | SetOBIntInput (Maybe Int -> ObstetricFormFirstStep -> ObstetricFormFirstStep) String
    | SaveOBHistoryStep1 PersonId (Maybe ( ObstetricHistoryId, ObstetricHistory ))
      -- HistoryMsgs, OB, Step 2
    | SetCSectionReason CSectionReason
    | SetNumberOfCSections String
    | SetOBBoolInput (Bool -> ObstetricFormSecondStep -> ObstetricFormSecondStep) Bool
    | SetPreviousDeliveryPeriod PreviousDeliveryPeriod
    | BackToOBHistoryStep1
    | SaveOBHistoryStep2 PersonId (Maybe ( ObstetricHistoryStep2Id, ObstetricHistoryStep2 )) (Maybe HistoryTask)
      -- HistoryMsgs, Medical
    | SetMedicalBoolInput (Bool -> MedicalHistoryForm -> MedicalHistoryForm) Bool
    | SaveMedicalHistory PersonId (Maybe ( MedicalHistoryId, MedicalHistory )) (Maybe HistoryTask)
      -- HistoryMsgs, Social
    | SetSocialBoolInput (Bool -> SocialHistoryForm -> SocialHistoryForm) Bool
    | SetSocialHivTestingResult String
    | SaveSocialHistory PersonId (Maybe ( SocialHistoryId, SocialHistory )) (Maybe HistoryTask)
      -- HistoryMsgs, Outside Care
    | SetOutsideCareStep OutsideCareStep
    | SetOutsideCareSignBoolInput (Bool -> OutsideCareForm -> OutsideCareForm) Bool
    | SetOutsideCareDiagnosis PrenatalDiagnosis
    | SetOutsideCareMalariaMedication PrenatalOutsideCareMedication
    | SetOutsideCareHypertensionMedication PrenatalOutsideCareMedication
    | SetOutsideCareSyphilisMedication PrenatalOutsideCareMedication
    | SaveOutsideCare PersonId (Maybe ( PrenatalOutsideCareId, PrenatalOutsideCare )) (Maybe HistoryTask)
      -- ExaminationMsgs
    | SetActiveExaminationTask ExaminationTask
      -- ExaminationMsgs, Vitals
    | SetVitalsIntInput (Maybe Int -> VitalsForm -> VitalsForm) String
    | SetVitalsFloatInput (Maybe Float -> VitalsForm -> VitalsForm) String
    | SaveVitals PersonId (Maybe ( VitalsId, Vitals )) (Maybe ExaminationTask)
      -- ExaminationMsgs, Nutrition Assessment
    | SetNutritionAssessmentMeasurement (Maybe Float -> NutritionAssessmentForm -> NutritionAssessmentForm) String
    | SaveNutritionAssessment PersonId (Maybe ( PrenatalNutritionId, PrenatalNutrition )) (Maybe Float) (Maybe ExaminationTask)
      -- ExaminationMsgs, Core Physical Exam
    | SetCorePhysicalExamBoolInput (Bool -> CorePhysicalExamForm -> CorePhysicalExamForm) Bool
    | SetCorePhysicalExamHeart HeartCPESign
    | SetCorePhysicalExamNeck NeckCPESign
    | SetCorePhysicalExamLungs LungsCPESign
    | SetCorePhysicalExamAbdomen AbdomenCPESign
    | SetCorePhysicalExamHands HandsCPESign
    | SetCorePhysicalExamLegs LegsCPESign
    | SaveCorePhysicalExam PersonId (Maybe ( CorePhysicalExamId, CorePhysicalExam )) (Maybe ExaminationTask)
      -- ExaminationMsgs, Obstetrical Exam
    | SetObstetricalExamBoolInput (Bool -> ObstetricalExamForm -> ObstetricalExamForm) Bool
    | SetObstetricalExamIntMeasurement (Maybe Int -> ObstetricalExamForm -> ObstetricalExamForm) String
    | SetObstetricalExamFloatMeasurement (Maybe Float -> ObstetricalExamForm -> ObstetricalExamForm) String
    | SetObstetricalExamFetalPresentation FetalPresentation
    | SetObstetricalExamCSectionScar CSectionScar
    | SaveObstetricalExam PersonId (Maybe ( ObstetricalExamId, ObstetricalExam )) (Maybe ExaminationTask)
      -- ExaminationMsgs, Breast Exam
    | SetBreastExamBoolInput (Bool -> BreastExamForm -> BreastExamForm) Bool
    | SetBreastExamBreast BreastExamSign
    | SaveBreastExam PersonId (Maybe ( BreastExamId, BreastExam )) (Maybe ExaminationTask)
      -- FamilyPlanningMsgs
    | SetFamilyPlanningSign FamilyPlanningSign
    | SaveFamilyPlanning PersonId (Maybe ( PrenatalFamilyPlanningId, PrenatalFamilyPlanning ))
      -- MedicationMsgs
    | SetMedicationBoolInput (Bool -> MedicationForm -> MedicationForm) Bool
    | SaveMedication PersonId (Maybe ( MedicationId, Medication ))
      -- MalariaPreventionMsgs
    | SetMalariaPreventionBoolInput (Bool -> MalariaPreventionForm -> MalariaPreventionForm) Bool
    | SaveMalariaPrevention PersonId (Maybe ( MalariaPreventionId, MalariaPrevention ))
      -- DangerSignsMsgs
    | SetDangerSign DangerSign
    | SetPostpartumMotherDangerSign PostpartumMotherDangerSign
    | SetPostpartumChildDangerSign PostpartumChildDangerSign
    | SaveDangerSigns PersonId (Maybe ( DangerSignsId, DangerSigns ))
      -- PrenatalPhotoMsgs
    | SavePrenatalPhoto PersonId (Maybe PrenatalPhotoId) PhotoUrl
      -- BirthPlanMsgs
    | SetBirthPlanBoolInput (Bool -> BirthPlanForm -> BirthPlanForm) Bool
    | SetBirthPlanFamilyPlanning FamilyPlanningSign
    | SaveBirthPlan PersonId (Maybe ( BirthPlanId, BirthPlan ))
      -- LABORATORYMsgs
    | SetActiveLaboratoryTask LaboratoryTask
    | SetPregnancyTestResult String
    | SavePregnancyTest PersonId (Maybe ( PregnancyTestId, PregnancyTest ))
    | SetHIVTestFormBoolInput (Bool -> PrenatalHIVTestForm -> PrenatalHIVTestForm) Bool
    | SetHIVTestExecutionNote PrenatalTestExecutionNote
    | SetHIVTestExecutionDate NominalDate
    | SetHIVTestResult String
    | SetHIVTestDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SaveHIVTest PersonId (Maybe ( PrenatalHIVTestId, PrenatalHIVTest )) (Maybe LaboratoryTask)
    | SetSyphilisTestFormBoolInput (Bool -> PrenatalLabsNonRDTForm -> PrenatalLabsNonRDTForm) Bool
    | SetSyphilisTestExecutionNote PrenatalTestExecutionNote
    | SetSyphilisTestExecutionDate NominalDate
    | SetSyphilisTestDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SaveSyphilisTest PersonId (Maybe ( PrenatalSyphilisTestId, PrenatalSyphilisTest )) (Maybe LaboratoryTask)
    | SetHepatitisBTestFormBoolInput (Bool -> PrenatalLabsNonRDTForm -> PrenatalLabsNonRDTForm) Bool
    | SetHepatitisBTestExecutionNote PrenatalTestExecutionNote
    | SetHepatitisBTestExecutionDate NominalDate
    | SetHepatitisBTestDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SaveHepatitisBTest PersonId (Maybe ( PrenatalHepatitisBTestId, PrenatalHepatitisBTest )) (Maybe LaboratoryTask)
    | SetMalariaTestFormBoolInput (Bool -> PrenatalMalariaTestForm -> PrenatalMalariaTestForm) Bool
    | SetMalariaTestExecutionNote PrenatalTestExecutionNote
    | SetMalariaTestExecutionDate NominalDate
    | SetMalariaTestResult String
    | SetMalariaTestDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SaveMalariaTest PersonId (Maybe ( PrenatalMalariaTestId, PrenatalMalariaTest )) (Maybe LaboratoryTask)
    | SetBloodGpRsTestFormBoolInput (Bool -> PrenatalLabsNonRDTForm -> PrenatalLabsNonRDTForm) Bool
    | SetBloodGpRsTestExecutionNote PrenatalTestExecutionNote
    | SetBloodGpRsTestExecutionDate NominalDate
    | SetBloodGpRsTestDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SaveBloodGpRsTest PersonId (Maybe ( PrenatalBloodGpRsTestId, PrenatalBloodGpRsTest )) (Maybe LaboratoryTask)
    | SetUrineDipstickTestFormBoolInput (Bool -> PrenatalUrineDipstickForm -> PrenatalUrineDipstickForm) Bool
    | SetUrineDipstickTestExecutionNote PrenatalTestExecutionNote
    | SetUrineDipstickTestVariant PrenatalTestVariant
    | SetUrineDipstickTestExecutionDate NominalDate
    | SetUrineDipstickTestDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SaveUrineDipstickTest PersonId (Maybe ( PrenatalUrineDipstickTestId, PrenatalUrineDipstickTest )) (Maybe LaboratoryTask)
    | SetHemoglobinTestFormBoolInput (Bool -> PrenatalLabsNonRDTForm -> PrenatalLabsNonRDTForm) Bool
    | SetHemoglobinTestExecutionNote PrenatalTestExecutionNote
    | SetHemoglobinTestExecutionDate NominalDate
    | SetHemoglobinTestDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SaveHemoglobinTest PersonId (Maybe ( PrenatalHemoglobinTestId, PrenatalHemoglobinTest )) (Maybe LaboratoryTask)
    | SetRandomBloodSugarTestFormBoolInput (Bool -> PrenatalLabsNonRDTForm -> PrenatalLabsNonRDTForm) Bool
    | SetRandomBloodSugarTestExecutionNote PrenatalTestExecutionNote
    | SetRandomBloodSugarTestExecutionDate NominalDate
    | SetRandomBloodSugarTestDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SaveRandomBloodSugarTest PersonId (Maybe ( PrenatalRandomBloodSugarTestId, PrenatalRandomBloodSugarTest )) (Maybe LaboratoryTask)
      -- HealtEducationMsgs
    | SetHealthEducationBoolInput (Bool -> HealthEducationForm -> HealthEducationForm) Bool
    | SaveHealthEducation PersonId (Maybe ( PrenatalHealthEducationId, PrenatalHealthEducation ))
      -- NextStepsMsgs
    | SetActiveNextStepsTask NextStepsTask
    | SetHealthEducationSubActivityBoolInput (Bool -> HealthEducationForm -> HealthEducationForm) Bool
    | SaveHealthEducationSubActivity PersonId (Maybe ( PrenatalHealthEducationId, PrenatalHealthEducation )) Bool (Maybe NextStepsTask)
    | SetFollowUpOption FollowUpOption
    | SaveFollowUp PersonId PrenatalAssesment (Maybe ( PrenatalFollowUpId, PrenatalFollowUp )) Bool (Maybe NextStepsTask)
    | SaveNewbornEnrollment Bool (Maybe NextStepsTask)
    | SetReferToHealthCenter Bool
    | SetHandReferralForm Bool
    | SetAccompanyToHC Bool
    | SetReasonForNotSendingToHC ReasonForNotSendingToHC
    | SaveSendToHC PersonId (Maybe ( PrenatalSendToHcId, PrenatalSendToHC )) Bool (Maybe NextStepsTask)
    | SetAppointmentDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SetAppointmentConfirmation Date
    | SaveAppointmentConfirmation PersonId (Maybe ( PrenatalAppointmentConfirmationId, PrenatalAppointmentConfirmation )) Bool (Maybe NextStepsTask)
    | SetMedicationDistributionBoolInput (Bool -> MedicationDistributionForm -> MedicationDistributionForm) Bool
    | SetMedicationDistributionAdministrationNote (Maybe AdministrationNote) MedicationDistributionSign AdministrationNote
    | SetRecommendedTreatmentSign (List RecommendedTreatmentSign) RecommendedTreatmentSign
    | SetAvoidingGuidanceReason AvoidingGuidanceReason
    | SaveMedicationDistribution PersonId (Maybe ( PrenatalMedicationDistributionId, PrenatalMedicationDistribution )) Bool (Maybe NextStepsTask)
    | SaveWait PersonId (Maybe PrenatalLabsResultsId) PrenatalLabsResultsValue Bool (Maybe NextStepsTask)
      -- SYMPTOMREVIEWMsgs
    | SetSymptomReviewStep SymptomReviewStep
    | SetPrenatalSymptom PrenatalSymptom
    | SetPrenatalSymptomQuestionBoolInput (Bool -> SymptomReviewForm -> SymptomReviewForm) Bool
    | SetFlankPainSign PrenatalFlankPainSign
    | SaveSymptomReview PersonId (Maybe ( PrenatalSymptomReviewId, PrenatalSymptomReview ))
      -- TREATMENTREVIEWMsgs
    | SetActiveTreatmentReviewTask TreatmentReviewTask
    | SetTreatmentReviewWarningPopupState (Maybe Msg)
    | SetMedicationSubActivityBoolInput (Bool -> MedicationForm -> MedicationForm) Bool
    | SetHIVMedicationNotGivenReason HIVTreatmentSign
    | SaveMedicationSubActivity PersonId (Maybe ( MedicationId, Medication )) (Maybe TreatmentReviewTask)


type alias Model =
    { pregnancyDatingData : PregnancyDatingData
    , historyData : HistoryData
    , examinationData : ExaminationData
    , familyPlanningData : FamilyPlanningData
    , malariaPreventionData : MalariaPreventionData
    , medicationData : MedicationData
    , dangerSignsData : DangerSignsData
    , prenatalPhotoData : PrenatalPhotoData
    , birthPlanData : BirthPlanData
    , laboratoryData : LaboratoryData
    , healthEducationData : HealthEducationData
    , symptomReviewData : SymptomReviewData
    , treatmentReviewData : TreatmentReviewData
    , nextStepsData : NextStepsData
    , showAlertsDialog : Bool
    , warningPopupState : Maybe ( String, String )
    }


emptyModel : Model
emptyModel =
    { pregnancyDatingData = emptyPregnancyDatingData
    , historyData = emptyHistoryData
    , examinationData = emptyExaminationData
    , familyPlanningData = emptyFamilyPlanningData
    , malariaPreventionData = emptyMalariaPreventionData
    , medicationData = emptyMedicationData
    , dangerSignsData = emptyDangerSignsData
    , prenatalPhotoData = emptyPrenatalPhotoData
    , birthPlanData = emptyBirthPlanData
    , laboratoryData = emptyLaboratoryData
    , healthEducationData = emptyHealthEducationData
    , symptomReviewData = emptySymptomReviewData
    , treatmentReviewData = emptyTreatmentReviewData
    , nextStepsData = emptyNextStepsData
    , showAlertsDialog = False
    , warningPopupState = Nothing
    }



-- DATA


type alias PregnancyDatingData =
    { form : PregnancyDatingForm
    }


emptyPregnancyDatingData : PregnancyDatingData
emptyPregnancyDatingData =
    { form = emptyPregnancyDatingForm
    }


type alias HistoryData =
    { obstetricFormFirstStep : ObstetricFormFirstStep
    , obstetricFormSecondStep : ObstetricFormSecondStep
    , obstetricHistoryStep : ObstetricHistoryStep
    , medicalForm : MedicalHistoryForm
    , socialForm : SocialHistoryForm
    , outsideCareForm : OutsideCareForm
    , outsideCareStep : OutsideCareStep
    , activeTask : Maybe HistoryTask
    }


emptyHistoryData : HistoryData
emptyHistoryData =
    { obstetricFormFirstStep = emptyObstetricFormFirstStep
    , obstetricFormSecondStep = emptyObstetricFormSecondStep
    , obstetricHistoryStep = ObstetricHistoryFirstStep
    , medicalForm = emptyMedicalHistoryForm
    , socialForm = emptySocialHistoryForm
    , outsideCareForm = emptyOutsideCareForm
    , outsideCareStep = OutsideCareStepDiagnoses
    , activeTask = Nothing
    }


type alias ExaminationData =
    { vitalsForm : VitalsForm
    , nutritionAssessmentForm : NutritionAssessmentForm
    , corePhysicalExamForm : CorePhysicalExamForm
    , obstetricalExamForm : ObstetricalExamForm
    , breastExamForm : BreastExamForm
    , activeTask : ExaminationTask
    }


emptyExaminationData : ExaminationData
emptyExaminationData =
    { vitalsForm = emptyVitalsForm
    , nutritionAssessmentForm = emptyNutritionAssessmentForm
    , corePhysicalExamForm = emptyCorePhysicalExamForm
    , obstetricalExamForm = emptyObstetricalExamForm
    , breastExamForm = emptyBreastExamForm
    , activeTask = Vitals
    }


type alias FamilyPlanningData =
    { form : FamilyPlanningForm
    }


emptyFamilyPlanningData : FamilyPlanningData
emptyFamilyPlanningData =
    { form = emptyFamilyPlanningForm
    }


type alias MalariaPreventionData =
    { form : MalariaPreventionForm
    }


emptyMalariaPreventionData : MalariaPreventionData
emptyMalariaPreventionData =
    { form = emptyMalariaPreventionForm
    }


type alias MalariaPreventionForm =
    { receivedMosquitoNet : Maybe Bool
    }


emptyMalariaPreventionForm : MalariaPreventionForm
emptyMalariaPreventionForm =
    MalariaPreventionForm Nothing


type alias MedicationData =
    { form : MedicationForm
    }


emptyMedicationData : MedicationData
emptyMedicationData =
    { form = emptyMedicationForm
    }


type alias MedicationForm =
    { receivedIronFolicAcid : Maybe Bool
    , receivedDewormingPill : Maybe Bool
    , receivedMebendazole : Maybe Bool
    , hivMedicationByPMTCT : Maybe Bool
    , hivMedicationNotGivenReason : Maybe HIVTreatmentSign
    , hivMedicationNotGivenReasonDirty : Bool
    , hivStillTaking : Maybe Bool
    , hivMissedDoses : Maybe Bool
    , hivAdverseEvents : Maybe Bool
    , hivAdverseEventsHospitalization : Maybe Bool
    , hivAdverseEventsHospitalizationDirty : Bool
    , hypertensionStillTaking : Maybe Bool
    , hypertensionMissedDoses : Maybe Bool
    , hypertensionAdverseEvents : Maybe Bool
    , hypertensionAdverseEventsHospitalization : Maybe Bool
    , hypertensionAdverseEventsHospitalizationDirty : Bool
    , malariaStillTaking : Maybe Bool
    , malariaMissedDoses : Maybe Bool
    , malariaAdverseEvents : Maybe Bool
    , malariaAdverseEventsHospitalization : Maybe Bool
    , malariaAdverseEventsHospitalizationDirty : Bool
    , anemiaStillTaking : Maybe Bool
    , anemiaMissedDoses : Maybe Bool
    , anemiaAdverseEvents : Maybe Bool
    , anemiaAdverseEventsHospitalization : Maybe Bool
    , anemiaAdverseEventsHospitalizationDirty : Bool
    , syphilisStillTaking : Maybe Bool
    , syphilisMissedDoses : Maybe Bool
    , syphilisAdverseEvents : Maybe Bool
    , syphilisAdverseEventsHospitalization : Maybe Bool
    , syphilisAdverseEventsHospitalizationDirty : Bool
    }


emptyMedicationForm : MedicationForm
emptyMedicationForm =
    { receivedIronFolicAcid = Nothing
    , receivedDewormingPill = Nothing
    , receivedMebendazole = Nothing
    , hivMedicationByPMTCT = Nothing
    , hivMedicationNotGivenReason = Nothing
    , hivMedicationNotGivenReasonDirty = False
    , hivStillTaking = Nothing
    , hivMissedDoses = Nothing
    , hivAdverseEvents = Nothing
    , hivAdverseEventsHospitalization = Nothing
    , hivAdverseEventsHospitalizationDirty = False
    , hypertensionStillTaking = Nothing
    , hypertensionMissedDoses = Nothing
    , hypertensionAdverseEvents = Nothing
    , hypertensionAdverseEventsHospitalization = Nothing
    , hypertensionAdverseEventsHospitalizationDirty = False
    , malariaStillTaking = Nothing
    , malariaMissedDoses = Nothing
    , malariaAdverseEvents = Nothing
    , malariaAdverseEventsHospitalization = Nothing
    , malariaAdverseEventsHospitalizationDirty = False
    , anemiaStillTaking = Nothing
    , anemiaMissedDoses = Nothing
    , anemiaAdverseEvents = Nothing
    , anemiaAdverseEventsHospitalization = Nothing
    , anemiaAdverseEventsHospitalizationDirty = False
    , syphilisStillTaking = Nothing
    , syphilisMissedDoses = Nothing
    , syphilisAdverseEvents = Nothing
    , syphilisAdverseEventsHospitalization = Nothing
    , syphilisAdverseEventsHospitalizationDirty = False
    }


type alias DangerSignsData =
    { form : DangerSignsForm
    }


emptyDangerSignsData : DangerSignsData
emptyDangerSignsData =
    { form = emptyDangerSignsForm
    }


type alias PrenatalPhotoData =
    { url : Maybe PhotoUrl }


emptyPrenatalPhotoData : PrenatalPhotoData
emptyPrenatalPhotoData =
    { url = Nothing }


type alias BirthPlanData =
    { form : BirthPlanForm
    }


emptyBirthPlanData : BirthPlanData
emptyBirthPlanData =
    BirthPlanData emptyBirthPlanForm


type alias LaboratoryData =
    { pregnancyTestForm : PregnancyTestForm
    , bloodGpRsTestForm : PrenatalLabsNonRDTForm
    , hemoglobinTestForm : PrenatalLabsNonRDTForm
    , hepatitisBTestForm : PrenatalLabsNonRDTForm
    , hivTestForm : PrenatalHIVTestForm
    , malariaTestForm : PrenatalMalariaTestForm
    , randomBloodSugarTestForm : PrenatalLabsNonRDTForm
    , syphilisTestForm : PrenatalLabsNonRDTForm
    , urineDipstickTestForm : PrenatalUrineDipstickForm
    , activeTask : Maybe LaboratoryTask
    }


emptyLaboratoryData : LaboratoryData
emptyLaboratoryData =
    { pregnancyTestForm = PregnancyTestForm Nothing
    , bloodGpRsTestForm = emptyPrenatalLabsNonRDTForm
    , hemoglobinTestForm = emptyPrenatalLabsNonRDTForm
    , hepatitisBTestForm = emptyPrenatalLabsNonRDTForm
    , hivTestForm = emptyPrenatalHIVTestForm
    , malariaTestForm = emptyPrenatalMalariaTestForm
    , randomBloodSugarTestForm = emptyPrenatalLabsNonRDTForm
    , syphilisTestForm = emptyPrenatalLabsNonRDTForm
    , urineDipstickTestForm = emptyPrenatalUrineDipstickForm
    , activeTask = Nothing
    }


type alias HealthEducationData =
    { form : HealthEducationForm
    }


emptyHealthEducationData : HealthEducationData
emptyHealthEducationData =
    HealthEducationData emptyHealthEducationForm


type alias SymptomReviewData =
    { form : SymptomReviewForm
    , step : SymptomReviewStep
    }


emptySymptomReviewData : SymptomReviewData
emptySymptomReviewData =
    { form = emptySymptomReviewForm
    , step = SymptomReviewStepSymptoms
    }


type alias SymptomReviewForm =
    { symptoms : Maybe (List PrenatalSymptom)
    , dizziness : Maybe Bool
    , lowUrineOutput : Maybe Bool
    , darkUrine : Maybe Bool
    , pelvicPainHospitalization : Maybe Bool
    , problemLeftLeg : Maybe Bool
    , legPainful : Maybe Bool
    , legWarm : Maybe Bool
    , legSwollen : Maybe Bool
    , nightSweats : Maybe Bool
    , bloodInSputum : Maybe Bool
    , weightLoss : Maybe Bool
    , severeFatigue : Maybe Bool
    , vaginalDischarge : Maybe Bool
    , frequentUrination : Maybe Bool
    , vaginalItching : Maybe Bool
    , partnerUrethralDischarge : Maybe Bool
    , flankPainSign : Maybe PrenatalFlankPainSign
    }


emptySymptomReviewForm : SymptomReviewForm
emptySymptomReviewForm =
    { symptoms = Nothing
    , dizziness = Nothing
    , lowUrineOutput = Nothing
    , darkUrine = Nothing
    , pelvicPainHospitalization = Nothing
    , problemLeftLeg = Nothing
    , legPainful = Nothing
    , legWarm = Nothing
    , legSwollen = Nothing
    , nightSweats = Nothing
    , bloodInSputum = Nothing
    , weightLoss = Nothing
    , severeFatigue = Nothing
    , vaginalDischarge = Nothing
    , frequentUrination = Nothing
    , vaginalItching = Nothing
    , partnerUrethralDischarge = Nothing
    , flankPainSign = Nothing
    }


type alias TreatmentReviewData =
    { medicationForm : MedicationForm
    , activeTask : Maybe TreatmentReviewTask
    , warningPopupState : Maybe Msg
    }


emptyTreatmentReviewData : TreatmentReviewData
emptyTreatmentReviewData =
    { medicationForm = emptyMedicationForm
    , activeTask = Nothing
    , warningPopupState = Nothing
    }


type alias NextStepsData =
    { appointmentConfirmationForm : AppointmentConfirmationForm
    , followUpForm : FollowUpForm
    , sendToHCForm : SendToHCForm
    , healthEducationForm : HealthEducationForm
    , newbornEnrolmentForm : NewbornEnrolmentForm
    , medicationDistributionForm : MedicationDistributionForm
    , activeTask : Maybe NextStepsTask
    }


emptyNextStepsData : NextStepsData
emptyNextStepsData =
    { appointmentConfirmationForm = emptyAppointmentConfirmationForm
    , followUpForm = emptyFollowUpForm
    , sendToHCForm = emptySendToHCForm
    , healthEducationForm = emptyHealthEducationForm
    , newbornEnrolmentForm = emptyNewbornEnrolmentForm
    , medicationDistributionForm = emptyMedicationDistributionForm
    , activeTask = Nothing
    }



-- FORMS


type ObstetricHistoryStep
    = ObstetricHistoryFirstStep
    | ObstetricHistorySecondStep


type alias PregnancyDatingForm =
    { lmpRange : Maybe LmpRange
    , lmpDate : Maybe Date
    , lmpDateConfident : Maybe Bool
    , chwLmpConfirmation : Maybe Bool
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


emptyPregnancyDatingForm : PregnancyDatingForm
emptyPregnancyDatingForm =
    PregnancyDatingForm Nothing Nothing Nothing Nothing Nothing


type alias ObstetricFormFirstStep =
    { currentlyPregnant : Maybe Bool
    , termPregnancy : Maybe Int
    , termPregnancyDirty : Bool
    , preTermPregnancy : Maybe Int
    , preTermPregnancyDirty : Bool
    , stillbirthsAtTerm : Maybe Int
    , stillbirthsAtTermDirty : Bool
    , stillbirthsPreTerm : Maybe Int
    , stillbirthsPreTermDirty : Bool
    , abortions : Maybe Int
    , abortionsDirty : Bool
    , liveChildren : Maybe Int
    , liveChildrenDirty : Bool
    }


emptyObstetricFormFirstStep : ObstetricFormFirstStep
emptyObstetricFormFirstStep =
    { currentlyPregnant = Nothing
    , termPregnancy = Nothing
    , termPregnancyDirty = False
    , preTermPregnancy = Nothing
    , preTermPregnancyDirty = False
    , stillbirthsAtTerm = Nothing
    , stillbirthsAtTermDirty = False
    , stillbirthsPreTerm = Nothing
    , stillbirthsPreTermDirty = False
    , abortions = Nothing
    , abortionsDirty = False
    , liveChildren = Nothing
    , liveChildrenDirty = False
    }


type alias ObstetricFormSecondStep =
    { cSections : Maybe Int
    , cSectionsDirty : Bool
    , cSectionInPreviousDelivery : Maybe Bool
    , cSectionReason : Maybe CSectionReason
    , previousDeliveryPeriod : Maybe PreviousDeliveryPeriod
    , successiveAbortions : Maybe Bool
    , successivePrematureDeliveries : Maybe Bool
    , stillbornPreviousDelivery : Maybe Bool
    , babyDiedOnDayOfBirthPreviousDelivery : Maybe Bool
    , partialPlacentaPreviousDelivery : Maybe Bool
    , severeHemorrhagingPreviousDelivery : Maybe Bool
    , preeclampsiaPreviousPregnancy : Maybe Bool
    , convulsionsPreviousDelivery : Maybe Bool
    , convulsionsAndUnconsciousPreviousDelivery : Maybe Bool
    , gestationalDiabetesPreviousPregnancy : Maybe Bool
    , incompleteCervixPreviousPregnancy : Maybe Bool
    , rhNegative : Maybe Bool
    }


emptyObstetricFormSecondStep : ObstetricFormSecondStep
emptyObstetricFormSecondStep =
    { cSections = Nothing
    , cSectionsDirty = False
    , cSectionInPreviousDelivery = Nothing
    , cSectionReason = Nothing
    , previousDeliveryPeriod = Nothing
    , successiveAbortions = Nothing
    , successivePrematureDeliveries = Nothing
    , stillbornPreviousDelivery = Nothing
    , babyDiedOnDayOfBirthPreviousDelivery = Nothing
    , partialPlacentaPreviousDelivery = Nothing
    , severeHemorrhagingPreviousDelivery = Nothing
    , preeclampsiaPreviousPregnancy = Nothing
    , convulsionsPreviousDelivery = Nothing
    , convulsionsAndUnconsciousPreviousDelivery = Nothing
    , gestationalDiabetesPreviousPregnancy = Nothing
    , incompleteCervixPreviousPregnancy = Nothing
    , rhNegative = Nothing
    }


type alias MedicalHistoryForm =
    { uterineMyoma : Maybe Bool
    , diabetes : Maybe Bool
    , cardiacDisease : Maybe Bool
    , renalDisease : Maybe Bool
    , hypertensionBeforePregnancy : Maybe Bool
    , tuberculosisPast : Maybe Bool
    , tuberculosisPresent : Maybe Bool
    , asthma : Maybe Bool
    , bowedLegs : Maybe Bool
    , hiv : Maybe Bool
    , mentalHealthHistory : Maybe Bool
    }


emptyMedicalHistoryForm : MedicalHistoryForm
emptyMedicalHistoryForm =
    { uterineMyoma = Nothing
    , diabetes = Nothing
    , cardiacDisease = Nothing
    , renalDisease = Nothing
    , hypertensionBeforePregnancy = Nothing
    , tuberculosisPast = Nothing
    , tuberculosisPresent = Nothing
    , asthma = Nothing
    , bowedLegs = Nothing
    , hiv = Nothing
    , mentalHealthHistory = Nothing
    }


type alias SocialHistoryForm =
    { accompaniedByPartner : Maybe Bool
    , partnerReceivedCounseling : Maybe Bool
    , partnerReceivedTesting : Maybe Bool
    , partnerTestingResult : Maybe SocialHistoryHivTestingResult
    }


emptySocialHistoryForm : SocialHistoryForm
emptySocialHistoryForm =
    SocialHistoryForm Nothing Nothing Nothing Nothing


type alias OutsideCareForm =
    { seenAtAnotherFacility : Maybe Bool
    , givenNewDiagnosis : Maybe Bool
    , givenMedicine : Maybe Bool
    , diagnoses : Maybe (List PrenatalDiagnosis)
    , diagnosesDirty : Bool
    , malariaMedication : Maybe PrenatalOutsideCareMedication
    , malariaMedicationDirty : Bool
    , hypertensionMedication : Maybe PrenatalOutsideCareMedication
    , hypertensionMedicationDirty : Bool
    , syphilisMedication : Maybe PrenatalOutsideCareMedication
    , syphilisMedicationDirty : Bool
    , hivMedication : Maybe Bool
    , anemiaMedication : Maybe Bool
    }


emptyOutsideCareForm : OutsideCareForm
emptyOutsideCareForm =
    { seenAtAnotherFacility = Nothing
    , givenNewDiagnosis = Nothing
    , givenMedicine = Nothing
    , diagnoses = Nothing
    , diagnosesDirty = False
    , malariaMedication = Nothing
    , malariaMedicationDirty = False
    , hypertensionMedication = Nothing
    , hypertensionMedicationDirty = False
    , syphilisMedication = Nothing
    , syphilisMedicationDirty = False
    , hivMedication = Nothing
    , anemiaMedication = Nothing
    }


encodeLmpRange : LmpRange -> String
encodeLmpRange range =
    case range of
        OneMonth ->
            "one-month"

        ThreeMonth ->
            "three-month"

        SixMonth ->
            "six-month"


decodeLmpRange : String -> Maybe LmpRange
decodeLmpRange s =
    case s of
        "one-month" ->
            Just OneMonth

        "three-month" ->
            Just ThreeMonth

        "six-month" ->
            Just SixMonth

        _ ->
            Nothing


type alias NutritionAssessmentForm =
    { height : Maybe Float
    , heightDirty : Bool
    , weight : Maybe Float
    , weightDirty : Bool
    , muac : Maybe Float
    , muacDirty : Bool
    }


emptyNutritionAssessmentForm : NutritionAssessmentForm
emptyNutritionAssessmentForm =
    { height = Nothing
    , heightDirty = False
    , weight = Nothing
    , weightDirty = False
    , muac = Nothing
    , muacDirty = False
    }


type alias CorePhysicalExamForm =
    { brittleHair : Maybe Bool
    , paleConjuctiva : Maybe Bool
    , neck : Maybe (List NeckCPESign)
    , heart : Maybe HeartCPESign
    , heartMurmur : Maybe Bool
    , lungs : Maybe (List LungsCPESign)
    , abdomen : Maybe (List AbdomenCPESign)
    , hands : Maybe (List HandsCPESign)
    , legs : Maybe (List LegsCPESign)
    }


emptyCorePhysicalExamForm : CorePhysicalExamForm
emptyCorePhysicalExamForm =
    { brittleHair = Nothing
    , paleConjuctiva = Nothing
    , neck = Nothing
    , heart = Nothing
    , heartMurmur = Nothing
    , lungs = Nothing
    , abdomen = Nothing
    , hands = Nothing
    , legs = Nothing
    }


type alias ObstetricalExamForm =
    { fundalHeight : Maybe Float
    , fundalHeightDirty : Bool
    , fetalPresentation : Maybe FetalPresentation
    , fetalMovement : Maybe Bool
    , fetalHeartRate : Maybe Int
    , fetalHeartRateDirty : Bool
    , cSectionScar : Maybe CSectionScar
    }


emptyObstetricalExamForm : ObstetricalExamForm
emptyObstetricalExamForm =
    { fundalHeight = Nothing
    , fundalHeightDirty = False
    , fetalPresentation = Nothing
    , fetalMovement = Nothing
    , fetalHeartRate = Nothing
    , fetalHeartRateDirty = False
    , cSectionScar = Nothing
    }


type alias BreastExamForm =
    -- Should be EverySet, since you can have more than one sign.
    { breast : Maybe (List BreastExamSign)
    , selfGuidance : Maybe Bool
    }


emptyBreastExamForm : BreastExamForm
emptyBreastExamForm =
    BreastExamForm Nothing Nothing


type alias FamilyPlanningForm =
    { signs : Maybe (List FamilyPlanningSign)
    }


emptyFamilyPlanningForm : FamilyPlanningForm
emptyFamilyPlanningForm =
    FamilyPlanningForm Nothing


type alias DangerSignsForm =
    { signs : Maybe (List DangerSign)
    , postpartumMother : Maybe (List PostpartumMotherDangerSign)
    , postpartumChild : Maybe (List PostpartumChildDangerSign)
    }


emptyDangerSignsForm : DangerSignsForm
emptyDangerSignsForm =
    DangerSignsForm Nothing Nothing Nothing


type alias BirthPlanForm =
    { haveInsurance : Maybe Bool
    , boughtClothes : Maybe Bool
    , caregiverAccompany : Maybe Bool
    , savedMoney : Maybe Bool
    , haveTransportation : Maybe Bool
    , familyPlanning : Maybe (List FamilyPlanningSign)
    }


emptyBirthPlanForm : BirthPlanForm
emptyBirthPlanForm =
    { haveInsurance = Nothing
    , boughtClothes = Nothing
    , caregiverAccompany = Nothing
    , savedMoney = Nothing
    , haveTransportation = Nothing
    , familyPlanning = Nothing
    }


type alias PregnancyTestForm =
    { pregnancyTestResult : Maybe PregnancyTestResult
    }


type alias PrenatalMalariaTestForm =
    { testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , testPerformedToday : Maybe Bool
    , testPerformedTodayDirty : Bool
    , executionNote : Maybe PrenatalTestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool
    , testResult : Maybe PrenatalTestResult
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


emptyPrenatalMalariaTestForm : PrenatalMalariaTestForm
emptyPrenatalMalariaTestForm =
    PrenatalMalariaTestForm Nothing False Nothing False Nothing False Nothing False Nothing Nothing


type alias PrenatalLabsNonRDTForm =
    { knownAsPositive : Maybe Bool
    , testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , testPerformedToday : Maybe Bool
    , testPerformedTodayDirty : Bool
    , executionNote : Maybe PrenatalTestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


emptyPrenatalLabsNonRDTForm : PrenatalLabsNonRDTForm
emptyPrenatalLabsNonRDTForm =
    PrenatalLabsNonRDTForm Nothing Nothing False Nothing False Nothing False Nothing False Nothing


type alias PrenatalUrineDipstickForm =
    { testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , testPerformedToday : Maybe Bool
    , testPerformedTodayDirty : Bool
    , testVariant : Maybe PrenatalTestVariant
    , executionNote : Maybe PrenatalTestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


emptyPrenatalUrineDipstickForm : PrenatalUrineDipstickForm
emptyPrenatalUrineDipstickForm =
    PrenatalUrineDipstickForm Nothing False Nothing False Nothing Nothing False Nothing False Nothing


type alias PrenatalHIVTestForm =
    { knownAsPositive : Maybe Bool
    , testPerformed : Maybe Bool
    , testPerformedDirty : Bool
    , testPerformedToday : Maybe Bool
    , testPerformedTodayDirty : Bool
    , executionNote : Maybe PrenatalTestExecutionNote
    , executionNoteDirty : Bool
    , executionDate : Maybe NominalDate
    , executionDateDirty : Bool
    , testResult : Maybe PrenatalTestResult
    , hivProgramHC : Maybe Bool
    , hivProgramHCDirty : Bool
    , partnerHIVPositive : Maybe Bool
    , partnerHIVPositiveDirty : Bool
    , partnerTakingARV : Maybe Bool
    , partnerTakingARVDirty : Bool
    , partnerSurpressedViralLoad : Maybe Bool
    , partnerSurpressedViralLoadDirty : Bool
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


emptyPrenatalHIVTestForm : PrenatalHIVTestForm
emptyPrenatalHIVTestForm =
    { knownAsPositive = Nothing
    , testPerformed = Nothing
    , testPerformedDirty = False
    , testPerformedToday = Nothing
    , testPerformedTodayDirty = False
    , executionNote = Nothing
    , executionNoteDirty = False
    , executionDate = Nothing
    , executionDateDirty = False
    , testResult = Nothing
    , hivProgramHC = Nothing
    , hivProgramHCDirty = False
    , partnerHIVPositive = Nothing
    , partnerHIVPositiveDirty = False
    , partnerTakingARV = Nothing
    , partnerTakingARVDirty = False
    , partnerSurpressedViralLoad = Nothing
    , partnerSurpressedViralLoadDirty = False
    , dateSelectorPopupState = Nothing
    }


type alias AppointmentConfirmationForm =
    { appointmentDate : Maybe Date
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


emptyAppointmentConfirmationForm : AppointmentConfirmationForm
emptyAppointmentConfirmationForm =
    AppointmentConfirmationForm Nothing Nothing


type alias FollowUpForm =
    { option : Maybe FollowUpOption

    -- We do not display this. Using it when saving.
    , assesment : Maybe PrenatalAssesment
    , resolutionDate : Maybe NominalDate
    }


emptyFollowUpForm : FollowUpForm
emptyFollowUpForm =
    FollowUpForm Nothing Nothing Nothing


type alias HealthEducationForm =
    { expectations : Maybe Bool
    , visitsReview : Maybe Bool
    , warningSigns : Maybe Bool
    , hemorrhaging : Maybe Bool
    , familyPlanning : Maybe Bool
    , breastfeeding : Maybe Bool
    , immunization : Maybe Bool
    , hygiene : Maybe Bool
    , positiveHIV : Maybe Bool
    , saferSexHIV : Maybe Bool
    , partnerTesting : Maybe Bool
    , nauseaVomiting : Maybe Bool
    , legCramps : Maybe Bool
    , lowBackPain : Maybe Bool
    , constipation : Maybe Bool
    , heartburn : Maybe Bool
    , varicoseVeins : Maybe Bool
    , legPainRedness : Maybe Bool
    , pelvicPain : Maybe Bool
    , saferSex : Maybe Bool
    }


emptyHealthEducationForm : HealthEducationForm
emptyHealthEducationForm =
    { expectations = Nothing
    , visitsReview = Nothing
    , warningSigns = Nothing
    , hemorrhaging = Nothing
    , familyPlanning = Nothing
    , breastfeeding = Nothing
    , immunization = Nothing
    , hygiene = Nothing
    , positiveHIV = Nothing
    , saferSexHIV = Nothing
    , partnerTesting = Nothing
    , nauseaVomiting = Nothing
    , legCramps = Nothing
    , lowBackPain = Nothing
    , constipation = Nothing
    , heartburn = Nothing
    , varicoseVeins = Nothing
    , legPainRedness = Nothing
    , pelvicPain = Nothing
    , saferSex = Nothing
    }


type alias NewbornEnrolmentForm =
    {}


emptyNewbornEnrolmentForm : NewbornEnrolmentForm
emptyNewbornEnrolmentForm =
    {}
