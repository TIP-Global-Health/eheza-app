module Pages.Prenatal.Activity.Model exposing (..)

import AssocList exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis)
import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model
    exposing
        ( BloodGpRsTestForm
        , CorePhysicalExamForm
        , DropZoneFile
        , FamilyPlanningForm
        , HIVPCRTestForm
        , HIVTestUniversalForm
        , HemoglobinTestForm
        , HepatitisBTestForm
        , LaboratoryTask
        , MalariaTestForm
        , MedicationAdministrationForm
        , OutsideCareForm
        , OutsideCareStep
        , PartnerHIVTestForm
        , RandomBloodSugarTestUniversalForm
        , SyphilisTestForm
        , UrineDipstickTestUniversalForm
        , VaccinationForm
        , VaccinationFormViewMode
        , VitalsForm
        , emptyBloodGpRsTestForm
        , emptyCorePhysicalExamForm
        , emptyFamilyPlanningForm
        , emptyHIVPCRTestForm
        , emptyHIVTestUniversalForm
        , emptyHemoglobinTestForm
        , emptyHepatitisBTestForm
        , emptyMalariaTestForm
        , emptyMedicationAdministrationForm
        , emptyOutsideCareForm
        , emptyPartnerHIVTestForm
        , emptyRandomBloodSugarTestUniversalForm
        , emptySyphilisTestForm
        , emptyUrineDipstickTestUniversalForm
        , emptyVaccinationForm
        , emptyVitalsForm
        )
import Pages.Page exposing (Page)
import Pages.Prenatal.Activity.Types exposing (..)
import Pages.Prenatal.Model exposing (..)


type Msg
    = NoOp
    | DropZoneComplete DropZoneFile
    | SetActivePage Page
    | SetAlertsDialogState Bool
    | SetWarningPopupState (Maybe (WarningPopupType Msg))
      -- PregnancyDatingMsgs
    | SetLmpDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SetConfirmLmpDate LastMenstrualPeriodValue Bool
    | SetLmpDate Date
    | SetLmpDateConfident Bool
    | SetLmpDateNotConfidentReason LmpDateNotConfidentReason
    | SetPrePregnancyWeight String
    | SavePregnancyDating IndividualEncounterParticipantId PersonId (Maybe ( LastMenstrualPeriodId, LastMenstrualPeriod ))
      -- HistoryMsgs
    | SetActiveHistoryTask HistoryTask
      -- HistoryMsgs, OB, Step 1
    | SetCurrentlyPregnant Bool
    | SetOBIntInput (Maybe Int -> ObstetricFormFirstStep -> ObstetricFormFirstStep) String
    | SaveOBHistoryStep1 Bool PersonId (Maybe ( ObstetricHistoryId, ObstetricHistory )) (Maybe HistoryTask)
      -- HistoryMsgs, OB, Step 2
    | SetCSectionReason CSectionReason
    | SetOBBoolInput (Bool -> ObstetricFormSecondStep -> ObstetricFormSecondStep) Bool
    | SetPreviousDeliveryPeriod PreviousDeliveryPeriod
    | SetObstetricFormSecondStepSign ObstetricHistoryStep2Sign
    | BackToOBHistoryStep1
    | SaveOBHistoryStep2 PersonId (Maybe ( ObstetricHistoryStep2Id, ObstetricHistoryStep2 )) (Maybe HistoryTask)
      -- HistoryMsgs, Medical
    | SetMedicalHistorySigns MedicalHistorySign
    | SetMedicalHistoryPhysicalCondition MedicalHistoryPhysicalCondition
    | SetMedicalHistoryInfectiousDisease MedicalHistoryInfectiousDisease
    | SetMedicalHistoryMentalHealthIssue MedicalHistoryMentalHealthIssue
    | SaveMedicalHistory PersonId (Maybe ( MedicalHistoryId, MedicalHistory )) (Maybe HistoryTask)
      -- HistoryMsgs, Social
    | SetSocialBoolInput (Bool -> SocialHistoryForm -> SocialHistoryForm) Bool
    | SaveSocialHistory PersonId (Maybe ( SocialHistoryId, SocialHistory )) (Maybe HistoryTask)
      -- HistoryMsgs, Outside Care
    | SetOutsideCareStep OutsideCareStep
    | SetOutsideCareSignBoolInput (Bool -> OutsideCareForm PrenatalDiagnosis -> OutsideCareForm PrenatalDiagnosis) Bool
    | SetOutsideCareDiagnosis PrenatalDiagnosis
    | SetOutsideCareMalariaMedication OutsideCareMedication
    | SetOutsideCareHypertensionMedication OutsideCareMedication
    | SetOutsideCareSyphilisMedication OutsideCareMedication
    | SetOutsideCareAnemiaMedication OutsideCareMedication
    | SetOutsideCareHIVMedication OutsideCareMedication
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
    | HideFundalPalpablePopup
    | SaveObstetricalExam PersonId (Maybe ( ObstetricalExamId, ObstetricalExam )) (Maybe ExaminationTask)
      -- ExaminationMsgs, Breast Exam
    | SetBreastExamBoolInput (Bool -> BreastExamForm -> BreastExamForm) Bool
    | SetBreastExamBreast BreastExamSign
    | SetDischargeType DischargeType
    | SaveBreastExam PersonId (Maybe ( BreastExamId, BreastExam )) (Maybe ExaminationTask)
      -- ExaminationMsgs, GU Exam
    | SetGUExamBoolInput (Bool -> GUExamForm -> GUExamForm) Bool
    | SetPostpartumHealingProblem PostpartumHealingProblem
    | SetVaginalExamSign VaginalExamSign
    | SaveGUExam PersonId (Maybe ( PrenatalGUExamId, PrenatalGUExam )) (Maybe ExaminationTask)
      -- FamilyPlanningMsgs
    | SetFamilyPlanningSign FamilyPlanningSign
    | SaveFamilyPlanning PersonId (Maybe ( PrenatalFamilyPlanningId, PrenatalFamilyPlanning ))
      -- MedicationMsgs
    | SetActiveMedicationTask MedicationTask
    | SetCalciumAdministered Bool
    | SetCalciumReasonForNonAdministration AdministrationNote
    | SaveCalcium PersonId (Maybe ( PrenatalCalciumId, PrenatalCalcium )) (Maybe MedicationTask)
    | SetFolateAdministered Bool
    | SetFolateReasonForNonAdministration AdministrationNote
    | SaveFolate PersonId (Maybe ( PrenatalFolateId, PrenatalFolate )) (Maybe MedicationTask)
    | SetIronAdministered Bool
    | SetIronReasonForNonAdministration AdministrationNote
    | SaveIron PersonId (Maybe ( PrenatalIronId, PrenatalIron )) (Maybe MedicationTask)
    | SetMMSAdministered Bool
    | SetMMSReasonForNonAdministration AdministrationNote
    | SaveMMS PersonId (Maybe ( PrenatalMMSId, PrenatalMMS )) (Maybe MedicationTask)
    | SetMebendazoleAdministered Bool
    | SetMebendazoleReasonForNonAdministration AdministrationNote
    | SaveMebendazole PersonId (Maybe ( PrenatalMebendazoleId, PrenatalMebendazole )) (Maybe MedicationTask)
      -- MalariaPreventionMsgs
    | SetMalariaPreventionBoolInput (Bool -> MalariaPreventionForm -> MalariaPreventionForm) Bool
    | SaveMalariaPrevention PersonId (Maybe ( MalariaPreventionId, MalariaPrevention ))
      -- DangerSignsMsgs
    | SetDangerSign DangerSign
    | SetPostpartumMotherDangerSign PostpartumMotherDangerSign
    | SetPostpartumChildDangerSign PostpartumChildDangerSign
    | SaveDangerSigns PersonId (Maybe ( DangerSignsId, DangerSigns ))
      -- PrenatalPhotoMsgs
    | SavePrenatalPhoto PersonId (Maybe PrenatalPhotoId) ImageUrl
      -- BirthPlanMsgs
    | SetBirthPlanBoolInput (Bool -> BirthPlanForm -> BirthPlanForm) Bool
    | SetBirthPlanFamilyPlanning FamilyPlanningSign
    | SaveBirthPlan PersonId (Maybe ( BirthPlanId, BirthPlan ))
      -- LABORATORYMsgs
    | SetActiveLaboratoryTask LaboratoryTask
    | SetPregnancyTestResult String
    | SavePregnancyTest PersonId (Maybe ( PregnancyTestId, PregnancyTest ))
    | SetHIVTestFormBoolInput (Bool -> HIVTestUniversalForm -> HIVTestUniversalForm) Bool
    | SetHIVTestExecutionNote TestExecutionNote
    | SetHIVTestResult String
    | SaveHIVTest PersonId (Maybe ( PrenatalHIVTestId, PrenatalHIVTest )) (Maybe LaboratoryTask)
    | SetSyphilisTestFormBoolInput (Bool -> SyphilisTestForm -> SyphilisTestForm) Bool
    | SetSyphilisTestExecutionNote TestExecutionNote
    | SetSyphilisTestResult String
    | SetIllnessSymptom IllnessSymptom
    | SaveSyphilisTest PersonId (Maybe ( PrenatalSyphilisTestId, PrenatalSyphilisTest )) (Maybe LaboratoryTask)
    | SetHepatitisBTestFormBoolInput (Bool -> HepatitisBTestForm -> HepatitisBTestForm) Bool
    | SetHepatitisBTestExecutionNote TestExecutionNote
    | SetHepatitisBTestResult String
    | SaveHepatitisBTest PersonId (Maybe ( PrenatalHepatitisBTestId, PrenatalHepatitisBTest )) (Maybe LaboratoryTask)
    | SetMalariaTestFormBoolInput (Bool -> MalariaTestForm -> MalariaTestForm) Bool
    | SetMalariaTestExecutionNote TestExecutionNote
    | SetMalariaTestResult String
    | SetBloodSmearResult String
    | SaveMalariaTest PersonId (Maybe ( PrenatalMalariaTestId, PrenatalMalariaTest )) (Maybe LaboratoryTask)
    | SetBloodGpRsTestFormBoolInput (Bool -> BloodGpRsTestForm -> BloodGpRsTestForm) Bool
    | SetBloodGpRsTestExecutionNote TestExecutionNote
    | SetBloodGroup String
    | SetRhesus String
    | SaveBloodGpRsTest PersonId (Maybe ( PrenatalBloodGpRsTestId, PrenatalBloodGpRsTest )) (Maybe LaboratoryTask)
    | SetUrineDipstickTestFormBoolInput (Bool -> UrineDipstickTestUniversalForm -> UrineDipstickTestUniversalForm) Bool
    | SetUrineDipstickTestExecutionNote TestExecutionNote
    | SetUrineDipstickTestVariant TestVariant
    | SetProtein String
    | SetPH String
    | SetGlucose String
    | SetLeukocytes String
    | SetNitrite String
    | SetUrobilinogen String
    | SetHaemoglobin String
    | SetKetone String
    | SetBilirubin String
    | SaveUrineDipstickTest PersonId (Maybe ( PrenatalUrineDipstickTestId, PrenatalUrineDipstickTest )) (Maybe LaboratoryTask)
    | SetHemoglobinTestFormBoolInput (Bool -> HemoglobinTestForm -> HemoglobinTestForm) Bool
    | SetHemoglobinTestExecutionNote TestExecutionNote
    | SetHemoglobinCount String
    | SaveHemoglobinTest PersonId (Maybe ( PrenatalHemoglobinTestId, PrenatalHemoglobinTest )) (Maybe LaboratoryTask)
    | SetRandomBloodSugarTestFormBoolInput (Bool -> RandomBloodSugarTestUniversalForm -> RandomBloodSugarTestUniversalForm) Bool
    | SetRandomBloodSugarTestExecutionNote TestExecutionNote
    | SetRandomBloodSugarResult String
    | SaveRandomBloodSugarTest PersonId (Maybe ( PrenatalRandomBloodSugarTestId, PrenatalRandomBloodSugarTest )) (Maybe LaboratoryTask)
    | SetHIVPCRTestFormBoolInput (Bool -> HIVPCRTestForm -> HIVPCRTestForm) Bool
    | SetHIVPCRTestExecutionNote TestExecutionNote
    | SetHIVViralLoadUndetectable Bool
    | SetHIVViralLoad String
    | SaveHIVPCRTest PersonId (Maybe ( PrenatalHIVPCRTestId, PrenatalHIVPCRTest )) (Maybe LaboratoryTask)
    | SetLabsHistoryCompleted Bool
    | SaveLabsHistory
    | SetPartnerHIVTestFormBoolInput (Bool -> PartnerHIVTestForm -> PartnerHIVTestForm) Bool
    | SetPartnerHIVTestExecutionNote TestExecutionNote
    | SetPartnerHIVTestResult String
    | SavePartnerHIVTest PersonId (Maybe ( PrenatalPartnerHIVTestId, PrenatalPartnerHIVTest )) (Maybe LaboratoryTask)
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
    | SetReferralBoolInput (Bool -> ReferralForm -> ReferralForm) Bool
    | SetHealthCenterNonReferralReason ReasonForNonReferral
    | SetFacilityNonReferralReason (Maybe ReasonForNonReferral) ReferralFacility ReasonForNonReferral
    | SaveSendToHC PersonId (Maybe ( PrenatalSendToHCId, PrenatalSendToHC )) Bool (Maybe NextStepsTask)
    | SetAppointmentDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SetAppointmentConfirmation Date
    | SaveAppointmentConfirmation PersonId (Maybe ( PrenatalAppointmentConfirmationId, PrenatalAppointmentConfirmation )) Bool (Maybe NextStepsTask)
    | SetMedicationDistributionBoolInput (Bool -> MedicationDistributionForm -> MedicationDistributionForm) Bool
    | SetMedicationDistributionAdministrationNote (Maybe AdministrationNote) MedicationDistributionSign AdministrationNote
    | SetRecommendedTreatmentSign (List RecommendedTreatmentSign) RecommendedTreatmentSign
    | SetAvoidingGuidanceReason AvoidingGuidanceReason
    | SaveMedicationDistribution PersonId (Maybe ( PrenatalMedicationDistributionId, PrenatalMedicationDistribution )) Bool (Maybe NextStepsTask)
    | SaveWait PersonId (Maybe PrenatalLabsResultsId) LabsResultsValue
      -- SYMPTOMREVIEWMsgs
    | SetSymptomReviewStep SymptomReviewStep
    | SetPrenatalSymptom PrenatalSymptom
    | SetPrenatalSymptomQuestionBoolInput (Bool -> SymptomReviewForm -> SymptomReviewForm) Bool
    | SetFlankPainSign PrenatalFlankPainSign
    | SaveSymptomReview PersonId (Maybe ( PrenatalSymptomReviewId, PrenatalSymptomReview ))
      -- TREATMENTREVIEWMsgs
    | SetActiveTreatmentReviewTask TreatmentReviewTask
    | SetMedicationSubActivityBoolInput (Bool -> MedicationForm -> MedicationForm) Bool
    | SetHIVMedicationNotGivenReason HIVTreatmentSign
    | SaveMedicationSubActivity PersonId (Maybe ( MedicationId, Medication )) (Maybe TreatmentReviewTask)
      -- MENTALHEALTHMsgs
    | SetMentalHealthStep MentalHealthStep
    | SetMentalHealthOptionForQuestion PrenatalMentalHealthQuestion PrenatalMentalHealthQuestionOption
    | SetSpecialistAtHC Bool
    | SaveMentalHealth PersonId (Maybe ( PrenatalMentalHealthId, PrenatalMentalHealth ))
      -- IMMUNISATIONMsgs
    | SetActiveImmunisationTask ImmunisationTask
    | SetVaccinationFormViewMode PrenatalVaccineType VaccinationFormViewMode
    | SetUpdatePreviousVaccines PrenatalVaccineType VaccineDose Bool
    | SetWillReceiveVaccineToday PrenatalVaccineType VaccineDose Bool
    | SetAdministrationNote PrenatalVaccineType AdministrationNote
    | SetVaccinationUpdateDateSelectorState PrenatalVaccineType (Maybe (DateSelectorConfig Msg))
    | SetVaccinationUpdateDate PrenatalVaccineType NominalDate
    | SaveVaccinationUpdateDate PrenatalVaccineType VaccineDose
    | DeleteVaccinationUpdateDate PrenatalVaccineType VaccineDose NominalDate
    | SaveTetanusImmunisation PersonId (Maybe ( PrenatalTetanusImmunisationId, PrenatalTetanusImmunisation ))
      -- PostpartumTreatmentReviewMsgs
    | SetPostpartumTreatmentReviewBoolInput (Bool -> MedicationForm -> MedicationForm) Bool
    | SavePostpartumTreatmentReview PersonId (Maybe ( MedicationId, Medication ))
      -- BREASTFEEDINGMsgs
    | SetBreastfeedingBoolInput (Bool -> BreastfeedingForm -> BreastfeedingForm) Bool
    | SetReasonForNotBreastfeeding BreastfeedingSign
    | SaveBreastfeeding PersonId (Maybe ( PrenatalBreastfeedingId, PrenatalBreastfeeding ))
      --  SpecialityCareMsgs
    | SetSpecialityCareBoolInput (Bool -> SpecialityCareForm -> SpecialityCareForm) Bool
    | SaveSpecialityCare PersonId (Maybe ( PrenatalSpecialityCareId, PrenatalSpecialityCare ))


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
    , mentalHealthData : MentalHealthData
    , immunisationData : ImmunisationData
    , postpartumTreatmentReviewData : PostpartumTreatmentReviewData
    , breastfeedingData : BreastfeedingData
    , specialityCareData : SpecialityCareData
    , nextStepsData : NextStepsData
    , showAlertsDialog : Bool
    , warningPopupState : Maybe (WarningPopupType Msg)
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
    , mentalHealthData = emptyMentalHealthData
    , immunisationData = emptyImmunisationData
    , postpartumTreatmentReviewData = emptyPostpartumTreatmentReviewData
    , breastfeedingData = emptyBreastfeedingData
    , specialityCareData = emptySpecialityCareData
    , nextStepsData = emptyNextStepsData
    , showAlertsDialog = False
    , warningPopupState = Nothing
    }


type PrePregnancyClassification
    = PrePregnancyUnderWeight
    | PrePregnancyNormal
    | PrePregnancyOverweight
    | PrePregnancyObesity


type GWGClassification
    = GWGSeverelyInadequate
    | GWGInadequate
    | GWGAdequate
    | GWGExcessive



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
    , outsideCareForm : OutsideCareForm PrenatalDiagnosis
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
    , activeTask = Nothing
    }


type alias ExaminationData =
    { vitalsForm : VitalsForm
    , nutritionAssessmentForm : NutritionAssessmentForm
    , corePhysicalExamForm : CorePhysicalExamForm
    , obstetricalExamForm : ObstetricalExamForm
    , breastExamForm : BreastExamForm
    , guExamForm : GUExamForm
    , activeTask : Maybe ExaminationTask
    }


emptyExaminationData : ExaminationData
emptyExaminationData =
    { vitalsForm = emptyVitalsForm
    , nutritionAssessmentForm = emptyNutritionAssessmentForm
    , corePhysicalExamForm = emptyCorePhysicalExamForm
    , obstetricalExamForm = emptyObstetricalExamForm
    , breastExamForm = emptyBreastExamForm
    , guExamForm = emptyGUExamForm
    , activeTask = Nothing
    }


type alias FamilyPlanningData =
    { form : FamilyPlanningForm
    }


emptyFamilyPlanningData : FamilyPlanningData
emptyFamilyPlanningData =
    { form = emptyFamilyPlanningForm
    }


type alias MedicationData =
    { calciumForm : MedicationAdministrationForm
    , folateForm : MedicationAdministrationForm
    , ironForm : MedicationAdministrationForm
    , mmsForm : MedicationAdministrationForm
    , mebendazoleForm : MedicationAdministrationForm
    , activeTask : Maybe MedicationTask
    }


emptyMedicationData : MedicationData
emptyMedicationData =
    { calciumForm = emptyMedicationAdministrationForm
    , folateForm = emptyMedicationAdministrationForm
    , ironForm = emptyMedicationAdministrationForm
    , mmsForm = emptyMedicationAdministrationForm
    , mebendazoleForm = emptyMedicationAdministrationForm
    , activeTask = Nothing
    }


type alias MedicationForm =
    { receivedIronFolicAcid : Maybe Bool
    , receivedDewormingPill : Maybe Bool
    , receivedMebendazole : Maybe Bool

    -- Following 2 are for Postpartum encounter
    , receivedFolicAcid : Maybe Bool
    , receivedVitaminA : Maybe Bool
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
    , receivedFolicAcid = Nothing
    , receivedVitaminA = Nothing
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
    { url : Maybe ImageUrl }


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
    { -- Only test taken by CHW.
      pregnancyTestForm : PregnancyTestForm

    -- Tests taken by nurses.
    , bloodGpRsTestForm : BloodGpRsTestForm
    , hemoglobinTestForm : HemoglobinTestForm
    , hepatitisBTestForm : HepatitisBTestForm
    , hivPCRTestForm : HIVPCRTestForm
    , hivTestForm : HIVTestUniversalForm
    , malariaTestForm : MalariaTestForm
    , partnerHIVTestForm : PartnerHIVTestForm
    , randomBloodSugarTestForm : RandomBloodSugarTestUniversalForm
    , syphilisTestForm : SyphilisTestForm
    , urineDipstickTestForm : UrineDipstickTestUniversalForm
    , labsHistoryForm : LabsHistoryForm
    , activeTask : Maybe LaboratoryTask
    }


emptyLaboratoryData : LaboratoryData
emptyLaboratoryData =
    { pregnancyTestForm = PregnancyTestForm Nothing
    , bloodGpRsTestForm = emptyBloodGpRsTestForm
    , hemoglobinTestForm = emptyHemoglobinTestForm
    , hepatitisBTestForm = emptyHepatitisBTestForm
    , hivPCRTestForm = emptyHIVPCRTestForm
    , hivTestForm = emptyHIVTestUniversalForm
    , malariaTestForm = emptyMalariaTestForm
    , partnerHIVTestForm = emptyPartnerHIVTestForm
    , randomBloodSugarTestForm = emptyRandomBloodSugarTestUniversalForm
    , syphilisTestForm = emptySyphilisTestForm
    , urineDipstickTestForm = emptyUrineDipstickTestUniversalForm
    , labsHistoryForm = emptyLabsHistoryForm
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
    }


emptyTreatmentReviewData : TreatmentReviewData
emptyTreatmentReviewData =
    { medicationForm = emptyMedicationForm
    , activeTask = Nothing
    }


type alias MentalHealthData =
    { form : MentalHealthForm
    }


emptyMentalHealthData : MentalHealthData
emptyMentalHealthData =
    { form = emptyMentalHealthForm
    }


type alias ImmunisationData =
    { tetanusForm : PrenatalVaccinationForm
    , activeTask : Maybe ImmunisationTask
    }


type alias PrenatalVaccinationForm =
    VaccinationForm Msg


emptyImmunisationData : ImmunisationData
emptyImmunisationData =
    { tetanusForm = emptyVaccinationForm
    , activeTask = Nothing
    }


type alias SpecialityCareData =
    { form : SpecialityCareForm
    }


emptySpecialityCareData : SpecialityCareData
emptySpecialityCareData =
    { form = emptySpecialityCareForm
    }


type alias SpecialityCareForm =
    { enrolledToARVProgram : Maybe Bool
    , enrolledToNCDProgram : Maybe Bool
    }


emptySpecialityCareForm : SpecialityCareForm
emptySpecialityCareForm =
    { enrolledToARVProgram = Nothing
    , enrolledToNCDProgram = Nothing
    }


type alias NextStepsData =
    { appointmentConfirmationForm : AppointmentConfirmationForm
    , followUpForm : FollowUpForm
    , referralForm : ReferralForm
    , healthEducationForm : HealthEducationForm
    , newbornEnrolmentForm : NewbornEnrolmentForm
    , medicationDistributionForm : MedicationDistributionForm
    , activeTask : Maybe NextStepsTask
    }


emptyNextStepsData : NextStepsData
emptyNextStepsData =
    { appointmentConfirmationForm = emptyAppointmentConfirmationForm
    , followUpForm = emptyFollowUpForm
    , referralForm = emptyReferralForm
    , healthEducationForm = emptyHealthEducationForm
    , newbornEnrolmentForm = emptyNewbornEnrolmentForm
    , medicationDistributionForm = emptyMedicationDistributionForm
    , activeTask = Nothing
    }



-- FORMS


type alias PregnancyDatingForm =
    { lmpDate : Maybe Date
    , prePregnancyWeight : Maybe Float
    , lmpDateConfident : Maybe Bool
    , chwLmpConfirmation : Maybe Bool
    , lmpDateNotConfidentReason : Maybe LmpDateNotConfidentReason
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


emptyPregnancyDatingForm : PregnancyDatingForm
emptyPregnancyDatingForm =
    PregnancyDatingForm Nothing Nothing Nothing Nothing Nothing Nothing


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
    { cSectionInPast : Maybe Bool
    , cSectionInPreviousDelivery : Maybe Bool
    , cSectionInPreviousDeliveryDirty : Bool
    , cSectionReason : Maybe CSectionReason
    , cSectionReasonDirty : Bool
    , previousDeliveryPeriod : Maybe PreviousDeliveryPeriod
    , signs : Maybe (List ObstetricHistoryStep2Sign)
    }


emptyObstetricFormSecondStep : ObstetricFormSecondStep
emptyObstetricFormSecondStep =
    { cSectionInPast = Nothing
    , cSectionInPreviousDelivery = Nothing
    , cSectionInPreviousDeliveryDirty = False
    , cSectionReason = Nothing
    , cSectionReasonDirty = False
    , previousDeliveryPeriod = Nothing
    , signs = Nothing
    }


type alias MedicalHistoryForm =
    { signs : Maybe (List MedicalHistorySign)
    , physicalConditions : Maybe (List MedicalHistoryPhysicalCondition)
    , infectiousDiseases : Maybe (List MedicalHistoryInfectiousDisease)
    , mentalHealthIssues : Maybe (List MedicalHistoryMentalHealthIssue)
    }


emptyMedicalHistoryForm : MedicalHistoryForm
emptyMedicalHistoryForm =
    { signs = Nothing
    , physicalConditions = Nothing
    , infectiousDiseases = Nothing
    , mentalHealthIssues = Nothing
    }


type alias SocialHistoryForm =
    { accompaniedByPartner : Maybe Bool
    , partnerReceivedCounseling : Maybe Bool
    }


emptySocialHistoryForm : SocialHistoryForm
emptySocialHistoryForm =
    SocialHistoryForm Nothing Nothing


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


type alias ObstetricalExamForm =
    { fundalPalpable : Maybe Bool
    , fundalHeight : Maybe Float
    , fundalHeightDirty : Bool
    , fetalPresentation : Maybe FetalPresentation
    , fetalMovement : Maybe Bool
    , fetalHeartRate : Maybe Int
    , fetalHeartRateDirty : Bool
    , cSectionScar : Maybe CSectionScar
    , displayFundalPalpablePopup : Bool
    }


emptyObstetricalExamForm : ObstetricalExamForm
emptyObstetricalExamForm =
    { fundalPalpable = Nothing
    , fundalHeight = Nothing
    , fundalHeightDirty = False
    , fetalPresentation = Nothing
    , fetalMovement = Nothing
    , fetalHeartRate = Nothing
    , fetalHeartRateDirty = False
    , cSectionScar = Nothing
    , displayFundalPalpablePopup = False
    }


type alias BreastExamForm =
    { breast : Maybe (List BreastExamSign)
    , dischargeType : Maybe DischargeType
    , dischargeTypeDirty : Bool
    , selfGuidance : Maybe Bool
    }


emptyBreastExamForm : BreastExamForm
emptyBreastExamForm =
    BreastExamForm Nothing Nothing False Nothing


type alias GUExamForm =
    { vaginalExamSigns : Maybe (List VaginalExamSign)
    , episiotomyOrPerinealTear : Maybe Bool
    , healingNormally : Maybe Bool
    , healingNormallyDirty : Bool
    , postpartumHealingProblems : Maybe (List PostpartumHealingProblem)
    , postpartumHealingProblemsDirty : Bool
    , rectalHemorrhoids : Maybe Bool
    }


emptyGUExamForm : GUExamForm
emptyGUExamForm =
    { vaginalExamSigns = Nothing
    , episiotomyOrPerinealTear = Nothing
    , healingNormally = Nothing
    , healingNormallyDirty = False
    , postpartumHealingProblems = Nothing
    , postpartumHealingProblemsDirty = False
    , rectalHemorrhoids = Nothing
    }


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


type alias LabsHistoryForm =
    { completed : Maybe Bool
    }


emptyLabsHistoryForm : LabsHistoryForm
emptyLabsHistoryForm =
    LabsHistoryForm Nothing


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


type alias NewbornEnrolmentForm =
    {}


emptyNewbornEnrolmentForm : NewbornEnrolmentForm
emptyNewbornEnrolmentForm =
    {}


type alias MentalHealthForm =
    { signs : Maybe (Dict PrenatalMentalHealthQuestion PrenatalMentalHealthQuestionOption)
    , specialistAtHC : Maybe Bool
    , step : MentalHealthStep
    }


type MentalHealthStep
    = MentalHealthQuestion PrenatalMentalHealthQuestion
    | MentalHealthSpecialistQuestion


emptyMentalHealthForm : MentalHealthForm
emptyMentalHealthForm =
    { signs = Nothing
    , specialistAtHC = Nothing
    , step = MentalHealthQuestion MentalHealthQuestion1
    }


type alias PostpartumTreatmentReviewData =
    { form : MedicationForm
    }


emptyPostpartumTreatmentReviewData : PostpartumTreatmentReviewData
emptyPostpartumTreatmentReviewData =
    { form = emptyMedicationForm
    }


type alias BreastfeedingData =
    { form : BreastfeedingForm
    }


emptyBreastfeedingData : BreastfeedingData
emptyBreastfeedingData =
    { form = emptyBreastfeedingForm
    }


type alias BreastfeedingForm =
    { isBreastfeeding : Maybe Bool
    , reasonForNotBreastfeeding : Maybe BreastfeedingSign
    , reasonForNotBreastfeedingDirty : Bool
    , breastPain : Maybe Bool
    , breastPainDirty : Bool
    , breastRedness : Maybe Bool
    , breastRednessDirty : Bool
    , enoughMilk : Maybe Bool
    , enoughMilkDirty : Bool
    , latchingWell : Maybe Bool
    , latchingWellDirty : Bool
    }


emptyBreastfeedingForm : BreastfeedingForm
emptyBreastfeedingForm =
    { isBreastfeeding = Nothing
    , reasonForNotBreastfeeding = Nothing
    , reasonForNotBreastfeedingDirty = False
    , breastPain = Nothing
    , breastPainDirty = False
    , breastRedness = Nothing
    , breastRednessDirty = False
    , enoughMilk = Nothing
    , enoughMilkDirty = False
    , latchingWell = Nothing
    , latchingWellDirty = False
    }
