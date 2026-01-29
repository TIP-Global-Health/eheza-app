module Pages.Prenatal.RecurrentActivity.Model exposing (ExaminationData, LabResultsData, Model, Msg(..), NextStepsData, emptyExaminationData, emptyLabResultsData, emptyModel, emptyNextStepsData)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Measurement.Model
    exposing
        ( BloodGpRsResultForm
        , HIVPCRResultForm
        , HIVResultForm
        , HemoglobinResultForm
        , HepatitisBResultForm
        , LaboratoryTask
        , MalariaResultForm
        , PartnerHIVResultForm
        , RandomBloodSugarResultForm
        , SyphilisResultForm
        , UrineDipstickResultForm
        , VitalsForm
        , emptyBloodGpRsResultForm
        , emptyHIVPCRResultForm
        , emptyHIVResultForm
        , emptyHemoglobinResultForm
        , emptyHepatitisBResultForm
        , emptyMalariaResultForm
        , emptyPartnerHIVResultForm
        , emptyRandomBloodSugarResultForm
        , emptySyphilisResultForm
        , emptyUrineDipstickResultForm
        , emptyVitalsForm
        )
import Pages.Page exposing (Page)
import Pages.Prenatal.Activity.Types exposing (WarningPopupType)
import Pages.Prenatal.Model exposing (..)
import Pages.Prenatal.RecurrentActivity.Types exposing (..)


type Msg
    = NoOp
    | SetActivePage Page
    | SetAlertsDialogState Bool
    | SetWarningPopupState (Maybe (WarningPopupType Msg))
      -- ExaminationMsgs
    | SetVitalsFloatInput (Maybe Float -> VitalsForm -> VitalsForm) String
    | SaveVitals PersonId (Maybe ( VitalsId, Vitals ))
      -- Lab Results msgs
    | SetActiveLabResultsTask LaboratoryTask
    | SetSyphilisTestFormBoolInput (Bool -> SyphilisResultForm PrenatalEncounterId -> SyphilisResultForm PrenatalEncounterId) Bool
    | SetSyphilisTestExecutionNote TestExecutionNote
    | SetSyphilisTestResult String
    | SetIllnessSymptom IllnessSymptom
    | SaveSyphilisResult PersonId (Maybe ( PrenatalSyphilisTestId, PrenatalSyphilisTest )) (Maybe LaboratoryTask)
    | SetHepatitisBTestFormBoolInput (Bool -> HepatitisBResultForm PrenatalEncounterId -> HepatitisBResultForm PrenatalEncounterId) Bool
    | SetHepatitisBTestExecutionNote TestExecutionNote
    | SetHepatitisBTestResult String
    | SaveHepatitisBResult PersonId (Maybe ( PrenatalHepatitisBTestId, PrenatalHepatitisBTest )) (Maybe LaboratoryTask)
    | SetBloodGpRsTestFormBoolInput (Bool -> BloodGpRsResultForm PrenatalEncounterId -> BloodGpRsResultForm PrenatalEncounterId) Bool
    | SetBloodGpRsTestExecutionNote TestExecutionNote
    | SetBloodGroup String
    | SetRhesus String
    | SaveBloodGpRsResult PersonId (Maybe ( PrenatalBloodGpRsTestId, PrenatalBloodGpRsTest )) (Maybe LaboratoryTask)
    | SetUrineDipstickTestFormBoolInput (Bool -> UrineDipstickResultForm -> UrineDipstickResultForm) Bool
    | SetUrineDipstickTestExecutionNote TestExecutionNote
    | SetProtein String
    | SetPH String
    | SetGlucose String
    | SetLeukocytes String
    | SetNitrite String
    | SetUrobilinogen String
    | SetHaemoglobin String
    | SetKetone String
    | SetBilirubin String
    | SaveUrineDipstickResult PersonId (Maybe ( PrenatalUrineDipstickTestId, PrenatalUrineDipstickTest )) (Maybe LaboratoryTask)
    | SetHemoglobinTestFormBoolInput (Bool -> HemoglobinResultForm -> HemoglobinResultForm) Bool
    | SetHemoglobinTestExecutionNote TestExecutionNote
    | SetHemoglobin String
    | SaveHemoglobinResult PersonId (Maybe ( PrenatalHemoglobinTestId, PrenatalHemoglobinTest )) (Maybe LaboratoryTask)
    | SetRandomBloodSugarTestFormBoolInput (Bool -> RandomBloodSugarResultForm PrenatalEncounterId -> RandomBloodSugarResultForm PrenatalEncounterId) Bool
    | SetRandomBloodSugarTestExecutionNote TestExecutionNote
    | SetRandomBloodSugar String
    | SaveRandomBloodSugarResult PersonId (Maybe ( PrenatalRandomBloodSugarTestId, PrenatalRandomBloodSugarTest )) (Maybe LaboratoryTask)
    | SetHIVPCRTestFormBoolInput (Bool -> HIVPCRResultForm -> HIVPCRResultForm) Bool
    | SetHIVPCRTestExecutionNote TestExecutionNote
    | SetHIVViralLoadUndetectable Bool
    | SetHIVViralLoad String
    | SaveHIVPCRResult PersonId (Maybe ( PrenatalHIVPCRTestId, PrenatalHIVPCRTest )) (Maybe LaboratoryTask)
    | SetHIVTestFormBoolInput (Bool -> HIVResultForm -> HIVResultForm) Bool
    | SetHIVTestExecutionNote TestExecutionNote
    | SetHIVTestResult String
    | SaveHIVResult PersonId (Maybe ( PrenatalHIVTestId, PrenatalHIVTest )) (Maybe LaboratoryTask)
    | SetPartnerHIVTestFormBoolInput (Bool -> PartnerHIVResultForm -> PartnerHIVResultForm) Bool
    | SetPartnerHIVTestExecutionNote TestExecutionNote
    | SetPartnerHIVTestResult String
    | SavePartnerHIVResult PersonId (Maybe ( PrenatalPartnerHIVTestId, PrenatalPartnerHIVTest )) (Maybe LaboratoryTask)
    | SetMalariaTestFormBoolInput (Bool -> MalariaResultForm -> MalariaResultForm) Bool
    | SetMalariaTestExecutionNote TestExecutionNote
    | SetMalariaTestResult String
    | SetBloodSmearResult String
    | SaveMalariaResult PersonId (Maybe ( PrenatalMalariaTestId, PrenatalMalariaTest )) (Maybe LaboratoryTask)
      -- MalariaPreventionMsgs
    | SetMalariaPreventionBoolInput (Bool -> MalariaPreventionForm -> MalariaPreventionForm) Bool
    | SaveMalariaPrevention PersonId (Maybe ( MalariaPreventionId, MalariaPrevention ))
      -- NextStepsMsgs
    | SetActiveNextStepsTask NextStepsTask
    | SetReferralBoolInput (Bool -> ReferralForm -> ReferralForm) Bool
    | SetFacilityNonReferralReason (Maybe ReasonForNonReferral) ReferralFacility ReasonForNonReferral
    | SaveSendToHC PersonId (Maybe ( PrenatalSendToHCId, PrenatalSendToHC )) (Maybe NextStepsTask)
    | SetMedicationDistributionBoolInput (Bool -> MedicationDistributionForm -> MedicationDistributionForm) Bool
    | SetMedicationDistributionAdministrationNote (Maybe AdministrationNote) MedicationDistributionSign AdministrationNote
    | SetRecommendedTreatmentSign (List RecommendedTreatmentSign) RecommendedTreatmentSign
    | SaveMedicationDistribution PersonId (Maybe ( PrenatalMedicationDistributionId, PrenatalMedicationDistribution )) (Maybe NextStepsTask)
    | SetHealthEducationBoolInput (Bool -> HealthEducationForm -> HealthEducationForm) Bool
    | SaveHealthEducation PersonId (Maybe ( PrenatalHealthEducationId, PrenatalHealthEducation )) (Maybe NextStepsTask)


type alias Model =
    { examinationData : ExaminationData
    , labResultsData : LabResultsData
    , malariaPreventionData : MalariaPreventionData
    , nextStepsData : NextStepsData
    , showAlertsDialog : Bool
    , warningPopupState : Maybe (WarningPopupType Msg)
    }


emptyModel : Model
emptyModel =
    { examinationData = emptyExaminationData
    , labResultsData = emptyLabResultsData
    , malariaPreventionData = emptyMalariaPreventionData
    , nextStepsData = emptyNextStepsData
    , showAlertsDialog = False
    , warningPopupState = Nothing
    }


type alias ExaminationData =
    { vitalsForm : VitalsForm
    , activeTask : Maybe ExaminationTask
    }


emptyExaminationData : ExaminationData
emptyExaminationData =
    { vitalsForm = emptyVitalsForm
    , activeTask = Nothing
    }


type alias LabResultsData =
    { bloodGpRsTestForm : BloodGpRsResultForm PrenatalEncounterId
    , hemoglobinTestForm : HemoglobinResultForm
    , hepatitisBTestForm : HepatitisBResultForm PrenatalEncounterId
    , randomBloodSugarTestForm : RandomBloodSugarResultForm PrenatalEncounterId
    , syphilisTestForm : SyphilisResultForm PrenatalEncounterId
    , urineDipstickTestForm : UrineDipstickResultForm
    , hivPCRTestForm : HIVPCRResultForm
    , hivTestForm : HIVResultForm
    , partnerHIVTestForm : PartnerHIVResultForm
    , malariaTestForm : MalariaResultForm
    , activeTask : Maybe LaboratoryTask
    }


emptyLabResultsData : LabResultsData
emptyLabResultsData =
    { bloodGpRsTestForm = emptyBloodGpRsResultForm
    , hemoglobinTestForm = emptyHemoglobinResultForm
    , hepatitisBTestForm = emptyHepatitisBResultForm
    , randomBloodSugarTestForm = emptyRandomBloodSugarResultForm
    , syphilisTestForm = emptySyphilisResultForm
    , urineDipstickTestForm = emptyUrineDipstickResultForm
    , hivPCRTestForm = emptyHIVPCRResultForm
    , hivTestForm = emptyHIVResultForm
    , partnerHIVTestForm = emptyPartnerHIVResultForm
    , malariaTestForm = emptyMalariaResultForm
    , activeTask = Nothing
    }


type alias NextStepsData =
    { referralForm : ReferralForm
    , medicationDistributionForm : MedicationDistributionForm
    , healthEducationForm : HealthEducationForm
    , activeTask : Maybe NextStepsTask
    }


emptyNextStepsData : NextStepsData
emptyNextStepsData =
    { referralForm = emptyReferralForm
    , medicationDistributionForm = emptyMedicationDistributionForm
    , healthEducationForm = emptyHealthEducationForm
    , activeTask = Nothing
    }
