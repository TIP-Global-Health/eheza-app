module Pages.Prenatal.RecurrentActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Date exposing (Date)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model
    exposing
        ( BloodGpRsResultForm
        , HIVPCRResultForm
        , HemoglobinResultForm
        , HepatitisBResultForm
        , LaboratoryTask
        , RandomBloodSugarResultForm
        , SendToHCForm
        , SyphilisResultForm
        , UrineDipstickResultForm
        , VitalsForm
        , emptyBloodGpRsResultForm
        , emptyHIVPCRResultForm
        , emptyHemoglobinResultForm
        , emptyHepatitisBResultForm
        , emptyRandomBloodSugarResultForm
        , emptySendToHCForm
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
    | SetSyphilisTestResult String
    | SetIllnessSymptom IllnessSymptom
    | SaveSyphilisResult PersonId (Maybe ( PrenatalSyphilisTestId, PrenatalSyphilisTest )) (Maybe LaboratoryTask)
    | SetHepatitisBTestResult String
    | SaveHepatitisBResult PersonId (Maybe ( PrenatalHepatitisBTestId, PrenatalHepatitisBTest )) (Maybe LaboratoryTask)
    | SetBloodGroup String
    | SetRhesus String
    | SaveBloodGpRsResult PersonId (Maybe ( PrenatalBloodGpRsTestId, PrenatalBloodGpRsTest )) (Maybe LaboratoryTask)
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
    | SetHemoglobin String
    | SaveHemoglobinResult PersonId (Maybe ( PrenatalHemoglobinTestId, PrenatalHemoglobinTest )) (Maybe LaboratoryTask)
    | SetRandomBloodSugar String
    | SaveRandomBloodSugarResult PersonId (Maybe ( PrenatalRandomBloodSugarTestId, PrenatalRandomBloodSugarTest )) (Maybe LaboratoryTask)
    | SetHIVViralLoadUndetectable Bool
    | SetHIVViralLoad String
    | SaveHIVPCRResult PersonId (Maybe ( PrenatalHIVPCRTestId, PrenatalHIVPCRTest )) (Maybe LaboratoryTask)
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
    | CloseLabsResultsEntry PersonId PrenatalLabsResultsId LabsResultsValue


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
