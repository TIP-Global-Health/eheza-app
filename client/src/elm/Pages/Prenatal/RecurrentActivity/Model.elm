module Pages.Prenatal.RecurrentActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Date exposing (Date)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (SendToHCForm, VitalsForm, emptySendToHCForm, emptyVitalsForm)
import Pages.Page exposing (Page)
import Pages.Prenatal.Activity.Types exposing (LaboratoryTask)
import Pages.Prenatal.Model exposing (..)
import Pages.Prenatal.RecurrentActivity.Types exposing (..)


type Msg
    = NoOp
    | SetActivePage Page
    | SetAlertsDialogState Bool
    | SetWarningPopupState (Maybe ( String, String ))
    | ViewWarningPopupForNonUrgentDiagnoses
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
      -- NextStepsMsgs
    | SetActiveNextStepsTask NextStepsTask
    | SetReferToHealthCenter Bool
    | SetHandReferralForm Bool
    | SetReasonForNonReferral ReasonForNonReferral
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
    , nextStepsData : NextStepsData
    , showAlertsDialog : Bool
    , warningPopupState : Maybe ( String, String )
    }


emptyModel : Model
emptyModel =
    { examinationData = emptyExaminationData
    , labResultsData = emptyLabResultsData
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
    { bloodGpRsTestForm : PrenatalBloodGpRsResultForm
    , hemoglobinTestForm : PrenatalHemoglobinResultForm
    , hepatitisBTestForm : HepatitisBResultForm
    , randomBloodSugarTestForm : PrenatalRandomBloodSugarResultForm
    , syphilisTestForm : SyphilisResultForm
    , urineDipstickTestForm : PrenatalUrineDipstickResultForm
    , hivPCRTestForm : PrenatalHIVPCRResultForm
    , activeTask : Maybe LaboratoryTask
    }


emptyLabResultsData : LabResultsData
emptyLabResultsData =
    { bloodGpRsTestForm = emptyPrenatalBloodGpRsResultForm
    , hemoglobinTestForm = emptyPrenatalHemoglobinResultForm
    , hepatitisBTestForm = emptyHepatitisBResultForm
    , randomBloodSugarTestForm = emptyPrenatalRandomBloodSugarResultForm
    , syphilisTestForm = emptySyphilisResultForm
    , urineDipstickTestForm = emptyPrenatalUrineDipstickResultForm
    , hivPCRTestForm = emptyPrenatalHIVPCRResultForm
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


type alias SyphilisResultForm =
    { executionNote : Maybe PrenatalTestExecutionNote
    , executionDate : Maybe NominalDate
    , testResult : Maybe PrenatalTestResult
    , symptoms : Maybe (List IllnessSymptom)
    , symptomsDirty : Bool
    , originatingEncounter : Maybe PrenatalEncounterId
    }


emptySyphilisResultForm : SyphilisResultForm
emptySyphilisResultForm =
    SyphilisResultForm Nothing Nothing Nothing Nothing False Nothing


type alias HepatitisBResultForm =
    { executionNote : Maybe PrenatalTestExecutionNote
    , executionDate : Maybe NominalDate
    , testResult : Maybe PrenatalTestResult
    , originatingEncounter : Maybe PrenatalEncounterId
    }


emptyHepatitisBResultForm : HepatitisBResultForm
emptyHepatitisBResultForm =
    HepatitisBResultForm Nothing Nothing Nothing Nothing


type alias PrenatalBloodGpRsResultForm =
    { executionNote : Maybe PrenatalTestExecutionNote
    , executionDate : Maybe NominalDate
    , bloodGroup : Maybe BloodGroup
    , rhesus : Maybe Rhesus
    , originatingEncounter : Maybe PrenatalEncounterId
    }


emptyPrenatalBloodGpRsResultForm : PrenatalBloodGpRsResultForm
emptyPrenatalBloodGpRsResultForm =
    PrenatalBloodGpRsResultForm Nothing Nothing Nothing Nothing Nothing


type alias PrenatalUrineDipstickResultForm =
    { testVariant : Maybe PrenatalTestVariant
    , executionNote : Maybe PrenatalTestExecutionNote
    , executionDate : Maybe NominalDate
    , protein : Maybe ProteinValue
    , ph : Maybe PHValue
    , glucose : Maybe GlucoseValue
    , leukocytes : Maybe LeukocytesValue
    , nitrite : Maybe NitriteValue
    , urobilinogen : Maybe UrobilinogenValue
    , haemoglobin : Maybe HaemoglobinValue
    , ketone : Maybe KetoneValue
    , bilirubin : Maybe BilirubinValue
    }


emptyPrenatalUrineDipstickResultForm : PrenatalUrineDipstickResultForm
emptyPrenatalUrineDipstickResultForm =
    { testVariant = Nothing
    , executionNote = Nothing
    , executionDate = Nothing
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


type alias PrenatalHemoglobinResultForm =
    { executionNote : Maybe PrenatalTestExecutionNote
    , executionDate : Maybe NominalDate
    , hemoglobinCount : Maybe Float
    }


emptyPrenatalHemoglobinResultForm : PrenatalHemoglobinResultForm
emptyPrenatalHemoglobinResultForm =
    PrenatalHemoglobinResultForm Nothing Nothing Nothing


type alias PrenatalRandomBloodSugarResultForm =
    { executionNote : Maybe PrenatalTestExecutionNote
    , executionDate : Maybe NominalDate
    , testPrerequisites : Maybe (EverySet TestPrerequisite)
    , sugarCount : Maybe Float
    , originatingEncounter : Maybe PrenatalEncounterId
    }


emptyPrenatalRandomBloodSugarResultForm : PrenatalRandomBloodSugarResultForm
emptyPrenatalRandomBloodSugarResultForm =
    PrenatalRandomBloodSugarResultForm Nothing Nothing Nothing Nothing Nothing


type alias PrenatalHIVPCRResultForm =
    { executionNote : Maybe PrenatalTestExecutionNote
    , executionDate : Maybe NominalDate
    , hivViralLoadStatus : Maybe ViralLoadStatus
    , hivViralLoad : Maybe Float
    }


emptyPrenatalHIVPCRResultForm : PrenatalHIVPCRResultForm
emptyPrenatalHIVPCRResultForm =
    PrenatalHIVPCRResultForm Nothing Nothing Nothing Nothing
