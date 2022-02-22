module Pages.PrenatalRecurrentActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Date exposing (Date)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (SendToHCForm, emptySendToHCForm)
import Pages.Page exposing (Page)
import Pages.PrenatalActivity.Types exposing (LaboratoryTask)
import Pages.PrenatalRecurrentActivity.Types exposing (NextStepsTask(..))


type Msg
    = SetActivePage Page
    | SetAlertsDialogState Bool
      -- Lab Results msgs
    | SetActiveLabResultsTask LaboratoryTask
    | SetSyphilisTestResult String
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
    | SetSpecificGravity String
    | SetKetone String
    | SetBilirubin String
    | SaveUrineDipstickResult PersonId (Maybe ( PrenatalUrineDipstickTestId, PrenatalUrineDipstickTest )) (Maybe LaboratoryTask)
    | SetHemoglobin String
    | SaveHemoglobinResult PersonId (Maybe ( PrenatalHemoglobinTestId, PrenatalHemoglobinTest )) (Maybe LaboratoryTask)
    | SetRandomBloodSugar String
    | SaveRandomBloodSugarResult PersonId (Maybe ( PrenatalRandomBloodSugarTestId, PrenatalRandomBloodSugarTest )) (Maybe LaboratoryTask)
      -- NextStepsMsgs
    | SetActiveNextStepsTask NextStepsTask
    | SetReferToHealthCenter Bool
    | SetHandReferralForm Bool
    | SetReasonForNotSendingToHC ReasonForNotSendingToHC
    | SaveMedicationDistribution PersonId (Maybe ( PrenatalMedicationDistributionId, PrenatalMedicationDistribution )) (Maybe NextStepsTask)
    | SaveSendToHC PersonId (Maybe ( PrenatalSendToHcId, PrenatalSendToHC )) (Maybe NextStepsTask)


type alias Model =
    { labResultsData : LabResultsData
    , nextStepsData : NextStepsData
    , showAlertsDialog : Bool
    }


emptyModel : Model
emptyModel =
    { labResultsData = emptyLabResultsData
    , nextStepsData = emptyNextStepsData
    , showAlertsDialog = False
    }


type alias LabResultsData =
    { bloodGpRsTestForm : PrenatalBloodGpRsResultForm
    , hemoglobinTestForm : PrenatalHemoglobinResultForm
    , hepatitisBTestForm : PrenatalTestResultForm
    , randomBloodSugarTestForm : PrenatalRandomBloodSugarResultForm
    , syphilisTestForm : PrenatalTestResultForm
    , urineDipstickTestForm : PrenatalUrineDipstickResultForm
    , activeTask : Maybe LaboratoryTask
    }


emptyLabResultsData : LabResultsData
emptyLabResultsData =
    { bloodGpRsTestForm = emptyPrenatalBloodGpRsResultForm
    , hemoglobinTestForm = emptyPrenatalHemoglobinResultForm
    , hepatitisBTestForm = emptyPrenatalTestResultForm
    , randomBloodSugarTestForm = emptyPrenatalRandomBloodSugarResultForm
    , syphilisTestForm = emptyPrenatalTestResultForm
    , urineDipstickTestForm = emptyPrenatalUrineDipstickResultForm
    , activeTask = Nothing
    }


type alias NextStepsData =
    { sendToHCForm : SendToHCForm
    , medicationDistributionForm : MedicationDistributionForm
    , activeTask : Maybe NextStepsTask
    }


emptyNextStepsData : NextStepsData
emptyNextStepsData =
    { sendToHCForm = emptySendToHCForm
    , medicationDistributionForm = emptyMedicationDistributionForm
    , activeTask = Nothing
    }


type alias MedicationDistributionForm =
    {}


emptyMedicationDistributionForm : MedicationDistributionForm
emptyMedicationDistributionForm =
    MedicationDistributionForm


type alias PrenatalTestResultForm =
    { executionNote : Maybe PrenatalTestExecutionNote
    , executionDate : Maybe NominalDate
    , testResult : Maybe PrenatalTestResult
    }


emptyPrenatalTestResultForm : PrenatalTestResultForm
emptyPrenatalTestResultForm =
    PrenatalTestResultForm Nothing Nothing Nothing


type alias PrenatalBloodGpRsResultForm =
    { executionNote : Maybe PrenatalTestExecutionNote
    , executionDate : Maybe NominalDate
    , bloodGroup : Maybe BloodGroup
    , rhesus : Maybe Rhesus
    }


emptyPrenatalBloodGpRsResultForm : PrenatalBloodGpRsResultForm
emptyPrenatalBloodGpRsResultForm =
    PrenatalBloodGpRsResultForm Nothing Nothing Nothing Nothing


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
    , specificGravity : Maybe SpecificGravityValue
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
    , specificGravity = Nothing
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
    , sugarCount : Maybe Float
    }


emptyPrenatalRandomBloodSugarResultForm : PrenatalRandomBloodSugarResultForm
emptyPrenatalRandomBloodSugarResultForm =
    PrenatalRandomBloodSugarResultForm Nothing Nothing Nothing
