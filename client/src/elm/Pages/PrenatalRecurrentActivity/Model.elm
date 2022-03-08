module Pages.PrenatalRecurrentActivity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Date exposing (Date)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)
import Pages.PrenatalActivity.Types exposing (LaboratoryTask)


type Msg
    = SetActivePage Page
    | SetAlertsDialogState Bool
    | SetActiveTask LaboratoryTask
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


type alias Model =
    { bloodGpRsTestForm : PrenatalBloodGpRsResultForm
    , hemoglobinTestForm : PrenatalHemoglobinResultForm
    , hepatitisBTestForm : PrenatalTestResultForm
    , randomBloodSugarTestForm : PrenatalRandomBloodSugarResultForm
    , syphilisTestForm : PrenatalTestResultForm
    , urineDipstickTestForm : PrenatalUrineDipstickResultForm
    , activeTask : Maybe LaboratoryTask
    , showAlertsDialog : Bool
    }


emptyModel : Model
emptyModel =
    { bloodGpRsTestForm = emptyPrenatalBloodGpRsResultForm
    , hemoglobinTestForm = emptyPrenatalHemoglobinResultForm
    , hepatitisBTestForm = emptyPrenatalTestResultForm
    , randomBloodSugarTestForm = emptyPrenatalRandomBloodSugarResultForm
    , syphilisTestForm = emptyPrenatalTestResultForm
    , urineDipstickTestForm = emptyPrenatalUrineDipstickResultForm
    , activeTask = Nothing
    , showAlertsDialog = False
    }


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
