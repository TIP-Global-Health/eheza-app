module Pages.PrenatalLabResults.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Date exposing (Date)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)
import Pages.PrenatalActivity.Model exposing (LaboratoryTask)


type Msg
    = SetActivePage Page
    | SetActiveTask LaboratoryTask
    | SetHepatitisBTestResult String
    | SetSyphilisTestResult String


type alias Model =
    { -- , bloodGpRsTestForm : PrenatalLabsNonRDTForm
      -- , hemoglobinTestForm : PrenatalLabsNonRDTForm
      hepatitisBTestForm : PrenatalTestResultForm

    -- , randomBloodSugarTestForm : PrenatalLabsNonRDTForm
    , syphilisTestForm : PrenatalTestResultForm

    -- , urineDipstickTestForm : PrenatalUrineDipstickForm
    , activeTask : Maybe LaboratoryTask
    }


emptyModel : Model
emptyModel =
    { -- , bloodGpRsTestForm = emptyPrenatalLabsNonRDTForm
      -- , hemoglobinTestForm = emptyPrenatalLabsNonRDTForm
      hepatitisBTestForm = emptyPrenatalTestResultForm

    -- , randomBloodSugarTestForm = emptyPrenatalLabsNonRDTForm
    , syphilisTestForm = emptyPrenatalTestResultForm

    -- , urineDipstickTestForm = emptyPrenatalUrineDipstickForm
    , activeTask = Nothing
    }


type alias PrenatalTestResultForm =
    { executionNote : Maybe PrenatalTestExecutionNote
    , executionDate : Maybe NominalDate
    , testResult : Maybe PrenatalTestResult
    }


emptyPrenatalTestResultForm : PrenatalTestResultForm
emptyPrenatalTestResultForm =
    PrenatalTestResultForm Nothing Nothing Nothing
