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


type alias Model =
    { -- pregnancyTestForm : PregnancyTestForm
      -- , bloodGpRsTestForm : PrenatalLabsNonRDTForm
      -- , hemoglobinTestForm : PrenatalLabsNonRDTForm
      -- , hepatitisBTestForm : PrenatalLabsNonRDTForm
      -- , randomBloodSugarTestForm : PrenatalLabsNonRDTForm
      -- , syphilisTestForm : PrenatalLabsNonRDTForm
      -- , urineDipstickTestForm : PrenatalUrineDipstickForm
      activeTask : Maybe LaboratoryTask
    }


emptyModel : Model
emptyModel =
    { --  pregnancyTestForm = PregnancyTestForm Nothing
      -- , bloodGpRsTestForm = emptyPrenatalLabsNonRDTForm
      -- , hemoglobinTestForm = emptyPrenatalLabsNonRDTForm
      -- , hepatitisBTestForm = emptyPrenatalLabsNonRDTForm
      --
      -- , randomBloodSugarTestForm = emptyPrenatalLabsNonRDTForm
      -- , syphilisTestForm = emptyPrenatalLabsNonRDTForm
      -- , urineDipstickTestForm = emptyPrenatalUrineDipstickForm
      activeTask = Nothing
    }
