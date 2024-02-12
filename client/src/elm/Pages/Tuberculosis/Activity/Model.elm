module Pages.Tuberculosis.Activity.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model
import Pages.Page exposing (Page)


type alias Model =
    { diagnosticsData : DiagnosticsData
    }


emptyModel : Model
emptyModel =
    { diagnosticsData = emptyDiagnosticsData
    }


type alias DiagnosticsData =
    { form : DiagnosticsForm
    }


emptyDiagnosticsData : DiagnosticsData
emptyDiagnosticsData =
    { form = emptyDiagnosticsForm
    }


type alias DiagnosticsForm =
    { diagnosed : Maybe Bool
    , isPulmonary : Maybe Bool
    , isPulmonaryDirty : Bool
    }


emptyDiagnosticsForm : DiagnosticsForm
emptyDiagnosticsForm =
    { diagnosed = Nothing
    , isPulmonary = Nothing
    , isPulmonaryDirty = False
    }


type NextStepsTask
    = TaskReferral
    | TaskHealthEducation
    | TaskFollowUp


type Msg
    = SetActivePage Page
    | SetDiagnosticsBoolInput (Bool -> DiagnosticsForm -> DiagnosticsForm) Bool
    | SaveDiagnostics PersonId (Maybe ( TuberculosisDiagnosticsId, TuberculosisDiagnostics ))
