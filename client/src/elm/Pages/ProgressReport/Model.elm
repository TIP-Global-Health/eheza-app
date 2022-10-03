module Pages.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Pages.Page exposing (Page)
import Pages.Report.Model exposing (DiagnosisMode(..))


type alias Model =
    { diagnosisMode : DiagnosisMode
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    }


type Msg
    = SetActivePage Page
    | SetDiagnosisMode DiagnosisMode
