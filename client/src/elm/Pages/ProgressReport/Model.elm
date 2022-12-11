module Pages.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Pages.Page exposing (Page)
import Pages.Report.Model exposing (DiagnosisMode(..), ReportTab(..))


type alias Model =
    { diagnosisMode : DiagnosisMode
    , reportTab : ReportTab
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , reportTab = TabSPVReport
    }


type Msg
    = SetActivePage Page
    | SetDiagnosisMode DiagnosisMode
    | SetReportTab ReportTab
