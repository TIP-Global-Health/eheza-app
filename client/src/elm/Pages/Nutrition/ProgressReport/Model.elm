module Pages.Nutrition.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Pages.Page exposing (Page)
import Pages.Report.Model exposing (DiagnosisMode(..), ReportTab(..))


type alias Model =
    { diagnosisMode : DiagnosisMode
    , showEndEncounterDialog : Bool
    , reportTab : ReportTab
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , showEndEncounterDialog = False
    , reportTab = TabSPVReport
    }


type Msg
    = NoOp
    | CloseEncounter NutritionEncounterId
    | SetActivePage Page
    | SetEndEncounterDialogState Bool
    | SetDiagnosisMode DiagnosisMode
    | SetReportTab ReportTab
