module Pages.ChildScoreboard.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Components.ReportToWhatsAppDialog.Model
import EverySet exposing (EverySet)
import Pages.ChildScoreboard.Encounter.Model exposing (AssembledData)
import Pages.Page exposing (Page)
import Pages.Report.Model exposing (DiagnosisMode(..), ReportTab(..))


type alias Model =
    { diagnosisMode : DiagnosisMode
    , showAIEncounterPopup : Bool
    , reportToWhatsAppDialog : Components.ReportToWhatsAppDialog.Model.Model
    , components : Maybe (EverySet Components.ReportToWhatsAppDialog.Model.ReportComponentWellChild)
    , reportTab : ReportTab
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , showAIEncounterPopup = False
    , reportToWhatsAppDialog = Components.ReportToWhatsAppDialog.Model.emptyModel
    , components = Nothing
    , reportTab = TabSPVReport
    }


type Msg
    = NoOp
    | CloseEncounter ChildScoreboardEncounterId
    | SetActivePage Page
    | ShowAIEncounterPopup
    | TriggerAcuteIllnessEncounter AssembledData
    | SetDiagnosisMode DiagnosisMode
    | MsgReportToWhatsAppDialog (Components.ReportToWhatsAppDialog.Model.Msg Msg)
    | SetReportComponents (Maybe Components.ReportToWhatsAppDialog.Model.ReportComponentsList)
    | SetReportTab ReportTab
