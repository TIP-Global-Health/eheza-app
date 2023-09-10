module Pages.ProgressReport.Model exposing (Model, Msg(..), emptyModel)

import Components.SendViaWhatsAppDialog.Model
import EverySet exposing (EverySet)
import Pages.Page exposing (Page)
import Pages.Report.Model exposing (DiagnosisMode(..), ReportTab(..))


type alias Model =
    { diagnosisMode : DiagnosisMode
    , sendViaWhatsAppDialog : Components.SendViaWhatsAppDialog.Model.Model
    , components : Maybe (EverySet Components.SendViaWhatsAppDialog.Model.ReportComponentWellChild)
    , reportTab : ReportTab
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , sendViaWhatsAppDialog = Components.SendViaWhatsAppDialog.Model.emptyModel
    , components = Nothing
    , reportTab = TabSPVReport
    }


type Msg
    = SetActivePage Page
    | SetDiagnosisMode DiagnosisMode
    | MsgSendViaWhatsAppDialog (Components.SendViaWhatsAppDialog.Model.Msg Msg)
    | SetReportComponents (Maybe Components.SendViaWhatsAppDialog.Model.ReportComponentsList)
    | SetReportTab ReportTab
