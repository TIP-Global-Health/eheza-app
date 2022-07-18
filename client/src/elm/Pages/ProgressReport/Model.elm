module Pages.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Components.SendViaWhatsAppDialog.Model
import EverySet exposing (EverySet)
import Pages.Page exposing (Page)
import Pages.WellChild.ProgressReport.Model exposing (DiagnosisMode(..))


type alias Model =
    { diagnosisMode : DiagnosisMode
    , sendViaWhatsAppDialog : Components.SendViaWhatsAppDialog.Model.Model
    , components : Maybe (EverySet Components.SendViaWhatsAppDialog.Model.ReportComponentWellChild)
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , sendViaWhatsAppDialog = Components.SendViaWhatsAppDialog.Model.emptyModel
    , components = Nothing
    }


type Msg
    = SetActivePage Page
    | SetDiagnosisMode DiagnosisMode
    | MsgSendViaWhatsAppDialog (Components.SendViaWhatsAppDialog.Model.Msg Msg)
    | SetReportComponents (Maybe Components.SendViaWhatsAppDialog.Model.ReportComponentsList)
