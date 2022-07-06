module Pages.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Components.SendViaWhatsAppDialog.Model
import Pages.Page exposing (Page)
import Pages.WellChild.ProgressReport.Model exposing (DiagnosisMode(..))


type alias Model =
    { diagnosisMode : DiagnosisMode
    , sendViaWhatsAppDialog : Components.SendViaWhatsAppDialog.Model.Model
    }


emptyModel : Model
emptyModel =
    { diagnosisMode = ModeActiveDiagnosis
    , sendViaWhatsAppDialog = Components.SendViaWhatsAppDialog.Model.emptyModel
    }


type Msg
    = SetActivePage Page
    | SetDiagnosisMode DiagnosisMode
    | MsgSendViaWhatsAppDialog Components.SendViaWhatsAppDialog.Model.Msg
