module Pages.AcuteIllness.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Components.ReportToWhatsAppDialog.Model
import Pages.Page exposing (Page)


type alias Model =
    { showEndEncounterDialog : Bool
    , reportToWhatsAppDialog : Components.ReportToWhatsAppDialog.Model.Model
    }


emptyModel : Model
emptyModel =
    { showEndEncounterDialog = False
    , reportToWhatsAppDialog = Components.ReportToWhatsAppDialog.Model.emptyModel
    }


type Msg
    = CloseEncounter AcuteIllnessEncounterId
    | SetActivePage Page
    | SetEndEncounterDialogState Bool
    | MsgReportToWhatsAppDialog (Components.ReportToWhatsAppDialog.Model.Msg Msg)


type AcuteIllnessStatus
    = AcuteIllnessBegan
    | AcuteIllnessUpdated
    | AcuteIllnessResolved
