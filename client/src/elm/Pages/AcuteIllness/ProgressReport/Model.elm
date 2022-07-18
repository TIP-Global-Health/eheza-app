module Pages.AcuteIllness.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Components.SendViaWhatsAppDialog.Model
import Pages.Page exposing (Page)


type alias Model =
    { showEndEncounterDialog : Bool
    , sendViaWhatsAppDialog : Components.SendViaWhatsAppDialog.Model.Model
    }


emptyModel : Model
emptyModel =
    { showEndEncounterDialog = False
    , sendViaWhatsAppDialog = Components.SendViaWhatsAppDialog.Model.emptyModel
    }


type Msg
    = CloseEncounter AcuteIllnessEncounterId
    | SetActivePage Page
    | SetEndEncounterDialogState Bool
    | MsgSendViaWhatsAppDialog (Components.SendViaWhatsAppDialog.Model.Msg Msg)


type AcuteIllnessStatus
    = AcuteIllnessBegan
    | AcuteIllnessUpdated
    | AcuteIllnessResolved
