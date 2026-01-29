module Pages.Tuberculosis.ProgressReport.Model exposing (Model, Msg(..), ViewMode(..), emptyModel)

import Backend.Entities exposing (..)
import Components.ReportToWhatsAppDialog.Model
import Pages.Page exposing (Page)


type alias Model =
    { viewMode : ViewMode
    , showEndEncounterDialog : Bool
    , reportToWhatsAppDialog : Components.ReportToWhatsAppDialog.Model.Model
    }


emptyModel : Model
emptyModel =
    { viewMode = ViewModeGlobal
    , showEndEncounterDialog = False
    , reportToWhatsAppDialog = Components.ReportToWhatsAppDialog.Model.emptyModel
    }


type ViewMode
    = ViewModeGlobal
    | ViewModeEncounter TuberculosisEncounterId


type Msg
    = CloseEncounter TuberculosisEncounterId
    | SetActivePage Page
    | SetViewMode ViewMode
    | SetEndEncounterDialogState Bool
    | MsgReportToWhatsAppDialog (Components.ReportToWhatsAppDialog.Model.Msg Msg)
