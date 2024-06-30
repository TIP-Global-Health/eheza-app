module Pages.Tuberculosis.ProgressReport.Model exposing (..)

import Backend.Entities exposing (..)
import Components.ReportToWhatsAppDialog.Model
import EverySet exposing (EverySet)
import Pages.Page exposing (Page)


type alias Model =
    { viewMode : ViewMode
    , showEndEncounterDialog : Bool
    , reportToWhatsAppDialog : Components.ReportToWhatsAppDialog.Model.Model

    -- , components : Maybe (EverySet Components.ReportToWhatsAppDialog.Model.ReportComponentTuberculosis)
    }


emptyModel : Model
emptyModel =
    { viewMode = ViewModeGlobal
    , showEndEncounterDialog = False
    , reportToWhatsAppDialog = Components.ReportToWhatsAppDialog.Model.emptyModel

    -- , components = Nothing
    }


type ViewMode
    = ViewModeGlobal
    | ViewModeEncounter TuberculosisEncounterId


type Msg
    = NoOp
    | CloseEncounter TuberculosisEncounterId
    | SetActivePage Page
    | SetViewMode ViewMode
    | SetEndEncounterDialogState Bool
    | MsgReportToWhatsAppDialog (Components.ReportToWhatsAppDialog.Model.Msg Msg)
    | SetReportComponents (Maybe Components.ReportToWhatsAppDialog.Model.ReportComponentsList)
