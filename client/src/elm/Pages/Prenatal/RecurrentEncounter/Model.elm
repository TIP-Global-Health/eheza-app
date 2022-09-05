module Pages.Prenatal.RecurrentEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.PrenatalEncounter.Model exposing (..)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , showAlertsDialog : Bool
    , showEndEncounterDialog : Bool
    }


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , showAlertsDialog = False
    , showEndEncounterDialog = False
    }


type Msg
    = CloseEncounter
    | SetActivePage Page
    | SetAlertsDialogState Bool
    | SetEndEncounterDialogState Bool
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending
    | Reports
