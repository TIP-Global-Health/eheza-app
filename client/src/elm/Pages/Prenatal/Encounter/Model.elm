module Pages.Prenatal.Encounter.Model exposing (Model, Msg(..), Tab(..), emptyModel)

import Backend.Entities exposing (..)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , showAlertsDialog : Bool
    , showWarningForChw : Bool
    , showEndEncounterDialog : Bool
    }


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , showAlertsDialog = False
    , showWarningForChw = False
    , showEndEncounterDialog = False
    }


type Msg
    = CloseEncounter
    | SetActivePage Page
    | SetAlertsDialogState Bool
    | SetChwWarningVisible Bool
    | SetEndEncounterDialogState Bool
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending
    | Reports
