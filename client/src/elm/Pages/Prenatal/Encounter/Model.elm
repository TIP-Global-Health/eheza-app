module Pages.Prenatal.Encounter.Model exposing (Model, Msg(..), Tab(..), emptyModel)

import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , showAlertsDialog : Bool
    , showWarningForChw : Bool
    , showEndEncounterDialog : Bool
    , undeterminedDiagnosesWarningAcknowledged : Bool
    }


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , showAlertsDialog = False
    , showWarningForChw = False
    , showEndEncounterDialog = False
    , undeterminedDiagnosesWarningAcknowledged = False
    }


type Msg
    = CloseEncounter
    | SetActivePage Page
    | SetAlertsDialogState Bool
    | SetChwWarningVisible Bool
    | SetEndEncounterDialogState Bool
    | SetSelectedTab Tab
    | UndeterminedDiagnosesWarningAcknowledged


type Tab
    = Completed
    | Pending
    | Reports
