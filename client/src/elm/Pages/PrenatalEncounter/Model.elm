module Pages.PrenatalEncounter.Model exposing (Model, Msg(..), Tab(..), emptyModel)

import Backend.Entities exposing (..)
import Backend.PrenatalEncounter.Model
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , showAlertsDialog : Bool
    }


type Msg
    = CloseEncounter PrenatalEncounterId
    | SetActivePage Page
    | SetAlertsDialogState Bool
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending
    | Reports


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , showAlertsDialog = False
    }
