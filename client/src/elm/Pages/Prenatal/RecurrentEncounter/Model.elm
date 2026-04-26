module Pages.Prenatal.RecurrentEncounter.Model exposing (Model, Msg(..), Tab(..), emptyModel)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (LabsResultsValue)
import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , showAlertsDialog : Bool
    }


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , showAlertsDialog = False
    }


type Msg
    = SetActivePage Page
    | SetAlertsDialogState Bool
    | SetSelectedTab Tab
    | ConcludeEncounter PersonId PrenatalEncounterId PrenatalLabsResultsId LabsResultsValue


type Tab
    = Completed
    | Pending
    | Reports
