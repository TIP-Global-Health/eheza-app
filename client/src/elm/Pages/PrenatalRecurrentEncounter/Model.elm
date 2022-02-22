module Pages.PrenatalRecurrentEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (ObstetricHistoryValue, PrenatalMeasurements)
import Backend.Person.Model exposing (Person)
import Backend.PrenatalEncounter.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
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


type Tab
    = Completed
    | Pending
    | Reports
