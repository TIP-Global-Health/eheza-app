module Pages.Prenatal.RecurrentEncounter.Model exposing (..)

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
