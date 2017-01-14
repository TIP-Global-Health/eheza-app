module Pages.Patients.Model exposing (..)

import Activity.Model exposing (ActivityType)
import App.PageType exposing (Page(..))
import Set exposing (Set)
import Patient.Model exposing (PatientTypeFilter(..))
import Table


type alias Model =
    { activityTypeFilter : Set String
    , patientTypeFilter : PatientTypeFilter
    , query : String
    , tableState : Table.State
    }


type Msg
    = SetActivityTypeFilter ActivityType Bool
    | SetPatientTypeFilter String
    | SetRedirectPage Page
    | SetTableState Table.State
    | SetQuery String


emptyModel : Model
emptyModel =
    { activityTypeFilter = Set.empty
    , patientTypeFilter = All
    , query = ""
    , tableState = Table.initialSort "Name"
    }
