module Pages.Patients.Model exposing (..)

import Activity.Model exposing (ActivityType)
import App.PageType exposing (Page(..))
import Patient.Model exposing (PatientTypeFilter(..))
import Table


type alias Model =
    { activityTypeFilter : List ActivityType
    , patientTypeFilter : PatientTypeFilter
    , query : String
    , tableState : Table.State
    }


type Msg
    = SetActivityTypeFilter
    | SetPatientTypeFilter String
    | SetRedirectPage Page
    | SetTableState Table.State
    | SetQuery String


emptyModel : Model
emptyModel =
    { activityTypeFilter = []
    , patientTypeFilter = All
    , query = ""
    , tableState = Table.initialSort "Name"
    }
