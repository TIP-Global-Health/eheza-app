module Pages.Patients.Model exposing (..)

import Activity.Model exposing (ActivityType)
import Activity.Utils exposing (getActivityTypeList)
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
    = SetActivityTypeFilter ActivityType Bool
    | SetActivityTypeFilters (List ActivityType)
    | SetPatientTypeFilter String
    | SetRedirectPage Page
    | SetTableState Table.State
    | SetQuery String


emptyModel : Model
emptyModel =
    { activityTypeFilter = getActivityTypeList All
    , patientTypeFilter = All
    , query = ""
    , tableState = Table.initialSort "Name"
    }
