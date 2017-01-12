module Pages.Patients.Model exposing (..)

import App.PageType exposing (Page(..))
import Patient.Model exposing (PatientTypeFilter(..))
import Table


type alias Model =
    { patientTypeFilter : PatientTypeFilter
    , query : String
    , tableState : Table.State
    }


type Msg
    = SetPatientTypeFilter String
    | SetRedirectPage Page
    | SetTableState Table.State
    | SetQuery String


emptyModel : Model
emptyModel =
    { patientTypeFilter = All
    , query = ""
    , tableState = Table.initialSort "Name"
    }
