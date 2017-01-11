module Pages.Patients.Model exposing (..)

import App.PageType exposing (Page(..))
import Table


type alias Model =
    { patientFilter : PatientFilter
    , query : String
    , tableState : Table.State
    }


type PatientFilter
    = Child
    | Mother
    | All


type Msg
    = SetPatientFilter PatientFilter
    | SetRedirectPage Page
    | SetTableState Table.State
    | SetQuery String


emptyModel : Model
emptyModel =
    { patientFilter = All
    , query = ""
    , tableState = Table.initialSort "Name"
    }
