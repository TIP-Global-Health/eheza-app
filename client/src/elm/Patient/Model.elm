module Patient.Model
    exposing
        ( Patient
        , PatientId
        , PatientTypeFilter(..)
        , PatientType(..)
        , PatientsDict
        )

import Dict exposing (Dict)
import Child.Model exposing (Child)
import Mother.Model exposing (Mother)


type alias PatientId =
    String


type PatientType
    = PatientChild Child
    | PatientMother Mother


type alias Patient =
    { info : PatientType
    }


type alias PatientsDict =
    Dict PatientId Patient


type PatientTypeFilter
    = All
    | Children
    | Mothers
