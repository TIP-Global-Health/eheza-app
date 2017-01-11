module Patient.Model
    exposing
        ( Patient
        , PatientId
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
    { name : String
    , image : String
    }


type alias PatientsDict =
    Dict PatientId Patient
