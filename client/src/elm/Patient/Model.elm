module Patient.Model
    exposing
        ( Patient
        , PatientId
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
    -- @todo: Replace with Dict of activities.
    { info : PatientType
    , activities : String
    }


type alias PatientsDict =
    Dict PatientId Patient
