module Item.Model
    exposing
        ( Item
        , ItemId
        , ItemsDict
        )

import Dict exposing (Dict)
import Child.Model exposing (Child)
import Mother.Model exposing (Mother)


type alias ItemId =
    String


type Patient
    = PatientChild Child
    | PatientMother Mother


type alias Item =
    { name : String
    , image : String
    }


type alias ItemsDict =
    Dict ItemId Item
