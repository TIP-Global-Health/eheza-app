module Clinic.Model exposing (..)

{-| Represents a location at which measurements are taken.
-}


type ClinicId
    = ClinicId Int


type alias Clinic =
    { name : String
    }
