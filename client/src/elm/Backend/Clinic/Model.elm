module Backend.Clinic.Model exposing (Clinic)

import Backend.Entities exposing (..)


{-| Represents a location at which measurements are taken.
-}
type alias Clinic =
    { name : String
    , healthCenterId : HealthCenterId
    }
