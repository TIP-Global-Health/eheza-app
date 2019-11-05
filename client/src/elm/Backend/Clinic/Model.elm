module Backend.Clinic.Model exposing (Clinic, ClinicType(..))

import Backend.Entities exposing (..)


{-| Represents a location at which measurements are taken.
-}
type alias Clinic =
    { name : String
    , healthCenterId : HealthCenterId
    , clinicType : ClinicType
    }


type ClinicType
    = Fbf
    | Pmtct
    | Sorwathe
