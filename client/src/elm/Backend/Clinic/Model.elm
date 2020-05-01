module Backend.Clinic.Model exposing (Clinic, ClinicType(..), allClinicTypes)

import Backend.Entities exposing (..)


{-| Represents a location at which measurements are taken.
-}
type alias Clinic =
    { name : String
    , healthCenterId : HealthCenterId
    , clinicType : ClinicType
    , villageId : Maybe VillageId
    }


type ClinicType
    = Chw
    | Fbf
    | Pmtct
    | Sorwathe


{-| Chw is a special type used for villages, therefore,
not included in allowed list.
-}
allClinicTypes : List ClinicType
allClinicTypes =
    [ Pmtct, Fbf, Sorwathe ]
