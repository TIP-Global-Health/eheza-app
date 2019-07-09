module Backend.Nurse.Model exposing (Nurse, Role(..))

import Backend.Entities exposing (..)
import EverySet exposing (EverySet)


type alias Nurse =
    { name : String
    , healthCenters : EverySet HealthCenterId
    , roles : EverySet Role
    , email : Maybe String
    }


type Role
    = RoleAdministrator
    | RoleNurse
