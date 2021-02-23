module Backend.Nurse.Model exposing (Nurse, Role(..))

import Backend.Entities exposing (..)
import EverySet exposing (EverySet)


type alias Nurse =
    { name : String
    , healthCenters : EverySet HealthCenterId
    , villages : EverySet VillageId
    , roles : EverySet Role
    , email : Maybe String
    , pinCode : String
    }


type Role
    = RoleAdministrator
    | RoleCHW
    | RoleNurse
