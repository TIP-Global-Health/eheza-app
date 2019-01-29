module Backend.Nurse.Model exposing (Nurse, Role(..))

import Backend.Entities exposing (..)


type alias Nurse =
    { name : String
    , pinCode : String
    , clinics : List ClinicId
    , role : List Role
    , email : Maybe String
    }


type Role
    = RoleAdministrator
    | RoleNurse
