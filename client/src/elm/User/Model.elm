module User.Model exposing (Role(..), User)

import Backend.Entities exposing (..)
import EverySet exposing (EverySet)


type alias UserId =
    Int


type alias User =
    { id : UserId
    , name : String
    , avatarUrl : String

    -- Which clinics is this user assigned to?
    , clinics : List ClinicId

    -- What roles does the user have assigned?
    , roles : EverySet Role
    }


type Role
    = Anonymous
    | Authenticated
    | Administrator
    | Nurse
