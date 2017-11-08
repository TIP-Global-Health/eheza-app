module User.Model exposing (..)

import Backend.Entities exposing (..)


type alias UserId =
    String


type alias User =
    { id : Int
    , name : String
    , avatarUrl : String

    -- Which clinics is this user assigned to?
    , clinics : List ClinicId
    }
