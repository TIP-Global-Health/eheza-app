module Backend.Components.Model exposing (..)

{-| The return value of Backend update functions
-}

import Error.Model exposing (Error)


type alias HealthCenterData =
    { id : HealthCenterId
    , name : String
    }


type alias HealthCenterId =
    Int


type MenuScope
    = ScopeFull
    | ScopeHealthCenters
