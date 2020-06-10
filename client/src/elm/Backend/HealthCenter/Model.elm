module Backend.HealthCenter.Model exposing (CatchmentArea, HealthCenter)

import Backend.Entities exposing (..)


{-| A Health Center.
-}
type alias HealthCenter =
    { catchmentAreaId : CatchmentAreaId
    , name : String
    }


type alias CatchmentArea =
    { name : String
    }
