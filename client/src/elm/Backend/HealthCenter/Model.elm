module Backend.HealthCenter.Model exposing (CatchmentArea, HealthCenter)

import Backend.Entities exposing (..)


{-| A Health Center.
-}
type alias HealthCenter =
    { catchmentAreaUuid : CatchmentAreaUuid
    , name : String
    }


type alias CatchmentArea =
    { name : String
    }
