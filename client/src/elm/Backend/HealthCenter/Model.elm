module Backend.HealthCenter.Model exposing (HealthCenter)

import Backend.Entities exposing (..)


{-| A Health Center.
-}
type alias HealthCenter =
    { catchmentAreaUuid : CatchmentAreaUuid
    , name : String
    }
