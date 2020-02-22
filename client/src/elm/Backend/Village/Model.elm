module Backend.Village.Model exposing (Village)

import Backend.Entities exposing (..)


{-| A Health Center.
-}
type alias Village =
    { healthCenterId : HealthCenterId
    , name : String
    , province : String
    , district : String
    , sector : String
    , cell : String
    , village : String
    }
