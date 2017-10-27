module Backend.Mother.Model exposing (..)

import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)


{-| `examinations` is a simple list for now ... will likely need to be more
complex in future.
-}
type alias Mother =
    { name : String
    , image : String
    , children : List ChildId
    , birthDate : NominalDate
    }
