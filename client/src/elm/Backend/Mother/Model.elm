module Backend.Mother.Model exposing (..)

import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)


type alias Mother =
    { name : String
    , image : String
    , children : List ChildId
    , birthDate : NominalDate
    }
