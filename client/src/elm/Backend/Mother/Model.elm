module Backend.Mother.Model exposing (..)

import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)


type ChildrenRelationType
    = MotherRelation
    | CaregiverRelation


type alias Mother =
    { name : String
    , avatarUrl : Maybe String
    , children : List ChildId
    , birthDate : NominalDate
    , relation : ChildrenRelationType
    }
