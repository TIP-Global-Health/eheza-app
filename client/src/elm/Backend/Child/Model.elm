module Backend.Child.Model exposing (..)

import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)


type Gender
    = Female
    | Male


type alias Child =
    { name : String
    , image : String
    , motherId : Maybe MotherId
    , siblingId : Maybe ChildId
    , birthDate : NominalDate
    , gender : Gender
    }
