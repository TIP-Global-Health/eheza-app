module Mother.Model exposing (..)

import Date exposing (Date)
import Examination.Model exposing (ExaminationMother, ExaminationId)


type alias ChildId =
    Int


type alias MotherId =
    Int


{-| `examinations` is a simple list for now ... will likely need to be more
complex in future.
-}
type alias Mother =
    { name : String
    , image : String
    , children : List ChildId
    , examinations : List ExaminationMother
    , birthDate : Date
    }
