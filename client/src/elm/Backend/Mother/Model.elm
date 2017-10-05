module Backend.Mother.Model exposing (..)

import Backend.Entities exposing (..)
import Date exposing (Date)
import Examination.Model exposing (ExaminationMother, ExaminationId)


{-| `examinations` is a simple list for now ... will likely need to be more
complex in future.
-}
type alias Mother =
    { name : String
    , image : String
    , children : List ChildId

    -- TODO: Remove examinations
    , examinations : List ExaminationMother
    , birthDate : Date
    }
