module Child.Model exposing (..)

import Date exposing (Date)
import Examination.Model exposing (ExaminationChild, ExaminationId)


type alias ChildId =
    Int


type alias ExaminationId =
    Int


type alias MotherId =
    Int


type Gender
    = Female
    | Male


{-| `examinations` is just a simple list, for the moment,
implying that we'll fetch them with the `Child`. This will
likely need to become more complex at some point -- for
instance, we might want to fetch the examinations separately,
and thus use a `WebData`.
-}
type alias Child =
    { name : String
    , image : String
    , motherId : Maybe MotherId
    , siblingId : Maybe ChildId
    , examinations : List ExaminationChild
    , birthDate : Date
    , gender : Gender
    }
