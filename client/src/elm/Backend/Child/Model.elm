module Backend.Child.Model exposing (..)

import Backend.Entities exposing (..)
import Date exposing (Date)


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
    , birthDate : Date
    , gender : Gender
    }
