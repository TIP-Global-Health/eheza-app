module Backend.Child.Model exposing (..)

import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)


type Gender
    = Female
    | Male


{-| We keep just the basic information in the `Child` record itself.
For things like measurements, you need an `OfflineSession`.
-}
type alias Child =
    { name : String
    , avatarUrl : Maybe String
    , motherId : Maybe MotherId
    , birthDate : NominalDate
    , gender : Gender
    }
