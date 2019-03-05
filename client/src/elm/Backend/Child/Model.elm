module Backend.Child.Model exposing (Child, Gender(..), ModeOfDelivery(..), VaginalDelivery(..), allModesOfDelivery)

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


type ModeOfDelivery
    = VaginalDelivery VaginalDelivery
    | CesareanDelivery


{-| The bool indicates whether an episiotomy was performed
-}
type VaginalDelivery
    = Spontaneous Bool
    | WithVacuumExtraction


allModesOfDelivery : List ModeOfDelivery
allModesOfDelivery =
    [ VaginalDelivery (Spontaneous True)
    , VaginalDelivery (Spontaneous False)
    , VaginalDelivery WithVacuumExtraction
    , CesareanDelivery
    ]
