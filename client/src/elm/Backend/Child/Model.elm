module Backend.Child.Model exposing (Child, Gender(..), ModeOfDelivery(..), modeOfDeliveryToValue)

import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import Uuid exposing (Uuid)


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
    , motherUuid : Maybe Uuid
    , birthDate : NominalDate
    , gender : Gender
    }


type ModeOfDelivery
    = SpontaneousVaginalDeliveryWithEpisiotomy
    | SpontaneousVaginalDeliveryWithoutEpisiotomy
    | VaginalDeliveryWithVacuumExtraction
    | CesareanDelivery


modeOfDeliveryToValue : ModeOfDelivery -> String
modeOfDeliveryToValue mode =
    case mode of
        SpontaneousVaginalDeliveryWithEpisiotomy ->
            "svd-episiotomy"

        SpontaneousVaginalDeliveryWithoutEpisiotomy ->
            "svd-no-episiotomy"

        VaginalDeliveryWithVacuumExtraction ->
            "vd-vacuum"

        CesareanDelivery ->
            "cesarean-delivery"
