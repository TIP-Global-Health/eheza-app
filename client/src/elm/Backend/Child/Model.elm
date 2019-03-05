module Backend.Child.Model exposing (Child, ModeOfDelivery(..), VaginalDelivery(..), allModesOfDelivery)

import Backend.Entities exposing (..)
import Backend.Patient.Model exposing (Gender, Ubudehe)
import Gizra.NominalDate exposing (NominalDate)


{-| We keep just the basic information in the `Child` record itself.
For things like measurements, you need an `OfflineSession`.
-}
type alias Child =
    { name : String
    , firstName : String
    , middleName : Maybe String
    , secondName : String
    , nationalIdNumber : Maybe String
    , avatarUrl : Maybe String
    , motherId : Maybe MotherId
    , birthDate : NominalDate
    , isDateOfBirthEstimated : Bool
    , gender : Gender
    , modeOfDelivery : Maybe ModeOfDelivery
    , ubudehe : Maybe Ubudehe
    , motherName : Maybe String
    , motherNationalId : Maybe String
    , fatherName : Maybe String
    , fatherNationalId : Maybe String
    , caregiverName : Maybe String
    , caregiverNationalId : Maybe String
    , province : Maybe String
    , district : Maybe String
    , sector : Maybe String
    , cell : Maybe String
    , village : Maybe String
    , telephoneNumber : Maybe String
    , healthCenterName : Maybe String
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
