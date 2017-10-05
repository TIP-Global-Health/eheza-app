module Backend.Measurement.Model exposing (..)

{-| This will eventually replace the `Measurement.Model`, but it's easier
to keep things compiling for the moment to do it here first. Or, it may
make sense to actually keep a `Data` hierarchy, to mark things that are
in the data layer.
-}

import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)


{-| A base that expresses some things all the measurements
have in common, plus two things whose type varis:

  - the type if the ID for the participant
  - the type of the value for this measurement

-}
type alias Measurement participantId value =
    { participantId : participantId
    , sessionId : Maybe SessionId
    , dateMeasured : NominalDate
    , value : value
    }


{-| The string represents the URL of the photo.
-}
type PhotoValue
    = PhotoValue String


type alias Photo =
    Measurement ChildId PhotoValue


type MuacValue
    = MuacValue Float


type alias Muac =
    Measurement ChildId MuacValue


type HeightValue
    = HeightValue Float


type alias Height =
    Measurement ChildId HeightValue


type WeightValue
    = WeightValue Float


type alias Weight =
    Measurement ChildId WeightValue


type FamilyPlanningValue
    = Condoms
    | IUD
    | Injection
    | Necklace
    | NoFamilyPlanning
    | Pill


type alias FamilyPlanning =
    Measurement MotherId FamilyPlanningValue


type ChildNutritionValue
    = AbdominalDisortion
    | Apathy
    | BrittleHair
    | DrySkin
    | Edema
    | None
    | PoorAppetite


type alias ChildNutrition =
    Measurement ChildId ChildNutritionValue
