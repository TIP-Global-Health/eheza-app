module Measurement.Model exposing (..)

import Activity.Model exposing (ActivityType, ChildNutritionSign)
import EveryDict exposing (EveryDict)
import Http
import RemoteData exposing (RemoteData(..), WebData)
import Set exposing (Set)


{-| Indicate which Activity was completed, and to which Activity to redirect to.
This can be used as a return value in an `update` function upon form save.
-}
type alias CompletedAndRedirectToActivityTuple =
    ( ActivityType, ActivityType )


type alias FloatInputConstraints =
    { minVal : Float
    , maxVal : Float
    }


type alias FloatInput =
    Maybe Float


type alias EveryDictChildNutritionSign =
    EveryDict ChildNutritionSign ()


type Msg
    = HandleDropzoneUploadedFile Int
    | HandlePhotoSave (Result Http.Error ())
    | HandleWeightSave (Result Http.Error ())
    | HeightSave
    | HeightUpdate Float
    | MuacUpdate Float
    | MuacSave
    | NutritionSignsToggle ChildNutritionSign
    | NutritionSignsSave
    | PhotoSave
    | WeightSave
    | WeightUpdate Float


type alias Model =
    { status : WebData ()
    , height : FloatInput
    , muac : FloatInput

    -- We use EveryDict instead of Set, as we want the key to be a typed value
    -- and not have to cast it to string.
    , nutritionSigns : EveryDictChildNutritionSign
    , photo : Int
    , weight : FloatInput
    }


type FloatMeasurements
    = HeightFloat
    | MuacFloat
    | WeightFloat


getInputConstraintsHeight : FloatInputConstraints
getInputConstraintsHeight =
    { minVal = 0.5
    , maxVal = 100
    }


getInputConstraintsMuac : FloatInputConstraints
getInputConstraintsMuac =
    { minVal = 0.5
    , maxVal = 40
    }


getInputConstraintsWeight : FloatInputConstraints
getInputConstraintsWeight =
    { minVal = 0.5
    , maxVal = 60
    }


emptyModel : Model
emptyModel =
    { status = NotAsked
    , height = Nothing
    , muac = Nothing
    , nutritionSigns = EveryDict.empty
    , photo = 0
    , weight = Nothing
    }
