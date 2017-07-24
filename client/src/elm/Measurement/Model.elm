module Measurement.Model exposing (..)

import Activity.Model exposing (ActivityType)
import Http
import RemoteData exposing (RemoteData(..), WebData)


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



-- @todo: Use ChildMeasurements in the model?


type alias ChildMeasurements =
    { height : Maybe Float
    , muac : Maybe Float
    , photo : Maybe Int
    , weight : Maybe Float
    }


emptyChildMeasurements : ChildMeasurements
emptyChildMeasurements =
    { height = Nothing
    , muac = Nothing
    , photo = Nothing
    , weight = Nothing
    }


type alias MotherMeasurements =
    {}


type Msg
    = HandleDropzoneUploadedFile Int
    | HandlePhotoSave (Result Http.Error ())
    | HandleWeightSave (Result Http.Error ())
    | HeightSave
    | HeightUpdate Float
    | MuacUpdate Float
    | MuacSave
    | NutritionSignsSave
    | PhotoSave
    | WeightSave
    | WeightUpdate Float


type alias Model =
    { status : WebData ()
    , height : FloatInput
    , muac : FloatInput
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
    , photo = 0
    , weight = Nothing
    }
