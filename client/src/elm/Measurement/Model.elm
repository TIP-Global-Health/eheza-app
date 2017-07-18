module Measurement.Model exposing (..)

import Http
import RemoteData exposing (RemoteData(..), WebData)


type alias FloatInputConstraints =
    { defaultValue : Float
    , minVal : Float
    , maxVal : Float
    }


type alias FloatInput =
    { initialValue : Maybe Float
    , value : Float
    }


emptyFloatInput : FloatInputConstraints -> FloatInput
emptyFloatInput constraints =
    { initialValue = Nothing
    , value = constraints.defaultValue
    }


type Msg
    = HandlePhotoSave (Result Http.Error ())
    | HandleWeightSave (Result Http.Error ())
    | HeightSave
    | HeightUpdate Float
    | MuacUpdate Float
    | NutritionSignsSave
    | PhotoSave
    | WeightSave
    | WeightUpdate Float


type alias Model =
    { status : WebData ()
    , height : FloatInput
    , muac : FloatInput
    , photo : FloatInput
    , weight : FloatInput
    }


getInputConstraintsHeight : FloatInputConstraints
getInputConstraintsHeight =
    { defaultValue = 1
    , minVal = 0.5
    , maxVal = 100
    }


getInputConstraintsMuac : FloatInputConstraints
getInputConstraintsMuac =
    { defaultValue = 1
    , minVal = 0.5
    , maxVal = 40
    }


getInputConstraintsWeight : FloatInputConstraints
getInputConstraintsWeight =
    { defaultValue = 1
    , minVal = 0.5
    , maxVal = 60
    }


emptyModel : Model
emptyModel =
    { status = NotAsked
    , height = emptyFloatInput getInputConstraintsHeight
    , muac = emptyFloatInput getInputConstraintsHeight
    , photo = emptyFloatInput getInputConstraintsWeight
    , weight = emptyFloatInput getInputConstraintsWeight
    }
