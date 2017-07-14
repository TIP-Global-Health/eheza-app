module Measurement.Model exposing (..)

import Http
import RemoteData exposing (RemoteData(..), WebData)


type ExaminationId
    = ExaminationId Int


type WeightId
    = WeightId Int


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


type
    Msg
    -- @todo: Change () to WeightId ?
    = HandleWeightSave (Result Http.Error ())
    | WeightSave
    | WeightUpdate Float


type alias Model =
    { status : WebData ()
    , height : FloatInput
    , weight : FloatInput
    }


getInputConstraintsHeight : FloatInputConstraints
getInputConstraintsHeight =
    { defaultValue = 1
    , minVal = 0.5
    , maxVal = 60
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
    , weight = emptyFloatInput getInputConstraintsWeight
    }
