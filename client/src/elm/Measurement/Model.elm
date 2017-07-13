module Measurement.Model exposing (..)

import Activity.Model exposing (ChildActivityType)
import Http
import RemoteData exposing (RemoteData(..), WebData)


type ExaminationId
    = ExaminationId Int


type WeightId
    = WeightId Int


type alias FloatInputConstraints =
    { defaultVal : Float
    , minVal : Float
    , maxVal : Float
    }


type alias FloatInput =
    { value : Float
    , isDirty : Bool
    }


emptyFloatInput : FloatInput
emptyFloatInput =
    { value = 0
    , isDirty = False
    }


type
    Msg
    -- @todo: Change () to WeightId ?
    = HandleWeightSave (Result Http.Error ())
    | WeightSave
    | WeightUpdate Float


type alias Model =
    { status : WebData ()
    , weight : FloatInput
    }


emptyModel : Model
emptyModel =
    { status = NotAsked
    , weight = emptyFloatInput
    }
