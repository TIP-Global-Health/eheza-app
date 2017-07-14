module Measurement.Model exposing (..)

import Child.Model exposing (ChildId, ExaminationId)
import EveryDictList exposing (EveryDictList)
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


type Msg
    = HandlePostWeight ChildId (Result Http.Error PostWeightResponse)
      -- @todo: Change () to WeightId ?
    | HandleWeightSave (Result Http.Error ())
    | HeightUpdate Float
    | MuacUpdate Float
    | WeightSave ChildId Float
    | WeightUpdate Float


type alias Model =
    { status : WebData ()
    , height : FloatInput
    , muac : FloatInput
    , weight : FloatInput
    }


type alias Measurements =
    { height : Float
    , muac : Float
    , weight : Float
    }


type alias PostWeightData =
    { child : ChildId
    , weight : Float
    }


type alias PostWeightResponse =
    { weight : Float
    }


type alias MeasurementsDict =
    EveryDictList ExaminationId Measurements


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
    , weight = emptyFloatInput getInputConstraintsWeight
    }
