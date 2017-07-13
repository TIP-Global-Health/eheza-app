module Measurement.Model exposing (..)

import Activity.Model exposing (ChildActivityType)
import Http
import RemoteData exposing (WebData)


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
    { value : Float, isDirty : Bool }


type alias Model =
    { selectedActivity : Maybe ChildActivityType
    , status : WebData ()
    , weight : FloatInput
    }


type
    Msg
    -- @todo: Change () to WeightId ?
    = HandleWeightSave (Result Http.Error ())
    | WeightSave
    | WeightUpdate Float
