module Measurement.Utils exposing (..)

import Measurement.Model exposing (FloatInputConstraints)


getInputConstraintsWeight : FloatInputConstraints
getInputConstraintsWeight =
    { defaultVal = 1
    , minVal = 0.5
    , maxVal = 60
    }
