module Measurement.Utils exposing (..)

import Measurement.Model exposing (FloatInputConstraints)


getInputConstraintsWeight : FloatInputConstraints
getInputConstraintsWeight =
    { default = 1
    , min = 0.5
    , max = 60
    }
