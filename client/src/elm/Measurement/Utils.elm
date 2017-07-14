module Measurement.Utils exposing (..)

import Measurement.Model exposing (FloatInput, FloatInputConstraints)


isDirty : FloatInputConstraints -> FloatInput -> Bool
isDirty constraints state =
    case state.initialValue of
        Nothing ->
            not (constraints.defaultValue == state.value)

        Just initial ->
            not (initial == state.value)
