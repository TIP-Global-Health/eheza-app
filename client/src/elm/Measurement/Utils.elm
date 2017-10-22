module Measurement.Utils exposing (..)

import Measurement.Model exposing (..)


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


getFloatInputValue : String -> Float
getFloatInputValue input =
    let
        normalizedInput =
            if String.endsWith "." input && (List.length <| String.indexes "." input) == 1 then
                String.dropRight 1 input
            else
                input
    in
        case String.toFloat normalizedInput of
            Ok value ->
                value

            Err error ->
                0.0


normalizeFloatFormInput : String -> String
normalizeFloatFormInput input =
    let
        normilizedInput =
            if String.endsWith "." input && (List.length <| String.indexes "." input) == 1 then
                String.dropRight 1 input
            else
                input
    in
        case String.toFloat normilizedInput of
            Ok value ->
                input

            Err error ->
                if input == "." then
                    "0."
                else
                    String.dropRight 1 input


normalizeFloatInput : FloatInput -> FloatInput
normalizeFloatInput floatInput =
    let
        input =
            floatInput |> Maybe.withDefault "0.0"
    in
        if String.endsWith "." input && (List.length <| String.indexes "." input) == 1 then
            Just <| String.dropRight 1 input
        else
            Just input
