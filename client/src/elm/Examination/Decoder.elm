module Examination.Decoder exposing (..)

import Examination.Model exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Utils.Json exposing (decodeFloat)


decodeExaminationChild : Decoder ExaminationChild
decodeExaminationChild =
    decode ExaminationChild
        |> optionalAt [ "height", "height" ] (nullable decodeFloat) Nothing
        |> optionalAt [ "weight", "weight" ] (nullable decodeFloat) Nothing
        |> hardcoded Nothing
        |> optionalAt [ "muac", "muac" ] (nullable decodeFloat) Nothing


decodeExaminationMother : Decoder ExaminationMother
decodeExaminationMother =
    decode ExaminationMother
