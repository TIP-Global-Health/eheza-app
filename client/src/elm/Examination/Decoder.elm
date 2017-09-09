module Examination.Decoder exposing (..)

import EverySet exposing (EverySet)
import Examination.Model exposing (..)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Measurement.Decoder exposing (decodeHeight, decodeWeight, decodeMuac, decodeNutrition, decodeFamilyPlanning)
import StorageKey exposing (StorageKey(..))


decodeExaminationChild : Decoder ExaminationChild
decodeExaminationChild =
    decode ExaminationChild
        |> optional "height" (map Just decodeHeight) Nothing
        |> optional "muac" (map Just decodeMuac) Nothing
        |> optional "nutrition" decodeNutrition ( New, EverySet.empty )
        |> hardcoded Nothing
        |> optional "weight" (map Just decodeWeight) Nothing


decodeExaminationMother : Decoder ExaminationMother
decodeExaminationMother =
    decode ExaminationMother
        |> optional "family_planning" decodeFamilyPlanning ( New, EverySet.empty )
