module Backend.Measurement.Encoder exposing (..)

import Backend.Measurement.Model exposing (ChildNutritionSign(..), FamilyPlanningSign(..))
import Json.Encode as Encoder exposing (Value, float, int, list, string)


encodeNutritionSign : ChildNutritionSign -> Value
encodeNutritionSign sign =
    case sign of
        AbdominalDisortion ->
            string "abdominal-disortion"

        Apathy ->
            string "apathy"

        BrittleHair ->
            string "brittle-hair"

        DrySkin ->
            string "dry-skin"

        Edema ->
            string "edema"

        None ->
            string "none"

        PoorAppetite ->
            string "poor-appetite"


encodeFamilyPlanningSign : FamilyPlanningSign -> Value
encodeFamilyPlanningSign sign =
    case sign of
        Condoms ->
            string "condoms"

        IUD ->
            string "iud"

        Injection ->
            string "injection"

        Necklace ->
            string "necklace"

        NoFamilyPlanning ->
            string "none"

        Pill ->
            string "pill"
