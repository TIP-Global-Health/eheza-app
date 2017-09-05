module Measurement.Encoder exposing (..)

import Activity.Model exposing (ChildNutritionSign(..), FamilyPlanningSign(..))
import Child.Model exposing (ChildId)
import EverySet exposing (EverySet)
import Json.Encode as Encoder exposing (Value, float, int, list, string)
import Measurement.Model exposing (FloatMeasurements(..))
import Mother.Model exposing (MotherId)


encodeHeight : ChildId -> Float -> Value
encodeHeight childId value =
    encodeFloatMeasurement childId HeightFloat value


encodeMuac : ChildId -> Float -> Value
encodeMuac childId value =
    encodeFloatMeasurement childId MuacFloat value


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


encodeNutritionSigns : ChildId -> EverySet ChildNutritionSign -> Value
encodeNutritionSigns childId value =
    let
        signsList =
            List.map (\sign -> encodeNutritionSign sign) (EverySet.toList value)
                |> list
    in
        Encoder.object <|
            [ ( "child", Encoder.int childId )
            , ( "nutrition_signs", signsList )
            ]


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


encodeFamilyPlanning : MotherId -> EverySet FamilyPlanningSign -> Value
encodeFamilyPlanning motherId value =
    let
        familyPlanning =
            List.map (\method -> encodeFamilyPlanningSign method) (EverySet.toList value)
                |> list
    in
        Encoder.object <|
            [ ( "mother", Encoder.int motherId )
            , ( "family_planning_signs", familyPlanning )
            ]


encodePhoto : ChildId -> Int -> Value
encodePhoto childId value =
    Encoder.object <|
        [ ( "child", Encoder.int childId )
        , ( "photo", Encoder.int value )
        ]


encodeWeight : ChildId -> Float -> Value
encodeWeight childId value =
    encodeFloatMeasurement childId WeightFloat value


encodeFloatMeasurement : ChildId -> FloatMeasurements -> Float -> Value
encodeFloatMeasurement childId floatMeasurement value =
    let
        key =
            case floatMeasurement of
                HeightFloat ->
                    "height"

                MuacFloat ->
                    "muac"

                WeightFloat ->
                    "weight"
    in
        Encoder.object <|
            [ ( "child", Encoder.int childId )
            , ( key, Encoder.float value )
            ]
