module Measurement.Encoder exposing (..)

import Activity.Model exposing (ChildNutritionSign(..), FamilyPlanningSign(..))
import Backend.Entities exposing (ChildId, MotherId)
import Drupal.Restful exposing (encodeNodeId)
import EverySet exposing (EverySet)
import Json.Encode as Encoder exposing (Value, float, int, list, string)
import Measurement.Model exposing (..)
import StorageKey exposing (StorageKey(..))


encodeHeight : ChildId -> ( StorageKey HeightId, Float ) -> Value
encodeHeight childId ( key, value ) =
    case key of
        New ->
            Encoder.object
                [ childField childId
                , ( "height", Encoder.float value )
                ]

        Existing _ ->
            Encoder.object
                [ ( "height", Encoder.float value ) ]


encodeMuac : ChildId -> ( StorageKey MuacId, Float ) -> Value
encodeMuac childId ( key, value ) =
    case key of
        New ->
            Encoder.object
                [ childField childId
                , ( "muac", Encoder.float value )
                ]

        Existing _ ->
            Encoder.object
                [ ( "muac", Encoder.float value ) ]


encodeWeight : ChildId -> ( StorageKey WeightId, Float ) -> Value
encodeWeight childId ( key, value ) =
    case key of
        New ->
            Encoder.object
                [ childField childId
                , ( "weight", Encoder.float value )
                ]

        Existing _ ->
            Encoder.object
                [ ( "weight", Encoder.float value ) ]


childField : ChildId -> ( String, Value )
childField childId =
    ( "child", encodeNodeId childId )


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


encodeNutritionSigns : ChildId -> ( StorageKey NutritionId, EverySet ChildNutritionSign ) -> Value
encodeNutritionSigns childId ( key, value ) =
    let
        signsList =
            value
                |> EverySet.toList
                |> List.map encodeNutritionSign
                |> list
    in
        case key of
            New ->
                Encoder.object
                    [ ( "child", encodeNodeId childId )
                    , ( "nutrition_signs", signsList )
                    ]

            Existing _ ->
                Encoder.object
                    [ ( "nutrition_signs", signsList ) ]


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


encodeFamilyPlanning : MotherId -> ( StorageKey FamilyPlanningId, EverySet FamilyPlanningSign ) -> Value
encodeFamilyPlanning motherId ( key, value ) =
    let
        familyPlanning =
            value
                |> EverySet.toList
                |> List.map encodeFamilyPlanningSign
                |> list
    in
        case key of
            New ->
                Encoder.object
                    [ ( "mother", encodeNodeId motherId )
                    , ( "family_planning_signs", familyPlanning )
                    ]

            Existing _ ->
                Encoder.object
                    [ ( "family_planning_signs", familyPlanning ) ]


encodePhoto : ChildId -> Int -> Value
encodePhoto childId value =
    Encoder.object <|
        [ childField childId
        , ( "photo", Encoder.int value )
        ]
