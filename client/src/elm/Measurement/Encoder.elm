module Measurement.Encoder exposing (..)

import Backend.Entities exposing (ChildId, MotherId)
import Backend.Measurement.Encoder exposing (encodeNutritionSign, encodeFamilyPlanningSign)
import Backend.Measurement.Model exposing (ChildNutritionSign(..), FamilyPlanningSign(..))
import Drupal.Restful exposing (encodeEntityId)
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
    ( "child", encodeEntityId childId )


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
                    [ ( "child", encodeEntityId childId )
                    , ( "nutrition_signs", signsList )
                    ]

            Existing _ ->
                Encoder.object
                    [ ( "nutrition_signs", signsList ) ]


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
                    [ ( "mother", encodeEntityId motherId )
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
