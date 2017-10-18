module Backend.Measurement.Encoder exposing (..)

import Backend.Measurement.Model exposing (..)
import Drupal.Restful exposing (EntityId(..), encodeEntityId)
import EverySet
import Gizra.NominalDate
import Json.Encode as Encoder exposing (Value, float, int, list, string)
import Json.Encode.Extra exposing (maybe)


encodeHeight : Height -> List ( String, Value )
encodeHeight =
    encodeChildMeasurement (\(HeightValue height) -> ( "height", float height ))


encodeMuac : Muac -> List ( String, Value )
encodeMuac =
    encodeChildMeasurement (\(MuacValue muac) -> ( "muac", float muac ))


encodeWeight : Weight -> List ( String, Value )
encodeWeight =
    encodeChildMeasurement (\(WeightValue weight) -> ( "weight", float weight ))


encodeNutrition : ChildNutrition -> List ( String, Value )
encodeNutrition =
    encodeChildMeasurement
        (\nutritions ->
            ( "nutrition_signs"
            , EverySet.toList nutritions
                |> List.map encodeNutritionSign
                |> list
            )
        )


encodeFamilyPlanning : FamilyPlanning -> List ( String, Value )
encodeFamilyPlanning =
    encodeMotherMeasurement
        (\familyPlannings ->
            ( "family_planning_signs"
            , EverySet.toList familyPlannings
                |> List.map encodeFamilyPlanningSign
                |> list
            )
        )


encodeChildMeasurement : (value -> ( String, Value )) -> Measurement (EntityId a) value -> List ( String, Value )
encodeChildMeasurement =
    encodeMeasurement "child"


encodeMotherMeasurement : (value -> ( String, Value )) -> Measurement (EntityId a) value -> List ( String, Value )
encodeMotherMeasurement =
    encodeMeasurement "mother"


encodeMeasurement : String -> (value -> ( String, Value )) -> Measurement (EntityId a) value -> List ( String, Value )
encodeMeasurement participantField encoder measurement =
    [ ( participantField, encodeEntityId measurement.participantId )
    , ( "session", maybe encodeEntityId measurement.sessionId )
    , ( "date_measured", Gizra.NominalDate.encodeYYYYMMDD measurement.dateMeasured )
    , encoder measurement.value
    ]


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
