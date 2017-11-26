module Backend.Measurement.Encoder exposing (..)

import Backend.Measurement.Model exposing (..)
import EveryDict
import EverySet
import Gizra.NominalDate
import Json.Encode as Encoder exposing (Value, float, int, list, string, object, bool)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (Entity, EntityId(..), encodeEntityId, fromEntityId)


encodeHeight : Height -> List ( String, Value )
encodeHeight =
    encodeChildMeasurement (\(HeightInCm height) -> ( "height", float height ))


encodeMuac : Muac -> List ( String, Value )
encodeMuac =
    encodeChildMeasurement (\(MuacInCm muac) -> ( "muac", float muac ))


encodeWeight : Weight -> List ( String, Value )
encodeWeight =
    encodeChildMeasurement (\(WeightInKg weight) -> ( "weight", float weight ))


{-| TODO: Revisit this, as it's probably not what we'll need for the backend..
-}
encodePhoto : Photo -> List ( String, Value )
encodePhoto =
    encodeChildMeasurement (\(PhotoValue photo) -> ( "photo", string photo ))


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
encodeNutritionSign =
    encodeNutritionSignAsString >> string


encodeNutritionSignAsString : ChildNutritionSign -> String
encodeNutritionSignAsString sign =
    case sign of
        AbdominalDisortion ->
            "abdominal-disortion"

        Apathy ->
            "apathy"

        BrittleHair ->
            "brittle-hair"

        DrySkin ->
            "dry-skin"

        Edema ->
            "edema"

        None ->
            "none"

        PoorAppetite ->
            "poor-appetite"


encodeFamilyPlanningSign : FamilyPlanningSign -> Value
encodeFamilyPlanningSign =
    encodeFamilyPlanningSignAsString >> string


encodeFamilyPlanningSignAsString : FamilyPlanningSign -> String
encodeFamilyPlanningSignAsString sign =
    case sign of
        Condoms ->
            "condoms"

        IUD ->
            "iud"

        Injection ->
            "injection"

        Necklace ->
            "necklace"

        NoFamilyPlanning ->
            "none"

        Pill ->
            "pill"


encodeMeasurementEdits : MeasurementEdits -> Value
encodeMeasurementEdits edits =
    object
        [ ( "mothers"
          , edits.mothers
                |> EveryDict.toList
                |> List.map
                    (Tuple.mapFirst (fromEntityId >> toString)
                        >> Tuple.mapSecond encodeMotherEdits
                    )
                |> object
          )
        , ( "children"
          , edits.children
                |> EveryDict.toList
                |> List.map
                    (Tuple.mapFirst (fromEntityId >> toString)
                        >> Tuple.mapSecond encodeChildEdits
                    )
                |> object
          )
        , ( "closed", bool edits.explicitlyClosed )
        ]


encodeMotherEdits : MotherEdits -> Value
encodeMotherEdits edits =
    object
        [ ( "family_planning", encodeEdit (object << encodeFamilyPlanning) edits.familyPlanning )
        , ( "checked_in", bool edits.explicitlyCheckedIn )
        ]


encodeChildEdits : ChildEdits -> Value
encodeChildEdits edits =
    object
        [ ( "height", encodeEdit (object << encodeHeight) edits.height )
        , ( "muac", encodeEdit (object << encodeMuac) edits.muac )
        , ( "nutrition", encodeEdit (object << encodeNutrition) edits.nutrition )
        , ( "photo", encodeEdit (object << encodePhoto) edits.photo )
        , ( "weight", encodeEdit (object << encodeWeight) edits.weight )
        ]


encodeEdit : (value -> Value) -> Edit value -> Value
encodeEdit encodeValue edit =
    case edit of
        Unedited ->
            object [ ( "tag", string "unedited" ) ]

        Created value ->
            object
                [ ( "tag", string "created" )
                , ( "value", encodeValue value )
                ]

        Edited { backend, edited } ->
            object
                [ ( "tag", string "edited" )
                , ( "backend", encodeValue backend )
                , ( "edited", encodeValue edited )
                ]

        Deleted value ->
            object
                [ ( "tag", string "deleted" )
                , ( "value", encodeValue value )
                ]


encodeChildMeasurementList : ChildMeasurementList -> Value
encodeChildMeasurementList measurements =
    object
        [ ( "height"
          , measurements.heights
                |> List.map (encodeEntity encodeHeight)
                |> list
          )
        , ( "muac"
          , measurements.muacs
                |> List.map (encodeEntity encodeMuac)
                |> list
          )
        , ( "nutrition"
          , measurements.nutritions
                |> List.map (encodeEntity encodeNutrition)
                |> list
          )
        , ( "photo"
          , measurements.photos
                |> List.map (encodeEntity encodePhoto)
                |> list
          )
        , ( "weight"
          , measurements.weights
                |> List.map (encodeEntity encodeWeight)
                |> list
          )
        ]


encodeMotherMeasurementList : MotherMeasurementList -> Value
encodeMotherMeasurementList measurements =
    object
        [ ( "family_planning"
          , measurements.familyPlannings
                |> List.map (encodeEntity encodeFamilyPlanning)
                |> list
          )
        ]


encodeEntity : (b -> List ( String, Value )) -> ( EntityId a, b ) -> Value
encodeEntity encoder ( id, value ) =
    object <|
        ( "id", encodeEntityId id )
            :: encoder value
