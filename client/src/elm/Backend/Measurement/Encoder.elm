module Backend.Measurement.Encoder exposing (..)

import Backend.Counseling.Encoder exposing (encodeCounselingTiming)
import Backend.Measurement.Model exposing (..)
import EveryDict
import EverySet
import Gizra.NominalDate
import Json.Encode as Encoder exposing (Value, bool, float, int, list, object, string)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (EntityId(..), encodeEntityId, fromEntityId)
import Translate.Utils exposing (encodeLanguage)


encodeHeight : Height -> List ( String, Value )
encodeHeight =
    encodeChildMeasurement (\(HeightInCm height) -> [ ( "height", float height ) ])


encodeMuac : Muac -> List ( String, Value )
encodeMuac =
    encodeChildMeasurement (\(MuacInCm muac) -> [ ( "muac", float muac ) ])


encodeWeight : Weight -> List ( String, Value )
encodeWeight =
    encodeChildMeasurement (\(WeightInKg weight) -> [ ( "weight", float weight ) ])


encodePhoto : Photo -> List ( String, Value )
encodePhoto =
    encodeChildMeasurement
        (\photo ->
            [ ( "photo"
              , object
                    [ ( "styles"
                      , object
                            [ ( "patient-photo"
                              , string photo.url
                              )
                            ]
                      )
                    , ( "id", maybe int photo.fid )
                    ]
              )
            ]
        )


encodeNutrition : ChildNutrition -> List ( String, Value )
encodeNutrition =
    encodeChildMeasurement
        (\nutritions ->
            [ ( "nutrition_signs"
              , EverySet.toList nutritions
                    |> List.map encodeNutritionSign
                    |> list
              )
            ]
        )


encodeParticipantConsent : ParticipantConsent -> List ( String, Value )
encodeParticipantConsent =
    encodeMotherMeasurement
        (\consent ->
            [ ( "witness", encodeEntityId consent.witness )
            , ( "language", encodeLanguage consent.language )
            , ( "participant_form", encodeEntityId consent.formId )
            ]
        )


encodeCounselingSession : CounselingSession -> List ( String, Value )
encodeCounselingSession =
    encodeChildMeasurement
        (\( timing, topics ) ->
            [ ( "topics"
              , EverySet.toList topics
                    |> List.map encodeEntityId
                    |> list
              )
            , ( "timing"
              , encodeCounselingTiming timing
              )
            ]
        )


encodeFamilyPlanning : FamilyPlanning -> List ( String, Value )
encodeFamilyPlanning =
    encodeMotherMeasurement
        (\familyPlannings ->
            [ ( "family_planning_signs"
              , EverySet.toList familyPlannings
                    |> List.map encodeFamilyPlanningSign
                    |> list
              )
            ]
        )


encodeChildMeasurement : (value -> List ( String, Value )) -> Measurement (EntityId a) value -> List ( String, Value )
encodeChildMeasurement =
    encodeMeasurement "child"


encodeMotherMeasurement : (value -> List ( String, Value )) -> Measurement (EntityId a) value -> List ( String, Value )
encodeMotherMeasurement =
    encodeMeasurement "mother"


encodeMeasurement : String -> (value -> List ( String, Value )) -> Measurement (EntityId a) value -> List ( String, Value )
encodeMeasurement participantField encoder measurement =
    List.concat
        [ [ ( participantField, encodeEntityId measurement.participantId )
          , ( "session", maybe encodeEntityId measurement.sessionId )
          , ( "date_measured", Gizra.NominalDate.encodeYYYYMMDD measurement.dateMeasured )
          ]
        , encoder measurement.value
        ]


encodeNutritionSign : ChildNutritionSign -> Value
encodeNutritionSign =
    encodeNutritionSignAsString >> string


encodeNutritionSignAsString : ChildNutritionSign -> String
encodeNutritionSignAsString sign =
    case sign of
        AbdominalDistension ->
            "abdominal-distension"

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

        Implant ->
            "implant"

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
        , ( "participant_consent"
          , edits.consent
                |> List.map (encodeEdit (object << encodeParticipantConsent))
                |> list
          )
        , ( "checked_in", bool edits.explicitlyCheckedIn )
        ]


{-| The keys should match the machine name of the entity on the backend, in
order for the upload mechanism to work as expected.
-}
encodeChildEdits : ChildEdits -> Value
encodeChildEdits edits =
    object
        [ ( "height", encodeEdit (object << encodeHeight) edits.height )
        , ( "muac", encodeEdit (object << encodeMuac) edits.muac )
        , ( "nutrition", encodeEdit (object << encodeNutrition) edits.nutrition )
        , ( "counseling_session", encodeEdit (object << encodeCounselingSession) edits.counseling )
        , ( "photo", encodeEdit (object << encodePhoto) edits.photo )
        , ( "weight", encodeEdit (object << encodeWeight) edits.weight )
        ]


encodeEdit : (value -> Value) -> Edit (EntityId id) value -> Value
encodeEdit encodeValue edit =
    case edit of
        Unedited ->
            object [ ( "tag", string "unedited" ) ]

        Created value ->
            object
                [ ( "tag", string "created" )
                , ( "value", encodeValue value )
                ]

        Edited { id, backend, edited } ->
            object
                [ ( "tag", string "edited" )
                , ( "backend", encodeValue backend )
                , ( "edited", encodeValue edited )
                , ( "id", encodeEntityId id )
                ]

        Deleted id value ->
            object
                [ ( "tag", string "deleted" )
                , ( "value", encodeValue value )
                , ( "id", encodeEntityId id )
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
        , ( "counseling_session"
          , measurements.counselingSessions
                |> List.map (encodeEntity encodeCounselingSession)
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
        , ( "participant_consent"
          , measurements.consents
                |> List.map (encodeEntity encodeParticipantConsent)
                |> list
          )
        ]


encodeEntity : (b -> List ( String, Value )) -> ( EntityId a, b ) -> Value
encodeEntity encoder ( id, value ) =
    object <|
        ( "id", encodeEntityId id )
            :: encoder value
