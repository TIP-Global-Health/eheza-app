module Backend.Measurement.Encoder exposing (encodeAttendance, encodeAttendanceValue, encodeChildMeasurement, encodeChildMeasurementList, encodeCounselingSession, encodeEntity, encodeFamilyPlanning, encodeFamilyPlanningSign, encodeFamilyPlanningSignAsString, encodeHeight, encodeMeasurement, encodeMotherMeasurement, encodeMotherMeasurementList, encodeMuac, encodeNutrition, encodeNutritionSign, encodeNutritionSignAsString, encodeParticipantConsent, encodePhoto, encodeWeight)

import Backend.Counseling.Encoder exposing (encodeCounselingTiming)
import Backend.Measurement.Model exposing (..)
import EveryDict
import EveryDictList
import EverySet
import Gizra.NominalDate
import Json.Encode as Encoder exposing (Value, bool, float, int, list, object, string)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (EntityUuid(..), encodeEntityUuid, fromEntityUuid)
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
            [ ( "language", encodeLanguage consent.language )
            , ( "participant_form", encodeEntityUuid consent.formId )
            ]
        )


encodeCounselingSession : CounselingSession -> List ( String, Value )
encodeCounselingSession =
    encodeChildMeasurement
        (\( timing, topics ) ->
            [ ( "topics"
              , EverySet.toList topics
                    |> List.map encodeEntityUuid
                    |> list
              )
            , ( "timing"
              , encodeCounselingTiming timing
              )
            ]
        )


encodeAttendanceValue : Bool -> List ( String, Value )
encodeAttendanceValue attended =
    [ ( "attended", bool attended ) ]


encodeAttendance : Attendance -> List ( String, Value )
encodeAttendance =
    encodeMotherMeasurement encodeAttendanceValue


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


encodeChildMeasurement : (value -> List ( String, Value )) -> Measurement (EntityUuid a) value -> List ( String, Value )
encodeChildMeasurement =
    encodeMeasurement "child"


encodeMotherMeasurement : (value -> List ( String, Value )) -> Measurement (EntityUuid a) value -> List ( String, Value )
encodeMotherMeasurement =
    encodeMeasurement "mother"


encodeMeasurement : String -> (value -> List ( String, Value )) -> Measurement (EntityUuid a) value -> List ( String, Value )
encodeMeasurement participantField encoder measurement =
    List.concat
        [ [ ( participantField, encodeEntityUuid measurement.participantId )
          , ( "session", maybe encodeEntityUuid measurement.sessionId )
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


encodeChildMeasurementList : ChildMeasurementList -> Value
encodeChildMeasurementList measurements =
    object
        [ ( "height"
          , measurements.heights
                |> EveryDictList.toList
                |> List.map (encodeEntity encodeHeight)
                |> list
          )
        , ( "muac"
          , measurements.muacs
                |> EveryDictList.toList
                |> List.map (encodeEntity encodeMuac)
                |> list
          )
        , ( "nutrition"
          , measurements.nutritions
                |> EveryDictList.toList
                |> List.map (encodeEntity encodeNutrition)
                |> list
          )
        , ( "counseling_session"
          , measurements.counselingSessions
                |> EveryDictList.toList
                |> List.map (encodeEntity encodeCounselingSession)
                |> list
          )
        , ( "photo"
          , measurements.photos
                |> EveryDictList.toList
                |> List.map (encodeEntity encodePhoto)
                |> list
          )
        , ( "weight"
          , measurements.weights
                |> EveryDictList.toList
                |> List.map (encodeEntity encodeWeight)
                |> list
          )
        ]


encodeMotherMeasurementList : MotherMeasurementList -> Value
encodeMotherMeasurementList measurements =
    object
        [ ( "attendance"
          , measurements.attendances
                |> EveryDictList.toList
                |> List.map (encodeEntity encodeAttendance)
                |> list
          )
        , ( "family_planning"
          , measurements.familyPlannings
                |> EveryDictList.toList
                |> List.map (encodeEntity encodeFamilyPlanning)
                |> list
          )
        , ( "participant_consent"
          , measurements.consents
                |> EveryDictList.toList
                |> List.map (encodeEntity encodeParticipantConsent)
                |> list
          )
        ]


encodeEntity : (b -> List ( String, Value )) -> ( EntityUuid a, b ) -> Value
encodeEntity encoder ( id, value ) =
    object <|
        ( "id", encodeEntityUuid id )
            :: encoder value
