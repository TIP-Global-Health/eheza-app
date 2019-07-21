module Backend.Measurement.Encoder exposing (encodeAttendance, encodeAttendanceValue, encodeChildMeasurement, encodeChildMeasurementList, encodeCounselingSession, encodeCounselingSessionValue, encodeEntity, encodeFamilyPlanning, encodeFamilyPlanningSign, encodeFamilyPlanningSignAsString, encodeFamilyPlanningValue, encodeHeight, encodeHeightValue, encodeMeasurement, encodeMotherMeasurement, encodeMotherMeasurementList, encodeMuac, encodeMuacValue, encodeNutrition, encodeNutritionSign, encodeNutritionSignAsString, encodeNutritionValue, encodeParticipantConsent, encodeParticipantConsentValue, encodePhoto, encodePhotoUrl, encodeWeight, encodeWeightValue)

import AssocList as Dict
import Backend.Counseling.Encoder exposing (encodeCounselingTiming)
import Backend.Counseling.Model exposing (CounselingTiming)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate
import Json.Encode as Encoder exposing (Value, bool, float, int, list, object, string)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (EntityUuid(..), encodeEntityUuid, fromEntityUuid)
import Translate.Utils exposing (encodeLanguage)


encodeHeight : Height -> List ( String, Value )
encodeHeight =
    encodeChildMeasurement encodeHeightValue


encodeHeightValue : HeightInCm -> List ( String, Value )
encodeHeightValue (HeightInCm height) =
    [ ( "height", float height ) ]


encodeMuac : Muac -> List ( String, Value )
encodeMuac =
    encodeChildMeasurement encodeMuacValue


encodeMuacValue : MuacInCm -> List ( String, Value )
encodeMuacValue (MuacInCm muac) =
    [ ( "muac", float muac ) ]


encodeWeight : Weight -> List ( String, Value )
encodeWeight =
    encodeChildMeasurement encodeWeightValue


encodeWeightValue : WeightInKg -> List ( String, Value )
encodeWeightValue (WeightInKg weight) =
    [ ( "weight", float weight ) ]


encodePhoto : Photo -> List ( String, Value )
encodePhoto =
    encodeChildMeasurement encodePhotoUrl


encodePhotoUrl : PhotoUrl -> List ( String, Value )
encodePhotoUrl (PhotoUrl url) =
    [ ( "photo", string url ) ]


encodeNutrition : ChildNutrition -> List ( String, Value )
encodeNutrition =
    encodeChildMeasurement encodeNutritionValue


encodeNutritionValue : EverySet ChildNutritionSign -> List ( String, Value )
encodeNutritionValue nutritions =
    [ ( "nutrition_signs"
      , EverySet.toList nutritions
            |> list encodeNutritionSign
      )
    ]


encodeParticipantConsentValue : ParticipantConsentValue -> List ( String, Value )
encodeParticipantConsentValue consent =
    [ ( "language", encodeLanguage consent.language )
    , ( "participant_form", encodeEntityUuid consent.formId )
    ]


encodeParticipantConsent : ParticipantConsent -> List ( String, Value )
encodeParticipantConsent =
    encodeMotherMeasurement encodeParticipantConsentValue


encodeCounselingSession : CounselingSession -> List ( String, Value )
encodeCounselingSession =
    encodeChildMeasurement encodeCounselingSessionValue


encodeCounselingSessionValue : ( CounselingTiming, EverySet CounselingTopicId ) -> List ( String, Value )
encodeCounselingSessionValue ( timing, topics ) =
    [ ( "topics"
      , EverySet.toList topics
            |> list encodeEntityUuid
      )
    , ( "timing"
      , encodeCounselingTiming timing
      )
    ]


encodeAttendanceValue : Bool -> List ( String, Value )
encodeAttendanceValue attended =
    [ ( "attended", bool attended ) ]


encodeAttendance : Attendance -> List ( String, Value )
encodeAttendance =
    encodeMotherMeasurement encodeAttendanceValue


encodeFamilyPlanningValue : EverySet FamilyPlanningSign -> List ( String, Value )
encodeFamilyPlanningValue familyPlannings =
    [ ( "family_planning_signs"
      , EverySet.toList familyPlannings
            |> list encodeFamilyPlanningSign
      )
    ]


encodeFamilyPlanning : FamilyPlanning -> List ( String, Value )
encodeFamilyPlanning =
    encodeMotherMeasurement encodeFamilyPlanningValue


encodeChildMeasurement : (value -> List ( String, Value )) -> Measurement (EntityUuid a) value -> List ( String, Value )
encodeChildMeasurement =
    encodeMeasurement "person"


encodeMotherMeasurement : (value -> List ( String, Value )) -> Measurement (EntityUuid a) value -> List ( String, Value )
encodeMotherMeasurement =
    encodeMeasurement "person"


encodeMeasurement : String -> (value -> List ( String, Value )) -> Measurement (EntityUuid a) value -> List ( String, Value )
encodeMeasurement participantField encoder measurement =
    List.concat
        [ [ ( participantField, encodeEntityUuid measurement.participantId )
          , ( "session", maybe encodeEntityUuid measurement.sessionId )
          , ( "date_measured", Gizra.NominalDate.encodeYYYYMMDD measurement.dateMeasured )
          , ( "nurse", maybe encodeEntityUuid measurement.nurse )
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
                |> Dict.toList
                |> list (encodeEntity encodeHeight)
          )
        , ( "muac"
          , measurements.muacs
                |> Dict.toList
                |> list (encodeEntity encodeMuac)
          )
        , ( "nutrition"
          , measurements.nutritions
                |> Dict.toList
                |> list (encodeEntity encodeNutrition)
          )
        , ( "counseling_session"
          , measurements.counselingSessions
                |> Dict.toList
                |> list (encodeEntity encodeCounselingSession)
          )
        , ( "photo"
          , measurements.photos
                |> Dict.toList
                |> list (encodeEntity encodePhoto)
          )
        , ( "weight"
          , measurements.weights
                |> Dict.toList
                |> list (encodeEntity encodeWeight)
          )
        ]


encodeMotherMeasurementList : MotherMeasurementList -> Value
encodeMotherMeasurementList measurements =
    object
        [ ( "attendance"
          , measurements.attendances
                |> Dict.toList
                |> list (encodeEntity encodeAttendance)
          )
        , ( "family_planning"
          , measurements.familyPlannings
                |> Dict.toList
                |> list (encodeEntity encodeFamilyPlanning)
          )
        , ( "participant_consent"
          , measurements.consents
                |> Dict.toList
                |> list (encodeEntity encodeParticipantConsent)
          )
        ]


encodeEntity : (b -> List ( String, Value )) -> ( EntityUuid a, b ) -> Value
encodeEntity encoder ( id, value ) =
    object <|
        ( "id", encodeEntityUuid id )
            :: encoder value
