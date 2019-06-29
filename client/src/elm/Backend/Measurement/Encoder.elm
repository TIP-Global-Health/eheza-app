module Backend.Measurement.Encoder exposing
    ( encodeAttendance
    , encodeAttendanceValue
    , encodeCounselingSession
    , encodeCounselingSessionValue
    , encodeFamilyPlanning
    , encodeFamilyPlanningSignAsString
    , encodeFamilyPlanningValue
    , encodeHeight
    , encodeHeightValue
    , encodeMuac
    , encodeMuacValue
    , encodeNutrition
    , encodeNutritionSignAsString
    , encodeNutritionValue
    , encodeParticipantConsent
    , encodeParticipantConsentValue
    , encodePhoto
    , encodePhotoUrl
    , encodeWeight
    , encodeWeightValue
    )

import AllDictList
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
    encodeGroupMeasurement encodeHeightValue


encodeHeightValue : HeightInCm -> List ( String, Value )
encodeHeightValue (HeightInCm height) =
    [ ( "height", float height ) ]


encodeMuac : Muac -> List ( String, Value )
encodeMuac =
    encodeGroupMeasurement encodeMuacValue


encodeMuacValue : MuacInCm -> List ( String, Value )
encodeMuacValue (MuacInCm muac) =
    [ ( "muac", float muac ) ]


encodeWeight : Weight -> List ( String, Value )
encodeWeight =
    encodeGroupMeasurement encodeWeightValue


encodeWeightValue : WeightInKg -> List ( String, Value )
encodeWeightValue (WeightInKg weight) =
    [ ( "weight", float weight ) ]


encodePhoto : Photo -> List ( String, Value )
encodePhoto =
    encodeGroupMeasurement encodePhotoUrl


encodePhotoUrl : PhotoUrl -> List ( String, Value )
encodePhotoUrl (PhotoUrl url) =
    [ ( "photo", string url ) ]


encodeNutrition : ChildNutrition -> List ( String, Value )
encodeNutrition =
    encodeGroupMeasurement encodeNutritionValue


encodeNutritionValue : EverySet ChildNutritionSign -> List ( String, Value )
encodeNutritionValue nutritions =
    [ ( "nutrition_signs"
      , EverySet.toList nutritions
            |> List.map encodeNutritionSign
            |> list
      )
    ]


encodeParticipantConsentValue : ParticipantConsentValue -> List ( String, Value )
encodeParticipantConsentValue consent =
    [ ( "language", encodeLanguage consent.language )
    , ( "participant_form", encodeEntityUuid consent.formId )
    ]


encodeParticipantConsent : ParticipantConsent -> List ( String, Value )
encodeParticipantConsent =
    encodeGroupMeasurement encodeParticipantConsentValue


encodeCounselingSession : CounselingSession -> List ( String, Value )
encodeCounselingSession =
    encodeGroupMeasurement encodeCounselingSessionValue


encodeCounselingSessionValue : ( CounselingTiming, EverySet CounselingTopicId ) -> List ( String, Value )
encodeCounselingSessionValue ( timing, topics ) =
    [ ( "topics"
      , EverySet.toList topics
            |> List.map encodeEntityUuid
            |> list
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
    encodeGroupMeasurement encodeAttendanceValue


encodeFamilyPlanningValue : EverySet FamilyPlanningSign -> List ( String, Value )
encodeFamilyPlanningValue familyPlannings =
    [ ( "family_planning_signs"
      , EverySet.toList familyPlannings
            |> List.map encodeFamilyPlanningSign
            |> list
      )
    ]


encodeFamilyPlanning : FamilyPlanning -> List ( String, Value )
encodeFamilyPlanning =
    encodeGroupMeasurement encodeFamilyPlanningValue


encodeGroupMeasurement : (value -> List ( String, Value )) -> GroupMeasurement value -> List ( String, Value )
encodeGroupMeasurement =
    encodeMeasurement "session"


encodePrenatalMeasurement : (value -> List ( String, Value )) -> PrenatalMeasurement value -> List ( String, Value )
encodePrenatalMeasurement =
    encodeMeasurement "prenatal_encounter"


encodeMeasurement : String -> (value -> List ( String, Value )) -> Measurement (EntityUuid a) value -> List ( String, Value )
encodeMeasurement encounterTag encoder measurement =
    List.concat
        [ [ ( "person", encodeEntityUuid measurement.participantId )
          , ( encounterTag, maybe encodeEntityUuid measurement.encounterId )
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
                |> AllDictList.toList
                |> List.map (encodeEntity encodeHeight)
                |> list
          )
        , ( "muac"
          , measurements.muacs
                |> AllDictList.toList
                |> List.map (encodeEntity encodeMuac)
                |> list
          )
        , ( "nutrition"
          , measurements.nutritions
                |> AllDictList.toList
                |> List.map (encodeEntity encodeNutrition)
                |> list
          )
        , ( "counseling_session"
          , measurements.counselingSessions
                |> AllDictList.toList
                |> List.map (encodeEntity encodeCounselingSession)
                |> list
          )
        , ( "photo"
          , measurements.photos
                |> AllDictList.toList
                |> List.map (encodeEntity encodePhoto)
                |> list
          )
        , ( "weight"
          , measurements.weights
                |> AllDictList.toList
                |> List.map (encodeEntity encodeWeight)
                |> list
          )
        ]


encodeMotherMeasurementList : MotherMeasurementList -> Value
encodeMotherMeasurementList measurements =
    object
        [ ( "attendance"
          , measurements.attendances
                |> AllDictList.toList
                |> List.map (encodeEntity encodeAttendance)
                |> list
          )
        , ( "family_planning"
          , measurements.familyPlannings
                |> AllDictList.toList
                |> List.map (encodeEntity encodeFamilyPlanning)
                |> list
          )
        , ( "participant_consent"
          , measurements.consents
                |> AllDictList.toList
                |> List.map (encodeEntity encodeParticipantConsent)
                |> list
          )
        ]


encodeEntity : (b -> List ( String, Value )) -> ( EntityUuid a, b ) -> Value
encodeEntity encoder ( id, value ) =
    object <|
        ( "id", encodeEntityUuid id )
            :: encoder value
