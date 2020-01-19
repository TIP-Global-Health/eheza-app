module Backend.Measurement.Encoder exposing (encodeAttendance, encodeAttendanceValue, encodeChildMeasurementList, encodeCounselingSession, encodeCounselingSessionValue, encodeEntity, encodeFamilyPlanning, encodeFamilyPlanningSign, encodeFamilyPlanningSignAsString, encodeFamilyPlanningValue, encodeHeight, encodeHeightValue, encodeMeasurement, encodeMotherMeasurementList, encodeMuac, encodeMuacValue, encodeNutrition, encodeNutritionSign, encodeNutritionSignAsString, encodeNutritionValue, encodeParticipantConsent, encodeParticipantConsentValue, encodePhoto, encodePhotoUrl, encodeWeight, encodeWeightValue)

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
    encodeMeasurement encodeHeightValue


encodeHeightValue : HeightInCm -> List ( String, Value )
encodeHeightValue (HeightInCm height) =
    [ ( "height", float height ) ]


encodeMuac : Muac -> List ( String, Value )
encodeMuac =
    encodeMeasurement encodeMuacValue


encodeMuacValue : MuacInCm -> List ( String, Value )
encodeMuacValue (MuacInCm muac) =
    [ ( "muac", float muac ) ]


encodeWeight : Weight -> List ( String, Value )
encodeWeight =
    encodeMeasurement encodeWeightValue


encodeWeightValue : WeightInKg -> List ( String, Value )
encodeWeightValue (WeightInKg weight) =
    [ ( "weight", float weight ) ]


encodePhoto : Photo -> List ( String, Value )
encodePhoto =
    encodeMeasurement encodePhotoUrl


encodePhotoUrl : PhotoUrl -> List ( String, Value )
encodePhotoUrl (PhotoUrl url) =
    [ ( "photo", string url ) ]


encodeNutrition : ChildNutrition -> List ( String, Value )
encodeNutrition =
    encodeMeasurement encodeNutritionValue


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
    encodeMeasurement encodeParticipantConsentValue


encodeCounselingSession : CounselingSession -> List ( String, Value )
encodeCounselingSession =
    encodeMeasurement encodeCounselingSessionValue


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
    encodeMeasurement encodeAttendanceValue


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
    encodeMeasurement encodeFamilyPlanningValue


encodeMeasurement : (value -> List ( String, Value )) -> Measurement value -> List ( String, Value )
encodeMeasurement encoder measurement =
    List.concat
        [ [ ( "person", encodeEntityUuid measurement.participantId )
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
        AutoObservation ->
            "auto-observation"

        Condoms ->
            "condoms"

        CycleBeads ->
            "cycle-beads"

        CycleCounting ->
            "cycle-counting"

        Hysterectomy ->
            "hysterectomy"

        Implants ->
            "implant"

        Injectables ->
            "injection"

        IUD ->
            "iud"

        LactationAmenorrhea ->
            "lactation-amenorrhea"

        NoFamilyPlanning ->
            "none"

        OralContraceptives ->
            "oral-contraceptives"

        Spermicide ->
            "spermicide"

        TubalLigatures ->
            "tubal-ligatures"

        Vasectomy ->
            "vasectomy"


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
