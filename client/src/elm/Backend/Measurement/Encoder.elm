module Backend.Measurement.Encoder exposing (encodeAttendance, encodeAttendanceValue, encodeChildMeasurementList, encodeCounselingSession, encodeCounselingSessionValue, encodeDistributionNotice, encodeDistributionNoticeAsString, encodeEntity, encodeFamilyPlanning, encodeFamilyPlanningSign, encodeFamilyPlanningSignAsString, encodeFamilyPlanningValue, encodeFbf, encodeFbfValue, encodeHeight, encodeHeightValue, encodeLactation, encodeLactationSign, encodeLactationValue, encodeMeasurement, encodeMotherMeasurementList, encodeMuac, encodeMuacValue, encodeNutrition, encodeNutritionSign, encodeNutritionSignAsString, encodeNutritionValue, encodeParticipantConsent, encodeParticipantConsentValue, encodePhoto, encodePhotoUrl, encodeWeight, encodeWeightValue)

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
    encodeMeasurement encodeParticipantConsentValue


encodeCounselingSession : CounselingSession -> List ( String, Value )
encodeCounselingSession =
    encodeMeasurement encodeCounselingSessionValue


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
    encodeMeasurement encodeAttendanceValue


encodeFamilyPlanningValue : EverySet FamilyPlanningSign -> List ( String, Value )
encodeFamilyPlanningValue familyPlannings =
    [ ( "family_planning_signs"
      , EverySet.toList familyPlannings
            |> list encodeFamilyPlanningSign
      )
    ]


encodeFamilyPlanning : FamilyPlanning -> List ( String, Value )
encodeFamilyPlanning =
    encodeMeasurement encodeFamilyPlanningValue


encodeLactationValue : EverySet LactationSign -> List ( String, Value )
encodeLactationValue signs =
    [ ( "lactation_signs"
      , EverySet.toList signs
            |> list encodeLactationSign
      )
    ]


encodeLactation : Lactation -> List ( String, Value )
encodeLactation =
    encodeMeasurement encodeLactationValue


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
            "necklace"

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
            "pill"

        Spermicide ->
            "spermicide"

        TubalLigatures ->
            "tubal-ligatures"

        Vasectomy ->
            "vasectomy"


encodeLactationSign : LactationSign -> Value
encodeLactationSign sign =
    case sign of
        Breastfeeding ->
            string "breastfeeding"

        NoLactationSigns ->
            string "none"


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


encodeFbf : Fbf -> List ( String, Value )
encodeFbf =
    encodeMeasurement encodeFbfValue


encodeFbfValue : FbfValue -> List ( String, Value )
encodeFbfValue value =
    [ ( "distributed_amount", float value.distributedAmount )
    , ( "distribution_notice", encodeDistributionNotice value.distributionNotice )
    ]


encodeDistributionNotice : DistributionNotice -> Value
encodeDistributionNotice =
    encodeDistributionNoticeAsString >> string


encodeDistributionNoticeAsString : DistributionNotice -> String
encodeDistributionNoticeAsString notice =
    case notice of
        DistributedFully ->
            "complete"

        DistributedPartiallyLackOfStock ->
            "lack-of-stock"

        DistributedPartiallyOther ->
            "other"


encodeEntity : (b -> List ( String, Value )) -> ( EntityUuid a, b ) -> Value
encodeEntity encoder ( id, value ) =
    object <|
        ( "id", encodeEntityUuid id )
            :: encoder value
