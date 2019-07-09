module Backend.Measurement.Encoder exposing
    ( encodeAttendance
    , encodeAttendanceValue
    , encodeBreastExam
    , encodeCorePhysicalExam
    , encodeCounselingSession
    , encodeCounselingSessionValue
    , encodeDangerSigns
    , encodeFamilyPlanning
    , encodeFamilyPlanningSignAsString
    , encodeFamilyPlanningValue
    , encodeHeight
    , encodeHeightValue
    , encodeLastMenstrualPeriod
    , encodeMedicalHistory
    , encodeMedication
    , encodeMuac
    , encodeMuacValue
    , encodeNutrition
    , encodeNutritionSignAsString
    , encodeNutritionValue
    , encodeObstetricHistory
    , encodeObstetricalExam
    , encodeParticipantConsent
    , encodeParticipantConsentValue
    , encodePhoto
    , encodePhotoUrl
    , encodePrenatalFamilyPlanning
    , encodePrenatalNutrition
    , encodeResource
    , encodeSocialHistory
    , encodeVitals
    , encodeWeight
    , encodeWeightValue
    )

import Backend.Counseling.Encoder exposing (encodeCounselingTiming)
import Backend.Counseling.Model exposing (CounselingTiming)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EveryDictList
import EverySet exposing (EverySet)
import Gizra.NominalDate
import Json.Encode as Encoder exposing (Value, bool, float, int, list, object, string)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (EntityUuid(..), encodeEntityUuid, fromEntityUuid)
import Translate.Utils exposing (encodeLanguage)


encodeEverySet : (a -> Value) -> EverySet a -> Value
encodeEverySet encoder set =
    EverySet.toList set
        |> List.map encoder
        |> list


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
      , encodeEverySet encodeNutritionSign nutritions
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
      , encodeEverySet encodeEntityUuid topics
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
      , encodeEverySet encodeFamilyPlanningSign familyPlannings
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

        NormalChildNutrition ->
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


encodeBreastExamSign : BreastExamSign -> Value
encodeBreastExamSign sign =
    string <|
        case sign of
            Mass ->
                "mass"

            Discharge ->
                "discharge"

            Infection ->
                "infection"

            NormalBreast ->
                "normal"


encodeBreastExamValue : BreastExamValue -> List ( String, Value )
encodeBreastExamValue value =
    [ ( "breast", encodeEverySet encodeBreastExamSign value.exam )
    , ( "breast_self_exam", bool value.selfGuidance )
    ]


encodeBreastExam : BreastExam -> List ( String, Value )
encodeBreastExam =
    encodePrenatalMeasurement encodeBreastExamValue


encodeHairHeadCPESign : HairHeadCPESign -> Value
encodeHairHeadCPESign sign =
    string <|
        case sign of
            BrittleHairCPE ->
                "brittle-hair"

            NormalHairHead ->
                "normal"


encodeEyesCPESign : EyesCPESign -> Value
encodeEyesCPESign sign =
    string <|
        case sign of
            PaleConjuctiva ->
                "pale-conjuctiva"

            NormalEyes ->
                "normal"


encodeHeartCPESign : HeartCPESign -> Value
encodeHeartCPESign sign =
    string <|
        case sign of
            AbnormalHeart ->
                "abnormal"

            NormalHeart ->
                "normal"


encodeNeckCPESign : NeckCPESign -> Value
encodeNeckCPESign sign =
    string <|
        case sign of
            EnlargedThyroid ->
                "enlarged-thyroid"

            EnlargedLymphNodes ->
                "enlarged-lymph-nodes"

            NormalNeck ->
                "normal"


encodeAbdomenCPESign : AbdomenCPESign -> Value
encodeAbdomenCPESign sign =
    string <|
        case sign of
            Heptomegaly ->
                "heptomegaly"

            Splenomegaly ->
                "splenomegaly"

            TPRightUpper ->
                "tender-to-palpitation-right-upper"

            TPLeftUpper ->
                "tender-to-palpitation-left-upper"

            TPRightLower ->
                "tender-to-palpitation-right-lower"

            TPLeftLower ->
                "tender-to-palpitation-left-lower"

            Hernia ->
                "hernia"

            NormalAbdomen ->
                "normal"


encodeLungsCPESign : LungsCPESign -> Value
encodeLungsCPESign sign =
    string <|
        case sign of
            Wheezes ->
                "wheezes"

            Crackles ->
                "crackles"

            NormalLungs ->
                "normal"


encodeHandsCPESign : HandsCPESign -> Value
encodeHandsCPESign sign =
    string <|
        case sign of
            PallorHands ->
                "pallor"

            EdemaHands ->
                "edema"

            NormalHands ->
                "normal"


encodeLegsCPESign : LegsCPESign -> Value
encodeLegsCPESign sign =
    string <|
        case sign of
            PallorLegs ->
                "pallor"

            EdemaLegs ->
                "edema"

            NormalLegs ->
                "normal"


encodeCorePhysicalExamValue : CorePhysicalExamValue -> List ( String, Value )
encodeCorePhysicalExamValue value =
    [ ( "head_hair", encodeEverySet encodeHairHeadCPESign value.hairHead )
    , ( "eyes", encodeEverySet encodeEyesCPESign value.eyes )
    , ( "heart", encodeEverySet encodeHeartCPESign value.heart )
    , ( "neck", encodeEverySet encodeNeckCPESign value.neck )
    , ( "lungs", encodeEverySet encodeLungsCPESign value.lungs )
    , ( "abdomen", encodeEverySet encodeAbdomenCPESign value.abdomen )
    , ( "hands", encodeEverySet encodeHandsCPESign value.hands )
    , ( "legs", encodeEverySet encodeLegsCPESign value.legs )
    ]


encodeCorePhysicalExam : CorePhysicalExam -> List ( String, Value )
encodeCorePhysicalExam =
    encodePrenatalMeasurement encodeCorePhysicalExamValue


encodeDangerSign : DangerSign -> Value
encodeDangerSign sign =
    string <|
        case sign of
            VaginalBleeding ->
                "vaginal-bleeding"

            HeadacheBlurredVision ->
                "sever-headaches-with-blurred-vision"

            Convulsions ->
                "convulsions"

            AbdominalPain ->
                "abdominal-pain"

            DifficultyBreathing ->
                "difficulty-breathing"

            Fever ->
                "fever"

            ExtremeWeakness ->
                "extreme-weakness"

            NoDangerSign ->
                "none"


encodeDangerSignsValue : EverySet DangerSign -> List ( String, Value )
encodeDangerSignsValue value =
    [ ( "danger_signs", encodeEverySet encodeDangerSign value ) ]


encodeDangerSigns : DangerSigns -> List ( String, Value )
encodeDangerSigns =
    encodePrenatalMeasurement encodeDangerSignsValue


encodeLastMenstrualPeriod : LastMenstrualPeriod -> List ( String, Value )
encodeLastMenstrualPeriod =
    encodePrenatalMeasurement
        (\value ->
            [ ( "last_menstrual_period", Gizra.NominalDate.encodeYYYYMMDD value.date )
            , ( "confident", bool value.confident )
            ]
        )


encodeMedicalHistorySign : MedicalHistorySign -> Value
encodeMedicalHistorySign sign =
    string <|
        case sign of
            UterineMyoma ->
                "uterine-myonma"

            Diabetes ->
                "diabetes"

            CardiacDisease ->
                "cardiac-disease"

            RenalDisease ->
                "renal-disease"

            HypertensionBeforePregnancy ->
                "hypertension-before-pregnancy"

            TuberculosisPast ->
                "tuberculosis-past"

            TuberculosisPresent ->
                "tuberculosis-present"

            Asthma ->
                "asthma"

            BowedLegs ->
                "bowed-legs"

            HIV ->
                "hiv"

            NoMedicalHistorySigns ->
                "none"


encodeMedicalHistory : MedicalHistory -> List ( String, Value )
encodeMedicalHistory =
    encodePrenatalMeasurement
        (\value ->
            [ ( "medical_history", encodeEverySet encodeMedicalHistorySign value ) ]
        )


encodeMedicationSign : MedicationSign -> Value
encodeMedicationSign sign =
    string <|
        case sign of
            IronAndFolicAcidSupplement ->
                "iron-and-folic-acid-supplement"

            DewormingPill ->
                "deworming-pill"

            NoMedication ->
                "none"


encodeMedication : Medication -> List ( String, Value )
encodeMedication =
    encodePrenatalMeasurement
        (\value ->
            [ ( "medication", encodeEverySet encodeMedicationSign value ) ]
        )


encodeFetalPresentation : FetalPresentation -> Value
encodeFetalPresentation sign =
    string <|
        case sign of
            Transverse ->
                "transverse"

            Cephalic ->
                "cephalic"

            Breach ->
                "breach"


encodeHeightInCm : HeightInCm -> Value
encodeHeightInCm (HeightInCm cm) =
    float cm


encodeWeightInKg : WeightInKg -> Value
encodeWeightInKg (WeightInKg kg) =
    float kg


encodeMuacInCm : MuacInCm -> Value
encodeMuacInCm (MuacInCm cm) =
    float cm


encodeObstetricalExamValue : ObstetricalExamValue -> List ( String, Value )
encodeObstetricalExamValue value =
    [ ( "fundal_height", encodeHeightInCm value.fundalHeight )
    , ( "fetal_presentation", encodeFetalPresentation value.fetalPresentation )
    , ( "fetal_movement", bool value.fetalMovement )
    , ( "fetal_heart_rate", int value.fetalHeartRate )
    , ( "c_section_scar", bool value.cSectionScar )
    ]


encodeObstetricalExam : ObstetricalExam -> List ( String, Value )
encodeObstetricalExam =
    encodePrenatalMeasurement encodeObstetricalExamValue


encodeObstetricHistory : ObstetricHistory -> List ( String, Value )
encodeObstetricHistory =
    encodePrenatalMeasurement <|
        \value ->
            [ ( "currently_pregnant", bool value.currentlyPregnant )
            , ( "term_pregnancy", int value.termPregnancy )
            , ( "preterm_pregnancy", int value.pretermPregnancy )
            , ( "stillbirths_at_term", int value.stillBirthsAtTerm )
            , ( "stillbirths_preterm", int value.stillBirthsPreTerm )
            , ( "abortions", int value.abortions )
            , ( "live_children", int value.liveChildren )
            ]


encodePrenatalFamilyPlanning : PrenatalFamilyPlanning -> List ( String, Value )
encodePrenatalFamilyPlanning =
    encodePrenatalMeasurement <|
        \value ->
            [ ( "family_planning_signs", encodeEverySet encodeFamilyPlanningSign value ) ]


encodePrenatalNutrition : PrenatalNutrition -> List ( String, Value )
encodePrenatalNutrition =
    encodePrenatalMeasurement <|
        \value ->
            [ ( "height", encodeHeightInCm value.height )
            , ( "weight", encodeWeightInKg value.weight )
            , ( "muac", encodeMuacInCm value.muac )
            ]


encodeResourceSign : ResourceSign -> Value
encodeResourceSign sign =
    string <|
        case sign of
            MosquitoNet ->
                "mosquito-net"

            NoResource ->
                "none"


encodeResource : Resource -> List ( String, Value )
encodeResource =
    encodePrenatalMeasurement <|
        \value ->
            [ ( "resources", encodeEverySet encodeResourceSign value ) ]


encodeSocialHistorySign : SocialHistorySign -> Value
encodeSocialHistorySign sign =
    string <|
        case sign of
            AccompaniedByPartner ->
                "accompanied-by-partner"

            PartnerHivCounseling ->
                "partner-hiv-counseling"

            MentalHealthHistory ->
                "mental-health-history"

            NoSocialHistorySign ->
                "none"


encodeSocialHistory : SocialHistory -> List ( String, Value )
encodeSocialHistory =
    encodePrenatalMeasurement <|
        \value ->
            [ ( "social_history", encodeEverySet encodeSocialHistorySign value ) ]


encodeVitals : Vitals -> List ( String, Value )
encodeVitals =
    encodePrenatalMeasurement <|
        \value ->
            [ ( "sys", float value.sys )
            , ( "dia", float value.dia )
            , ( "heart_rate", int value.heartRate )
            , ( "respiratory_rate", int value.respiratoryRate )
            , ( "body_temperature", float value.bodyTemperature )
            ]
