module Backend.Measurement.Encoder exposing (..)

import AssocList as Dict exposing (Dict)
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


encodeEverySet : (a -> Value) -> EverySet a -> Value
encodeEverySet encoder set =
    EverySet.toList set
        |> list encoder


encodeHeight : Height -> List ( String, Value )
encodeHeight =
    encodeGroupMeasurement encodeHeightValue


encodeNutritionHeight : NutritionHeight -> List ( String, Value )
encodeNutritionHeight =
    encodeNutritionMeasurement encodeHeightValue


encodeHeightValue : HeightInCm -> List ( String, Value )
encodeHeightValue (HeightInCm height) =
    [ ( "height", float height ) ]


encodeMuac : Muac -> List ( String, Value )
encodeMuac =
    encodeGroupMeasurement encodeMuacValue


encodeNutritionMuac : NutritionMuac -> List ( String, Value )
encodeNutritionMuac =
    encodeNutritionMeasurement encodeMuacValue


encodeMuacValue : MuacInCm -> List ( String, Value )
encodeMuacValue (MuacInCm muac) =
    [ ( "muac", float muac ) ]


encodeWeight : Weight -> List ( String, Value )
encodeWeight =
    encodeGroupMeasurement encodeWeightValue


encodeNutritionWeight : NutritionWeight -> List ( String, Value )
encodeNutritionWeight =
    encodeNutritionMeasurement encodeWeightValue


encodeWeightValue : WeightInKg -> List ( String, Value )
encodeWeightValue (WeightInKg weight) =
    [ ( "weight", float weight ) ]


encodePhoto : Photo -> List ( String, Value )
encodePhoto =
    encodeGroupMeasurement encodePhotoUrl


encodeNutritionPhoto : NutritionPhoto -> List ( String, Value )
encodeNutritionPhoto =
    encodeNutritionMeasurement encodePhotoUrl


encodePrenatalPhoto : PrenatalPhoto -> List ( String, Value )
encodePrenatalPhoto =
    encodePrenatalMeasurement encodePhotoUrl


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


encodeNutritionNutrition : NutritionNutrition -> List ( String, Value )
encodeNutritionNutrition =
    encodeNutritionMeasurement encodeNutritionValue


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


encodeLactationValue : EverySet LactationSign -> List ( String, Value )
encodeLactationValue signs =
    [ ( "lactation_signs"
      , EverySet.toList signs
            |> list encodeLactationSign
      )
    ]


encodeLactation : Lactation -> List ( String, Value )
encodeLactation =
    encodeGroupMeasurement encodeLactationValue


encodeGroupMeasurement : (value -> List ( String, Value )) -> GroupMeasurement value -> List ( String, Value )
encodeGroupMeasurement =
    encodeMeasurement "session"


encodePrenatalMeasurement : (value -> List ( String, Value )) -> PrenatalMeasurement value -> List ( String, Value )
encodePrenatalMeasurement =
    encodeMeasurement "prenatal_encounter"


encodeNutritionMeasurement : (value -> List ( String, Value )) -> NutritionMeasurement value -> List ( String, Value )
encodeNutritionMeasurement =
    encodeMeasurement "nutrition_encounter"


encodeAcuteIllnessMeasurement : (value -> List ( String, Value )) -> AcuteIllnessMeasurement value -> List ( String, Value )
encodeAcuteIllnessMeasurement =
    encodeMeasurement "acute_illness_encounter"


encodeMeasurement : String -> (value -> List ( String, Value )) -> Measurement (EntityUuid a) value -> List ( String, Value )
encodeMeasurement encounterTag encoder measurement =
    List.concat
        [ [ ( "person", encodeEntityUuid measurement.participantId )
          , ( encounterTag, maybe encodeEntityUuid measurement.encounterId )
          , ( "date_measured", Gizra.NominalDate.encodeYYYYMMDD measurement.dateMeasured )
          , ( "nurse", maybe encodeEntityUuid measurement.nurse )
          , ( "health_center", maybe encodeEntityUuid measurement.healthCenter )
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
            IrregularRhythm ->
                "irregular-rhythm"

            NormalRateAndRhythm ->
                "normal-rate-and-rhythm"

            SinusTachycardia ->
                "sinus-tachycardia"


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
            Hepatomegaly ->
                "hepatomegaly"

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
    , ( "heart_murmur", bool value.heartMurmur )
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
    encodePrenatalMeasurement encodeLastMenstrualPeriodValue


encodeLastMenstrualPeriodValue : LastMenstrualPeriodValue -> List ( String, Value )
encodeLastMenstrualPeriodValue value =
    [ ( "last_menstrual_period", Gizra.NominalDate.encodeYYYYMMDD value.date )
    , ( "confident", bool value.confident )
    ]


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

            MentalHealthHistory ->
                "mental-health-history"

            NoMedicalHistorySigns ->
                "none"


encodeMedicalHistory : MedicalHistory -> List ( String, Value )
encodeMedicalHistory =
    encodePrenatalMeasurement encodeMedicalHistoryValue


encodeMedicalHistoryValue : EverySet MedicalHistorySign -> List ( String, Value )
encodeMedicalHistoryValue value =
    [ ( "medical_history", encodeEverySet encodeMedicalHistorySign value ) ]


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
    encodePrenatalMeasurement encodeMedicationValue


encodeFbf : Fbf -> List ( String, Value )
encodeFbf =
    encodeGroupMeasurement encodeFbfValue


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


encodeMedicationValue : EverySet MedicationSign -> List ( String, Value )
encodeMedicationValue value =
    [ ( "medication", encodeEverySet encodeMedicationSign value ) ]


encodeFetalPresentation : FetalPresentation -> Value
encodeFetalPresentation sign =
    string <|
        case sign of
            Transverse ->
                "transverse"

            Cephalic ->
                "cephalic"

            FetalBreech ->
                "breech"

            Twins ->
                "twins"

            Unknown ->
                "unknown"


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
    , ( "c_section_scar", encodeCSectionScar value.cSectionScar )
    ]


encodeObstetricalExam : ObstetricalExam -> List ( String, Value )
encodeObstetricalExam =
    encodePrenatalMeasurement encodeObstetricalExamValue


encodeObstetricHistory : ObstetricHistory -> List ( String, Value )
encodeObstetricHistory =
    encodePrenatalMeasurement encodeObstetricHistoryValue


encodeObstetricHistoryValue : ObstetricHistoryValue -> List ( String, Value )
encodeObstetricHistoryValue value =
    [ ( "currently_pregnant", bool value.currentlyPregnant )
    , ( "term_pregnancy", int value.termPregnancy )
    , ( "preterm_pregnancy", int value.preTermPregnancy )
    , ( "stillbirths_at_term", int value.stillbirthsAtTerm )
    , ( "stillbirths_preterm", int value.stillbirthsPreTerm )
    , ( "abortions", int value.abortions )
    , ( "live_children", int value.liveChildren )
    ]


encodeCSectionReason : CSectionReason -> Value
encodeCSectionReason sign =
    string <|
        case sign of
            Breech ->
                "breech"

            Emergency ->
                "emergency"

            FailureToProgress ->
                "failure-to-progress"

            None ->
                "none"

            Other ->
                "other"


encodeCSectionScar : CSectionScar -> Value
encodeCSectionScar sign =
    string <|
        case sign of
            Vertical ->
                "vertical"

            Horizontal ->
                "horizontal"

            NoScar ->
                "none"


encodePreviousDeliveryPeriod : PreviousDeliveryPeriod -> Value
encodePreviousDeliveryPeriod sign =
    string <|
        case sign of
            LessThan18Month ->
                "less-than-18-month"

            MoreThan5Years ->
                "more-than-5-years"

            Neither ->
                "neither"


encodePreviousDeliverySign : PreviousDeliverySign -> Value
encodePreviousDeliverySign sign =
    string <|
        case sign of
            CSectionInPreviousDelivery ->
                "c-section-in-previous-delivery"

            StillbornPreviousDelivery ->
                "stillborn-previous-delivery"

            BabyDiedOnDayOfBirthPreviousDelivery ->
                "baby-died-on-day-of-birth-previous-delivery"

            PartialPlacentaPreviousDelivery ->
                "partial-placenta-previous-delivery"

            SevereHemorrhagingPreviousDelivery ->
                "severe-hemorrhaging-previous-delivery"

            ConvulsionsPreviousDelivery ->
                "convulsions-previous-delivery"

            ConvulsionsAndUnconsciousPreviousDelivery ->
                "convulsions-and-unconscious-previous-delivery"

            NoPreviousDeliverySign ->
                "none"


encodeObstetricHistorySign : ObstetricHistorySign -> Value
encodeObstetricHistorySign sign =
    string <|
        case sign of
            SuccessiveAbortions ->
                "successive-abortions"

            SuccessivePrematureDeliveries ->
                "successive-premature-deliveries"

            PreeclampsiaPreviousPregnancy ->
                "preeclampsia-previous-pregnancy"

            GestationalDiabetesPreviousPregnancy ->
                "gestational-diabetes-previous-pregnancy"

            IncompleteCervixPreviousPregnancy ->
                "incomplete-cervix-previous-pregnancy"

            RhNegative ->
                "rh-negative"

            NoObstetricHistorySign ->
                "none"


encodeObstetricHistoryStep2 : ObstetricHistoryStep2 -> List ( String, Value )
encodeObstetricHistoryStep2 =
    encodePrenatalMeasurement encodeObstetricHistoryStep2Value


encodeObstetricHistoryStep2Value : ObstetricHistoryStep2Value -> List ( String, Value )
encodeObstetricHistoryStep2Value value =
    [ ( "c_sections", int value.cSections )
    , ( "c_section_reason", encodeEverySet encodeCSectionReason value.cSectionReason )
    , ( "obstetric_history", encodeEverySet encodeObstetricHistorySign value.obstetricHistory )
    , ( "previous_delivery", encodeEverySet encodePreviousDeliverySign value.previousDelivery )
    , ( "previous_delivery_period", encodeEverySet encodePreviousDeliveryPeriod value.previousDeliveryPeriod )
    ]


encodePrenatalFamilyPlanning : PrenatalFamilyPlanning -> List ( String, Value )
encodePrenatalFamilyPlanning =
    encodePrenatalMeasurement encodeFamilyPlanningValue


encodePrenatalNutrition : PrenatalNutrition -> List ( String, Value )
encodePrenatalNutrition =
    encodePrenatalMeasurement encodePrenatalNutritionValue


encodePrenatalNutritionValue : PrenatalNutritionValue -> List ( String, Value )
encodePrenatalNutritionValue value =
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
    encodePrenatalMeasurement encodeResourceValue


encodeResourceValue : EverySet ResourceSign -> List ( String, Value )
encodeResourceValue value =
    [ ( "resources", encodeEverySet encodeResourceSign value ) ]


encodeSocialHistorySign : SocialHistorySign -> Value
encodeSocialHistorySign sign =
    string <|
        case sign of
            AccompaniedByPartner ->
                "accompanied-by-partner"

            PartnerHivCounseling ->
                "partner-hiv-counseling"

            NoSocialHistorySign ->
                "none"


socialHistoryHivTestingResultToString : SocialHistoryHivTestingResult -> String
socialHistoryHivTestingResultToString result =
    case result of
        ResultHivPositive ->
            "positive"

        ResultHivNegative ->
            "negative"

        ResultHivIndeterminate ->
            "indeterminate"

        NoHivTesting ->
            "none"


encodeSocialHistoryHivTestingResult : SocialHistoryHivTestingResult -> Value
encodeSocialHistoryHivTestingResult result =
    socialHistoryHivTestingResultToString result |> string


encodeSocialHistory : SocialHistory -> List ( String, Value )
encodeSocialHistory =
    encodePrenatalMeasurement encodeSocialHistoryValue


encodeSocialHistoryValue : SocialHistoryValue -> List ( String, Value )
encodeSocialHistoryValue value =
    [ ( "social_history", encodeEverySet encodeSocialHistorySign value.socialHistory )
    , ( "partner_hiv_testing", encodeSocialHistoryHivTestingResult value.hivTestingResult )
    ]


encodeVitals : Vitals -> List ( String, Value )
encodeVitals =
    encodePrenatalMeasurement encodeVitalsValue


encodeVitalsValue : VitalsValue -> List ( String, Value )
encodeVitalsValue value =
    [ ( "sys", float value.sys )
    , ( "dia", float value.dia )
    , ( "heart_rate", int value.heartRate )
    , ( "respiratory_rate", int value.respiratoryRate )
    , ( "body_temperature", float value.bodyTemperature )
    ]


encodeSymptomsGeneral : SymptomsGeneral -> List ( String, Value )
encodeSymptomsGeneral =
    encodeAcuteIllnessMeasurement encodeSymptomsGeneralValue


encodeSymptomsGeneralValue : Dict SymptomsGeneralSign Int -> List ( String, Value )
encodeSymptomsGeneralValue signs =
    let
        fever =
            Dict.get SymptomGeneralFever signs |> Maybe.withDefault 0

        chills =
            Dict.get Chills signs |> Maybe.withDefault 0

        nightSweats =
            Dict.get NightSweats signs |> Maybe.withDefault 0

        bodyAches =
            Dict.get BodyAches signs |> Maybe.withDefault 0

        headache =
            Dict.get Headache signs |> Maybe.withDefault 0

        lethargy =
            Dict.get Lethargy signs |> Maybe.withDefault 0

        poorSuck =
            Dict.get PoorSuck signs |> Maybe.withDefault 0

        unableToDrink =
            Dict.get UnableToDrink signs |> Maybe.withDefault 0

        unableToEat =
            Dict.get UnableToEat signs |> Maybe.withDefault 0

        increasedThirst =
            Dict.get IncreasedThirst signs |> Maybe.withDefault 0

        dryMouth =
            Dict.get DryMouth signs |> Maybe.withDefault 0

        severeWeakness =
            Dict.get SevereWeakness signs |> Maybe.withDefault 0

        yellowEyes =
            Dict.get YellowEyes signs |> Maybe.withDefault 0

        cokeColoredUrine =
            Dict.get CokeColoredUrine signs |> Maybe.withDefault 0

        convulsions =
            Dict.get SymptomsGeneralConvulsions signs |> Maybe.withDefault 0

        spontaneousBleeding =
            Dict.get SpontaneousBleeding signs |> Maybe.withDefault 0
    in
    [ ( "fever_period", int fever )
    , ( "chills_period", int chills )
    , ( "night_sweats_period", int nightSweats )
    , ( "body_aches_period", int bodyAches )
    , ( "headache_period", int headache )
    , ( "coke_colored_urine_period", int cokeColoredUrine )
    , ( "convulsions_period", int convulsions )
    , ( "dry_mouth_period", int dryMouth )
    , ( "increased_thirst_period", int increasedThirst )
    , ( "lethargy_period", int lethargy )
    , ( "poor_suck_period", int poorSuck )
    , ( "severe_weakness_period", int severeWeakness )
    , ( "spontaneos_bleeding_period", int spontaneousBleeding )
    , ( "unable_to_drink_period", int unableToDrink )
    , ( "unable_to_eat_period", int unableToEat )
    , ( "yellow_eyes_period", int yellowEyes )
    ]


encodeSymptomsRespiratory : SymptomsRespiratory -> List ( String, Value )
encodeSymptomsRespiratory =
    encodeAcuteIllnessMeasurement encodeSymptomsRespiratoryValue


encodeSymptomsRespiratoryValue : Dict SymptomsRespiratorySign Int -> List ( String, Value )
encodeSymptomsRespiratoryValue signs =
    let
        cough =
            Dict.get Cough signs |> Maybe.withDefault 0

        shortnessOfBreath =
            Dict.get ShortnessOfBreath signs |> Maybe.withDefault 0

        nasalCongestion =
            Dict.get NasalCongestion signs |> Maybe.withDefault 0

        bloodInSputum =
            Dict.get BloodInSputum signs |> Maybe.withDefault 0

        soreThroat =
            Dict.get SoreThroat signs |> Maybe.withDefault 0

        lossOfSmell =
            Dict.get LossOfSmell signs |> Maybe.withDefault 0

        stabbingChestPain =
            Dict.get StabbingChestPain signs |> Maybe.withDefault 0
    in
    [ ( "cough_period", int cough )
    , ( "shortness_of_breath_period", int shortnessOfBreath )
    , ( "nasal_congestion_period", int nasalCongestion )
    , ( "blood_in_sputum_period", int bloodInSputum )
    , ( "sore_throat_period", int soreThroat )
    , ( "loss_of_smell_period", int lossOfSmell )
    , ( "stabbing_chest_pain_period", int stabbingChestPain )
    ]


encodeSymptomsGI : SymptomsGI -> List ( String, Value )
encodeSymptomsGI =
    encodeAcuteIllnessMeasurement encodeSymptomsGIValue


encodeSymptomsGIValue : SymptomsGIValue -> List ( String, Value )
encodeSymptomsGIValue value =
    let
        bloodyDiarrhea =
            Dict.get BloodyDiarrhea value.signs |> Maybe.withDefault 0

        nonBloodyDiarrhea =
            Dict.get NonBloodyDiarrhea value.signs |> Maybe.withDefault 0

        nausea =
            Dict.get Nausea value.signs |> Maybe.withDefault 0

        vomiting =
            Dict.get Vomiting value.signs |> Maybe.withDefault 0

        abdominalPain =
            Dict.get SymptomGIAbdominalPain value.signs |> Maybe.withDefault 0
    in
    [ ( "bloody_diarrhea_period", int bloodyDiarrhea )
    , ( "non_bloody_diarrhea_period", int nonBloodyDiarrhea )
    , ( "nausea_period", int nausea )
    , ( "vomiting_period", int vomiting )
    , ( "abdominal_pain_period", int abdominalPain )
    , ( "symptoms_gi_derived_signs", encodeEverySet encodeSymptomsGIDerivedSigns value.derivedSigns )
    ]


encodeSymptomsGIDerivedSigns : SymptomsGIDerivedSign -> Value
encodeSymptomsGIDerivedSigns sign =
    string <|
        case sign of
            IntractableVomiting ->
                "intractable-vomiting"

            NoSymptomsGIDerived ->
                "none"


encodeAcuteIllnessVitals : AcuteIllnessVitals -> List ( String, Value )
encodeAcuteIllnessVitals =
    encodeAcuteIllnessMeasurement encodeAcuteIllnessVitalsValue


encodeAcuteIllnessVitalsValue : AcuteIllnessVitalsValue -> List ( String, Value )
encodeAcuteIllnessVitalsValue value =
    [ ( "respiratory_rate", int value.respiratoryRate )
    , ( "body_temperature", float value.bodyTemperature )
    ]


encodeAcuteFindings : AcuteFindings -> List ( String, Value )
encodeAcuteFindings =
    encodeAcuteIllnessMeasurement encodeAcuteFindingsValue


encodeAcuteFindingsValue : AcuteFindingsValue -> List ( String, Value )
encodeAcuteFindingsValue value =
    [ ( "findings_signs_general", encodeEverySet encodeAcuteFindingsGeneralSign value.signsGeneral )
    , ( "findings_signs_respiratory", encodeEverySet encodeAcuteFindingsRespiratorySign value.signsRespiratory )
    ]


encodeAcuteFindingsGeneralSign : AcuteFindingsGeneralSign -> Value
encodeAcuteFindingsGeneralSign sign =
    string <|
        case sign of
            LethargicOrUnconscious ->
                "lethargic-or-unconscious"

            AcuteFindingsPoorSuck ->
                "poor-suck"

            SunkenEyes ->
                "sunken-eyes"

            PoorSkinTurgor ->
                "poor-skin-turgor"

            Jaundice ->
                "jaundice"

            NoAcuteFindingsGeneralSigns ->
                "none"


encodeAcuteFindingsRespiratorySign : AcuteFindingsRespiratorySign -> Value
encodeAcuteFindingsRespiratorySign sign =
    string <|
        case sign of
            Stridor ->
                "stridor"

            NasalFlaring ->
                "nasal-flaring"

            SevereWheezing ->
                "severe-wheezing"

            SubCostalRetractions ->
                "sub-costal-retractions"

            NoAcuteFindingsRespiratorySigns ->
                "none"


encodeMalariaTesting : MalariaTesting -> List ( String, Value )
encodeMalariaTesting =
    encodeAcuteIllnessMeasurement encodeMalariaTestingValue


encodeMalariaTestingValue : MalariaRapidTestResult -> List ( String, Value )
encodeMalariaTestingValue value =
    [ ( "malaria_rapid_test", encodeMalariaRapidTestResult value ) ]


encodeMalariaRapidTestResult : MalariaRapidTestResult -> Value
encodeMalariaRapidTestResult =
    malariaRapidTestResultAsString >> string


malariaRapidTestResultAsString : MalariaRapidTestResult -> String
malariaRapidTestResultAsString sign =
    case sign of
        RapidTestPositive ->
            "positive"

        RapidTestPositiveAndPregnant ->
            "positive-and-pregnant"

        RapidTestNegative ->
            "negative"

        RapidTestIndeterminate ->
            "indeterminate"

        RapidTestUnableToRun ->
            "unable-to-run"


encodeSendToHC : SendToHC -> List ( String, Value )
encodeSendToHC =
    encodeAcuteIllnessMeasurement encodeSendToHCValue


encodeSendToHCValue : EverySet SendToHCSign -> List ( String, Value )
encodeSendToHCValue value =
    [ ( "send_to_hc", encodeEverySet encondeSendToHCSign value ) ]


encondeSendToHCSign : SendToHCSign -> Value
encondeSendToHCSign sign =
    string <|
        case sign of
            HandReferrerForm ->
                "referral-form"

            ReferToHealthCenter ->
                "refer-to-hc"

            NoSendToHCSigns ->
                "none"


encodeMedicationDistribution : MedicationDistribution -> List ( String, Value )
encodeMedicationDistribution =
    encodeAcuteIllnessMeasurement encodeMedicationDistributionValue


encodeMedicationDistributionValue : EverySet MedicationDistributionSign -> List ( String, Value )
encodeMedicationDistributionValue value =
    [ ( "prescribed_medication", encodeEverySet encondeMedicationDistributionSign value ) ]


encondeMedicationDistributionSign : MedicationDistributionSign -> Value
encondeMedicationDistributionSign sign =
    string <|
        case sign of
            Amoxicillin ->
                "amoxicillin"

            Coartem ->
                "coartem"

            ORS ->
                "ors"

            Zinc ->
                "zinc"

            LemonJuiceOrHoney ->
                "lemon-juice-or-honey"

            NoMedicationDistributionSigns ->
                "none"


encodeTravelHistory : TravelHistory -> List ( String, Value )
encodeTravelHistory =
    encodeAcuteIllnessMeasurement encodeTravelHistoryValue


encodeTravelHistoryValue : EverySet TravelHistorySign -> List ( String, Value )
encodeTravelHistoryValue value =
    [ ( "travel_history", encodeEverySet encodeTravelHistorySign value ) ]


encodeTravelHistorySign : TravelHistorySign -> Value
encodeTravelHistorySign sign =
    string <|
        case sign of
            COVID19Country ->
                "covid19-country"

            NoTravelHistorySigns ->
                "none"


encodeTreatmentReview : TreatmentReview -> List ( String, Value )
encodeTreatmentReview =
    encodeAcuteIllnessMeasurement encodeTreatmentReviewValue


encodeTreatmentReviewValue : EverySet TreatmentReviewSign -> List ( String, Value )
encodeTreatmentReviewValue value =
    [ ( "treatment_history", encodeEverySet encodeTreatmentReviewSign value ) ]


encodeTreatmentReviewSign : TreatmentReviewSign -> Value
encodeTreatmentReviewSign sign =
    string <|
        case sign of
            FeverPast6Hours ->
                "fever-past-six-hours"

            FeverPast6HoursHelped ->
                "fever-past-six-hours-helped"

            MalariaToday ->
                "malaria-today"

            MalariaTodayHelped ->
                "malaria-today-helped"

            MalariaWithinPastMonth ->
                "malaria-past-month"

            MalariaWithinPastMonthHelped ->
                "malaria-past-month-helped"

            NoTreatmentReviewSigns ->
                "none"


encodeExposure : Exposure -> List ( String, Value )
encodeExposure =
    encodeAcuteIllnessMeasurement encodeExposureValue


encodeExposureValue : EverySet ExposureSign -> List ( String, Value )
encodeExposureValue value =
    [ ( "exposure", encodeEverySet encodeExposureSign value ) ]


encodeExposureSign : ExposureSign -> Value
encodeExposureSign sign =
    string <|
        case sign of
            COVID19Symptoms ->
                "covid19-symptioms"

            NoExposureSigns ->
                "none"


encodeIsolation : Isolation -> List ( String, Value )
encodeIsolation =
    encodeAcuteIllnessMeasurement encodeIsolationValue


encodeIsolationValue : IsolationValue -> List ( String, Value )
encodeIsolationValue value =
    [ ( "isolation", encodeEverySet encodeIsolationSign value.signs )
    , ( "reason_for_not_isolating", encodeEverySet encodeReasonForNotIsolating value.reasonsForNotIsolating )
    ]


encodeIsolationSign : IsolationSign -> Value
encodeIsolationSign sign =
    string <|
        case sign of
            PatientIsolated ->
                "patient-isolated"

            SignOnDoor ->
                "sign-on-door"

            HealthEducation ->
                "health-education"

            NoIsolationSigns ->
                "none"


encodeReasonForNotIsolating : ReasonForNotIsolating -> Value
encodeReasonForNotIsolating reason =
    string <|
        case reason of
            NoSpace ->
                "no-space"

            TooIll ->
                "too-ill"

            CanNotSeparateFromFamily ->
                "can-not-separate"

            OtherReason ->
                "other"

            IsolationReasonNotApplicable ->
                "n-a"


encodeHCContact : HCContact -> List ( String, Value )
encodeHCContact =
    encodeAcuteIllnessMeasurement encodeHCContactValue


encodeHCContactValue : HCContactValue -> List ( String, Value )
encodeHCContactValue value =
    [ ( "hc_contact", encodeEverySet encodeHCContactSign value.signs )
    , ( "hc_recommendation", encodeEverySet encodeHCRecommendation value.hcRecommendations )
    , ( "site_recommendation", encodeEverySet encodeSiteRecommendation value.siteRecommendations )
    ]


encodeHCContactSign : HCContactSign -> Value
encodeHCContactSign sign =
    string <|
        case sign of
            Call114 ->
                "call-114"

            ContactSite ->
                "contact-site"

            NoHCContactSigns ->
                "none"


encodeHCRecommendation : HCRecommendation -> Value
encodeHCRecommendation recommendation =
    string <|
        case recommendation of
            SendToHealthCenter ->
                "send-to-hc"

            SendToRRTCenter ->
                "send-to-rrtc"

            SendToHospital ->
                "send-to-hospital"

            OtherHCRecommendation ->
                "other"

            NoneNoAnswer ->
                "none-no-answer"

            NoneBusySignal ->
                "none-busy-signal"

            NoneOtherHCRecommendation ->
                "none-other"


encodeSiteRecommendation : SiteRecommendation -> Value
encodeSiteRecommendation period =
    string <|
        case period of
            TeamComeToVillage ->
                "team-to-village"

            SendToSiteWithForm ->
                "send-with-form"

            OtherSiteRecommendation ->
                "other"

            NoneSentWithForm ->
                "none-sent-with-form"

            NonePatientRefused ->
                "none-patient-refused"

            NoneOtherSiteRecommendation ->
                "none-other"

            SiteRecommendationNotApplicable ->
                "n-a"
