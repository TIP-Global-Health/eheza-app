module Backend.Measurement.Decoder exposing
    ( decodeAttendance
    , decodeBreastExam
    , decodeChildMeasurementList
    , decodeCorePhysicalExam
    , decodeCounselingSession
    , decodeDangerSigns
    , decodeFamilyPlanning
    , decodeHeight
    , decodeLastMenstrualPeriod
    , decodeMedicalHistory
    , decodeMedication
    , decodeMotherMeasurementList
    , decodeMuac
    , decodeNutrition
    , decodeNutritionMeasurement
    , decodeNutritionMeasurements
    , decodeObstetricHistory
    , decodeObstetricHistoryStep2
    , decodeObstetricalExam
    , decodeParticipantConsent
    , decodePhoto
    , decodePrenatalFamilyPlanning
    , decodePrenatalMeasurements
    , decodePrenatalNutrition
    , decodePrenatalPhoto
    , decodeResource
    , decodeSocialHistory
    , decodeSocialHistoryHivTestingResult
    , decodeVitals
    , decodeWeight
    )

import AssocList as Dict exposing (Dict)
import Backend.Counseling.Decoder exposing (decodeCounselingTiming)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Gizra.Json exposing (decodeEmptyArrayAs, decodeFloat, decodeInt, decodeIntDict)
import Gizra.NominalDate
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (EntityUuid, decodeEntityUuid, toEntityUuid)
import Translate.Utils exposing (decodeLanguage)
import Utils.Json exposing (decodeEverySet)


decodeGroupMeasurement : Decoder value -> Decoder (Measurement SessionId value)
decodeGroupMeasurement =
    decodeMeasurement "session"


decodeNutritionMeasurement : Decoder value -> Decoder (Measurement NutritionEncounterId value)
decodeNutritionMeasurement =
    decodeMeasurement "nutrition_encounter"


decodePrenatalMeasurement : Decoder value -> Decoder (Measurement PrenatalEncounterId value)
decodePrenatalMeasurement =
    decodeMeasurement "prenatal_encounter"


decodeMeasurement : String -> Decoder value -> Decoder (Measurement (EntityUuid a) value)
decodeMeasurement encounterTag valueDecoder =
    succeed Measurement
        |> required "date_measured" Gizra.NominalDate.decodeYYYYMMDD
        |> optional "nurse" (nullable decodeEntityUuid) Nothing
        |> optional "health_center" (nullable decodeEntityUuid) Nothing
        |> required "person" decodeEntityUuid
        |> optional encounterTag (nullable decodeEntityUuid) Nothing
        |> custom valueDecoder


decodeWithEntityUuid : Decoder a -> Decoder ( EntityUuid b, a )
decodeWithEntityUuid decoder =
    map2 (\a b -> ( a, b ))
        (field "uuid" decodeEntityUuid)
        decoder


decodeMotherMeasurementList : Decoder MotherMeasurementList
decodeMotherMeasurementList =
    succeed MotherMeasurementList
        |> optional "attendance" (map Dict.fromList <| list (decodeWithEntityUuid decodeAttendance)) Dict.empty
        |> optional "family_planning" (map Dict.fromList <| list (decodeWithEntityUuid decodeFamilyPlanning)) Dict.empty
        |> optional "participant_consent" (map Dict.fromList <| list (decodeWithEntityUuid decodeParticipantConsent)) Dict.empty


decodeChildMeasurementList : Decoder ChildMeasurementList
decodeChildMeasurementList =
    succeed ChildMeasurementList
        |> optional "height" (map Dict.fromList <| list (decodeWithEntityUuid decodeHeight)) Dict.empty
        |> optional "muac" (map Dict.fromList <| list (decodeWithEntityUuid decodeMuac)) Dict.empty
        |> optional "nutrition" (map Dict.fromList <| list (decodeWithEntityUuid decodeNutrition)) Dict.empty
        |> optional "photo" (map Dict.fromList <| list (decodeWithEntityUuid decodePhoto)) Dict.empty
        |> optional "weight" (map Dict.fromList <| list (decodeWithEntityUuid decodeWeight)) Dict.empty
        |> optional "counseling_session" (map Dict.fromList <| list (decodeWithEntityUuid decodeCounselingSession)) Dict.empty


decodePrenatalMeasurements : Decoder PrenatalMeasurements
decodePrenatalMeasurements =
    succeed PrenatalMeasurements
        |> optional "breast_exam" (decodeHead decodeBreastExam) Nothing
        |> optional "core_physical_exam" (decodeHead decodeCorePhysicalExam) Nothing
        |> optional "danger_signs" (decodeHead decodeDangerSigns) Nothing
        |> optional "last_menstrual_period" (decodeHead decodeLastMenstrualPeriod) Nothing
        |> optional "medical_history" (decodeHead decodeMedicalHistory) Nothing
        |> optional "medication" (decodeHead decodeMedication) Nothing
        |> optional "obstetrical_exam" (decodeHead decodeObstetricalExam) Nothing
        |> optional "obstetric_history" (decodeHead decodeObstetricHistory) Nothing
        |> optional "obstetric_history_step2" (decodeHead decodeObstetricHistoryStep2) Nothing
        |> optional "prenatal_family_planning" (decodeHead decodePrenatalFamilyPlanning) Nothing
        |> optional "prenatal_nutrition" (decodeHead decodePrenatalNutrition) Nothing
        |> optional "resource" (decodeHead decodeResource) Nothing
        |> optional "social_history" (decodeHead decodeSocialHistory) Nothing
        |> optional "vitals" (decodeHead decodeVitals) Nothing
        |> optional "prenatal_photo" (decodeHead decodePrenatalPhoto) Nothing


decodeNutritionMeasurements : Decoder NutritionMeasurements
decodeNutritionMeasurements =
    succeed NutritionMeasurements
        |> optional "nutrition_nutrition" (decodeHead decodeNutritionNutrition) Nothing


decodeHead : Decoder a -> Decoder (Maybe ( EntityUuid b, a ))
decodeHead =
    map List.head << list << decodeWithEntityUuid


decodePhoto : Decoder Photo
decodePhoto =
    field "photo" string
        |> map PhotoUrl
        |> decodeGroupMeasurement


decodePrenatalPhoto : Decoder PrenatalPhoto
decodePrenatalPhoto =
    field "photo" string
        |> map PhotoUrl
        |> decodePrenatalMeasurement


decodeHeight : Decoder Height
decodeHeight =
    field "height" decodeFloat
        |> map HeightInCm
        |> decodeGroupMeasurement


decodeWeight : Decoder Weight
decodeWeight =
    field "weight" decodeFloat
        |> map WeightInKg
        |> decodeGroupMeasurement


decodeMuac : Decoder Muac
decodeMuac =
    field "muac" decodeFloat
        |> map MuacInCm
        |> decodeGroupMeasurement


decodeFamilyPlanning : Decoder FamilyPlanning
decodeFamilyPlanning =
    decodeEverySet decodeFamilyPlanningSign
        |> field "family_planning_signs"
        |> decodeGroupMeasurement


decodeAttendance : Decoder Attendance
decodeAttendance =
    field "attended" bool
        |> decodeGroupMeasurement


decodeParticipantConsent : Decoder ParticipantConsent
decodeParticipantConsent =
    decodeGroupMeasurement decodeParticipantConsentValue


decodeParticipantConsentValue : Decoder ParticipantConsentValue
decodeParticipantConsentValue =
    succeed ParticipantConsentValue
        |> required "language" decodeLanguage
        |> required "participant_form" decodeEntityUuid


decodeNutrition : Decoder ChildNutrition
decodeNutrition =
    decodeEverySet decodeChildNutritionSign
        |> field "nutrition_signs"
        |> decodeGroupMeasurement


decodeCounselingSession : Decoder CounselingSession
decodeCounselingSession =
    decodeGroupMeasurement <|
        map2 (\a b -> ( a, b ))
            (field "timing" decodeCounselingTiming)
            (field "topics" (decodeEverySet decodeEntityUuid))


decodeChildNutritionSign : Decoder ChildNutritionSign
decodeChildNutritionSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    -- We're keeping this one for back-compat, as
                    -- this value still may exists in the browser
                    -- local storage. Should be removed a little bit
                    -- later.
                    "abdominal-disortion" ->
                        succeed AbdominalDistension

                    -- We briefly used this typo as well, so also
                    -- keeping for back-compat.
                    "abdominal-distention" ->
                        succeed AbdominalDistension

                    "abdominal-distension" ->
                        succeed AbdominalDistension

                    "apathy" ->
                        succeed Apathy

                    "brittle-hair" ->
                        succeed BrittleHair

                    "dry-skin" ->
                        succeed DrySkin

                    "edema" ->
                        succeed Edema

                    "none" ->
                        succeed NormalChildNutrition

                    "poor-appetite" ->
                        succeed PoorAppetite

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized ChildNutritionSign"
            )


decodeFamilyPlanningSign : Decoder FamilyPlanningSign
decodeFamilyPlanningSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "auto-observation" ->
                        succeed AutoObservation

                    "condoms" ->
                        succeed Condoms

                    "cycle-counting" ->
                        succeed CycleCounting

                    "hysterectomy" ->
                        succeed Hysterectomy

                    "implant" ->
                        succeed Implants

                    "injection" ->
                        succeed Injectables

                    "iud" ->
                        succeed IUD

                    "lactation-amenorrhea" ->
                        succeed LactationAmenorrhea

                    "none" ->
                        succeed NoFamilyPlanning

                    "spermicide" ->
                        succeed Spermicide

                    "tubal-ligatures" ->
                        succeed TubalLigatures

                    "vasectomy" ->
                        succeed Vasectomy

                    "pill" ->
                        succeed OralContraceptives

                    "necklace" ->
                        succeed CycleBeads

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized FamilyPlanningSign"
            )


decodeBreastExamSign : Decoder BreastExamSign
decodeBreastExamSign =
    string
        |> andThen
            (\s ->
                case s of
                    "mass" ->
                        succeed Mass

                    "discharge" ->
                        succeed Discharge

                    "infection" ->
                        succeed Infection

                    "normal" ->
                        succeed NormalBreast

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized BreastExamSign"
            )


decodeBreastExam : Decoder BreastExam
decodeBreastExam =
    succeed BreastExamValue
        |> required "breast" (decodeEverySet decodeBreastExamSign)
        |> required "breast_self_exam" bool
        |> decodePrenatalMeasurement


decodeHairHeadCPESign : Decoder HairHeadCPESign
decodeHairHeadCPESign =
    string
        |> andThen
            (\s ->
                case s of
                    "brittle-hair" ->
                        succeed BrittleHairCPE

                    "normal" ->
                        succeed NormalHairHead

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized hair/head sign"
            )


decodeEyesCPESign : Decoder EyesCPESign
decodeEyesCPESign =
    string
        |> andThen
            (\s ->
                case s of
                    "pale-conjuctiva" ->
                        succeed PaleConjuctiva

                    "normal" ->
                        succeed NormalEyes

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized EyesCPESign"
            )


decodeHeartCPESign : Decoder HeartCPESign
decodeHeartCPESign =
    string
        |> andThen
            (\s ->
                case s of
                    "irregular-rhythm" ->
                        succeed IrregularRhythm

                    "normal-rate-and-rhythm" ->
                        succeed NormalRateAndRhythm

                    "sinus-tachycardia" ->
                        succeed SinusTachycardia

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized HeartCPESign"
            )


decodeNeckCPESign : Decoder NeckCPESign
decodeNeckCPESign =
    string
        |> andThen
            (\s ->
                case s of
                    "enlarged-thyroid" ->
                        succeed EnlargedThyroid

                    "enlarged-lymph-nodes" ->
                        succeed EnlargedLymphNodes

                    "normal" ->
                        succeed NormalNeck

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized NeckCPESign"
            )


decodeLungsCPESign : Decoder LungsCPESign
decodeLungsCPESign =
    string
        |> andThen
            (\s ->
                case s of
                    "wheezes" ->
                        succeed Wheezes

                    "crackles" ->
                        succeed Crackles

                    "normal" ->
                        succeed NormalLungs

                    _ ->
                        fail <| s ++ " is not a recognized LungsCPESign"
            )


decodeAbdomenCPESign : Decoder AbdomenCPESign
decodeAbdomenCPESign =
    string
        |> andThen
            (\s ->
                case s of
                    "hepatomegaly" ->
                        succeed Hepatomegaly

                    "splenomegaly" ->
                        succeed Splenomegaly

                    "tender-to-palpitation-right-upper" ->
                        succeed TPRightUpper

                    "tender-to-palpitation-left-upper" ->
                        succeed TPLeftUpper

                    "tender-to-palpitation-right-lower" ->
                        succeed TPRightLower

                    "tender-to-palpitation-left-lower" ->
                        succeed TPLeftLower

                    "hernia" ->
                        succeed Hernia

                    "normal" ->
                        succeed NormalAbdomen

                    _ ->
                        fail <| s ++ " is not a recognized AbdomenCPESign"
            )


decodeHandsCPESign : Decoder HandsCPESign
decodeHandsCPESign =
    string
        |> andThen
            (\s ->
                case s of
                    "pallor" ->
                        succeed PallorHands

                    "edema" ->
                        succeed EdemaHands

                    "normal" ->
                        succeed NormalHands

                    _ ->
                        fail <| s ++ " is not a recognized HandsCPESign"
            )


decodeLegsCPESign : Decoder LegsCPESign
decodeLegsCPESign =
    string
        |> andThen
            (\s ->
                case s of
                    "pallor" ->
                        succeed PallorLegs

                    "edema" ->
                        succeed EdemaLegs

                    "normal" ->
                        succeed NormalLegs

                    _ ->
                        fail <| s ++ " is not a recognized LegsCPESign"
            )


decodeCorePhysicalExam : Decoder CorePhysicalExam
decodeCorePhysicalExam =
    succeed CorePhysicalExamValue
        |> required "head_hair" (decodeEverySet decodeHairHeadCPESign)
        |> required "eyes" (decodeEverySet decodeEyesCPESign)
        |> required "heart" (decodeEverySet decodeHeartCPESign)
        |> required "heart_murmur" bool
        |> required "neck" (decodeEverySet decodeNeckCPESign)
        |> required "lungs" (decodeEverySet decodeLungsCPESign)
        |> required "abdomen" (decodeEverySet decodeAbdomenCPESign)
        |> required "hands" (decodeEverySet decodeHandsCPESign)
        |> required "legs" (decodeEverySet decodeLegsCPESign)
        |> decodePrenatalMeasurement


decodeDangerSign : Decoder DangerSign
decodeDangerSign =
    string
        |> andThen
            (\s ->
                case s of
                    "vaginal-bleeding" ->
                        succeed VaginalBleeding

                    "sever-headaches-with-blurred-vision" ->
                        succeed HeadacheBlurredVision

                    "convulsions" ->
                        succeed Convulsions

                    "abdominal-pain" ->
                        succeed AbdominalPain

                    "difficulty-breathing" ->
                        succeed DifficultyBreathing

                    "fever" ->
                        succeed Fever

                    "extreme-weakness" ->
                        succeed ExtremeWeakness

                    "none" ->
                        succeed NoDangerSign

                    _ ->
                        fail <| s ++ " is not a recognized DangerSign"
            )


decodeDangerSigns : Decoder DangerSigns
decodeDangerSigns =
    decodeEverySet decodeDangerSign
        |> field "danger_signs"
        |> decodePrenatalMeasurement


decodeLastMenstrualPeriod : Decoder LastMenstrualPeriod
decodeLastMenstrualPeriod =
    succeed LastMenstrualPeriodValue
        |> required "last_menstrual_period" Gizra.NominalDate.decodeYYYYMMDD
        |> required "confident" bool
        |> decodePrenatalMeasurement


decodeMedicalHistorySign : Decoder MedicalHistorySign
decodeMedicalHistorySign =
    string
        |> andThen
            (\s ->
                case s of
                    "uterine-myonma" ->
                        succeed UterineMyoma

                    "diabetes" ->
                        succeed Diabetes

                    "cardiac-disease" ->
                        succeed CardiacDisease

                    "renal-disease" ->
                        succeed RenalDisease

                    "hypertension-before-pregnancy" ->
                        succeed HypertensionBeforePregnancy

                    "tuberculosis-past" ->
                        succeed TuberculosisPast

                    "tuberculosis-present" ->
                        succeed TuberculosisPresent

                    "asthma" ->
                        succeed Asthma

                    "bowed-legs" ->
                        succeed BowedLegs

                    "hiv" ->
                        succeed HIV

                    "mental-health-history" ->
                        succeed MentalHealthHistory

                    "none" ->
                        succeed NoMedicalHistorySigns

                    _ ->
                        fail <| s ++ " is not a recognized MedicalHistorySign"
            )


decodeMedicalHistory : Decoder MedicalHistory
decodeMedicalHistory =
    field "medical_history" (decodeEverySet decodeMedicalHistorySign)
        |> decodePrenatalMeasurement


decodeMedicationSign : Decoder MedicationSign
decodeMedicationSign =
    string
        |> andThen
            (\s ->
                case s of
                    "iron-and-folic-acid-supplement" ->
                        succeed IronAndFolicAcidSupplement

                    "deworming-pill" ->
                        succeed DewormingPill

                    "none" ->
                        succeed NoMedication

                    _ ->
                        fail <| s ++ " is not a recognized MedicationSign"
            )


decodeMedication : Decoder Medication
decodeMedication =
    field "medication" (decodeEverySet decodeMedicationSign)
        |> decodePrenatalMeasurement


decodeFetalPresentation : Decoder FetalPresentation
decodeFetalPresentation =
    string
        |> andThen
            (\s ->
                case s of
                    "transverse" ->
                        succeed Transverse

                    "cephalic" ->
                        succeed Cephalic

                    "breech" ->
                        succeed FetalBreech

                    "twins" ->
                        succeed Twins

                    "unknown" ->
                        succeed Unknown

                    _ ->
                        fail <| s ++ " is not a recognized FetalPresentation"
            )


decodeObstetricalExam : Decoder ObstetricalExam
decodeObstetricalExam =
    succeed ObstetricalExamValue
        |> required "fundal_height" (map HeightInCm decodeFloat)
        |> required "fetal_presentation" decodeFetalPresentation
        |> required "fetal_movement" bool
        |> required "fetal_heart_rate" decodeInt
        |> required "c_section_scar" decodeCSectionScar
        |> decodePrenatalMeasurement


decodeObstetricHistory : Decoder ObstetricHistory
decodeObstetricHistory =
    succeed ObstetricHistoryValue
        |> required "currently_pregnant" bool
        |> required "term_pregnancy" decodeInt
        |> required "preterm_pregnancy" decodeInt
        |> required "stillbirths_at_term" decodeInt
        |> required "stillbirths_preterm" decodeInt
        |> required "abortions" decodeInt
        |> required "live_children" decodeInt
        |> decodePrenatalMeasurement


decodePrenatalFamilyPlanning : Decoder PrenatalFamilyPlanning
decodePrenatalFamilyPlanning =
    decodeEverySet decodeFamilyPlanningSign
        |> field "family_planning_signs"
        |> decodePrenatalMeasurement


decodePrenatalNutrition : Decoder PrenatalNutrition
decodePrenatalNutrition =
    succeed PrenatalNutritionValue
        |> required "height" (map HeightInCm decodeFloat)
        |> required "weight" (map WeightInKg decodeFloat)
        |> required "muac" (map MuacInCm decodeFloat)
        |> decodePrenatalMeasurement


decodeResourceSign : Decoder ResourceSign
decodeResourceSign =
    string
        |> andThen
            (\s ->
                case s of
                    "mosquito-net" ->
                        succeed MosquitoNet

                    "none" ->
                        succeed NoResource

                    _ ->
                        fail <| s ++ " is not a recognized ResourceSign"
            )


decodeResource : Decoder Resource
decodeResource =
    decodeEverySet decodeResourceSign
        |> field "resources"
        |> decodePrenatalMeasurement


decodeSocialHistorySign : Decoder SocialHistorySign
decodeSocialHistorySign =
    string
        |> andThen
            (\s ->
                case s of
                    "accompanied-by-partner" ->
                        succeed AccompaniedByPartner

                    "partner-hiv-counseling" ->
                        succeed PartnerHivCounseling

                    "none" ->
                        succeed NoSocialHistorySign

                    _ ->
                        fail <| s ++ " is not a recognized SocialHistorySign"
            )


decodeSocialHistoryHivTestingResult : Decoder SocialHistoryHivTestingResult
decodeSocialHistoryHivTestingResult =
    string
        |> andThen
            (\s ->
                case s of
                    "positive" ->
                        succeed ResultHivPositive

                    "negative" ->
                        succeed ResultHivNegative

                    "indeterminate" ->
                        succeed ResultHivIndeterminate

                    "none" ->
                        succeed NoHivTesting

                    _ ->
                        fail <| s ++ " is not a recognized SocialHistorySign"
            )


decodeSocialHistory : Decoder SocialHistory
decodeSocialHistory =
    succeed SocialHistoryValue
        |> required "social_history" (decodeEverySet decodeSocialHistorySign)
        |> required "partner_hiv_testing" decodeSocialHistoryHivTestingResult
        |> decodePrenatalMeasurement


decodeVitals : Decoder Vitals
decodeVitals =
    succeed VitalsValue
        |> required "sys" decodeFloat
        |> required "dia" decodeFloat
        |> required "heart_rate" decodeInt
        |> required "respiratory_rate" decodeInt
        |> required "body_temperature" decodeFloat
        |> decodePrenatalMeasurement


decodeCSectionReason : Decoder CSectionReason
decodeCSectionReason =
    string
        |> andThen
            (\s ->
                case s of
                    "breech" ->
                        succeed Breech

                    "emergency" ->
                        succeed Emergency

                    "failure-to-progress" ->
                        succeed FailureToProgress

                    "none" ->
                        succeed None

                    "other" ->
                        succeed Other

                    _ ->
                        fail <| s ++ " is not a recognized CSectionReason"
            )


decodeCSectionScar : Decoder CSectionScar
decodeCSectionScar =
    string
        |> andThen
            (\s ->
                case s of
                    "vertical" ->
                        succeed Vertical

                    "horizontal" ->
                        succeed Horizontal

                    "none" ->
                        succeed NoScar

                    _ ->
                        fail <| s ++ " is not a recognized CSectionScar"
            )


decodePreviousDeliveryPeriod : Decoder PreviousDeliveryPeriod
decodePreviousDeliveryPeriod =
    string
        |> andThen
            (\s ->
                case s of
                    "less-than-18-month" ->
                        succeed LessThan18Month

                    "more-than-5-years" ->
                        succeed MoreThan5Years

                    "neither" ->
                        succeed Neither

                    _ ->
                        fail <| s ++ " is not a recognized PreviousDeliveryPeriod"
            )


decodePreviousDeliverySign : Decoder PreviousDeliverySign
decodePreviousDeliverySign =
    string
        |> andThen
            (\s ->
                case s of
                    "c-section-in-previous-delivery" ->
                        succeed CSectionInPreviousDelivery

                    "stillborn-previous-delivery" ->
                        succeed StillbornPreviousDelivery

                    "baby-died-on-day-of-birth-previous-delivery" ->
                        succeed BabyDiedOnDayOfBirthPreviousDelivery

                    "partial-placenta-previous-delivery" ->
                        succeed PartialPlacentaPreviousDelivery

                    "severe-hemorrhaging-previous-delivery" ->
                        succeed SevereHemorrhagingPreviousDelivery

                    "convulsions-previous-delivery" ->
                        succeed ConvulsionsPreviousDelivery

                    "convulsions-and-unconscious-previous-delivery" ->
                        succeed ConvulsionsAndUnconsciousPreviousDelivery

                    "none" ->
                        succeed NoPreviousDeliverySign

                    _ ->
                        fail <| s ++ " is not a recognized PreviousDeliverySign"
            )


decodeObstetricHistorySign : Decoder ObstetricHistorySign
decodeObstetricHistorySign =
    string
        |> andThen
            (\s ->
                case s of
                    "successive-abortions" ->
                        succeed SuccessiveAbortions

                    "successive-premature-deliveries" ->
                        succeed SuccessivePrematureDeliveries

                    "preeclampsia-previous-pregnancy" ->
                        succeed PreeclampsiaPreviousPregnancy

                    "gestational-diabetes-previous-pregnancy" ->
                        succeed GestationalDiabetesPreviousPregnancy

                    "incomplete-cervix-previous-pregnancy" ->
                        succeed IncompleteCervixPreviousPregnancy

                    "rh-negative" ->
                        succeed RhNegative

                    "none" ->
                        succeed NoObstetricHistorySign

                    _ ->
                        fail <| s ++ " is not a recognized ObstetricHistorySign"
            )


decodeObstetricHistoryStep2 : Decoder ObstetricHistoryStep2
decodeObstetricHistoryStep2 =
    succeed ObstetricHistoryStep2Value
        |> required "c_sections" decodeInt
        |> required "c_section_reason" (decodeEverySet decodeCSectionReason)
        |> required "previous_delivery" (decodeEverySet decodePreviousDeliverySign)
        |> required "previous_delivery_period" (decodeEverySet decodePreviousDeliveryPeriod)
        |> required "obstetric_history" (decodeEverySet decodeObstetricHistorySign)
        |> decodePrenatalMeasurement


decodeNutritionNutrition : Decoder NutritionNutrition
decodeNutritionNutrition =
    decodeEverySet decodeChildNutritionSign
        |> field "nutrition_signs"
        |> decodeNutritionMeasurement
