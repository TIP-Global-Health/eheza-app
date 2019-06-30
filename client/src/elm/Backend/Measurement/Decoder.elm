module Backend.Measurement.Decoder exposing
    ( decodeAttendance
    , decodeBreastExam
    , decodeChildMeasurementList
    , decodeCounselingSession
    , decodeFamilyPlanning
    , decodeHeight
    , decodeMotherMeasurementList
    , decodeMuac
    , decodeNutrition
    , decodeParticipantConsent
    , decodePhoto
    , decodeWeight
    )

import AllDict
import Backend.Counseling.Decoder exposing (decodeCounselingTiming)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Dict exposing (Dict)
import Gizra.Json exposing (decodeEmptyArrayAs, decodeFloat, decodeInt, decodeIntDict)
import Gizra.NominalDate
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, decode, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (EntityUuid, decodeEntityUuid, toEntityUuid)
import Translate.Utils exposing (decodeLanguage)
import Utils.EntityUuidDict as EntityUuidDict exposing (EntityUuidDict)
import Utils.EntityUuidDictList as EntityUuidDictList exposing (EntityUuidDictList)
import Utils.Json exposing (decodeEverySet)


decodeGroupMeasurement : Decoder value -> Decoder (Measurement SessionId value)
decodeGroupMeasurement =
    decodeMeasurement "session"


decodePrenatalMeasurement : Decoder value -> Decoder (Measurement PrenatalEncounterId value)
decodePrenatalMeasurement =
    decodeMeasurement "prenatal_encounter"


decodeMeasurement : String -> Decoder value -> Decoder (Measurement (EntityUuid a) value)
decodeMeasurement encounterTag valueDecoder =
    decode Measurement
        |> required "date_measured" Gizra.NominalDate.decodeYYYYMMDD
        |> optional "nurse" (nullable decodeEntityUuid) Nothing
        |> required "person" decodeEntityUuid
        |> optional encounterTag (nullable decodeEntityUuid) Nothing
        |> custom valueDecoder


{-| Decodes `HistoricalMeasurements` as sent by `/api/offline_sessions/`
-}
decodeHistoricalMeasurements : Decoder HistoricalMeasurements
decodeHistoricalMeasurements =
    decode HistoricalMeasurements
        |> requiredAt [ "participants", "mother_activity" ]
            (oneOf
                [ decodeEmptyArrayAs EntityUuidDict.empty
                , map toEntityUuidDict (dict decodeMotherMeasurementList)
                ]
            )
        |> requiredAt [ "participants", "child_activity" ]
            (oneOf
                [ decodeEmptyArrayAs EntityUuidDict.empty
                , map toEntityUuidDict (dict decodeChildMeasurementList)
                ]
            )


{-| TODO: Put in elm-essentials.
-}
toEntityUuidDict : Dict String v -> EntityUuidDict (EntityUuid k) v
toEntityUuidDict =
    Dict.foldl (\key value acc -> AllDict.insert (toEntityUuid key) value acc) EntityUuidDict.empty


decodeWithEntityUuid : Decoder a -> Decoder ( EntityUuid b, a )
decodeWithEntityUuid decoder =
    map2 (,)
        (field "uuid" decodeEntityUuid)
        decoder


decodeMotherMeasurementList : Decoder MotherMeasurementList
decodeMotherMeasurementList =
    decode MotherMeasurementList
        |> optional "attendance" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodeAttendance)) EntityUuidDictList.empty
        |> optional "family_planning" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodeFamilyPlanning)) EntityUuidDictList.empty
        |> optional "participant_consent" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodeParticipantConsent)) EntityUuidDictList.empty


decodeChildMeasurementList : Decoder ChildMeasurementList
decodeChildMeasurementList =
    decode ChildMeasurementList
        |> optional "height" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodeHeight)) EntityUuidDictList.empty
        |> optional "muac" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodeMuac)) EntityUuidDictList.empty
        |> optional "nutrition" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodeNutrition)) EntityUuidDictList.empty
        |> optional "photo" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodePhoto)) EntityUuidDictList.empty
        |> optional "weight" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodeWeight)) EntityUuidDictList.empty
        |> optional "counseling_session" (map EntityUuidDictList.fromList <| list (decodeWithEntityUuid decodeCounselingSession)) EntityUuidDictList.empty


decodePhoto : Decoder Photo
decodePhoto =
    field "photo" string
        |> map PhotoUrl
        |> decodeGroupMeasurement


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
    decode ParticipantConsentValue
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
        map2 (,)
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
                    "pill" ->
                        succeed Pill

                    "condoms" ->
                        succeed Condoms

                    "iud" ->
                        succeed IUD

                    "implant" ->
                        succeed Implant

                    "injection" ->
                        succeed Injection

                    "necklace" ->
                        succeed Necklace

                    "none" ->
                        succeed NoFamilyPlanning

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
                    "abnormal" ->
                        succeed AbnormalHeart

                    "normal" ->
                        succeed NormalHeart

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
                    "heptomegaly" ->
                        succeed Heptomegaly

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

                    "breach" ->
                        succeed Breach

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
        |> required "c_section_scar" bool
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

                    "mental-health-history" ->
                        succeed MentalHealthHistory

                    "none" ->
                        succeed NoSocialHistorySign

                    _ ->
                        fail <| s ++ " is not a recognized SocialHistorySign"
            )


decodeSocialHistory : Decoder SocialHistory
decodeSocialHistory =
    decodeEverySet decodeSocialHistorySign
        |> field "social_history"
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
