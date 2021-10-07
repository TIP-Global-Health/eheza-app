module Backend.Measurement.Decoder exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Counseling.Decoder exposing (decodeCounselingTiming)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (..)
import EverySet exposing (EverySet)
import Gizra.Json exposing (decodeEmptyArrayAs, decodeFloat, decodeInt, decodeIntDict, decodeStringWithDefault)
import Gizra.NominalDate
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, optionalAt, required, requiredAt)
import Restful.Endpoint exposing (EntityUuid, decodeEntityUuid, toEntityUuid)
import Translate.Utils exposing (decodeLanguage)
import Utils.Json exposing (decodeEverySet, decodeWithFallback)


decodeGroupMeasurement : Decoder value -> Decoder (Measurement SessionId value)
decodeGroupMeasurement =
    decodeMeasurement "session"


decodePrenatalMeasurement : Decoder value -> Decoder (Measurement PrenatalEncounterId value)
decodePrenatalMeasurement =
    decodeMeasurement "prenatal_encounter"


decodeNutritionMeasurement : Decoder value -> Decoder (Measurement NutritionEncounterId value)
decodeNutritionMeasurement =
    decodeMeasurement "nutrition_encounter"


decodeAcuteIllnessMeasurement : Decoder value -> Decoder (Measurement AcuteIllnessEncounterId value)
decodeAcuteIllnessMeasurement =
    decodeMeasurement "acute_illness_encounter"


decodeHomeVisitMeasurement : Decoder value -> Decoder (Measurement HomeVisitEncounterId value)
decodeHomeVisitMeasurement =
    decodeMeasurement "home_visit_encounter"


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
        |> optional "lactation" (map Dict.fromList <| list (decodeWithEntityUuid decodeLactation)) Dict.empty
        |> optional "mother_fbf" (map Dict.fromList <| list (decodeWithEntityUuid decodeFbf)) Dict.empty


decodeChildMeasurementList : Decoder ChildMeasurementList
decodeChildMeasurementList =
    succeed ChildMeasurementList
        |> optional "height" (map Dict.fromList <| list (decodeWithEntityUuid decodeHeight)) Dict.empty
        |> optional "muac" (map Dict.fromList <| list (decodeWithEntityUuid decodeMuac)) Dict.empty
        |> optional "nutrition" (map Dict.fromList <| list (decodeWithEntityUuid decodeNutrition)) Dict.empty
        |> optional "photo" (map Dict.fromList <| list (decodeWithEntityUuid decodePhoto)) Dict.empty
        |> optional "weight" (map Dict.fromList <| list (decodeWithEntityUuid decodeWeight)) Dict.empty
        |> optional "counseling_session" (map Dict.fromList <| list (decodeWithEntityUuid decodeCounselingSession)) Dict.empty
        |> optional "child_fbf" (map Dict.fromList <| list (decodeWithEntityUuid decodeFbf)) Dict.empty
        |> optional "contributing_factors" (map Dict.fromList <| list (decodeWithEntityUuid decodeContributingFactors)) Dict.empty
        |> optional "follow_up" (map Dict.fromList <| list (decodeWithEntityUuid decodeFollowUp)) Dict.empty
        |> optional "group_health_education" (map Dict.fromList <| list (decodeWithEntityUuid decodeGroupHealthEducation)) Dict.empty
        |> optional "group_send_to_hc" (map Dict.fromList <| list (decodeWithEntityUuid decodeGroupSendToHC)) Dict.empty


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
        |> optional "birth_plan" (decodeHead decodeBirthPlan) Nothing
        |> optional "pregnancy_testing" (decodeHead decodePregnancyTesting) Nothing
        |> optional "prenatal_health_education" (decodeHead decodePrenatalHealthEducation) Nothing
        |> optional "prenatal_follow_up" (decodeHead decodePrenatalFollowUp) Nothing
        |> optional "prenatal_send_to_hc" (decodeHead decodePrenatalSendToHc) Nothing
        |> optional "appointment_confirmation" (decodeHead decodeAppointmentConfirmation) Nothing


decodeNutritionMeasurements : Decoder NutritionMeasurements
decodeNutritionMeasurements =
    succeed NutritionMeasurements
        |> optional "nutrition_muac" (decodeHead decodeNutritionMuac) Nothing
        |> optional "nutrition_height" (decodeHead decodeNutritionHeight) Nothing
        |> optional "nutrition_nutrition" (decodeHead decodeNutritionNutrition) Nothing
        |> optional "nutrition_photo" (decodeHead decodeNutritionPhoto) Nothing
        |> optional "nutrition_weight" (decodeHead decodeNutritionWeight) Nothing
        |> optional "nutrition_send_to_hc" (decodeHead decodeNutritionSendToHC) Nothing
        |> optional "nutrition_health_education" (decodeHead decodeNutritionHealthEducation) Nothing
        |> optional "nutrition_contributing_factors" (decodeHead decodeNutritionContributingFactors) Nothing
        |> optional "nutrition_follow_up" (decodeHead decodeNutritionFollowUp) Nothing


decodeAcuteIllnessMeasurements : Decoder AcuteIllnessMeasurements
decodeAcuteIllnessMeasurements =
    succeed AcuteIllnessMeasurements
        |> optional "symptoms_general" (decodeHead decodeSymptomsGeneral) Nothing
        |> optional "symptoms_respiratory" (decodeHead decodeSymptomsRespiratory) Nothing
        |> optional "symptoms_gi" (decodeHead decodeSymptomsGI) Nothing
        |> optional "acute_illness_vitals" (decodeHead decodeAcuteIllnessVitals) Nothing
        |> optional "acute_findings" (decodeHead decodeAcuteFindings) Nothing
        |> optional "malaria_testing" (decodeHead decodeMalariaTesting) Nothing
        |> optional "travel_history" (decodeHead decodeTravelHistory) Nothing
        |> optional "exposure" (decodeHead decodeExposure) Nothing
        |> optional "isolation" (decodeHead decodeIsolation) Nothing
        |> optional "hc_contact" (decodeHead decodeHCContact) Nothing
        |> optional "call_114" (decodeHead decodeCall114) Nothing
        |> optional "treatment_history" (decodeHead decodeTreatmentReview) Nothing
        |> optional "send_to_hc" (decodeHead decodeSendToHC) Nothing
        |> optional "medication_distribution" (decodeHead decodeMedicationDistribution) Nothing
        |> optional "acute_illness_muac" (decodeHead decodeAcuteIllnessMuac) Nothing
        |> optional "treatment_ongoing" (decodeHead decodeTreatmentOngoing) Nothing
        |> optional "acute_illness_danger_signs" (decodeHead decodeAcuteIllnessDangerSigns) Nothing
        |> optional "acute_illness_nutrition" (decodeHead decodeAcuteIllnessNutrition) Nothing
        |> optional "health_education" (decodeHead decodeHealthEducation) Nothing
        |> optional "acute_illness_follow_up" (decodeHead decodeAcuteIllnessFollowUp) Nothing


decodeFollowUpMeasurements : Decoder FollowUpMeasurements
decodeFollowUpMeasurements =
    succeed FollowUpMeasurements
        |> optional "follow_up" (map Dict.fromList <| list (decodeWithEntityUuid decodeFollowUp)) Dict.empty
        |> optional "nutrition_follow_up" (map Dict.fromList <| list (decodeWithEntityUuid decodeNutritionFollowUp)) Dict.empty
        |> optional "acute_illness_follow_up" (map Dict.fromList <| list (decodeWithEntityUuid decodeAcuteIllnessFollowUp)) Dict.empty
        |> optional "prenatal_follow_up" (map Dict.fromList <| list (decodeWithEntityUuid decodePrenatalFollowUp)) Dict.empty


decodeHomeVisitMeasurements : Decoder HomeVisitMeasurements
decodeHomeVisitMeasurements =
    succeed HomeVisitMeasurements
        |> optional "nutrition_feeding" (decodeHead decodeNutritionFeeding) Nothing
        |> optional "nutrition_hygiene" (decodeHead decodeNutritionHygiene) Nothing
        |> optional "nutrition_food_security" (decodeHead decodeNutritionFoodSecurity) Nothing
        |> optional "nutrition_caring" (decodeHead decodeNutritionCaring) Nothing


decodeHead : Decoder a -> Decoder (Maybe ( EntityUuid b, a ))
decodeHead =
    map List.head << list << decodeWithEntityUuid


decodePhoto : Decoder Photo
decodePhoto =
    field "photo" (decodeStringWithDefault "")
        |> map PhotoUrl
        |> decodeGroupMeasurement


decodePrenatalPhoto : Decoder PrenatalPhoto
decodePrenatalPhoto =
    field "photo" (decodeStringWithDefault "")
        |> map PhotoUrl
        |> decodePrenatalMeasurement


decodePregnancyTesting : Decoder PregnancyTest
decodePregnancyTesting =
    decodePregnancyTestResult
        |> field "urine_pregnancy_test"
        |> decodePrenatalMeasurement


decodePregnancyTestResult : Decoder PregnancyTestResult
decodePregnancyTestResult =
    string
        |> andThen
            (\result ->
                pregnancyTestResultFromString result
                    |> Maybe.map succeed
                    |> Maybe.withDefault (result ++ " is not a recognized PregnancyTestResult" |> fail)
            )


pregnancyTestResultFromString : String -> Maybe PregnancyTestResult
pregnancyTestResultFromString result =
    case result of
        "positive" ->
            Just PregnancyTestPositive

        "negative" ->
            Just PregnancyTestNegative

        "indeterminate" ->
            Just PregnancyTestIndeterminate

        "unable-to-conduct" ->
            Just PregnancyTestUnableToConduct

        _ ->
            Nothing


decodePrenatalHealthEducation : Decoder PrenatalHealthEducation
decodePrenatalHealthEducation =
    decodeEverySet decodePrenatalHealthEducationSign
        |> field "prenatal_health_education"
        |> decodePrenatalMeasurement


decodePrenatalHealthEducationSign : Decoder PrenatalHealthEducationSign
decodePrenatalHealthEducationSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "expectations" ->
                        succeed EducationExpectations

                    "visits-review" ->
                        succeed EducationVisitsReview

                    "warning-signs" ->
                        succeed EducationWarningSigns

                    "hemorrhaging" ->
                        succeed EducationHemorrhaging

                    "family-planning" ->
                        succeed EducationFamilyPlanning

                    "breastfeeding" ->
                        succeed EducationBreastfeeding

                    "immunization" ->
                        succeed EducationImmunization

                    "hygiene" ->
                        succeed EducationHygiene

                    "none" ->
                        succeed NoPrenatalHealthEducationSigns

                    _ ->
                        sign ++ " is not a recognized PrenatalHealthEducationSign" |> fail
            )


decodePrenatalFollowUp : Decoder PrenatalFollowUp
decodePrenatalFollowUp =
    decodePrenatalMeasurement decodePrenatalFollowUpValue


decodePrenatalFollowUpValue : Decoder PrenatalFollowUpValue
decodePrenatalFollowUpValue =
    succeed PrenatalFollowUpValue
        |> required "follow_up_options" (decodeEverySet decodeFollowUpOption)
        |> required "prenatal_assesment" decodePrenatalAssesment


decodePrenatalAssesment : Decoder PrenatalAssesment
decodePrenatalAssesment =
    string
        |> andThen
            (\assesment ->
                case assesment of
                    "normal" ->
                        succeed AssesmentNormalPregnancy

                    "high-risk" ->
                        succeed AssesmentHighRiskPregnancy

                    _ ->
                        assesment ++ " is not a recognized PrenatalAssesment" |> fail
            )


decodePrenatalSendToHc : Decoder PrenatalSendToHC
decodePrenatalSendToHc =
    decodePrenatalMeasurement decodeSendToHCValue


decodeAppointmentConfirmation : Decoder PrenatalAppointmentConfirmation
decodeAppointmentConfirmation =
    succeed PrenatalAppointmentConfirmationValue
        |> required "appointment_confirmation" Gizra.NominalDate.decodeYYYYMMDD
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


decodeLactation : Decoder Lactation
decodeLactation =
    decodeEverySet decodeLactationSign
        |> field "lactation_signs"
        |> decodeGroupMeasurement


decodeFbf : Decoder Fbf
decodeFbf =
    decodeGroupMeasurement decodeFbfValue


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


decodeLactationSign : Decoder LactationSign
decodeLactationSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "breastfeeding" ->
                        succeed Breastfeeding

                    "none" ->
                        succeed NoLactationSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized LactationSign"
            )


decodeFbfValue : Decoder FbfValue
decodeFbfValue =
    succeed FbfValue
        |> required "distributed_amount" decodeFloat
        |> required "distribution_notice" decodeDistributionNotice


decodeDistributionNotice : Decoder DistributionNotice
decodeDistributionNotice =
    string
        |> andThen
            (\notice ->
                case notice of
                    "complete" ->
                        succeed DistributedFully

                    "lack-of-stock" ->
                        succeed DistributedPartiallyLackOfStock

                    "other" ->
                        succeed DistributedPartiallyOther

                    _ ->
                        fail <|
                            notice
                                ++ " is not a recognized DistributionNotice"
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
    succeed DangerSignsValue
        |> required "danger_signs" (decodeEverySet decodeDangerSign)
        |> optional "postpartum_mother" (decodeEverySet decodePostpartumMotherDangerSign) (EverySet.fromList [ NoPostpartumMotherDangerSigns ])
        |> optional "postpartum_child" (decodeEverySet decodePostpartumChildDangerSign) (EverySet.fromList [ NoPostpartumChildDangerSigns ])
        |> decodePrenatalMeasurement


decodePostpartumMotherDangerSign : Decoder PostpartumMotherDangerSign
decodePostpartumMotherDangerSign =
    string
        |> andThen
            (\s ->
                postpartumMotherDangerSignFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (succeed NoPostpartumMotherDangerSigns)
            )


decodePostpartumChildDangerSign : Decoder PostpartumChildDangerSign
decodePostpartumChildDangerSign =
    string
        |> andThen
            (\s ->
                postpartumChildDangerSignFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (succeed NoPostpartumChildDangerSigns)
            )


decodeLastMenstrualPeriod : Decoder LastMenstrualPeriod
decodeLastMenstrualPeriod =
    succeed LastMenstrualPeriodValue
        |> required "last_menstrual_period" Gizra.NominalDate.decodeYYYYMMDD
        |> required "confident" bool
        |> required "confirmation" (decodeWithFallback False bool)
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


decodeBirthPlan : Decoder BirthPlan
decodeBirthPlan =
    decodePrenatalMeasurement decodeBirthPlanValue


decodeBirthPlanValue : Decoder BirthPlanValue
decodeBirthPlanValue =
    succeed BirthPlanValue
        |> required "birth_plan_signs" (decodeEverySet decodeBirthPlanSign)
        |> required "family_planning_signs" (decodeEverySet decodeFamilyPlanningSign)


decodeBirthPlanSign : Decoder BirthPlanSign
decodeBirthPlanSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "have-insurance" ->
                        succeed Insurance

                    "bought-clothes-for-child" ->
                        succeed BoughtClothes

                    "caregiver-to-accompany-you" ->
                        succeed CaregiverAccompany

                    "saved-money-for-use" ->
                        succeed SavedMoney

                    "planned-for-transportation" ->
                        succeed Transportation

                    "none" ->
                        succeed NoBirthPlan

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized BirthPlanSign"
            )


decodeNutritionMuac : Decoder NutritionMuac
decodeNutritionMuac =
    field "muac" decodeFloat
        |> map MuacInCm
        |> decodeNutritionMeasurement


decodeNutritionHeight : Decoder NutritionHeight
decodeNutritionHeight =
    field "height" decodeFloat
        |> map HeightInCm
        |> decodeNutritionMeasurement


decodeNutritionNutrition : Decoder NutritionNutrition
decodeNutritionNutrition =
    decodeEverySet decodeChildNutritionSign
        |> field "nutrition_signs"
        |> decodeNutritionMeasurement


decodeNutritionPhoto : Decoder NutritionPhoto
decodeNutritionPhoto =
    field "photo" (decodeStringWithDefault "")
        |> map PhotoUrl
        |> decodeNutritionMeasurement


decodeNutritionWeight : Decoder NutritionWeight
decodeNutritionWeight =
    field "weight" decodeFloat
        |> map WeightInKg
        |> decodeNutritionMeasurement


decodeContributingFactors : Decoder ContributingFactors
decodeContributingFactors =
    decodeGroupMeasurement decodeContributingFactorsValue


decodeNutritionContributingFactors : Decoder NutritionContributingFactors
decodeNutritionContributingFactors =
    decodeNutritionMeasurement decodeContributingFactorsValue


decodeContributingFactorsValue : Decoder (EverySet ContributingFactorsSign)
decodeContributingFactorsValue =
    decodeEverySet decodeContributingFactorsSign
        |> field "contributing_factors_signs"


decodeFollowUp : Decoder FollowUp
decodeFollowUp =
    decodeGroupMeasurement decodeFollowUpValue


decodeNutritionFollowUp : Decoder NutritionFollowUp
decodeNutritionFollowUp =
    decodeNutritionMeasurement decodeFollowUpValue


decodeFollowUpValue : Decoder FollowUpValue
decodeFollowUpValue =
    succeed FollowUpValue
        |> required "follow_up_options" (decodeEverySet decodeFollowUpOption)
        |> custom decodeNutritionAssesment


decodeAcuteIllnessFollowUp : Decoder AcuteIllnessFollowUp
decodeAcuteIllnessFollowUp =
    decodeAcuteIllnessMeasurement decodeAcuteIllnessFollowUpValue


decodeAcuteIllnessFollowUpValue : Decoder (EverySet FollowUpOption)
decodeAcuteIllnessFollowUpValue =
    decodeEverySet decodeFollowUpOption
        |> field "follow_up_options"


decodeNutritionFeeding : Decoder NutritionFeeding
decodeNutritionFeeding =
    decodeHomeVisitMeasurement decodeFeedingValue


decodeFeedingValue : Decoder NutritionFeedingValue
decodeFeedingValue =
    succeed NutritionFeedingValue
        |> required "nutrition_feeding_signs" (decodeEverySet decodeNutritionFeedingSign)
        |> required "supplement_type" decodeNutritionSupplementType
        |> required "sachets_per_day" decodeFloat


decodeNutritionSupplementType : Decoder NutritionSupplementType
decodeNutritionSupplementType =
    string
        |> andThen
            (\type_ ->
                case type_ of
                    "fortified-porridge" ->
                        succeed FortifiedPorridge

                    "rutf" ->
                        succeed Rutf

                    "ongera" ->
                        succeed Ongera

                    "therapeutic-milk" ->
                        succeed TherapeuticMilk

                    "none" ->
                        succeed NoNutritionSupplementType

                    _ ->
                        fail <|
                            type_
                                ++ " is not a recognized NutritionSupplementType"
            )


decodeNutritionFeedingSign : Decoder NutritionFeedingSign
decodeNutritionFeedingSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "receive-supplement" ->
                        succeed ReceiveSupplement

                    "ration-present-at-home" ->
                        succeed RationPresentAtHome

                    "enough-till-next-session" ->
                        succeed EnoughTillNextSession

                    "supplement-shared" ->
                        succeed SupplementShared

                    "encouraged-to-eat" ->
                        succeed EncouragedToEat

                    "refusing-to-eat" ->
                        succeed RefusingToEat

                    "breastfeeding" ->
                        succeed FeedingSignBreastfeeding

                    "clean-water-available" ->
                        succeed CleanWaterAvailable

                    "eaten-with-water" ->
                        succeed EatenWithWater

                    "none" ->
                        succeed NoNutritionFeedingSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized NutritionFeedingSign"
            )


decodeNutritionHygiene : Decoder NutritionHygiene
decodeNutritionHygiene =
    decodeHomeVisitMeasurement decodeNutritionHygieneValue


decodeNutritionHygieneValue : Decoder NutritionHygieneValue
decodeNutritionHygieneValue =
    succeed NutritionHygieneValue
        |> required "nutrition_hygiene_signs" (decodeEverySet decodeNutritionHygieneSign)
        |> required "main_water_source" decodeMainWaterSource
        |> required "water_preparation_option" decodeWaterPreparationOption


decodeNutritionHygieneSign : Decoder NutritionHygieneSign
decodeNutritionHygieneSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "soap-in-the-house" ->
                        succeed SoapInTheHouse

                    "wash-hands-before-feeding" ->
                        succeed WashHandsBeforeFeeding

                    "food-covered" ->
                        succeed FoodCovered

                    "none" ->
                        succeed NoNutritionHygieneSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized NutritionHygieneSign"
            )


decodeMainWaterSource : Decoder MainWaterSource
decodeMainWaterSource =
    string
        |> andThen
            (\sign ->
                case sign of
                    "piped-water-to-home" ->
                        succeed PipedWaterToHome

                    "public-water-tap" ->
                        succeed PublicWaterTap

                    "rain-water-collection-system" ->
                        succeed RainWaterCollectionSystem

                    "natural-source-flowing-water" ->
                        succeed NaturalSourceFlowingWater

                    "natural-source-standing-water" ->
                        succeed NaturalSourceStandingWater

                    "bottled-water" ->
                        succeed BottledWater

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized MainWaterSource"
            )


decodeWaterPreparationOption : Decoder WaterPreparationOption
decodeWaterPreparationOption =
    string
        |> andThen
            (\sign ->
                case sign of
                    "boiled" ->
                        succeed Boiled

                    "purification-solution" ->
                        succeed PurificationSolution

                    "filtered" ->
                        succeed Filtered

                    "bottled" ->
                        succeed Bottled

                    "none" ->
                        succeed NoWaterPreparationOption

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized WaterPreparationOption"
            )


decodeNutritionFoodSecurity : Decoder NutritionFoodSecurity
decodeNutritionFoodSecurity =
    decodeHomeVisitMeasurement decodeFoodSecurityValue


decodeFoodSecurityValue : Decoder NutritionFoodSecurityValue
decodeFoodSecurityValue =
    succeed NutritionFoodSecurityValue
        |> required "food_security_signs" (decodeEverySet decodeNutritionFoodSecuritySign)
        |> required "main_income_source" decodeMainIncomeSource


decodeNutritionFoodSecuritySign : Decoder NutritionFoodSecuritySign
decodeNutritionFoodSecuritySign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "household-got-food" ->
                        succeed HouseholdGotFood

                    "none" ->
                        succeed NoNutritionFoodSecuritySigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized NutritionFoodSecuritySign"
            )


decodeMainIncomeSource : Decoder MainIncomeSource
decodeMainIncomeSource =
    string
        |> andThen
            (\sign ->
                case sign of
                    "home-based-agriculture" ->
                        succeed HomeBasedAgriculture

                    "commercial-agriculture" ->
                        succeed CommercialAgriculture

                    "public-employee" ->
                        succeed PublicEmployee

                    "private-business-employee" ->
                        succeed PrivateBusinessEmpployee

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized MainIncomeSource"
            )


decodeNutritionCaring : Decoder NutritionCaring
decodeNutritionCaring =
    decodeHomeVisitMeasurement decodeNutritionCaringValue


decodeNutritionCaringValue : Decoder NutritionCaringValue
decodeNutritionCaringValue =
    succeed NutritionCaringValue
        |> required "nutrition_caring_signs" (decodeEverySet decodeNutritionCaringSign)
        |> required "child_caring_options" decodeNutritionCaringOption


decodeNutritionCaringSign : Decoder NutritionCaringSign
decodeNutritionCaringSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "parent-alive-and-healthy" ->
                        succeed ParentsAliveHealthy

                    "child-clean" ->
                        succeed ChildClean

                    "none" ->
                        succeed NoCaringSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized NutritionCaringSign"
            )


decodeNutritionCaringOption : Decoder CaringOption
decodeNutritionCaringOption =
    string
        |> andThen
            (\option ->
                case option of
                    "parent" ->
                        succeed CaredByParent

                    "grandparent" ->
                        succeed CaredByGrandparent

                    "sibling" ->
                        succeed CaredBySibling

                    "neighbor" ->
                        succeed CaredByNeighbor

                    "house-helper" ->
                        succeed CaredByHouseHelper

                    "daycare" ->
                        succeed CaredByDaycare

                    _ ->
                        fail <|
                            option
                                ++ " is not a recognized CaringOption"
            )


decodeSymptomsGeneral : Decoder SymptomsGeneral
decodeSymptomsGeneral =
    succeed SymptomsGeneralValue
        |> required "fever_period" decodeInt
        |> required "chills_period" decodeInt
        |> required "night_sweats_period" decodeInt
        |> required "body_aches_period" decodeInt
        |> required "headache_period" decodeInt
        |> required "lethargy_period" decodeInt
        |> required "poor_suck_period" decodeInt
        |> required "unable_to_drink_period" decodeInt
        |> required "unable_to_eat_period" decodeInt
        |> required "increased_thirst_period" decodeInt
        |> required "dry_mouth_period" decodeInt
        |> required "severe_weakness_period" decodeInt
        |> required "yellow_eyes_period" decodeInt
        |> required "coke_colored_urine_period" decodeInt
        |> required "convulsions_period" decodeInt
        |> required "spontaneos_bleeding_period" decodeInt
        |> map symptomsGeneralToDict
        |> decodeAcuteIllnessMeasurement


symptomsGeneralToDict : SymptomsGeneralValue -> Dict SymptomsGeneralSign Int
symptomsGeneralToDict value =
    [ ( SymptomGeneralFever, value.fever )
    , ( Chills, value.chills )
    , ( NightSweats, value.nightSweats )
    , ( BodyAches, value.bodyAches )
    , ( Headache, value.headache )
    , ( Lethargy, value.lethargy )
    , ( PoorSuck, value.poorSuck )
    , ( UnableToDrink, value.unableToDrink )
    , ( UnableToEat, value.unableToEat )
    , ( IncreasedThirst, value.increasedThirst )
    , ( DryMouth, value.dryMouth )
    , ( SevereWeakness, value.severeWeakness )
    , ( YellowEyes, value.yellowEyes )
    , ( CokeColoredUrine, value.cokeColoredUrine )
    , ( SymptomsGeneralConvulsions, value.convulsions )
    , ( SpontaneousBleeding, value.spontaneousBleeding )
    ]
        |> List.filter (Tuple.second >> (/=) 0)
        |> (\list ->
                if List.isEmpty list then
                    [ ( NoSymptomsGeneral, 0 ) ]

                else
                    list
           )
        |> Dict.fromList


decodeSymptomsRespiratory : Decoder SymptomsRespiratory
decodeSymptomsRespiratory =
    decodeAcuteIllnessMeasurement <|
        map7 symptomsRespiratoryToDict
            (field "cough_period" decodeInt)
            (field "shortness_of_breath_period" decodeInt)
            (field "nasal_congestion_period" decodeInt)
            (field "blood_in_sputum_period" decodeInt)
            (field "sore_throat_period" decodeInt)
            (field "loss_of_smell_period" decodeInt)
            (field "stabbing_chest_pain_period" decodeInt)


symptomsRespiratoryToDict : Int -> Int -> Int -> Int -> Int -> Int -> Int -> Dict SymptomsRespiratorySign Int
symptomsRespiratoryToDict cough shortnessOfBreath nasalCongestion bloodInSputum soreThroat lossOfSmell stabbingChestPain =
    [ ( Cough, cough )
    , ( ShortnessOfBreath, shortnessOfBreath )
    , ( NasalCongestion, nasalCongestion )
    , ( BloodInSputum, bloodInSputum )
    , ( SoreThroat, soreThroat )
    , ( LossOfSmell, lossOfSmell )
    , ( StabbingChestPain, stabbingChestPain )
    ]
        |> List.filter (Tuple.second >> (/=) 0)
        |> (\list ->
                if List.isEmpty list then
                    [ ( NoSymptomsRespiratory, 0 ) ]

                else
                    list
           )
        |> Dict.fromList


decodeSymptomsGI : Decoder SymptomsGI
decodeSymptomsGI =
    succeed SymptomsGIValue
        |> custom decodeSymptomsGIDict
        |> required "symptoms_gi_derived_signs" (decodeEverySet decodeSymptomsGIDerivedSign)
        |> decodeAcuteIllnessMeasurement


decodeSymptomsGIDict : Decoder (Dict SymptomsGISign Int)
decodeSymptomsGIDict =
    map5 symptomsGIToDict
        (field "bloody_diarrhea_period" decodeInt)
        (field "non_bloody_diarrhea_period" decodeInt)
        (field "nausea_period" decodeInt)
        (field "vomiting_period" decodeInt)
        (field "abdominal_pain_period" decodeInt)


symptomsGIToDict : Int -> Int -> Int -> Int -> Int -> Dict SymptomsGISign Int
symptomsGIToDict bloodyDiarrhea nonBloodyDiarrhea nausea vomiting abdominalPain =
    [ ( BloodyDiarrhea, bloodyDiarrhea )
    , ( NonBloodyDiarrhea, nonBloodyDiarrhea )
    , ( Nausea, nausea )
    , ( Vomiting, vomiting )
    , ( SymptomGIAbdominalPain, abdominalPain )
    ]
        |> List.filter (Tuple.second >> (/=) 0)
        |> (\list ->
                if List.isEmpty list then
                    [ ( NoSymptomsGI, 0 ) ]

                else
                    list
           )
        |> Dict.fromList


decodeSymptomsGIDerivedSign : Decoder SymptomsGIDerivedSign
decodeSymptomsGIDerivedSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "intractable-vomiting" ->
                        succeed IntractableVomiting

                    "none" ->
                        succeed NoSymptomsGIDerived

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized SymptomsGIDerivedSign"
            )


decodeAcuteIllnessVitals : Decoder AcuteIllnessVitals
decodeAcuteIllnessVitals =
    succeed AcuteIllnessVitalsValue
        |> required "respiratory_rate" decodeInt
        |> required "body_temperature" decodeFloat
        |> decodeAcuteIllnessMeasurement


decodeAcuteFindings : Decoder AcuteFindings
decodeAcuteFindings =
    succeed AcuteFindingsValue
        |> required "findings_signs_general" (decodeEverySet decodeAcuteFindingsGeneralSign)
        |> required "findings_signs_respiratory" (decodeEverySet decodeAcuteFindingsRespiratorySign)
        |> decodeAcuteIllnessMeasurement


decodeAcuteFindingsGeneralSign : Decoder AcuteFindingsGeneralSign
decodeAcuteFindingsGeneralSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "lethargic-or-unconscious" ->
                        succeed LethargicOrUnconscious

                    "poor-suck" ->
                        succeed AcuteFindingsPoorSuck

                    "sunken-eyes" ->
                        succeed SunkenEyes

                    "poor-skin-turgor" ->
                        succeed PoorSkinTurgor

                    "jaundice" ->
                        succeed Jaundice

                    "none" ->
                        succeed NoAcuteFindingsGeneralSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized AcuteFindingsGeneralSign"
            )


decodeAcuteFindingsRespiratorySign : Decoder AcuteFindingsRespiratorySign
decodeAcuteFindingsRespiratorySign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "stridor" ->
                        succeed Stridor

                    "nasal-flaring" ->
                        succeed NasalFlaring

                    "severe-wheezing" ->
                        succeed SevereWheezing

                    "sub-costal-retractions" ->
                        succeed SubCostalRetractions

                    "none" ->
                        succeed NoAcuteFindingsRespiratorySigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized AcuteFindingsRespiratorySign"
            )


decodeMalariaTesting : Decoder MalariaTesting
decodeMalariaTesting =
    decodeMalariaRapidTestResult
        |> field "malaria_rapid_test"
        |> decodeAcuteIllnessMeasurement


decodeMalariaRapidTestResult : Decoder MalariaRapidTestResult
decodeMalariaRapidTestResult =
    string
        |> andThen
            (\result ->
                malariaRapidTestResultFromString result
                    |> Maybe.map succeed
                    |> Maybe.withDefault (result ++ " is not a recognized MalariaRapidTestResult" |> fail)
            )


malariaRapidTestResultFromString : String -> Maybe MalariaRapidTestResult
malariaRapidTestResultFromString result =
    case result of
        "positive" ->
            Just RapidTestPositive

        "positive-and-pregnant" ->
            Just RapidTestPositiveAndPregnant

        "negative" ->
            Just RapidTestNegative

        "indeterminate" ->
            Just RapidTestIndeterminate

        "unable-to-run" ->
            Just RapidTestUnableToRun

        _ ->
            Nothing


decodeSendToHC : Decoder SendToHC
decodeSendToHC =
    decodeAcuteIllnessMeasurement decodeSendToHCValue


decodeNutritionSendToHC : Decoder NutritionSendToHC
decodeNutritionSendToHC =
    decodeNutritionMeasurement decodeSendToHCValue


decodeGroupSendToHC : Decoder GroupSendToHC
decodeGroupSendToHC =
    decodeGroupMeasurement decodeSendToHCValue


decodeSendToHCValue : Decoder SendToHCValue
decodeSendToHCValue =
    succeed SendToHCValue
        |> required "send_to_hc" (decodeEverySet decodeSendToHCSign)
        |> optional "reason_not_sent_to_hc" decodeReasonForNotSendingToHC NoReasonForNotSendingToHC


decodeSendToHCSign : Decoder SendToHCSign
decodeSendToHCSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "referral-form" ->
                        succeed HandReferrerForm

                    "refer-to-hc" ->
                        succeed ReferToHealthCenter

                    "accompany-to-hc" ->
                        succeed PrenatalAccompanyToHC

                    "none" ->
                        succeed NoSendToHCSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized SendToHCSign"
            )


decodeReasonForNotSendingToHC : Decoder ReasonForNotSendingToHC
decodeReasonForNotSendingToHC =
    string
        |> andThen
            (\event ->
                case event of
                    "client-refused" ->
                        succeed ClientRefused

                    "no-ambulance" ->
                        succeed NoAmbulance

                    "unable-to-afford-fee" ->
                        succeed ClientUnableToAffordFees

                    "other" ->
                        succeed ReasonForNotSendingToHCOther

                    "none" ->
                        succeed NoReasonForNotSendingToHC

                    _ ->
                        fail <|
                            event
                                ++ "is not a recognized ReasonForNotSendingToHC"
            )


decodeContributingFactorsSign : Decoder ContributingFactorsSign
decodeContributingFactorsSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "lack-of-breast-milk" ->
                        succeed FactorLackOfBreastMilk

                    "maternal-mastitis" ->
                        succeed FactorMaternalMastitis

                    "poor-suck" ->
                        succeed FactorPoorSuck

                    "diarrhea-or-vomiting" ->
                        succeed FactorDiarrheaOrVomiting

                    "none" ->
                        succeed NoContributingFactorsSign

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized ContributingFactorsSign"
            )


decodeFollowUpOption : Decoder FollowUpOption
decodeFollowUpOption =
    string
        |> andThen
            (\sign ->
                case sign of
                    "1-d" ->
                        succeed OneDay

                    "3-d" ->
                        succeed ThreeDays

                    "1-w" ->
                        succeed OneWeek

                    "2-w" ->
                        succeed TwoWeeks

                    "1-m" ->
                        succeed OneMonths

                    "2-m" ->
                        succeed TwoMonths

                    "3-m" ->
                        succeed ThreeMonths

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized FollowUpOption"
            )


decodeMedicationDistribution : Decoder MedicationDistribution
decodeMedicationDistribution =
    succeed MedicationDistributionValue
        |> required "prescribed_medication" (decodeEverySet decodeMedicationDistributionSign)
        |> required "non_administration_reason" (decodeEverySet decodeMedicationNonAdministrationSign)
        |> decodeAcuteIllnessMeasurement


decodeMedicationDistributionSign : Decoder MedicationDistributionSign
decodeMedicationDistributionSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "amoxicillin" ->
                        succeed Amoxicillin

                    "coartem" ->
                        succeed Coartem

                    "ors" ->
                        succeed ORS

                    "zinc" ->
                        succeed Zinc

                    "lemon-juice-or-honey" ->
                        succeed LemonJuiceOrHoney

                    "none" ->
                        succeed NoMedicationDistributionSigns

                    _ ->
                        fail <| sign ++ " is not a recognized MedicationDistributionSign"
            )


decodeMedicationNonAdministrationSign : Decoder MedicationNonAdministrationSign
decodeMedicationNonAdministrationSign =
    string
        |> andThen
            (\sign ->
                if sign == "none" then
                    succeed NoMedicationNonAdministrationSigns

                else
                    let
                        parts =
                            String.split "-" sign

                        failure =
                            fail <| sign ++ " is not a recognized MedicationNonAdministrationSign"
                    in
                    List.head parts
                        |> Maybe.map
                            (\prefix ->
                                let
                                    medicationNonAdministrationReason =
                                        List.tail parts
                                            |> Maybe.map (List.intersperse "-" >> String.concat)
                                            |> Maybe.andThen medicationNonAdministrationReasonFromString
                                in
                                case prefix of
                                    "amoxicillin" ->
                                        medicationNonAdministrationReason
                                            |> Maybe.map (MedicationAmoxicillin >> succeed)
                                            |> Maybe.withDefault failure

                                    "coartem" ->
                                        medicationNonAdministrationReason
                                            |> Maybe.map (MedicationCoartem >> succeed)
                                            |> Maybe.withDefault failure

                                    "ors" ->
                                        medicationNonAdministrationReason
                                            |> Maybe.map (MedicationORS >> succeed)
                                            |> Maybe.withDefault failure

                                    "zinc" ->
                                        medicationNonAdministrationReason
                                            |> Maybe.map (MedicationZinc >> succeed)
                                            |> Maybe.withDefault failure

                                    _ ->
                                        failure
                            )
                        |> Maybe.withDefault failure
            )


decodeTravelHistory : Decoder TravelHistory
decodeTravelHistory =
    decodeEverySet decodeTravelHistorySign
        |> field "travel_history"
        |> decodeAcuteIllnessMeasurement


decodeTravelHistorySign : Decoder TravelHistorySign
decodeTravelHistorySign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "covid19-country" ->
                        succeed COVID19Country

                    "none" ->
                        succeed NoTravelHistorySigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized TravelHistorySign"
            )


decodeTreatmentReview : Decoder TreatmentReview
decodeTreatmentReview =
    decodeEverySet decodeTreatmentReviewSign
        |> field "treatment_history"
        |> decodeAcuteIllnessMeasurement


decodeTreatmentReviewSign : Decoder TreatmentReviewSign
decodeTreatmentReviewSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "fever-past-six-hours" ->
                        succeed FeverPast6Hours

                    "fever-past-six-hours-helped" ->
                        succeed FeverPast6HoursHelped

                    "malaria-today" ->
                        succeed MalariaToday

                    "malaria-today-helped" ->
                        succeed MalariaTodayHelped

                    "malaria-past-month" ->
                        succeed MalariaWithinPastMonth

                    "malaria-past-month-helped" ->
                        succeed MalariaWithinPastMonthHelped

                    "none" ->
                        succeed NoTreatmentReviewSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized TreatmentReviewSign"
            )


decodeExposure : Decoder Exposure
decodeExposure =
    decodeEverySet decodeExposureSign
        |> field "exposure"
        |> decodeAcuteIllnessMeasurement


decodeExposureSign : Decoder ExposureSign
decodeExposureSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "covid19-symptoms" ->
                        succeed COVID19Symptoms

                    "none" ->
                        succeed NoExposureSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized ExposureSign"
            )


decodeIsolation : Decoder Isolation
decodeIsolation =
    succeed IsolationValue
        |> required "isolation" (decodeEverySet decodeIsolationSign)
        |> required "reason_for_not_isolating" (decodeEverySet decodeReasonForNotIsolating)
        |> decodeAcuteIllnessMeasurement


decodeIsolationSign : Decoder IsolationSign
decodeIsolationSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "patient-isolated" ->
                        succeed PatientIsolated

                    "sign-on-door" ->
                        succeed SignOnDoor

                    "health-education" ->
                        succeed HealthEducation

                    "none" ->
                        succeed NoIsolationSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized IsolationSign"
            )


decodeReasonForNotIsolating : Decoder ReasonForNotIsolating
decodeReasonForNotIsolating =
    string
        |> andThen
            (\sign ->
                case sign of
                    "no-space" ->
                        succeed NoSpace

                    "too-ill" ->
                        succeed TooIll

                    "can-not-separate" ->
                        succeed CanNotSeparateFromFamily

                    "other" ->
                        succeed OtherReason

                    "n-a" ->
                        succeed IsolationReasonNotApplicable

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized IsolationSign"
            )


decodeHCContact : Decoder HCContact
decodeHCContact =
    succeed HCContactValue
        |> required "hc_contact" (decodeEverySet decodeHCContactSign)
        |> required "hc_recommendation" (decodeEverySet decodeHCRecommendation)
        |> required "hc_response_time" (decodeEverySet decodeResponsePeriod)
        |> required "ambulance_arrival_time" (decodeEverySet decodeResponsePeriod)
        |> decodeAcuteIllnessMeasurement


decodeHCContactSign : Decoder HCContactSign
decodeHCContactSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "contact-hc" ->
                        succeed ContactedHealthCenter

                    "none" ->
                        succeed NoHCContactSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized HCContactSign"
            )


decodeHCRecommendation : Decoder HCRecommendation
decodeHCRecommendation =
    string
        |> andThen
            (\sign ->
                case sign of
                    "send-ambulance" ->
                        succeed SendAmbulance

                    "home-isolation" ->
                        succeed HomeIsolation

                    "come-to-hc" ->
                        succeed ComeToHealthCenter

                    "chw-monitoring" ->
                        succeed ChwMonitoring

                    "n-a" ->
                        succeed HCRecommendationNotApplicable

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized HCRecommendation"
            )


decodeResponsePeriod : Decoder ResponsePeriod
decodeResponsePeriod =
    string
        |> andThen
            (\sign ->
                case sign of
                    "less-than-30m" ->
                        succeed LessThan30Min

                    "30m-1h" ->
                        succeed Between30min1Hour

                    "1h-2h" ->
                        succeed Between1Hour2Hour

                    "2h-1d" ->
                        succeed Between2Hour1Day

                    "n-a" ->
                        succeed ResponsePeriodNotApplicable

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized ResponsePeriod"
            )


decodeCall114 : Decoder Call114
decodeCall114 =
    succeed Call114Value
        |> required "114_contact" (decodeEverySet decodeCall114Sign)
        |> required "114_recommendation" (decodeEverySet (decodeWithFallback NoneOtherRecommendation114 decodeRecommendation114))
        |> required "site_recommendation" (decodeEverySet (decodeWithFallback RecommendationSiteNotApplicable decodeRecommendationSite))
        |> decodeAcuteIllnessMeasurement


decodeCall114Sign : Decoder Call114Sign
decodeCall114Sign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "call-114" ->
                        succeed Call114

                    "contact-site" ->
                        succeed ContactSite

                    "none" ->
                        succeed NoCall114Signs

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized Call114Sign"
            )


decodeRecommendation114 : Decoder Recommendation114
decodeRecommendation114 =
    string
        |> andThen
            (\sign ->
                case sign of
                    "send-to-hc" ->
                        succeed SendToHealthCenter

                    "send-to-rrtc" ->
                        succeed SendToRRTCenter

                    "send-to-hospital" ->
                        succeed SendToHospital

                    "other" ->
                        succeed OtherRecommendation114

                    "none-no-answer" ->
                        succeed NoneNoAnswer

                    "none-busy-signal" ->
                        succeed NoneBusySignal

                    "none-other" ->
                        succeed NoneOtherRecommendation114

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized Recommendation114"
            )


decodeRecommendationSite : Decoder RecommendationSite
decodeRecommendationSite =
    string
        |> andThen
            (\sign ->
                case sign of
                    "team-to-village" ->
                        succeed TeamComeToVillage

                    "send-with-form" ->
                        succeed SendToSiteWithForm

                    "other" ->
                        succeed OtherRecommendationSite

                    "none-sent-with-form" ->
                        succeed NoneSentWithForm

                    "none-patient-refused" ->
                        succeed NonePatientRefused

                    "none-other" ->
                        succeed NoneOtherRecommendationSite

                    "n-a" ->
                        succeed RecommendationSiteNotApplicable

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized RecommendationSite"
            )


decodeAcuteIllnessMuac : Decoder AcuteIllnessMuac
decodeAcuteIllnessMuac =
    field "muac" decodeFloat
        |> map MuacInCm
        |> decodeAcuteIllnessMeasurement


decodeTreatmentOngoing : Decoder TreatmentOngoing
decodeTreatmentOngoing =
    succeed TreatmentOngoingValue
        |> required "treatment_ongoing" (decodeEverySet decodeTreatmentOngoingSign)
        |> required "reason_for_not_taking" decodeReasonForNotTaking
        |> required "missed_doses" decodeInt
        |> required "adverse_events" (decodeEverySet decodeAdverseEvent)
        |> decodeAcuteIllnessMeasurement


decodeTreatmentOngoingSign : Decoder TreatmentOngoingSign
decodeTreatmentOngoingSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "taken-as-prescribed" ->
                        succeed TakenAsPrescribed

                    "missed-doses" ->
                        succeed MissedDoses

                    "feel-better" ->
                        succeed FeelingBetter

                    "side-effects" ->
                        succeed SideEffects

                    "none" ->
                        succeed NoTreatmentOngoingSign

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized TreatmentOngoingSign"
            )


decodeReasonForNotTaking : Decoder ReasonForNotTaking
decodeReasonForNotTaking =
    string
        |> andThen
            (\reason ->
                case reason of
                    "adverse-event" ->
                        succeed NotTakingAdverseEvent

                    "no-money" ->
                        succeed NotTakingNoMoney

                    "memory-problems" ->
                        succeed NotTakingMemoryProblems

                    "other" ->
                        succeed NotTakingOther

                    "none" ->
                        succeed NoReasonForNotTakingSign

                    _ ->
                        fail <|
                            reason
                                ++ " is not a recognized ReasonForNotTaking"
            )


decodeAdverseEvent : Decoder AdverseEvent
decodeAdverseEvent =
    string
        |> andThen
            (\event ->
                case event of
                    "rash-itching" ->
                        succeed AdverseEventRashOrItching

                    "fever" ->
                        succeed AdverseEventFever

                    "diarrhea" ->
                        succeed AdverseEventDiarrhea

                    "vomiting" ->
                        succeed AdverseEventVomiting

                    "fatigue" ->
                        succeed AdverseEventFatigue

                    "other" ->
                        succeed AdverseEventOther

                    "none" ->
                        succeed NoAdverseEvent

                    _ ->
                        fail <|
                            event
                                ++ " is not a recognized AdverseEvent"
            )


decodeAcuteIllnessDangerSigns : Decoder AcuteIllnessDangerSigns
decodeAcuteIllnessDangerSigns =
    decodeEverySet decodeAcuteIllnessDangerSign
        |> field "acute_illness_danger_signs"
        |> decodeAcuteIllnessMeasurement


decodeAcuteIllnessDangerSign : Decoder AcuteIllnessDangerSign
decodeAcuteIllnessDangerSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "condition-not-improving" ->
                        succeed DangerSignConditionNotImproving

                    "unable-drink-suck" ->
                        succeed DangerSignUnableDrinkSuck

                    "vomiting" ->
                        succeed DangerSignVomiting

                    "convulsions" ->
                        succeed DangerSignConvulsions

                    "lethargy-unconsciousness" ->
                        succeed DangerSignLethargyUnconsciousness

                    "respiratory-distress" ->
                        succeed DangerSignRespiratoryDistress

                    "spontaneous-bleeding" ->
                        succeed DangerSignSpontaneousBleeding

                    "bloody-diarrhea" ->
                        succeed DangerSignBloodyDiarrhea

                    "new-skip-rash" ->
                        succeed DangerSignNewSkinRash

                    "none" ->
                        succeed NoAcuteIllnessDangerSign

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized AcuteIllnessDangerSign"
            )


decodeAcuteIllnessNutrition : Decoder AcuteIllnessNutrition
decodeAcuteIllnessNutrition =
    decodeEverySet decodeChildNutritionSign
        |> field "nutrition_signs"
        |> decodeAcuteIllnessMeasurement


decodeHealthEducation : Decoder HealthEducation
decodeHealthEducation =
    decodeAcuteIllnessMeasurement decodeHealthEducationValue


decodeNutritionHealthEducation : Decoder NutritionHealthEducation
decodeNutritionHealthEducation =
    decodeNutritionMeasurement decodeHealthEducationValue


decodeGroupHealthEducation : Decoder GroupHealthEducation
decodeGroupHealthEducation =
    decodeGroupMeasurement decodeHealthEducationValue


decodeHealthEducationValue : Decoder HealthEducationValue
decodeHealthEducationValue =
    succeed HealthEducationValue
        |> required "health_education_signs" (decodeEverySet decodeHealthEducationSign)
        |> optional "reason_not_given_education" decodeReasonForNotProvidingHealthEducation NoReasonForNotProvidingHealthEducation


decodeHealthEducationSign : Decoder HealthEducationSign
decodeHealthEducationSign =
    string
        |> andThen
            (\sign ->
                case sign of
                    "education-for-diagnosis" ->
                        succeed MalariaPrevention

                    "none" ->
                        succeed NoHealthEducationSigns

                    _ ->
                        fail <|
                            sign
                                ++ " is not a recognized HealthEducationSign"
            )


decodeReasonForNotProvidingHealthEducation : Decoder ReasonForNotProvidingHealthEducation
decodeReasonForNotProvidingHealthEducation =
    string
        |> andThen
            (\reason ->
                case reason of
                    "needs-emergency-referral" ->
                        succeed PatientNeedsEmergencyReferral

                    "received-emergency-case" ->
                        succeed ReceivedEmergencyCase

                    "lack-of-appropriate-education-guide" ->
                        succeed LackOfAppropriateEducationUserGuide

                    "patient-refused" ->
                        succeed PatientRefused

                    "patient-too-ill" ->
                        succeed PatientTooIll

                    "none" ->
                        succeed NoReasonForNotProvidingHealthEducation

                    _ ->
                        fail <|
                            reason
                                ++ " is not a recognized ReasonForNotProvidingHealthEducation"
            )


decodeNutritionAssesment : Decoder (EverySet NutritionAssesment)
decodeNutritionAssesment =
    map2 postProcessNutritionAssesment
        (field "nutrition_assesment" (decodeEverySet decodeNutritionAssesmentFromString))
        (field "nutrition_signs" (decodeEverySet decodeChildNutritionSign))


decodeNutritionAssesmentFromString : Decoder NutritionAssesment
decodeNutritionAssesmentFromString =
    string
        |> andThen
            (\s ->
                nutritionAssesmentFromString s
                    |> Maybe.map succeed
                    |> Maybe.withDefault (s ++ " is not a recognized NutritionAssesment" |> fail)
            )


postProcessNutritionAssesment : EverySet NutritionAssesment -> EverySet ChildNutritionSign -> EverySet NutritionAssesment
postProcessNutritionAssesment assesmentFromString nutritionSign =
    assesmentFromString
        |> EverySet.toList
        |> List.head
        |> Maybe.map
            (\assesment ->
                case assesment of
                    AssesmentMalnutritionSigns _ ->
                        EverySet.toList nutritionSign
                            |> AssesmentMalnutritionSigns
                            |> EverySet.singleton

                    _ ->
                        assesmentFromString
            )
        |> Maybe.withDefault assesmentFromString
