module Backend.Measurement.Encoder exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Counseling.Encoder exposing (encodeCounselingTiming)
import Backend.Counseling.Model exposing (CounselingTiming)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Measurement.Utils exposing (..)
import Backend.Person.Encoder exposing (encodeGender)
import Backend.Person.Utils exposing (genderToString)
import Backend.PrenatalEncounter.Encoder exposing (encodePrenatalDiagnosis)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (formatYYYYMMDD)
import Json.Encode as Encoder exposing (Value, bool, float, int, list, object, string)
import Json.Encode.Extra exposing (maybe)
import Restful.Endpoint exposing (EntityUuid(..), encodeEntityUuid, fromEntityUuid)
import Translate.Utils exposing (encodeLanguage)
import Utils.Json exposing (encodeEverySet)


encodeHeight : Height -> List ( String, Value )
encodeHeight =
    encodeGroupMeasurement (encodeHeightValueWithType "height")


encodeNutritionHeight : NutritionHeight -> List ( String, Value )
encodeNutritionHeight =
    encodeNutritionMeasurement (encodeHeightValueWithType "nutrition_height")


encodeWellChildHeight : WellChildHeight -> List ( String, Value )
encodeWellChildHeight =
    encodeWellChildMeasurement (encodeHeightValueWithType "well_child_height")


encodeHeightValueWithType : String -> HeightInCm -> List ( String, Value )
encodeHeightValueWithType type_ (HeightInCm height) =
    [ ( "height", float height )
    , ( "deleted", bool False )
    , ( "type", string type_ )
    ]


encodeMuac : Muac -> List ( String, Value )
encodeMuac =
    encodeGroupMeasurement (encodeMuacValueWithType "muac")


encodeNutritionMuac : NutritionMuac -> List ( String, Value )
encodeNutritionMuac =
    encodeNutritionMeasurement (encodeMuacValueWithType "nutrition_muac")


encodeWellChildMuac : WellChildMuac -> List ( String, Value )
encodeWellChildMuac =
    encodeWellChildMeasurement (encodeMuacValueWithType "well_child_muac")


encodeMuacValueWithType : String -> MuacInCm -> List ( String, Value )
encodeMuacValueWithType type_ (MuacInCm muac) =
    [ ( "muac", float muac )
    , ( "deleted", bool False )
    , ( "type", string type_ )
    ]


encodeWeight : Weight -> List ( String, Value )
encodeWeight =
    encodeGroupMeasurement (encodeWeightValueWithType "weight")


encodeNutritionWeight : NutritionWeight -> List ( String, Value )
encodeNutritionWeight =
    encodeNutritionMeasurement (encodeWeightValueWithType "nutrition_weight")


encodeWellChildWeight : WellChildWeight -> List ( String, Value )
encodeWellChildWeight =
    encodeWellChildMeasurement (encodeWeightValueWithType "well_child_weight")


encodeWeightValueWithType : String -> WeightInKg -> List ( String, Value )
encodeWeightValueWithType type_ (WeightInKg weight) =
    [ ( "weight", float weight )
    , ( "deleted", bool False )
    , ( "type", string type_ )
    ]


encodePhoto : Photo -> List ( String, Value )
encodePhoto =
    encodeGroupMeasurement (encodePhotoUrlWithType "photo")


encodeNutritionPhoto : NutritionPhoto -> List ( String, Value )
encodeNutritionPhoto =
    encodeNutritionMeasurement (encodePhotoUrlWithType "nutrition_photo")


encodePrenatalPhoto : PrenatalPhoto -> List ( String, Value )
encodePrenatalPhoto =
    encodePrenatalMeasurement (encodePhotoUrlWithType "prenatal_photo")


encodeWellChildPhoto : WellChildPhoto -> List ( String, Value )
encodeWellChildPhoto =
    encodeWellChildMeasurement (encodePhotoUrlWithType "well_child_photo")


encodePhotoUrlWithType : String -> PhotoUrl -> List ( String, Value )
encodePhotoUrlWithType type_ (PhotoUrl url) =
    [ ( "photo", string url )
    , ( "deleted", bool False )
    , ( "type", string type_ )
    ]


encodePregnancyTest : PregnancyTest -> List ( String, Value )
encodePregnancyTest =
    encodePrenatalMeasurement encodeCHWPregnancyTestValue


encodeCHWPregnancyTestValue : PregnancyTestResult -> List ( String, Value )
encodeCHWPregnancyTestValue value =
    [ ( "urine_pregnancy_test", encodePregnancyTestResult value )
    , ( "deleted", bool False )
    , ( "type", string "pregnancy_testing" )
    ]


encodePregnancyTestResult : PregnancyTestResult -> Value
encodePregnancyTestResult =
    pregnancyTestResultToString >> string


encodePrenatalHealthEducation : PrenatalHealthEducation -> List ( String, Value )
encodePrenatalHealthEducation =
    encodePrenatalMeasurement encodePrenatalHealthEducationValue


encodePrenatalHealthEducationValue : PrenatalHealthEducationValue -> List ( String, Value )
encodePrenatalHealthEducationValue value =
    let
        signsPhase2 =
            Maybe.map (\signs -> [ ( "health_education_signs_ph2", encodeEverySet encodePrenatalHealthEducationSign signs ) ])
                value.signsPhase2
                |> Maybe.withDefault []
    in
    [ ( "prenatal_health_education", encodeEverySet encodePrenatalHealthEducationSign value.signs )
    , ( "deleted", bool False )
    , ( "type", string "prenatal_health_education" )
    ]
        ++ signsPhase2


encodePrenatalHealthEducationSign : PrenatalHealthEducationSign -> Value
encodePrenatalHealthEducationSign sign =
    string <|
        case sign of
            EducationExpectations ->
                "expectations"

            EducationVisitsReview ->
                "visits-review"

            EducationWarningSigns ->
                "warning-signs"

            EducationHemorrhaging ->
                "hemorrhaging"

            EducationFamilyPlanning ->
                "family-planning"

            EducationBreastfeeding ->
                "breastfeeding"

            EducationImmunization ->
                "immunization"

            EducationHygiene ->
                "hygiene"

            EducationPositiveHIV ->
                "positive-hiv"

            EducationSaferSexHIV ->
                "safer-sex-hiv"

            EducationPartnerTesting ->
                "partner-testing"

            EducationNauseaVomiting ->
                "nausea-vomiting"

            EducationLegCramps ->
                "leg-cramps"

            EducationLowBackPain ->
                "low-back-pain"

            EducationConstipation ->
                "constipation"

            EducationHeartburn ->
                "heartburn"

            EducationVaricoseVeins ->
                "varicose-veins"

            EducationLegPainRedness ->
                "leg-pain-redness"

            EducationPelvicPain ->
                "pelvic-pain"

            EducationSaferSex ->
                "safer-sex"

            EducationHIVDetectableViralLoad ->
                "hiv-detectable-viral-load"

            EducationMentalHealth ->
                "mental-health"

            EducationDiabetes ->
                "diabetes"

            EducationEarlyMastitisOrEngorgment ->
                "early-mastitis-engorgment"

            EducationMastitis ->
                "mastitis"

            NoPrenatalHealthEducationSigns ->
                "none"


encodePrenatalFollowUp : PrenatalFollowUp -> List ( String, Value )
encodePrenatalFollowUp =
    encodePrenatalMeasurement encodePrenatalFollowUpValue


encodePrenatalFollowUpValue : PrenatalFollowUpValue -> List ( String, Value )
encodePrenatalFollowUpValue value =
    let
        resolutionDate =
            Maybe.map (\date -> [ ( "date_concluded", Gizra.NominalDate.encodeYYYYMMDD date ) ])
                value.resolutionDate
                |> Maybe.withDefault []
    in
    [ ( "follow_up_options", encodeEverySet encodeFollowUpOption value.options )
    , ( "prenatal_assesment", encodePrenatalAssesment value.assesment )
    , ( "deleted", bool False )
    , ( "type", string "prenatal_follow_up" )
    ]
        ++ resolutionDate


encodePrenatalAssesment : PrenatalAssesment -> Value
encodePrenatalAssesment assesment =
    string <|
        case assesment of
            AssesmentNormalPregnancy ->
                "normal"

            AssesmentHighRiskPregnancy ->
                "high-risk"


encodePrenatalSendToHC : PrenatalSendToHC -> List ( String, Value )
encodePrenatalSendToHC =
    encodePrenatalMeasurement encodePrenatalReferralValue


encodeAppointmentConfirmation : PrenatalAppointmentConfirmation -> List ( String, Value )
encodeAppointmentConfirmation =
    encodePrenatalMeasurement encodeAppointmentConfirmationValue


encodeAppointmentConfirmationValue : PrenatalAppointmentConfirmationValue -> List ( String, Value )
encodeAppointmentConfirmationValue value =
    [ ( "appointment_confirmation", Gizra.NominalDate.encodeYYYYMMDD value.date )
    , ( "deleted", bool False )
    , ( "type", string "appointment_confirmation" )
    ]


encodePrenatalBloodGpRsTest : PrenatalBloodGpRsTest -> List ( String, Value )
encodePrenatalBloodGpRsTest =
    encodePrenatalMeasurement encodeBloodGpRsTestValue


encodeBloodGpRsTestValue : BloodGpRsTestValue (EntityUuid a) -> List ( String, Value )
encodeBloodGpRsTestValue value =
    let
        executionDate =
            Maybe.map
                (\date -> [ ( "execution_date", Gizra.NominalDate.encodeYYYYMMDD date ) ])
                value.executionDate
                |> Maybe.withDefault []

        originatingEncounter =
            Maybe.map
                (\originEncounter ->
                    [ ( "originating_encounter", encodeEntityUuid originEncounter ) ]
                )
                value.originatingEncounter
                |> Maybe.withDefault []

        results =
            Maybe.map2
                (\bloodGroup rhesus ->
                    [ ( "blood_group", encodeBloodGroup bloodGroup )
                    , ( "rhesus", encodeRhesus rhesus )
                    ]
                )
                value.bloodGroup
                value.rhesus
                |> Maybe.withDefault []
    in
    ( "test_execution_note", encodeTestExecutionNote value.executionNote )
        :: executionDate
        ++ originatingEncounter
        ++ results
        ++ [ ( "deleted", bool False )
           , ( "type", string "prenatal_blood_gprs_test" )
           ]


encodeBloodGroup : BloodGroup -> Value
encodeBloodGroup =
    bloodGroupToString >> string


encodeRhesus : Rhesus -> Value
encodeRhesus =
    rhesusToString >> string


encodePrenatalHemoglobinTest : PrenatalHemoglobinTest -> List ( String, Value )
encodePrenatalHemoglobinTest =
    encodePrenatalMeasurement encodeHemoglobinTestValue


encodeHemoglobinTestValue : HemoglobinTestValue -> List ( String, Value )
encodeHemoglobinTestValue value =
    let
        executionDate =
            Maybe.map
                (\date -> [ ( "execution_date", Gizra.NominalDate.encodeYYYYMMDD date ) ])
                value.executionDate
                |> Maybe.withDefault []

        result =
            Maybe.map
                (\hemoglobinCount ->
                    [ ( "hemoglobin_count", float hemoglobinCount ) ]
                )
                value.hemoglobinCount
                |> Maybe.withDefault []
    in
    ( "test_execution_note", encodeTestExecutionNote value.executionNote )
        :: executionDate
        ++ result
        ++ [ ( "deleted", bool False )
           , ( "type", string "prenatal_hemoglobin_test" )
           ]


encodePrenatalHepatitisBTest : PrenatalHepatitisBTest -> List ( String, Value )
encodePrenatalHepatitisBTest =
    encodePrenatalMeasurement encodeHepatitisBTestValue


encodeHepatitisBTestValue : HepatitisBTestValue (EntityUuid a) -> List ( String, Value )
encodeHepatitisBTestValue value =
    let
        executionDate =
            Maybe.map
                (\date -> [ ( "execution_date", Gizra.NominalDate.encodeYYYYMMDD date ) ])
                value.executionDate
                |> Maybe.withDefault []

        result =
            Maybe.map
                (\testResult ->
                    [ ( "test_result", encodeTestResult testResult ) ]
                )
                value.testResult
                |> Maybe.withDefault []

        originatingEncounter =
            Maybe.map
                (\originEncounter ->
                    [ ( "originating_encounter", encodeEntityUuid originEncounter ) ]
                )
                value.originatingEncounter
                |> Maybe.withDefault []
    in
    ( "test_execution_note", encodeTestExecutionNote value.executionNote )
        :: executionDate
        ++ result
        ++ originatingEncounter
        ++ [ ( "deleted", bool False )
           , ( "type", string "prenatal_hepatitis_b_test" )
           ]


encodePrenatalHIVTest : PrenatalHIVTest -> List ( String, Value )
encodePrenatalHIVTest =
    encodePrenatalMeasurement (encodeHIVTestValue "prenatal_hiv_test")


encodeHIVTestValue : String -> HIVTestValue -> List ( String, Value )
encodeHIVTestValue type_ value =
    let
        executionDate =
            Maybe.map
                (\date -> [ ( "execution_date", Gizra.NominalDate.encodeYYYYMMDD date ) ])
                value.executionDate
                |> Maybe.withDefault []

        result =
            Maybe.map
                (\testResult -> [ ( "test_result", encodeTestResult testResult ) ])
                value.testResult
                |> Maybe.withDefault []

        hivSigns =
            Maybe.map
                (\signs ->
                    if EverySet.isEmpty signs then
                        EverySet.singleton NoPrenatalHIVSign

                    else
                        signs
                )
                value.hivSigns
                |> Maybe.withDefault (EverySet.singleton NoPrenatalHIVSign)
    in
    ( "test_execution_note", encodeTestExecutionNote value.executionNote )
        :: executionDate
        ++ result
        ++ [ ( "hiv_signs", encodeEverySet encodePrenatalHIVSign hivSigns ) ]
        ++ [ ( "deleted", bool False )
           , ( "type", string type_ )
           ]


encodePrenatalHIVPCRTest : PrenatalHIVPCRTest -> List ( String, Value )
encodePrenatalHIVPCRTest =
    encodePrenatalMeasurement encodeHIVPCRTestValue


encodeHIVPCRTestValue : HIVPCRTestValue -> List ( String, Value )
encodeHIVPCRTestValue value =
    let
        executionDate =
            Maybe.map
                (\date -> [ ( "execution_date", Gizra.NominalDate.encodeYYYYMMDD date ) ])
                value.executionDate
                |> Maybe.withDefault []

        hivViralLoadStatus =
            Maybe.map
                (\viralLoadStatus -> [ ( "hiv_viral_load_status", encodeViralLoadStatus viralLoadStatus ) ])
                value.hivViralLoadStatus
                |> Maybe.withDefault []

        hivViralLoad =
            Maybe.map
                (\viralLoad -> [ ( "hiv_viral_load", float viralLoad ) ])
                value.hivViralLoad
                |> Maybe.withDefault []
    in
    ( "test_execution_note", encodeTestExecutionNote value.executionNote )
        :: executionDate
        ++ hivViralLoadStatus
        ++ hivViralLoad
        ++ [ ( "deleted", bool False )
           , ( "type", string "prenatal_hiv_pcr_test" )
           ]


encodeViralLoadStatus : ViralLoadStatus -> Value
encodeViralLoadStatus value =
    string <|
        case value of
            ViralLoadDetectable ->
                "detectable"

            ViralLoadUndetectable ->
                "undetectable"


encodePrenatalHIVSign : PrenatalHIVSign -> Value
encodePrenatalHIVSign =
    prenatalHIVSignToString >> string


encodePrenatalMalariaTest : PrenatalMalariaTest -> List ( String, Value )
encodePrenatalMalariaTest =
    encodePrenatalMeasurement encodeMalariaTestValue


encodeMalariaTestValue : MalariaTestValue -> List ( String, Value )
encodeMalariaTestValue value =
    let
        executionDate =
            Maybe.map
                (\date -> [ ( "execution_date", Gizra.NominalDate.encodeYYYYMMDD date ) ])
                value.executionDate
                |> Maybe.withDefault []

        result =
            Maybe.map
                (\testResult -> [ ( "test_result", encodeTestResult testResult ) ])
                value.testResult
                |> Maybe.withDefault []
    in
    ( "test_execution_note", encodeTestExecutionNote value.executionNote )
        :: executionDate
        ++ result
        ++ [ ( "deleted", bool False )
           , ( "type", string "prenatal_malaria_test" )
           ]


encodePrenatalRandomBloodSugarTest : PrenatalRandomBloodSugarTest -> List ( String, Value )
encodePrenatalRandomBloodSugarTest =
    encodePrenatalMeasurement (encodeRandomBloodSugarTestValue "prenatal_random_blood_sugar_test")


encodeRandomBloodSugarTestValue : String -> RandomBloodSugarTestValue (EntityUuid a) -> List ( String, Value )
encodeRandomBloodSugarTestValue type_ value =
    let
        executionDate =
            Maybe.map
                (\date -> [ ( "execution_date", Gizra.NominalDate.encodeYYYYMMDD date ) ])
                value.executionDate
                |> Maybe.withDefault []

        testPrerequisites =
            Maybe.map
                (\prerequisites -> [ ( "test_prerequisites", encodeEverySet encodeTestPrerequisite prerequisites ) ])
                value.testPrerequisites
                |> Maybe.withDefault []

        originatingEncounter =
            Maybe.map
                (\originEncounter ->
                    [ ( "originating_encounter", encodeEntityUuid originEncounter ) ]
                )
                value.originatingEncounter
                |> Maybe.withDefault []

        result =
            Maybe.map
                (\sugarCount ->
                    [ ( "sugar_count", float sugarCount ) ]
                )
                value.sugarCount
                |> Maybe.withDefault []
    in
    ( "test_execution_note", encodeTestExecutionNote value.executionNote )
        :: executionDate
        ++ testPrerequisites
        ++ originatingEncounter
        ++ result
        ++ [ ( "deleted", bool False )
           , ( "type", string type_ )
           ]


encodeTestPrerequisite : TestPrerequisite -> Value
encodeTestPrerequisite value =
    string <|
        case value of
            PrerequisiteFastFor12h ->
                "fasting-12h"

            PrerequisiteImmediateResult ->
                "immediate-result"

            NoTestPrerequisites ->
                "none"


encodePrenatalSyphilisTest : PrenatalSyphilisTest -> List ( String, Value )
encodePrenatalSyphilisTest =
    encodePrenatalMeasurement encodeSyphilisTestValue


encodeSyphilisTestValue : SyphilisTestValue (EntityUuid a) -> List ( String, Value )
encodeSyphilisTestValue value =
    let
        executionDate =
            Maybe.map
                (\date -> [ ( "execution_date", Gizra.NominalDate.encodeYYYYMMDD date ) ])
                value.executionDate
                |> Maybe.withDefault []

        result =
            Maybe.map
                (\testResult -> [ ( "test_result", encodeTestResult testResult ) ])
                value.testResult
                |> Maybe.withDefault []

        illnessSymptoms =
            Maybe.map
                (\symptoms -> [ ( "illness_symptoms", encodeEverySet encodeIllnessSymptom symptoms ) ])
                value.symptoms
                |> Maybe.withDefault []

        originatingEncounter =
            Maybe.map
                (\originEncounter ->
                    [ ( "originating_encounter", encodeEntityUuid originEncounter ) ]
                )
                value.originatingEncounter
                |> Maybe.withDefault []
    in
    ( "test_execution_note", encodeTestExecutionNote value.executionNote )
        :: executionDate
        ++ result
        ++ illnessSymptoms
        ++ originatingEncounter
        ++ [ ( "deleted", bool False )
           , ( "type", string "prenatal_syphilis_test" )
           ]


encodeIllnessSymptom : IllnessSymptom -> Value
encodeIllnessSymptom =
    illnessSymptomToString >> string


encodePrenatalUrineDipstickTest : PrenatalUrineDipstickTest -> List ( String, Value )
encodePrenatalUrineDipstickTest =
    encodePrenatalMeasurement (encodeUrineDipstickTestValue "prenatal_urine_dipstick_test")


encodeUrineDipstickTestValue : String -> UrineDipstickTestValue -> List ( String, Value )
encodeUrineDipstickTestValue type_ value =
    let
        testVariant =
            Maybe.map
                (\variant -> [ ( "test_variant", encodeTestVariant variant ) ])
                value.testVariant
                |> Maybe.withDefault []

        executionDate =
            Maybe.map
                (\date -> [ ( "execution_date", Gizra.NominalDate.encodeYYYYMMDD date ) ])
                value.executionDate
                |> Maybe.withDefault []

        encodeField encoder fieldName fieldValue =
            Maybe.map
                (\value_ ->
                    [ ( fieldName, encoder value_ ) ]
                )
                fieldValue
                |> Maybe.withDefault []

        protein =
            encodeField encodeProteinValue "protein" value.protein

        ph =
            encodeField encodePHValue "ph" value.ph

        glucose =
            encodeField encodeGlucoseValue "glucose" value.glucose

        leukocytes =
            encodeField encodeLeukocytesValue "leukocytes" value.leukocytes

        nitrite =
            encodeField encodeNitriteValue "nitrite" value.nitrite

        urobilinogen =
            encodeField encodeUrobilinogenValue "urobilinogen" value.urobilinogen

        haemoglobin =
            encodeField encodeHaemoglobinValue "haemoglobin" value.haemoglobin

        ketone =
            encodeField encodeKetoneValue "ketone" value.ketone

        bilirubin =
            encodeField encodeBilirubinValue "bilirubin" value.bilirubin
    in
    ( "test_execution_note", encodeTestExecutionNote value.executionNote )
        :: testVariant
        ++ executionDate
        ++ protein
        ++ ph
        ++ glucose
        ++ leukocytes
        ++ nitrite
        ++ urobilinogen
        ++ haemoglobin
        ++ ketone
        ++ bilirubin
        ++ [ ( "deleted", bool False )
           , ( "type", string type_ )
           ]


encodeTestVariant : TestVariant -> Value
encodeTestVariant value =
    string <|
        case value of
            VariantShortTest ->
                "short"

            VariantLongTest ->
                "long"


encodeProteinValue : ProteinValue -> Value
encodeProteinValue =
    proteinValueToString >> string


encodePHValue : PHValue -> Value
encodePHValue =
    phValueToString >> string


encodeGlucoseValue : GlucoseValue -> Value
encodeGlucoseValue =
    glucoseValueToString >> string


encodeLeukocytesValue : LeukocytesValue -> Value
encodeLeukocytesValue =
    leukocytesValueToString >> string


encodeNitriteValue : NitriteValue -> Value
encodeNitriteValue =
    nitriteValueToString >> string


encodeUrobilinogenValue : UrobilinogenValue -> Value
encodeUrobilinogenValue =
    urobilinogenValueToString >> string


encodeHaemoglobinValue : HaemoglobinValue -> Value
encodeHaemoglobinValue =
    haemoglobinValueToString >> string


encodeKetoneValue : KetoneValue -> Value
encodeKetoneValue =
    ketoneValueToString >> string


encodeBilirubinValue : BilirubinValue -> Value
encodeBilirubinValue =
    bilirubinValueToString >> string


encodeTestExecutionNote : TestExecutionNote -> Value
encodeTestExecutionNote value =
    string <|
        case value of
            TestNoteRunToday ->
                "run-today"

            TestNoteRunPreviously ->
                "run-previously"

            TestNoteLackOfReagents ->
                "lack-of-reagents"

            TestNoteLackOfOtherSupplies ->
                "lack-of-other-supplies"

            TestNoteNoEquipment ->
                "no-equipment"

            TestNoteBrokenEquipment ->
                "broken-equipment"

            TestNoteNotIndicated ->
                "not-indicated"

            TestNoteKnownAsPositive ->
                "known-as-positive"

            TestNoteToBeDoneAtHospital ->
                "to-be-done-at-hospital"


encodeTestResult : TestResult -> Value
encodeTestResult =
    testResultToString >> string


encodePrenatalLabsResults : PrenatalLabsResults -> List ( String, Value )
encodePrenatalLabsResults =
    encodePrenatalMeasurement (encodeLabsResultsValue "prenatal_labs_results")


encodeLabsResultsValue : String -> LabsResultsValue -> List ( String, Value )
encodeLabsResultsValue type_ value =
    let
        patientNotified =
            if value.patientNotified then
                [ ( "patient_notified", bool True ) ]

            else
                []
    in
    [ ( "performed_tests", encodeEverySet encodeLaboratoryTest value.performedTests )
    , ( "completed_tests", encodeEverySet encodeLaboratoryTest value.completedTests )
    , ( "date_concluded", Gizra.NominalDate.encodeYYYYMMDD value.resolutionDate )
    ]
        ++ patientNotified
        ++ [ ( "deleted", bool False )
           , ( "type", string type_ )
           ]


encodeLaboratoryTest : LaboratoryTest -> Value
encodeLaboratoryTest =
    laboratoryTestToString >> string


encodePrenatalMentalHealth : PrenatalMentalHealth -> List ( String, Value )
encodePrenatalMentalHealth =
    encodePrenatalMeasurement encodePrenatalMentalHealthValue


encodePrenatalMentalHealthValue : PrenatalMentalHealthValue -> List ( String, Value )
encodePrenatalMentalHealthValue value =
    let
        signs =
            Dict.toList value.signs
                |> List.map
                    (\( question, answer ) ->
                        prenatalMentalHealthQuestionToString question
                            ++ "-"
                            ++ prenatalMentalHealthQuestionOptionToString answer
                    )
    in
    [ ( "mental_health_signs", list string signs )
    , ( "specialist_at_hc", bool value.specialistAtHC )
    ]
        ++ [ ( "deleted", bool False )
           , ( "type", string "prenatal_mental_health" )
           ]


encodePrenatalTetanusImmunisation : PrenatalTetanusImmunisation -> List ( String, Value )
encodePrenatalTetanusImmunisation =
    encodePrenatalMeasurement (encodeVaccinationValueWithType "prenatal_tetanus_immunisation")


encodePrenatalBreastfeeding : PrenatalBreastfeeding -> List ( String, Value )
encodePrenatalBreastfeeding =
    encodePrenatalMeasurement encodeBreastfeedingValue


encodeBreastfeedingValue : BreastfeedingValue -> List ( String, Value )
encodeBreastfeedingValue value =
    [ ( "breastfeeding_signs", encodeEverySet encodeBreastfeedingSign value )
    , ( "deleted", bool False )
    , ( "type", string "prenatal_breastfeeding" )
    ]


encodeBreastfeedingSign : BreastfeedingSign -> Value
encodeBreastfeedingSign =
    breastfeedingSignToString >> string


encodePrenatalGUExam : PrenatalGUExam -> List ( String, Value )
encodePrenatalGUExam =
    encodePrenatalMeasurement encodeGUExamValue


encodeGUExamValue : GUExamValue -> List ( String, Value )
encodeGUExamValue value =
    let
        postpartumHealingProblems =
            Maybe.map
                (\problems ->
                    [ ( "postpartum_healing_problem", encodeEverySet encodePostpartumHealingProblem problems ) ]
                )
                value.postpartumHealingProblems
                |> Maybe.withDefault []
    in
    [ ( "vaginal_exam_signs", encodeEverySet encodeVaginalExamSign value.vaginalExamSigns )
    , ( "gu_exam_signs", encodeEverySet encodeGUExamSign value.guExamSigns )
    , ( "deleted", bool False )
    , ( "type", string "prenatal_gu_exam" )
    ]
        ++ postpartumHealingProblems


encodeVaginalExamSign : VaginalExamSign -> Value
encodeVaginalExamSign =
    vaginalExamSignToString >> string


encodeGUExamSign : GUExamSign -> Value
encodeGUExamSign =
    guExamSignToString >> string


encodePostpartumHealingProblem : PostpartumHealingProblem -> Value
encodePostpartumHealingProblem =
    postpartumHealingProblemToString >> string


encodePrenatalSpecialityCare : PrenatalSpecialityCare -> List ( String, Value )
encodePrenatalSpecialityCare =
    encodePrenatalMeasurement encodeSpecialityCareValue


encodeSpecialityCareValue : SpecialityCareValue -> List ( String, Value )
encodeSpecialityCareValue value =
    [ ( "speciality_care_signs", encodeEverySet encodeSpecialityCareSign value )
    , ( "deleted", bool False )
    , ( "type", string "prenatal_speciality_care" )
    ]


encodeSpecialityCareSign : SpecialityCareSign -> Value
encodeSpecialityCareSign sign =
    string <|
        case sign of
            EnrolledToARVProgram ->
                "arv"

            EnrolledToNCDProgram ->
                "ncd"

            NoSpecialityCareSigns ->
                "none"


encodeNutrition : ChildNutrition -> List ( String, Value )
encodeNutrition =
    encodeGroupMeasurement (encodeNutritionValueWithType "nutrition")


encodeNutritionNutrition : NutritionNutrition -> List ( String, Value )
encodeNutritionNutrition =
    encodeNutritionMeasurement (encodeNutritionValueWithType "nutrition_nutrition")


encodeWellChildNutrition : WellChildNutrition -> List ( String, Value )
encodeWellChildNutrition =
    encodeWellChildMeasurement (encodeNutritionValueWithType "well_child_nutrition")


encodeNutritionValueWithType : String -> NutritionValue -> List ( String, Value )
encodeNutritionValueWithType type_ value =
    [ ( "nutrition_signs", encodeEverySet encodeNutritionSign value.signs )
    , ( "nutrition_assesment", encodeEverySet encodeNutritionAssessment value.assesment )
    , ( "deleted", bool False )
    , ( "type", string type_ )
    ]


encodeParticipantConsentValue : ParticipantConsentValue -> List ( String, Value )
encodeParticipantConsentValue consent =
    [ ( "language", encodeLanguage consent.language )
    , ( "participant_form", encodeEntityUuid consent.formId )
    , ( "deleted", bool False )
    , ( "type", string "participant_consent" )
    ]


encodeParticipantConsent : ParticipantConsent -> List ( String, Value )
encodeParticipantConsent =
    encodeGroupMeasurement encodeParticipantConsentValue


encodeCounselingSession : CounselingSession -> List ( String, Value )
encodeCounselingSession =
    encodeGroupMeasurement encodeCounselingSessionValue


encodeCounselingSessionValue : ( CounselingTiming, EverySet CounselingTopicId ) -> List ( String, Value )
encodeCounselingSessionValue ( timing, topics ) =
    [ ( "topics", encodeEverySet encodeEntityUuid topics )
    , ( "timing", encodeCounselingTiming timing )
    , ( "deleted", bool False )
    , ( "type", string "counseling_session" )
    ]


encodeAttendanceValue : Bool -> List ( String, Value )
encodeAttendanceValue attended =
    [ ( "attended", bool attended )
    , ( "deleted", bool False )
    , ( "type", string "attendance" )
    ]


encodeAttendance : Attendance -> List ( String, Value )
encodeAttendance =
    encodeGroupMeasurement encodeAttendanceValue


encodeFamilyPlanningValueWithType : String -> EverySet FamilyPlanningSign -> List ( String, Value )
encodeFamilyPlanningValueWithType type_ signs =
    [ ( "family_planning_signs", encodeEverySet encodeFamilyPlanningSign signs )
    , ( "deleted", bool False )
    , ( "type", string type_ )
    ]


encodeFamilyPlanning : FamilyPlanning -> List ( String, Value )
encodeFamilyPlanning =
    encodeGroupMeasurement (encodeFamilyPlanningValueWithType "family_planning")


encodeLactationValue : EverySet LactationSign -> List ( String, Value )
encodeLactationValue signs =
    [ ( "lactation_signs"
      , EverySet.toList signs
            |> list encodeLactationSign
      )
    , ( "deleted", bool False )
    , ( "type", string "lactation" )
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


encodeHomeVisitMeasurement : (value -> List ( String, Value )) -> HomeVisitMeasurement value -> List ( String, Value )
encodeHomeVisitMeasurement =
    encodeMeasurement "home_visit_encounter"


encodeWellChildMeasurement : (value -> List ( String, Value )) -> WellChildMeasurement value -> List ( String, Value )
encodeWellChildMeasurement =
    encodeMeasurement "well_child_encounter"


encodeNCDMeasurement : (value -> List ( String, Value )) -> NCDMeasurement value -> List ( String, Value )
encodeNCDMeasurement =
    encodeMeasurement "ncd_encounter"


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
    nutritionSignToString >> string


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


encodeBreastExam : BreastExam -> List ( String, Value )
encodeBreastExam =
    encodePrenatalMeasurement encodeBreastExamValue


encodeBreastExamValue : BreastExamValue -> List ( String, Value )
encodeBreastExamValue value =
    let
        dischargeType =
            Maybe.map
                (\type_ ->
                    [ ( "discharge_type", encodeDischargeType type_ ) ]
                )
                value.dischargeType
                |> Maybe.withDefault []
    in
    [ ( "breast", encodeEverySet encodeBreastExamSign value.exam )
    , ( "breast_self_exam", bool value.selfGuidance )
    , ( "deleted", bool False )
    , ( "type", string "breast_exam" )
    ]
        ++ dischargeType


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

            Warmth ->
                "warmth"

            NormalBreast ->
                "normal"


encodeDischargeType : DischargeType -> Value
encodeDischargeType sign =
    string <|
        case sign of
            DischargeMilky ->
                "milky"

            DischargeClear ->
                "clear"

            DischargeBrownOrBloody ->
                "brown-bloody"

            DischargeYellow ->
                "yellow"

            DischargeGreen ->
                "green"


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


encodeCorePhysicalExamValueWithType : String -> CorePhysicalExamValue -> List ( String, Value )
encodeCorePhysicalExamValueWithType type_ value =
    [ ( "head_hair", encodeEverySet encodeHairHeadCPESign value.hairHead )
    , ( "eyes", encodeEverySet encodeEyesCPESign value.eyes )
    , ( "heart", encodeEverySet encodeHeartCPESign value.heart )
    , ( "heart_murmur", bool value.heartMurmur )
    , ( "neck", encodeEverySet encodeNeckCPESign value.neck )
    , ( "lungs", encodeEverySet encodeLungsCPESign value.lungs )
    , ( "abdomen", encodeEverySet encodeAbdomenCPESign value.abdomen )
    , ( "hands", encodeEverySet encodeHandsCPESign value.hands )
    , ( "legs", encodeEverySet encodeLegsCPESign value.legs )
    , ( "deleted", bool False )
    , ( "type", string type_ )
    ]


encodeCorePhysicalExam : CorePhysicalExam -> List ( String, Value )
encodeCorePhysicalExam =
    encodePrenatalMeasurement (encodeCorePhysicalExamValueWithType "core_physical_exam")


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

            ImminentDelivery ->
                "imminent-delivery"

            Labor ->
                "labor"

            LooksVeryIll ->
                "looks-very-ill"

            SevereVomiting ->
                "severe-vomiting"

            Unconscious ->
                "unconscious"

            GushLeakingVaginalFluid ->
                "gush-leaking-vaginal-fluid"

            PrematureOnsetContractions ->
                "premature-onset-contractions"

            NoDangerSign ->
                "none"


encodeDangerSignsValue : DangerSignsValue -> List ( String, Value )
encodeDangerSignsValue value =
    [ ( "danger_signs", encodeEverySet encodeDangerSign value.signs )
    , ( "postpartum_mother", encodeEverySet encodePostpartumMotherDangerSign value.postpartumMother )
    , ( "postpartum_child", encodeEverySet encodePostpartumChildDangerSign value.postpartumChild )
    , ( "deleted", bool False )
    , ( "type", string "danger_signs" )
    ]


encodePostpartumMotherDangerSign : PostpartumMotherDangerSign -> Value
encodePostpartumMotherDangerSign =
    postpartumMotherDangerSignToString >> string


encodePostpartumChildDangerSign : PostpartumChildDangerSign -> Value
encodePostpartumChildDangerSign =
    postpartumChildDangerSignToString >> string


encodeDangerSigns : DangerSigns -> List ( String, Value )
encodeDangerSigns =
    encodePrenatalMeasurement encodeDangerSignsValue


encodeLastMenstrualPeriod : LastMenstrualPeriod -> List ( String, Value )
encodeLastMenstrualPeriod =
    encodePrenatalMeasurement encodeLastMenstrualPeriodValue


encodeLastMenstrualPeriodValue : LastMenstrualPeriodValue -> List ( String, Value )
encodeLastMenstrualPeriodValue value =
    let
        notConfidentReason =
            Maybe.map (\reason -> [ ( "not_confident_reason", encodeLmpDateNotConfidentReason reason ) ])
                value.notConfidentReason
                |> Maybe.withDefault []
    in
    [ ( "last_menstrual_period", Gizra.NominalDate.encodeYYYYMMDD value.date )
    , ( "confident", bool value.confident )
    , ( "confirmation", bool value.confirmation )
    , ( "deleted", bool False )
    , ( "type", string "last_menstrual_period" )
    ]
        ++ notConfidentReason


encodeLmpDateNotConfidentReason : LmpDateNotConfidentReason -> Value
encodeLmpDateNotConfidentReason =
    lmpDateNotConfidentReasonToString >> string


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
    [ ( "medical_history", encodeEverySet encodeMedicalHistorySign value )
    , ( "deleted", bool False )
    , ( "type", string "medical_history" )
    ]


encodeMedicationSign : MedicationSign -> Value
encodeMedicationSign sign =
    string <|
        case sign of
            IronAndFolicAcidSupplement ->
                "iron-and-folic-acid-supplement"

            DewormingPill ->
                "deworming-pill"

            Mebendazole ->
                "mebendezole"

            PostpartumFolicAcid ->
                "folic-acid"

            PostpartumVitaminA ->
                "vitamin-a"

            NoMedication ->
                "none"


encodeMedicationTreatmentSign : MedicationTreatmentSign -> Value
encodeMedicationTreatmentSign sign =
    string <|
        case sign of
            MedicationTreatmentStillTaking ->
                "still-taking"

            MedicationTreatmentMissedDoses ->
                "missed-doses"

            MedicationTreatmentAdverseEvents ->
                "adverse-events"

            MedicationTreatmentAdverseEventsHospitalization ->
                "adverse-events-hospitalization"

            NoMedicationTreatment ->
                "none"


encodeHIVTreatmentSign : HIVTreatmentSign -> Value
encodeHIVTreatmentSign sign =
    string <|
        case sign of
            HIVTreatmentStillTaking ->
                "still-taking"

            HIVTreatmentMissedDoses ->
                "missed-doses"

            HIVTreatmentAdverseEvents ->
                "adverse-events"

            HIVTreatmentAdverseEventsHospitalization ->
                "adverse-events-hospitalization"

            HIVTreatmentMedicineByPMTCT ->
                "medicine-pmtct"

            HIVTreatmentNoMedicineNotSeenAtPMTCT ->
                "no-medicine-not-seen"

            HIVTreatmentNoMedicineOutOfStock ->
                "no-medicine-out-of-stock"

            HIVTreatmentNoMedicinePatientRefused ->
                "no-medicine-patient-refused"

            HIVTreatmentNoMedicineOther ->
                "no-medicine-other"

            NoHIVTreatment ->
                "none"


encodeMedication : Medication -> List ( String, Value )
encodeMedication =
    encodePrenatalMeasurement encodeMedicationValue


encodeMedicationValue : MedicationValue -> List ( String, Value )
encodeMedicationValue value =
    let
        encodeEverySetNullable name encoder maybeValue =
            Maybe.map
                (\value_ ->
                    [ ( name, encodeEverySet encoder value_ ) ]
                )
                maybeValue
                |> Maybe.withDefault []
    in
    encodeEverySetNullable "medication" encodeMedicationSign value.signs
        ++ encodeEverySetNullable "hiv_treatment" encodeHIVTreatmentSign value.hivTreatment
        ++ encodeEverySetNullable "hypertension_treatment" encodeMedicationTreatmentSign value.hypertensionTreatment
        ++ encodeEverySetNullable "malaria_treatment" encodeMedicationTreatmentSign value.malariaTreatment
        ++ encodeEverySetNullable "anemia_treatment" encodeMedicationTreatmentSign value.anemiaTreatment
        ++ encodeEverySetNullable "syphilis_treatment" encodeMedicationTreatmentSign value.syphilisTreatment
        ++ [ ( "deleted", bool False )
           , ( "type", string "medication" )
           ]


encodeChildFbf : Fbf -> List ( String, Value )
encodeChildFbf =
    encodeFbfValueWithType "child_fbf"
        |> encodeGroupMeasurement


encodeMotherFbf : Fbf -> List ( String, Value )
encodeMotherFbf =
    encodeFbfValueWithType "mother_fbf"
        |> encodeGroupMeasurement


encodeFbfValueWithType : String -> FbfValue -> List ( String, Value )
encodeFbfValueWithType type_ value =
    [ ( "distributed_amount", float value.distributedAmount )
    , ( "distribution_notice", encodeDistributionNotice value.distributionNotice )
    , ( "deleted", bool False )
    , ( "type", string type_ )
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
    , ( "deleted", bool False )
    , ( "type", string "obstetrical_exam" )
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
    , ( "deleted", bool False )
    , ( "type", string "obstetric_history" )
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

            PreviousCSection ->
                "previous-c-section"


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
    let
        cSectionReason =
            Maybe.map
                (\reason ->
                    [ ( "c_section_reason", encodeEverySet encodeCSectionReason reason ) ]
                )
                value.cSectionReason
                |> Maybe.withDefault []
    in
    [ ( "c_sections", int value.cSections )
    , ( "obstetric_history", encodeEverySet encodeObstetricHistorySign value.obstetricHistory )
    , ( "previous_delivery", encodeEverySet encodePreviousDeliverySign value.previousDelivery )
    , ( "previous_delivery_period", encodeEverySet encodePreviousDeliveryPeriod value.previousDeliveryPeriod )
    , ( "deleted", bool False )
    , ( "type", string "obstetric_history_step2" )
    ]
        ++ cSectionReason


encodeBirthPlan : BirthPlan -> List ( String, Value )
encodeBirthPlan =
    encodePrenatalMeasurement encodeBirthPlanValue


encodeBirthPlanValue : BirthPlanValue -> List ( String, Value )
encodeBirthPlanValue value =
    [ ( "birth_plan_signs", encodeEverySet encodeBirthPlanSign value.signs )
    , ( "family_planning_signs", encodeEverySet encodeFamilyPlanningSign value.familyPlanning )
    , ( "deleted", bool False )
    , ( "type", string "birth_plan" )
    ]


encodeBirthPlanSign : BirthPlanSign -> Value
encodeBirthPlanSign sign =
    string <|
        case sign of
            Insurance ->
                "have-insurance"

            BoughtClothes ->
                "bought-clothes-for-child"

            CaregiverAccompany ->
                "caregiver-to-accompany-you"

            SavedMoney ->
                "saved-money-for-use"

            Transportation ->
                "planned-for-transportation"

            NoBirthPlan ->
                "none"


encodePrenatalFamilyPlanning : PrenatalFamilyPlanning -> List ( String, Value )
encodePrenatalFamilyPlanning =
    encodePrenatalMeasurement (encodeFamilyPlanningValueWithType "prenatal_family_planning")


encodePrenatalNutrition : PrenatalNutrition -> List ( String, Value )
encodePrenatalNutrition =
    encodePrenatalMeasurement encodePrenatalNutritionValue


encodePrenatalNutritionValue : PrenatalNutritionValue -> List ( String, Value )
encodePrenatalNutritionValue value =
    [ ( "height", encodeHeightInCm value.height )
    , ( "weight", encodeWeightInKg value.weight )
    , ( "muac", encodeMuacInCm value.muac )
    , ( "deleted", bool False )
    , ( "type", string "prenatal_nutrition" )
    ]


encodeMalariaPreventionSign : MalariaPreventionSign -> Value
encodeMalariaPreventionSign sign =
    string <|
        case sign of
            MosquitoNet ->
                "mosquito-net"

            NoMalariaPreventionSigns ->
                "none"


encodeMalariaPrevention : MalariaPrevention -> List ( String, Value )
encodeMalariaPrevention =
    encodePrenatalMeasurement encodeMalariaPreventionValue


encodeMalariaPreventionValue : EverySet MalariaPreventionSign -> List ( String, Value )
encodeMalariaPreventionValue value =
    [ ( "resources", encodeEverySet encodeMalariaPreventionSign value )
    , ( "deleted", bool False )
    , ( "type", string "resource" )
    ]


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


encodeSocialHistoryHivTestingResult : SocialHistoryHivTestingResult -> Value
encodeSocialHistoryHivTestingResult =
    socialHistoryHivTestingResultToString >> string


encodeSocialHistory : SocialHistory -> List ( String, Value )
encodeSocialHistory =
    encodePrenatalMeasurement encodeSocialHistoryValue


encodeSocialHistoryValue : SocialHistoryValue -> List ( String, Value )
encodeSocialHistoryValue value =
    [ ( "social_history", encodeEverySet encodeSocialHistorySign value.socialHistory )
    , ( "partner_hiv_testing", encodeSocialHistoryHivTestingResult value.hivTestingResult )
    , ( "deleted", bool False )
    , ( "type", string "social_history" )
    ]


encodeVitals : Vitals -> List ( String, Value )
encodeVitals =
    encodePrenatalMeasurement (encodeVitalsValueWithType "vitals")


encodeVitalsValueWithType : String -> VitalsValue -> List ( String, Value )
encodeVitalsValueWithType type_ value =
    let
        sysEntry =
            Maybe.map (\sys -> [ ( "sys", float sys ) ])
                value.sys
                |> Maybe.withDefault []

        diaEntry =
            Maybe.map (\dia -> [ ( "dia", float dia ) ])
                value.dia
                |> Maybe.withDefault []

        heartRateEntry =
            Maybe.map (\heartRate -> [ ( "heart_rate", int heartRate ) ])
                value.heartRate
                |> Maybe.withDefault []

        sysRepeatedEntry =
            Maybe.map (\sysRepeated -> [ ( "sys_repeated", float sysRepeated ) ])
                value.sysRepeated
                |> Maybe.withDefault []

        diaRepeatedEntry =
            Maybe.map (\diaRepeated -> [ ( "dia_repeated", float diaRepeated ) ])
                value.diaRepeated
                |> Maybe.withDefault []
    in
    sysEntry
        ++ diaEntry
        ++ heartRateEntry
        ++ sysRepeatedEntry
        ++ diaRepeatedEntry
        ++ [ ( "respiratory_rate", int value.respiratoryRate )
           , ( "body_temperature", float value.bodyTemperature )
           , ( "deleted", bool False )
           , ( "type", string type_ )
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
    , ( "deleted", bool False )
    , ( "type", string "symptoms_general" )
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
    , ( "deleted", bool False )
    , ( "type", string "symptoms_respiratory" )
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
    , ( "deleted", bool False )
    , ( "type", string "symptoms_gi" )
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
    encodeAcuteIllnessMeasurement (encodeVitalsValueWithType "acute_illness_vitals")


encodeAcuteFindings : AcuteFindings -> List ( String, Value )
encodeAcuteFindings =
    encodeAcuteIllnessMeasurement encodeAcuteFindingsValue


encodeAcuteFindingsValue : AcuteFindingsValue -> List ( String, Value )
encodeAcuteFindingsValue value =
    [ ( "findings_signs_general", encodeEverySet encodeAcuteFindingsGeneralSign value.signsGeneral )
    , ( "findings_signs_respiratory", encodeEverySet encodeAcuteFindingsRespiratorySign value.signsRespiratory )
    , ( "deleted", bool False )
    , ( "type", string "acute_findings" )
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


encodeMalariaTestingValue : RapidTestResult -> List ( String, Value )
encodeMalariaTestingValue value =
    [ ( "malaria_rapid_test", encodeRapidTestResult value )
    , ( "deleted", bool False )
    , ( "type", string "malaria_testing" )
    ]


encodeCovidTesting : CovidTesting -> List ( String, Value )
encodeCovidTesting =
    encodeAcuteIllnessMeasurement encodeCovidTestingValue


encodeCovidTestingValue : CovidTestingValue -> List ( String, Value )
encodeCovidTestingValue value =
    let
        optional =
            Maybe.map
                (\administrationNote ->
                    [ ( "administration_note", encodeAdministrationNote administrationNote ) ]
                )
                value.administrationNote
                |> Maybe.withDefault []
    in
    optional
        ++ [ ( "rapid_test_result", encodeRapidTestResult value.result )
           , ( "deleted", bool False )
           , ( "type", string "covid_testing" )
           ]


encodeRapidTestResult : RapidTestResult -> Value
encodeRapidTestResult =
    malariaRapidTestResultAsString >> string


malariaRapidTestResultAsString : RapidTestResult -> String
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

        RapidTestUnableToRunAndPregnant ->
            "unable-to-run-and-pregnant"


encodeSendToHC : SendToHC -> List ( String, Value )
encodeSendToHC =
    encodeAcuteIllnessMeasurement (encodeSendToHCValueWithType "send_to_hc")


encodeNutritionSendToHC : NutritionSendToHC -> List ( String, Value )
encodeNutritionSendToHC =
    encodeNutritionMeasurement (encodeSendToHCValueWithType "nutrition_send_to_hc")


encodeWellChildSendToHC : WellChildSendToHC -> List ( String, Value )
encodeWellChildSendToHC =
    encodeWellChildMeasurement (encodeSendToHCValueWithType "well_child_send_to_hc")


encodeGroupSendToHC : GroupSendToHC -> List ( String, Value )
encodeGroupSendToHC =
    encodeGroupMeasurement (encodeSendToHCValueWithType "group_send_to_hc")


encodeSendToHCValueWithType : String -> SendToHCValue -> List ( String, Value )
encodeSendToHCValueWithType type_ value =
    [ ( "send_to_hc", encodeEverySet encodeSendToHCSign value.signs )
    , ( "reason_not_sent_to_hc", encodeReasonForNonReferral value.reasonForNotSendingToHC )
    , ( "deleted", bool False )
    , ( "type", string type_ )
    ]


encodeSendToHCSign : SendToHCSign -> Value
encodeSendToHCSign sign =
    string <|
        case sign of
            HandReferrerForm ->
                "referral-form"

            ReferToHealthCenter ->
                "refer-to-hc"

            PrenatalAccompanyToHC ->
                "accompany-to-hc"

            EnrollToNutritionProgram ->
                "enroll-to-nutrition-program"

            ReferToNutritionProgram ->
                "refer-to-nutrition-program"

            NoSendToHCSigns ->
                "none"


encodeReasonForNonReferral : ReasonForNonReferral -> Value
encodeReasonForNonReferral =
    reasonForNonReferralToString >> string


encodePrenatalReferralValue : PrenatalReferralValue -> List ( String, Value )
encodePrenatalReferralValue value =
    let
        sendToHC =
            Maybe.map
                (\signs ->
                    if EverySet.isEmpty signs then
                        []

                    else
                        [ ( "send_to_hc", encodeEverySet encodeSendToHCSign signs ) ]
                )
                value.sendToHCSigns
                |> Maybe.withDefault []

        reasonNotSentToHC =
            Maybe.map (\reason -> [ ( "reason_not_sent_to_hc", encodeReasonForNonReferral reason ) ])
                value.reasonForNotSendingToHC
                |> Maybe.withDefault []

        referToFacilitySigns =
            Maybe.map
                (\signs ->
                    if EverySet.isEmpty signs then
                        []

                    else
                        [ ( "referrals", encodeEverySet encodeReferToFacilitySign signs ) ]
                )
                value.referToFacilitySigns
                |> Maybe.withDefault []

        facilityNonReferralReasons =
            Maybe.map
                (\reason ->
                    if EverySet.isEmpty reason then
                        []

                    else
                        [ ( "reasons_for_non_referrals", encodeEverySet encodeNonReferralSign reason ) ]
                )
                value.facilityNonReferralReasons
                |> Maybe.withDefault []
    in
    sendToHC
        ++ reasonNotSentToHC
        ++ referToFacilitySigns
        ++ facilityNonReferralReasons
        ++ [ ( "deleted", bool False )
           , ( "type", string "prenatal_send_to_hc" )
           ]


encodeReferToFacilitySign : ReferToFacilitySign -> Value
encodeReferToFacilitySign sign =
    string <|
        case sign of
            ReferToHospital ->
                "hospital"

            ReferralFormHospital ->
                "hospital-referral-form"

            ReferToMentalHealthSpecialist ->
                "mhs"

            ReferralFormMentalHealthSpecialist ->
                "mhs-referral-form"

            AccompanyToMentalHealthSpecialist ->
                "mhs-accompany"

            ReferToARVProgram ->
                "arv"

            ReferralFormARVProgram ->
                "arv-referral-form"

            AccompanyToARVProgram ->
                "arv-accompany"

            ReferToNCDProgram ->
                "ncd"

            ReferralFormNCDProgram ->
                "ncd-referral-form"

            AccompanyToNCDProgram ->
                "ncd-accompany"

            ReferToANCServices ->
                "anc"

            ReferralFormANCServices ->
                "anc-referral-form"

            AccompanyToANCServices ->
                "anc-accompany"

            ReferToUltrasound ->
                "us"

            ReferralFormUltrasound ->
                "us-referral-form"

            NoReferToFacilitySigns ->
                "none"


encodeNonReferralSign : NonReferralSign -> Value
encodeNonReferralSign sign =
    string <|
        case sign of
            NonReferralReasonHospital reason ->
                "hospital-" ++ reasonForNonReferralToString reason

            NonReferralReasonMentalHealthSpecialist reason ->
                "mhs-" ++ reasonForNonReferralToString reason

            NonReferralReasonARVProgram reason ->
                "arv-" ++ reasonForNonReferralToString reason

            NonReferralReasonNCDProgram reason ->
                "ncd-" ++ reasonForNonReferralToString reason

            NonReferralReasonANCServices reason ->
                "anc-" ++ reasonForNonReferralToString reason

            NonReferralReasonUltrasound reason ->
                "us-" ++ reasonForNonReferralToString reason

            NoNonReferralSigns ->
                "none"


encodeReferralFacility : ReferralFacility -> Value
encodeReferralFacility facility =
    string <|
        case facility of
            FacilityHealthCenter ->
                "hc"

            FacilityHospital ->
                "hospital"

            FacilityMentalHealthSpecialist ->
                "mhs"

            FacilityARVProgram ->
                "arv"

            FacilityNCDProgram ->
                "ncd"

            FacilityANCServices ->
                "anc"

            FacilityUltrasound ->
                "us"


encodeContributingFactors : ContributingFactors -> List ( String, Value )
encodeContributingFactors =
    encodeGroupMeasurement (encodeContributingFactorsValueWithType "contributing_factors")


encodeNutritionContributingFactors : NutritionContributingFactors -> List ( String, Value )
encodeNutritionContributingFactors =
    encodeNutritionMeasurement (encodeContributingFactorsValueWithType "nutrition_contributing_factors")


encodeWellChildContributingFactors : WellChildContributingFactors -> List ( String, Value )
encodeWellChildContributingFactors =
    encodeWellChildMeasurement (encodeContributingFactorsValueWithType "well_child_contributing_factors")


encodeContributingFactorsValueWithType : String -> EverySet ContributingFactorsSign -> List ( String, Value )
encodeContributingFactorsValueWithType type_ value =
    [ ( "contributing_factors_signs", encodeEverySet encodeContributingFactorsSign value )
    , ( "deleted", bool False )
    , ( "type", string type_ )
    ]


encodeContributingFactorsSign : ContributingFactorsSign -> Value
encodeContributingFactorsSign sign =
    string <|
        case sign of
            FactorLackOfBreastMilk ->
                "lack-of-breast-milk"

            FactorMaternalMastitis ->
                "maternal-mastitis"

            FactorPoorSuck ->
                "poor-suck"

            FactorDiarrheaOrVomiting ->
                "diarrhea-or-vomiting"

            NoContributingFactorsSign ->
                "none"


encodeFollowUp : FollowUp -> List ( String, Value )
encodeFollowUp =
    encodeGroupMeasurement (encodeFollowUpValueWithType "follow_up")


encodeNutritionFollowUp : NutritionFollowUp -> List ( String, Value )
encodeNutritionFollowUp =
    encodeNutritionMeasurement (encodeFollowUpValueWithType "nutrition_follow_up")


encodeWellChildFollowUp : WellChildFollowUp -> List ( String, Value )
encodeWellChildFollowUp =
    encodeWellChildMeasurement (encodeFollowUpValueWithType "well_child_follow_up")


encodeFollowUpValueWithType : String -> FollowUpValue -> List ( String, Value )
encodeFollowUpValueWithType type_ value =
    let
        assesment =
            EverySet.toList value.assesment
                |> List.head
                |> Maybe.withDefault NoNutritionAssessment

        nutritionSigns =
            case assesment of
                AssesmentMalnutritionSigns signs ->
                    EverySet.fromList signs

                _ ->
                    EverySet.singleton NormalChildNutrition

        resolutionDate =
            Maybe.map (\date -> [ ( "date_concluded", Gizra.NominalDate.encodeYYYYMMDD date ) ])
                value.resolutionDate
                |> Maybe.withDefault []
    in
    [ ( "follow_up_options", encodeEverySet encodeFollowUpOption value.options )
    , ( "nutrition_assesment", encodeEverySet encodeNutritionAssessment value.assesment )
    , ( "nutrition_signs", encodeEverySet encodeNutritionSign nutritionSigns )
    , ( "deleted", bool False )
    , ( "type", string type_ )
    ]
        ++ resolutionDate


encodeAcuteIllnessFollowUp : AcuteIllnessFollowUp -> List ( String, Value )
encodeAcuteIllnessFollowUp =
    encodeAcuteIllnessMeasurement encodeAcuteIllnessFollowUpValue


encodeAcuteIllnessFollowUpValue : AcuteIllnessFollowUpValue -> List ( String, Value )
encodeAcuteIllnessFollowUpValue value =
    let
        resolutionDate =
            Maybe.map (\date -> [ ( "date_concluded", Gizra.NominalDate.encodeYYYYMMDD date ) ])
                value.resolutionDate
                |> Maybe.withDefault []
    in
    [ ( "follow_up_options", encodeEverySet encodeFollowUpOption value.options )
    , ( "deleted", bool False )
    , ( "type", string "acute_illness_follow_up" )
    ]
        ++ resolutionDate


encodeNutritionAssessment : NutritionAssessment -> Value
encodeNutritionAssessment =
    nutritionAssessmentToString >> string


encodeFollowUpOption : FollowUpOption -> Value
encodeFollowUpOption option =
    string <|
        case option of
            OneDay ->
                "1-d"

            ThreeDays ->
                "3-d"

            OneWeek ->
                "1-w"

            TwoWeeks ->
                "2-w"

            OneMonths ->
                "1-m"

            TwoMonths ->
                "2-m"

            ThreeMonths ->
                "3-m"


encodeNutritionFeeding : NutritionFeeding -> List ( String, Value )
encodeNutritionFeeding =
    encodeHomeVisitMeasurement encodeNutritionFeedingValue


encodeNutritionFeedingValue : NutritionFeedingValue -> List ( String, Value )
encodeNutritionFeedingValue value =
    [ ( "nutrition_feeding_signs", encodeEverySet encodeNutritionFeedingSign value.signs )
    , ( "supplement_type", encodeNutritionSupplementType value.supplementType )
    , ( "sachets_per_day", float value.sachetsPerDay )
    , ( "deleted", bool False )
    , ( "type", string "nutrition_feeding" )
    ]


encodeNutritionSupplementType : NutritionSupplementType -> Value
encodeNutritionSupplementType type_ =
    string <|
        case type_ of
            FortifiedPorridge ->
                "fortified-porridge"

            Rutf ->
                "rutf"

            Ongera ->
                "ongera"

            TherapeuticMilk ->
                "therapeutic-milk"

            NoNutritionSupplementType ->
                "none"


encodeNutritionFeedingSign : NutritionFeedingSign -> Value
encodeNutritionFeedingSign sign =
    string <|
        case sign of
            ReceiveSupplement ->
                "receive-supplement"

            RationPresentAtHome ->
                "ration-present-at-home"

            EnoughTillNextSession ->
                "enough-till-next-session"

            SupplementShared ->
                "supplement-shared"

            EncouragedToEat ->
                "encouraged-to-eat"

            RefusingToEat ->
                "refusing-to-eat"

            FeedingSignBreastfeeding ->
                "breastfeeding"

            CleanWaterAvailable ->
                "clean-water-available"

            EatenWithWater ->
                "eaten-with-water"

            NoNutritionFeedingSigns ->
                "none"


encodeNutritionHygiene : NutritionHygiene -> List ( String, Value )
encodeNutritionHygiene =
    encodeHomeVisitMeasurement encodeNutritionHygieneValue


encodeNutritionHygieneValue : NutritionHygieneValue -> List ( String, Value )
encodeNutritionHygieneValue value =
    [ ( "nutrition_hygiene_signs", encodeEverySet encodeNutritionHygieneSign value.signs )
    , ( "main_water_source", encodeMainWaterSource value.mainWaterSource )
    , ( "water_preparation_option", encodeWaterPreparationOption value.waterPreparationOption )
    , ( "deleted", bool False )
    , ( "type", string "nutrition_hygiene" )
    ]


encodeNutritionHygieneSign : NutritionHygieneSign -> Value
encodeNutritionHygieneSign sign =
    string <|
        case sign of
            SoapInTheHouse ->
                "soap-in-the-house"

            WashHandsBeforeFeeding ->
                "wash-hands-before-feeding"

            FoodCovered ->
                "food-covered"

            NoNutritionHygieneSigns ->
                "none"


encodeMainWaterSource : MainWaterSource -> Value
encodeMainWaterSource type_ =
    string <|
        case type_ of
            PipedWaterToHome ->
                "piped-water-to-home"

            PublicWaterTap ->
                "public-water-tap"

            RainWaterCollectionSystem ->
                "rain-water-collection-system"

            NaturalSourceFlowingWater ->
                "natural-source-flowing-water"

            NaturalSourceStandingWater ->
                "natural-source-standing-water"

            BottledWater ->
                "bottled-water"


encodeWaterPreparationOption : WaterPreparationOption -> Value
encodeWaterPreparationOption type_ =
    string <|
        case type_ of
            Boiled ->
                "boiled"

            PurificationSolution ->
                "purification-solution"

            Filtered ->
                "filtered"

            Bottled ->
                "bottled"

            NoWaterPreparationOption ->
                "none"


encodeNutritionFoodSecurity : NutritionFoodSecurity -> List ( String, Value )
encodeNutritionFoodSecurity =
    encodeHomeVisitMeasurement encodeNutritionFoodSecurityValue


encodeNutritionFoodSecurityValue : NutritionFoodSecurityValue -> List ( String, Value )
encodeNutritionFoodSecurityValue value =
    [ ( "food_security_signs", encodeEverySet encodeNutritionFoodSecuritySign value.signs )
    , ( "main_income_source", encodeMainIncomeSource value.mainIncomeSource )
    , ( "deleted", bool False )
    , ( "type", string "nutrition_food_security" )
    ]


encodeNutritionFoodSecuritySign : NutritionFoodSecuritySign -> Value
encodeNutritionFoodSecuritySign sign =
    string <|
        case sign of
            HouseholdGotFood ->
                "household-got-food"

            NoNutritionFoodSecuritySigns ->
                "none"


encodeMainIncomeSource : MainIncomeSource -> Value
encodeMainIncomeSource type_ =
    string <|
        case type_ of
            HomeBasedAgriculture ->
                "home-based-agriculture"

            CommercialAgriculture ->
                "commercial-agriculture"

            PublicEmployee ->
                "public-employee"

            PrivateBusinessEmpployee ->
                "private-business-employee"


encodeNutritionCaring : NutritionCaring -> List ( String, Value )
encodeNutritionCaring =
    encodeHomeVisitMeasurement encodeNutritionCaringValue


encodeNutritionCaringValue : NutritionCaringValue -> List ( String, Value )
encodeNutritionCaringValue value =
    [ ( "nutrition_caring_signs", encodeEverySet encodeNutritionCaringSign value.signs )
    , ( "child_caring_options", encodeNutritionCaringOption value.caringOption )
    , ( "deleted", bool False )
    , ( "type", string "nutrition_caring" )
    ]


encodeNutritionCaringSign : NutritionCaringSign -> Value
encodeNutritionCaringSign sign =
    string <|
        case sign of
            ParentsAliveHealthy ->
                "parent-alive-and-healthy"

            ChildClean ->
                "child-clean"

            NoCaringSigns ->
                "none"


encodeNutritionCaringOption : CaringOption -> Value
encodeNutritionCaringOption option =
    string <|
        case option of
            CaredByParent ->
                "parent"

            CaredByGrandparent ->
                "grandparent"

            CaredBySibling ->
                "sibling"

            CaredByNeighbor ->
                "neighbor"

            CaredByHouseHelper ->
                "house-helper"

            CaredByDaycare ->
                "daycare"


encodeMedicationDistribution : MedicationDistribution -> List ( String, Value )
encodeMedicationDistribution =
    encodeAcuteIllnessMeasurement encodeMedicationDistributionValue


encodeMedicationDistributionValue : MedicationDistributionValue -> List ( String, Value )
encodeMedicationDistributionValue value =
    [ ( "prescribed_medication", encodeEverySet encodeMedicationDistributionSign value.distributionSigns )
    , ( "non_administration_reason", encodeEverySet encodeMedicationNonAdministrationSign value.nonAdministrationSigns )
    , ( "deleted", bool False )
    , ( "type", string "medication_distribution" )
    ]


encodePrenatalMedicationDistribution : PrenatalMedicationDistribution -> List ( String, Value )
encodePrenatalMedicationDistribution =
    encodePrenatalMeasurement encodePrenatalMedicationDistributionValue


encodePrenatalMedicationDistributionValue : PrenatalMedicationDistributionValue -> List ( String, Value )
encodePrenatalMedicationDistributionValue value =
    let
        treatment =
            Maybe.map
                (\signs ->
                    if EverySet.isEmpty signs then
                        []

                    else
                        [ ( "recommended_treatment", encodeEverySet encodeRecommendedTreatmentSign signs ) ]
                )
                value.recommendedTreatmentSigns
                |> Maybe.withDefault []

        avoidingGuidanceReason =
            Maybe.map
                (\signs ->
                    if EverySet.isEmpty signs then
                        []

                    else
                        [ ( "avoiding_guidance_reason", encodeEverySet encodeAvoidingGuidanceReason signs ) ]
                )
                value.avoidingGuidanceReason
                |> Maybe.withDefault []
    in
    [ ( "prescribed_medication", encodeEverySet encodeMedicationDistributionSign value.distributionSigns )
    , ( "non_administration_reason", encodeEverySet encodeMedicationNonAdministrationSign value.nonAdministrationSigns )
    , ( "deleted", bool False )
    , ( "type", string "prenatal_medication_distribution" )
    ]
        ++ treatment
        ++ avoidingGuidanceReason


encodeMedicationDistributionSign : MedicationDistributionSign -> Value
encodeMedicationDistributionSign sign =
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

            Albendazole ->
                "albendazole"

            Mebendezole ->
                "mebendezole"

            Paracetamol ->
                "paracetamol"

            Tenofovir ->
                "tenofovir"

            Lamivudine ->
                "lamivudine"

            Dolutegravir ->
                "dolutegravir"

            TDF3TC ->
                "tdf3tc"

            Iron ->
                "iron"

            FolicAcid ->
                "folicacid"

            Ceftriaxone ->
                "ceftriaxone"

            Azithromycin ->
                "azithromycin"

            Metronidazole ->
                "metronidazole"

            VitaminA ->
                "vitamina"

            NoMedicationDistributionSigns ->
                "none"

            NoMedicationDistributionSignsInitialPhase ->
                "none-initial"

            NoMedicationDistributionSignsRecurrentPhase ->
                "none-recurrent"


encodeMedicationNonAdministrationSign : MedicationNonAdministrationSign -> Value
encodeMedicationNonAdministrationSign sign =
    string <|
        case sign of
            MedicationAmoxicillin reason ->
                "amoxicillin-" ++ administrationNoteToString reason

            MedicationCoartem reason ->
                "coartem-" ++ administrationNoteToString reason

            MedicationORS reason ->
                "ors-" ++ administrationNoteToString reason

            MedicationZinc reason ->
                "zinc-" ++ administrationNoteToString reason

            MedicationParacetamol reason ->
                "paracetamol-" ++ administrationNoteToString reason

            MedicationMebendezole reason ->
                "mebendezole-" ++ administrationNoteToString reason

            MedicationTenofovir reason ->
                "tenofovir-" ++ administrationNoteToString reason

            MedicationLamivudine reason ->
                "lamivudine-" ++ administrationNoteToString reason

            MedicationDolutegravir reason ->
                "dolutegravir-" ++ administrationNoteToString reason

            MedicationTDF3TC reason ->
                "tdf3tc-" ++ administrationNoteToString reason

            MedicationIron reason ->
                "iron-" ++ administrationNoteToString reason

            MedicationFolicAcid reason ->
                "folicacid-" ++ administrationNoteToString reason

            MedicationCeftriaxone reason ->
                "ceftriaxone-" ++ administrationNoteToString reason

            MedicationAzithromycin reason ->
                "azithromycin-" ++ administrationNoteToString reason

            MedicationMetronidazole reason ->
                "metronidazole-" ++ administrationNoteToString reason

            MedicationVitaminA reason ->
                "vitamina-" ++ administrationNoteToString reason

            NoMedicationNonAdministrationSigns ->
                "none"


encodeRecommendedTreatmentSign : RecommendedTreatmentSign -> Value
encodeRecommendedTreatmentSign =
    recommendedTreatmentSignToString >> string


encodeAvoidingGuidanceReason : AvoidingGuidanceReason -> Value
encodeAvoidingGuidanceReason =
    avoidingGuidanceReasonToString >> string


encodeTravelHistory : TravelHistory -> List ( String, Value )
encodeTravelHistory =
    encodeAcuteIllnessMeasurement encodeTravelHistoryValue


encodeTravelHistoryValue : EverySet TravelHistorySign -> List ( String, Value )
encodeTravelHistoryValue value =
    [ ( "travel_history", encodeEverySet encodeTravelHistorySign value )
    , ( "deleted", bool False )
    , ( "type", string "travel_history" )
    ]


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
    [ ( "treatment_history", encodeEverySet encodeTreatmentReviewSign value )
    , ( "deleted", bool False )
    , ( "type", string "treatment_history" )
    ]


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
    [ ( "exposure", encodeEverySet encodeExposureSign value )
    , ( "deleted", bool False )
    , ( "type", string "exposure" )
    ]


encodeExposureSign : ExposureSign -> Value
encodeExposureSign sign =
    string <|
        case sign of
            COVID19Symptoms ->
                "covid19-symptoms"

            NoExposureSigns ->
                "none"


encodeIsolation : Isolation -> List ( String, Value )
encodeIsolation =
    encodeAcuteIllnessMeasurement encodeIsolationValue


encodeIsolationValue : IsolationValue -> List ( String, Value )
encodeIsolationValue value =
    [ ( "isolation", encodeEverySet encodeIsolationSign value.signs )
    , ( "reason_for_not_isolating", encodeEverySet encodeReasonForNotIsolating value.reasonsForNotIsolating )
    , ( "deleted", bool False )
    , ( "type", string "isolation" )
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
    , ( "hc_recommendation", encodeEverySet encodeHCRecommendation value.recommendations )
    , ( "hc_response_time", encodeEverySet encodeResponsePeriod value.responsePeriod )
    , ( "ambulance_arrival_time", encodeEverySet encodeResponsePeriod value.ambulanceArrivalPeriod )
    , ( "deleted", bool False )
    , ( "type", string "hc_contact" )
    ]


encodeHCContactSign : HCContactSign -> Value
encodeHCContactSign sign =
    string <|
        case sign of
            ContactedHealthCenter ->
                "contact-hc"

            NoHCContactSigns ->
                "none"


encodeHCRecommendation : HCRecommendation -> Value
encodeHCRecommendation recommendation =
    string <|
        case recommendation of
            SendAmbulance ->
                "send-ambulance"

            HomeIsolation ->
                "home-isolation"

            ComeToHealthCenter ->
                "come-to-hc"

            ChwMonitoring ->
                "chw-monitoring"

            HCRecommendationNotApplicable ->
                "n-a"


encodeResponsePeriod : ResponsePeriod -> Value
encodeResponsePeriod period =
    string <|
        case period of
            LessThan30Min ->
                "less-than-30m"

            Between30min1Hour ->
                "30m-1h"

            Between1Hour2Hour ->
                "1h-2h"

            Between2Hour1Day ->
                "2h-1d"

            ResponsePeriodNotApplicable ->
                "n-a"


encodeCall114 : Call114 -> List ( String, Value )
encodeCall114 =
    encodeAcuteIllnessMeasurement encodeCall114Value


encodeCall114Value : Call114Value -> List ( String, Value )
encodeCall114Value value =
    [ ( "114_contact", encodeEverySet encodeCall114Sign value.signs )
    , ( "114_recommendation", encodeEverySet encodeRecommendation114 value.recommendations114 )
    , ( "site_recommendation", encodeEverySet encodeRecommendationSite value.recommendationsSite )
    , ( "deleted", bool False )
    , ( "type", string "call_114" )
    ]


encodeCall114Sign : Call114Sign -> Value
encodeCall114Sign sign =
    string <|
        case sign of
            Call114 ->
                "call-114"

            ContactSite ->
                "contact-site"

            NoCall114Signs ->
                "none"


encodeRecommendation114 : Recommendation114 -> Value
encodeRecommendation114 recommendation =
    string <|
        case recommendation of
            SendToHealthCenter ->
                "send-to-hc"

            SendToRRTCenter ->
                "send-to-rrtc"

            SendToHospital ->
                "send-to-hospital"

            OtherRecommendation114 ->
                "other"

            NoneNoAnswer ->
                "none-no-answer"

            NoneBusySignal ->
                "none-busy-signal"

            NoneOtherRecommendation114 ->
                "none-other"


encodeRecommendationSite : RecommendationSite -> Value
encodeRecommendationSite period =
    string <|
        case period of
            TeamComeToVillage ->
                "team-to-village"

            SendToSiteWithForm ->
                "send-with-form"

            OtherRecommendationSite ->
                "other"

            NoneSentWithForm ->
                "none-sent-with-form"

            NonePatientRefused ->
                "none-patient-refused"

            NoneOtherRecommendationSite ->
                "none-other"

            RecommendationSiteNotApplicable ->
                "n-a"


encodeAcuteIllnessMuac : AcuteIllnessMuac -> List ( String, Value )
encodeAcuteIllnessMuac =
    encodeAcuteIllnessMeasurement (encodeMuacValueWithType "acute_illness_muac")


encodeTreatmentOngoing : TreatmentOngoing -> List ( String, Value )
encodeTreatmentOngoing =
    encodeAcuteIllnessMeasurement encodeTreatmentOngoingValue


encodeTreatmentOngoingValue : TreatmentOngoingValue -> List ( String, Value )
encodeTreatmentOngoingValue value =
    [ ( "treatment_ongoing", encodeEverySet encodeTreatmentOngoingSign value.signs )
    , ( "reason_for_not_taking", encodeReasonForNotTakingSign value.reasonForNotTaking )
    , ( "missed_doses", int value.missedDoses )
    , ( "adverse_events", encodeEverySet encodeAdverseEvent value.adverseEvents )
    , ( "deleted", bool False )
    , ( "type", string "treatment_ongoing" )
    ]


encodeTreatmentOngoingSign : TreatmentOngoingSign -> Value
encodeTreatmentOngoingSign sign =
    string <|
        case sign of
            TakenAsPrescribed ->
                "taken-as-prescribed"

            MissedDoses ->
                "missed-doses"

            FeelingBetter ->
                "feel-better"

            SideEffects ->
                "side-effects"

            NoTreatmentOngoingSign ->
                "none"


encodeReasonForNotTakingSign : ReasonForNotTaking -> Value
encodeReasonForNotTakingSign reason =
    string <|
        case reason of
            NotTakingAdverseEvent ->
                "adverse-event"

            NotTakingNoMoney ->
                "no-money"

            NotTakingMemoryProblems ->
                "memory-problems"

            NotTakingOther ->
                "other"

            NoReasonForNotTakingSign ->
                "none"


encodeAdverseEvent : AdverseEvent -> Value
encodeAdverseEvent event =
    string <|
        case event of
            AdverseEventRashOrItching ->
                "rash-itching"

            AdverseEventFever ->
                "fever"

            AdverseEventDiarrhea ->
                "diarrhea"

            AdverseEventVomiting ->
                "vomiting"

            AdverseEventFatigue ->
                "fatigue"

            AdverseEventOther ->
                "other"

            NoAdverseEvent ->
                "none"


encodeAcuteIllnessCoreExam : AcuteIllnessCoreExam -> List ( String, Value )
encodeAcuteIllnessCoreExam =
    encodeAcuteIllnessMeasurement encodeAcuteIllnessCoreExamValue


encodeAcuteIllnessCoreExamValue : AcuteIllnessCoreExamValue -> List ( String, Value )
encodeAcuteIllnessCoreExamValue value =
    [ ( "heart", encodeEverySet encodeHeartCPESign value.heart )
    , ( "lungs", encodeEverySet encodeLungsCPESign value.lungs )
    , ( "deleted", bool False )
    , ( "type", string "acute_illness_core_exam" )
    ]


encodeAcuteIllnessDangerSigns : AcuteIllnessDangerSigns -> List ( String, Value )
encodeAcuteIllnessDangerSigns =
    encodeAcuteIllnessMeasurement encodeAcuteIllnessDangerSignsValue


encodeAcuteIllnessDangerSignsValue : EverySet AcuteIllnessDangerSign -> List ( String, Value )
encodeAcuteIllnessDangerSignsValue value =
    [ ( "acute_illness_danger_signs", encodeEverySet encodeAcuteIllnessDangerSign value )
    , ( "deleted", bool False )
    , ( "type", string "acute_illness_danger_signs" )
    ]


encodeAcuteIllnessDangerSign : AcuteIllnessDangerSign -> Value
encodeAcuteIllnessDangerSign sign =
    string <|
        case sign of
            DangerSignConditionNotImproving ->
                "condition-not-improving"

            DangerSignUnableDrinkSuck ->
                "unable-drink-suck"

            DangerSignVomiting ->
                "vomiting"

            DangerSignConvulsions ->
                "convulsions"

            DangerSignLethargyUnconsciousness ->
                "lethargy-unconsciousness"

            DangerSignRespiratoryDistress ->
                "respiratory-distress"

            DangerSignSpontaneousBleeding ->
                "spontaneous-bleeding"

            DangerSignBloodyDiarrhea ->
                "bloody-diarrhea"

            DangerSignNewSkinRash ->
                "new-skip-rash"

            NoAcuteIllnessDangerSign ->
                "none"


encodeAcuteIllnessNutrition : AcuteIllnessNutrition -> List ( String, Value )
encodeAcuteIllnessNutrition =
    encodeAcuteIllnessMeasurement encodeAcuteIllnessNutritionValue


encodeAcuteIllnessNutritionValue : EverySet ChildNutritionSign -> List ( String, Value )
encodeAcuteIllnessNutritionValue nutritions =
    [ ( "nutrition_signs", encodeEverySet encodeNutritionSign nutritions )
    , ( "deleted", bool False )
    , ( "type", string "acute_illness_nutrition" )
    ]


encodeHealthEducation : HealthEducation -> List ( String, Value )
encodeHealthEducation =
    encodeAcuteIllnessMeasurement (encodeHealthEducationValueWithType "health_education")


encodeAcuteIllnessContactsTracing : AcuteIllnessContactsTracing -> List ( String, Value )
encodeAcuteIllnessContactsTracing =
    encodeAcuteIllnessMeasurement encodeAcuteIllnessContactsTracingValue


encodeAcuteIllnessContactsTracingValue : List ContactTraceItem -> List ( String, Value )
encodeAcuteIllnessContactsTracingValue items =
    [ ( "contacts_trace_data", list encodeContactTraceItemToString items )
    , ( "deleted", bool False )
    , ( "type", string "acute_illness_contacts_tracing" )
    ]


encodeContactTraceItemToString : ContactTraceItem -> Value
encodeContactTraceItemToString item =
    [ fromEntityUuid item.personId
    , item.firstName
    , item.secondName
    , genderToString item.gender
    , item.phoneNumber
    , formatYYYYMMDD item.contactDate
    ]
        |> String.join "[&]"
        |> string


encodeAcuteIllnessTraceContact : AcuteIllnessTraceContact -> List ( String, Value )
encodeAcuteIllnessTraceContact =
    encodeAcuteIllnessMeasurement encodeAcuteIllnessTraceContactValue


encodeAcuteIllnessTraceContactValue : ContactTraceItem -> List ( String, Value )
encodeAcuteIllnessTraceContactValue item =
    encodeContactTraceItem item
        ++ [ ( "deleted", bool False )
           , ( "type", string "acute_illness_trace_contact" )
           ]


encodeContactTraceItem : ContactTraceItem -> List ( String, Value )
encodeContactTraceItem item =
    let
        lastFollowUp =
            Maybe.map
                (\lastFollowUpDate ->
                    [ ( "last_follow_up_date", Gizra.NominalDate.encodeYYYYMMDD lastFollowUpDate ) ]
                )
                item.lastFollowUpDate
                |> Maybe.withDefault []

        signsGeneral =
            Maybe.map
                (\generalSigns ->
                    [ ( "symptoms_general", encodeEverySet encodeSymptomsGeneralSign generalSigns ) ]
                )
                item.generalSigns
                |> Maybe.withDefault []

        signsRespiratory =
            Maybe.map
                (\respiratorySigns ->
                    [ ( "symptoms_respiratory", encodeEverySet encodeSymptomsRespiratorySign respiratorySigns ) ]
                )
                item.respiratorySigns
                |> Maybe.withDefault []

        signsGI =
            Maybe.map
                (\giSigns ->
                    [ ( "symptoms_gi", encodeEverySet encodeSymptomsGISign giSigns ) ]
                )
                item.giSigns
                |> Maybe.withDefault []

        outcome =
            Maybe.map
                (\traceOutcome ->
                    [ ( "trace_outcome", encodeTraceOutcome traceOutcome ) ]
                )
                item.traceOutcome
                |> Maybe.withDefault []
    in
    [ ( "referred_person", encodeEntityUuid item.personId )
    , ( "first_name", string item.firstName )
    , ( "second_name", string item.secondName )
    , ( "gender", encodeGender item.gender )
    , ( "phone_number", string item.phoneNumber )
    , ( "contact_date", Gizra.NominalDate.encodeYYYYMMDD item.contactDate )
    , ( "date_concluded", Gizra.NominalDate.encodeYYYYMMDD item.resolutionDate )
    ]
        ++ lastFollowUp
        ++ signsGeneral
        ++ signsRespiratory
        ++ signsGI
        ++ outcome


encodeSymptomsGeneralSign : SymptomsGeneralSign -> Value
encodeSymptomsGeneralSign =
    symptomsGeneralSignToString >> string


encodeSymptomsRespiratorySign : SymptomsRespiratorySign -> Value
encodeSymptomsRespiratorySign =
    symptomsRespiratorySignToString >> string


encodeSymptomsGISign : SymptomsGISign -> Value
encodeSymptomsGISign =
    symptomsGISignToString >> string


encodeTraceOutcome : TraceOutcome -> Value
encodeTraceOutcome outcome =
    string <|
        case outcome of
            OutcomeNoAnswer ->
                "no-answer"

            OutcomeWrongContactInfo ->
                "wrong-contact-info"

            OutcomeDeclinedFollowUp ->
                "declined-follow-up"

            OutcomeNoSymptoms ->
                "no-symptoms"

            OutcomeReferredToHC ->
                "referred-to-hc"


encodeNutritionHealthEducation : NutritionHealthEducation -> List ( String, Value )
encodeNutritionHealthEducation =
    encodeNutritionMeasurement (encodeHealthEducationValueWithType "nutrition_health_education")


encodeWellChildHealthEducation : WellChildHealthEducation -> List ( String, Value )
encodeWellChildHealthEducation =
    encodeWellChildMeasurement (encodeHealthEducationValueWithType "well_child_health_education")


encodeGroupHealthEducation : GroupHealthEducation -> List ( String, Value )
encodeGroupHealthEducation =
    encodeGroupMeasurement (encodeHealthEducationValueWithType "group_health_education")


encodeHealthEducationValueWithType : String -> HealthEducationValue -> List ( String, Value )
encodeHealthEducationValueWithType type_ value =
    [ ( "health_education_signs", encodeEverySet encodeHealthEducationSign value.signs )
    , ( "reason_not_given_education", encodeReasonForNotProvidingHealthEducation value.reasonForNotProvidingHealthEducation )
    , ( "deleted", bool False )
    , ( "type", string type_ )
    ]


encodeHealthEducationSign : HealthEducationSign -> Value
encodeHealthEducationSign sign =
    string <|
        case sign of
            MalariaPrevention ->
                "education-for-diagnosis"

            NoHealthEducationSigns ->
                "none"


encodeReasonForNotProvidingHealthEducation : ReasonForNotProvidingHealthEducation -> Value
encodeReasonForNotProvidingHealthEducation reason =
    string <|
        case reason of
            PatientNeedsEmergencyReferral ->
                "needs-emergency-referral"

            ReceivedEmergencyCase ->
                "received-emergency-case"

            LackOfAppropriateEducationUserGuide ->
                "lack-of-appropriate-education-guide"

            PatientRefused ->
                "patient-refused"

            PatientTooIll ->
                "patient-too-ill"

            NoReasonForNotProvidingHealthEducation ->
                "none"


encodeWellChildSymptomsReview : WellChildSymptomsReview -> List ( String, Value )
encodeWellChildSymptomsReview =
    encodeWellChildMeasurement encodeWellChilSymptomsReviewValue


encodeWellChilSymptomsReviewValue : EverySet WellChildSymptom -> List ( String, Value )
encodeWellChilSymptomsReviewValue value =
    [ ( "well_child_symptoms", encodeEverySet encodeWellChildSymptom value )
    , ( "deleted", bool False )
    , ( "type", string "well_child_symptoms_review" )
    ]


encodeWellChildSymptom : WellChildSymptom -> Value
encodeWellChildSymptom symptom =
    string <|
        case symptom of
            SymptomBreathingProblems ->
                "breathing-problems"

            SymptomConvulsions ->
                "convulsions"

            SymptomLethargyOrUnresponsiveness ->
                "lethargy-or-unresponsiveness"

            SymptomDiarrhea ->
                "diarrhea"

            SymptomVomiting ->
                "vomiting"

            SymptomUmbilicalCordRedness ->
                "umbilical-cord-redness"

            SymptomStiffNeckOrBulgingFontanelle ->
                "stiff-neck-or-bulging-fontanelle"

            SymptomSevereEdema ->
                "severe-edema"

            SymptomPalmoplantarPallor ->
                "palmoplantar-pallor"

            SymptomHistoryOfFever ->
                "history-of-fever"

            SymptomBabyTiresQuicklyWhenFeeding ->
                "baby-tires-quickly-when-feeding"

            SymptomCoughingOrTearingWhileFeeding ->
                "coughing-or-tearing-while-feeding"

            SymptomRigidMusclesOrJawClenchingPreventingFeeding ->
                "rigid-muscles-or-jaw-clenching"

            ExcessiveSweatingWhenFeeding ->
                "excessive-sweating-when-feeding"

            NoWellChildSymptoms ->
                "none"


encodeWellChildVitals : WellChildVitals -> List ( String, Value )
encodeWellChildVitals =
    encodeWellChildMeasurement (encodeVitalsValueWithType "well_child_vitals")


encodeWellChildECD : WellChildECD -> List ( String, Value )
encodeWellChildECD =
    encodeWellChildMeasurement encodeWellChildECDValue


encodeWellChildECDValue : EverySet ECDSign -> List ( String, Value )
encodeWellChildECDValue value =
    [ ( "ecd_signs", encodeEverySet encodeECDSign value )
    , ( "deleted", bool False )
    , ( "type", string "well_child_ecd" )
    ]


encodeECDSign : ECDSign -> Value
encodeECDSign sign =
    string <|
        case sign of
            FollowMothersEyes ->
                "follow-mothers-eyes"

            MoveArmsAndLegs ->
                "move-arms-and-legs"

            RaiseHandsUp ->
                "raise-hands-up"

            Smile ->
                "smile"

            RollSideways ->
                "roll-sideways"

            BringHandsToMouth ->
                "bring-hands-to-mouth"

            HoldHeadWithoutSupport ->
                "hold-head-without-support"

            HoldAndShakeToys ->
                "hold-and-shake-toys"

            ReactToSuddenSounds ->
                "react-to-sudden-sounds"

            UseConsonantSounds ->
                "use-consonant-sounds"

            RespondToSoundWithSound ->
                "respond-to-sound-with-sound"

            TurnHeadWhenCalled ->
                "turn-head-when-called"

            SitWithoutSupport ->
                "sit-without-support"

            SmileBack ->
                "smile-back"

            RollTummyToBack ->
                "roll-tummy-to-back"

            ReachForToys ->
                "reach-for-toys"

            UseSimpleGestures ->
                "use-simple-gestures"

            StandOnTheirOwn ->
                "stand-on-their-own"

            CopyDuringPlay ->
                "copy-during-play"

            SayMamaDada ->
                "say-mama-dada"

            CanHoldSmallObjects ->
                "can-hold-small-objects"

            LooksWhenPointedAt ->
                "looks-when-pointed-at"

            UseSingleWords ->
                "use-single-words"

            WalkWithoutHelp ->
                "walk-without-help"

            PlayPretend ->
                "play-pretend"

            PointToThingsOfInterest ->
                "point-to-things-of-interest"

            UseShortPhrases ->
                "use-short-phrases"

            InterestedInOtherChildren ->
                "interested-in-other-children"

            FollowSimpleInstructions ->
                "follow-simple-instructions"

            KickBall ->
                "kick-ball"

            PointAtNamedObjects ->
                "point-at-named-objects"

            DressThemselves ->
                "dress-themselves"

            WashHandsGoToToiled ->
                "wash-hands-go-to-toiled"

            KnowsColorsAndNumbers ->
                "knows-colors-and-numbers"

            UseMediumPhrases ->
                "use-medium-phrases"

            PlayMakeBelieve ->
                "play-make-believe"

            FollowThreeStepInstructions ->
                "follow-three-step-instructions"

            StandOnOneFootFiveSeconds ->
                "stand-on-one-foot-five-seconds"

            UseLongPhrases ->
                "use-long-phrases"

            ShareWithOtherChildren ->
                "share-with-other-children"

            CountToTen ->
                "count-to-ten"

            NoECDSigns ->
                "none"


encodeWellChildHeadCircumference : WellChildHeadCircumference -> List ( String, Value )
encodeWellChildHeadCircumference =
    encodeWellChildMeasurement encodeHeadCircumferenceValue


encodeHeadCircumferenceValue : HeadCircumferenceValue -> List ( String, Value )
encodeHeadCircumferenceValue value =
    [ ( "head_circumference", encodeHeadCircumferenceInCm value.headCircumference )
    , ( "measurement_notes", encodeEverySet encodeMeasurementNote value.notes )
    , ( "deleted", bool False )
    , ( "type", string "well_child_head_circumference" )
    ]


encodeHeadCircumferenceInCm : HeadCircumferenceInCm -> Value
encodeHeadCircumferenceInCm (HeadCircumferenceInCm cm) =
    float cm


encodeMeasurementNote : MeasurementNote -> Value
encodeMeasurementNote note =
    string <|
        case note of
            NoteNotTaken ->
                "not-taken"

            NoMeasurementNotes ->
                "none"


encodeWellChildAlbendazole : WellChildAlbendazole -> List ( String, Value )
encodeWellChildAlbendazole =
    encodeWellChildMeasurement encodeWellChildAlbendazoleValue


encodeWellChildAlbendazoleValue : AdministrationNote -> List ( String, Value )
encodeWellChildAlbendazoleValue note =
    [ ( "administration_note", encodeAdministrationNote note )
    , ( "deleted", bool False )
    , ( "type", string "well_child_albendazole" )
    ]


encodeWellChildMebendezole : WellChildMebendezole -> List ( String, Value )
encodeWellChildMebendezole =
    encodeWellChildMeasurement encodeWellChildMebendezoleValue


encodeWellChildMebendezoleValue : AdministrationNote -> List ( String, Value )
encodeWellChildMebendezoleValue note =
    [ ( "administration_note", encodeAdministrationNote note )
    , ( "deleted", bool False )
    , ( "type", string "well_child_mebendezole" )
    ]


encodeWellChildVitaminA : WellChildVitaminA -> List ( String, Value )
encodeWellChildVitaminA =
    encodeWellChildMeasurement encodeWellChildVitaminAValue


encodeWellChildVitaminAValue : AdministrationNote -> List ( String, Value )
encodeWellChildVitaminAValue note =
    [ ( "administration_note", encodeAdministrationNote note )
    , ( "deleted", bool False )
    , ( "type", string "well_child_vitamin_a" )
    ]


encodeAdministrationNote : AdministrationNote -> Value
encodeAdministrationNote =
    administrationNoteToString >> string


encodeWellChildPregnancySummary : WellChildPregnancySummary -> List ( String, Value )
encodeWellChildPregnancySummary =
    encodeWellChildMeasurement encodePregnancySummaryValue


encodePregnancySummaryValue : PregnancySummaryValue -> List ( String, Value )
encodePregnancySummaryValue value =
    let
        apgar =
            Maybe.map2
                (\apgarOneMin apgarFiveMin ->
                    [ ( "apgar_one_min", float apgarOneMin )
                    , ( "apgar_five_min", float apgarFiveMin )
                    ]
                )
                value.apgarOneMin
                value.apgarFiveMin
                |> Maybe.withDefault []

        birthWeight =
            Maybe.map
                (\(WeightInGrm weight) ->
                    [ ( "weight", float weight ) ]
                )
                value.birthWeight
                |> Maybe.withDefault []

        birthLength =
            Maybe.map
                (\(HeightInCm length) ->
                    [ ( "height", float length ) ]
                )
                value.birthLength
                |> Maybe.withDefault []
    in
    [ ( "expected_date_concluded", Gizra.NominalDate.encodeYYYYMMDD value.expectedDateConcluded )
    , ( "delivery_complications", encodeEverySet encodeDeliveryComplication value.deliveryComplications )
    , ( "pregnancy_summary_signs", encodeEverySet encodePregnancySummarySign value.signs )
    , ( "birth_defects", encodeEverySet encodeBirthDefect value.birthDefects )
    , ( "deleted", bool False )
    , ( "type", string "well_child_pregnancy_summary" )
    ]
        ++ apgar
        ++ birthWeight
        ++ birthLength


encodeDeliveryComplication : DeliveryComplication -> Value
encodeDeliveryComplication complication =
    string <|
        case complication of
            ComplicationGestationalDiabetes ->
                "gestational-diabetes"

            ComplicationEmergencyCSection ->
                "emergency-c-section"

            ComplicationPreclampsia ->
                "preclampsia"

            ComplicationMaternalHemmorhage ->
                "maternal-hemmorhage"

            ComplicationHiv ->
                "hiv"

            ComplicationMaternalDeath ->
                "maternal-death"

            ComplicationOther ->
                "other"

            NoDeliveryComplications ->
                "none"


encodePregnancySummarySign : PregnancySummarySign -> Value
encodePregnancySummarySign sign =
    string <|
        case sign of
            ApgarScores ->
                "apgar-scores"

            BirthLength ->
                "birth-length"

            NoPregnancySummarySigns ->
                "none"


encodeBirthDefect : BirthDefect -> Value
encodeBirthDefect defect =
    string <|
        case defect of
            DefectBirthInjury ->
                "birth-injury"

            DefectCleftLipWithCleftPalate ->
                "cleft-lip-with-cleft-palate"

            DefectCleftPalate ->
                "cleft-palate"

            DefectClubFoot ->
                "club-foot"

            DefectMacrocephaly ->
                "macrocephaly"

            DefectGastroschisis ->
                "gastroschisis"

            DefectHearingLoss ->
                "hearing-loss"

            DefectUndescendedTestes ->
                "undescended-testes"

            DefectHypospadias ->
                "hypospadias"

            DefectInguinalHernia ->
                "inguinal-hernia"

            DefectMicrocephaly ->
                "microcephaly"

            DefectNeuralTubes ->
                "neural-tubes"

            DefectDownSyndrome ->
                "down-syndrome"

            DefectCongenitalHeart ->
                "congenital-heart"

            DefectVentricalSeptal ->
                "ventrical-septal"

            DefectPulmonaryValveAtresiaAndStenosis ->
                "pulmonary-valve-atresia-and-stenosis"

            NoBirthDefects ->
                "none"


encodeWellChildNextVisit : WellChildNextVisit -> List ( String, Value )
encodeWellChildNextVisit =
    encodeWellChildMeasurement encodeNextVisitValue


encodeNextVisitValue : NextVisitValue -> List ( String, Value )
encodeNextVisitValue value =
    [ ( "immunisation_date", maybe Gizra.NominalDate.encodeYYYYMMDD value.immunisationDate )
    , ( "pediatric_visit_date", maybe Gizra.NominalDate.encodeYYYYMMDD value.pediatricVisitDate )
    , ( "deleted", bool False )
    , ( "type", string "well_child_next_visit" )
    ]


encodeWellChildBCGImmunisation : WellChildBCGImmunisation -> List ( String, Value )
encodeWellChildBCGImmunisation =
    encodeWellChildMeasurement (encodeVaccinationValueWithType "well_child_bcg_immunisation")


encodeWellChildDTPImmunisation : WellChildDTPImmunisation -> List ( String, Value )
encodeWellChildDTPImmunisation =
    encodeWellChildMeasurement (encodeVaccinationValueWithType "well_child_dtp_immunisation")


encodeWellChildHPVImmunisation : WellChildHPVImmunisation -> List ( String, Value )
encodeWellChildHPVImmunisation =
    encodeWellChildMeasurement (encodeVaccinationValueWithType "well_child_hpv_immunisation")


encodeWellChildIPVImmunisation : WellChildIPVImmunisation -> List ( String, Value )
encodeWellChildIPVImmunisation =
    encodeWellChildMeasurement (encodeVaccinationValueWithType "well_child_ipv_immunisation")


encodeWellChildMRImmunisation : WellChildMRImmunisation -> List ( String, Value )
encodeWellChildMRImmunisation =
    encodeWellChildMeasurement (encodeVaccinationValueWithType "well_child_mr_immunisation")


encodeWellChildOPVImmunisation : WellChildOPVImmunisation -> List ( String, Value )
encodeWellChildOPVImmunisation =
    encodeWellChildMeasurement (encodeVaccinationValueWithType "well_child_opv_immunisation")


encodeWellChildPCV13Immunisation : WellChildPCV13Immunisation -> List ( String, Value )
encodeWellChildPCV13Immunisation =
    encodeWellChildMeasurement (encodeVaccinationValueWithType "well_child_pcv13_immunisation")


encodeWellChildRotarixImmunisation : WellChildRotarixImmunisation -> List ( String, Value )
encodeWellChildRotarixImmunisation =
    encodeWellChildMeasurement (encodeVaccinationValueWithType "well_child_rotarix_immunisation")


encodeVaccinationValueWithType : String -> VaccinationValue -> List ( String, Value )
encodeVaccinationValueWithType type_ value =
    [ ( "administered_doses", encodeEverySet encodeVaccinationDose value.administeredDoses )
    , ( "administration_dates", encodeEverySet Gizra.NominalDate.encodeYYYYMMDD value.administrationDates )
    , ( "administration_note", encodeAdministrationNote value.administrationNote )
    , ( "deleted", bool False )
    , ( "type", string type_ )
    ]


encodeVaccinationDose : VaccineDose -> Value
encodeVaccinationDose =
    vaccineDoseToString >> string


encodePrenatalSymptomReview : PrenatalSymptomReview -> List ( String, Value )
encodePrenatalSymptomReview =
    encodePrenatalMeasurement encodePrenatalSymptomReviewValue


encodePrenatalSymptomReviewValue : PrenatalSymptomReviewValue -> List ( String, Value )
encodePrenatalSymptomReviewValue value =
    let
        flankPainSign =
            Maybe.map
                (\sign -> [ ( "flank_pain_sign", encodePrenatalFlankPainSign sign ) ])
                value.flankPainSign
                |> Maybe.withDefault []
    in
    [ ( "prenatal_symptoms", encodeEverySet encodePrenatalSymptom value.symptoms )
    , ( "prenatal_symptom_questions", encodeEverySet encodePrenatalSymptomQuestion value.symptomQuestions )
    , ( "deleted", bool False )
    , ( "type", string "prenatal_symptom_review" )
    ]
        ++ flankPainSign


encodePrenatalSymptom : PrenatalSymptom -> Value
encodePrenatalSymptom =
    prenatalSymptomToString >> string


encodePrenatalSymptomQuestion : PrenatalSymptomQuestion -> Value
encodePrenatalSymptomQuestion =
    prenatalSymptomQuestionToString >> string


encodePrenatalFlankPainSign : PrenatalFlankPainSign -> Value
encodePrenatalFlankPainSign =
    prenatalFlankPainSignToString >> string


encodePrenatalOutsideCare : PrenatalOutsideCare -> List ( String, Value )
encodePrenatalOutsideCare =
    encodePrenatalMeasurement (encodeOutsideCareValue "prenatal_diagnoses" encodePrenatalDiagnosis "prenatal_outside_care")


encodeOutsideCareValue : String -> (diagnosis -> Value) -> String -> OutsideCareValue diagnosis -> List ( String, Value )
encodeOutsideCareValue fieldName diagnosisEncouder type_ value =
    let
        diagnoses =
            Maybe.map
                (\diagnoses_ ->
                    [ ( fieldName, encodeEverySet diagnosisEncouder diagnoses_ ) ]
                )
                value.diagnoses
                |> Maybe.withDefault []

        medications =
            Maybe.map
                (\medications_ ->
                    [ ( "outside_care_medications", encodeEverySet encodeOutsideCareMedication medications_ ) ]
                )
                value.medications
                |> Maybe.withDefault []
    in
    [ ( "outside_care_signs", encodeEverySet encodeOutsideCareSign value.signs )
    , ( "deleted", bool False )
    , ( "type", string type_ )
    ]
        ++ diagnoses
        ++ medications


encodeOutsideCareSign : OutsideCareSign -> Value
encodeOutsideCareSign =
    outsideCareSignToString >> string


encodeOutsideCareMedication : OutsideCareMedication -> Value
encodeOutsideCareMedication =
    outsideCareMedicationToString >> string


encodeNCDCoMorbidities : NCDCoMorbidities -> List ( String, Value )
encodeNCDCoMorbidities =
    encodeNCDMeasurement encodeNCDCoMorbiditiesValue


encodeNCDCoMorbiditiesValue : NCDCoMorbiditiesValue -> List ( String, Value )
encodeNCDCoMorbiditiesValue value =
    [ ( "comorbidities", encodeEverySet encodeMedicalCondition value )
    , ( "deleted", bool False )
    , ( "type", string "ncd_co_morbidities" )
    ]


encodeMedicalCondition : MedicalCondition -> Value
encodeMedicalCondition =
    medicalConditionToString >> string


encodeNCDCoreExam : NCDCoreExam -> List ( String, Value )
encodeNCDCoreExam =
    encodeNCDMeasurement (encodeCorePhysicalExamValueWithType "ncd_core_exam")


encodeNCDCreatinineTest : NCDCreatinineTest -> List ( String, Value )
encodeNCDCreatinineTest =
    encodeNCDMeasurement encodeCreatinineTestValue


encodeCreatinineTestValue : CreatinineTestValue -> List ( String, Value )
encodeCreatinineTestValue value =
    let
        executionDate =
            Maybe.map
                (\date -> [ ( "execution_date", Gizra.NominalDate.encodeYYYYMMDD date ) ])
                value.executionDate
                |> Maybe.withDefault []

        result =
            Maybe.map2
                (\creatinineResult bunResult ->
                    [ ( "creatinine_result", float creatinineResult )
                    , ( "bun_result", float bunResult )
                    ]
                )
                value.creatinineResult
                value.bunResult
                |> Maybe.withDefault []
    in
    ( "test_execution_note", encodeTestExecutionNote value.executionNote )
        :: executionDate
        ++ result
        ++ [ ( "deleted", bool False )
           , ( "type", string "ncd_creatinine_test" )
           ]


encodeNCDDangerSigns : NCDDangerSigns -> List ( String, Value )
encodeNCDDangerSigns =
    encodeNCDMeasurement encodeNCDDangerSignsValue


encodeNCDDangerSignsValue : NCDDangerSignsValue -> List ( String, Value )
encodeNCDDangerSignsValue value =
    [ ( "ncd_danger_signs", encodeEverySet encodeNCDDangerSign value )
    , ( "deleted", bool False )
    , ( "type", string "ncd_danger_signs" )
    ]


encodeNCDDangerSign : NCDDangerSign -> Value
encodeNCDDangerSign =
    ncdDangerSignToString >> string


encodeNCDFamilyHistory : NCDFamilyHistory -> List ( String, Value )
encodeNCDFamilyHistory =
    encodeNCDMeasurement encodeNCDFamilyHistoryValue


encodeNCDFamilyHistoryValue : NCDFamilyHistoryValue -> List ( String, Value )
encodeNCDFamilyHistoryValue value =
    let
        hypertensionPredecessors =
            Maybe.map
                (\predecessors ->
                    [ ( "hypertension_predecessors", encodeEverySet encodePredecessor predecessors ) ]
                )
                value.hypertensionPredecessors
                |> Maybe.withDefault []

        heartProblemPredecessors =
            Maybe.map
                (\predecessors ->
                    [ ( "heart_problem_predecessors", encodeEverySet encodePredecessor predecessors ) ]
                )
                value.heartProblemPredecessors
                |> Maybe.withDefault []

        diabetesPredecessors =
            Maybe.map
                (\predecessors ->
                    [ ( "diabetes_predecessors", encodeEverySet encodePredecessor predecessors ) ]
                )
                value.diabetesPredecessors
                |> Maybe.withDefault []
    in
    [ ( "ncd_family_history_signs", encodeEverySet encodeNCDFamilyHistorySign value.signs )
    , ( "deleted", bool False )
    , ( "type", string "ncd_family_history" )
    ]
        ++ hypertensionPredecessors
        ++ heartProblemPredecessors
        ++ diabetesPredecessors


encodeNCDFamilyHistorySign : NCDFamilyHistorySign -> Value
encodeNCDFamilyHistorySign =
    ncdFamilyHistorySignToString >> string


encodePredecessor : Predecessor -> Value
encodePredecessor =
    predecessorToString >> string


encodeNCDFamilyPlanning : NCDFamilyPlanning -> List ( String, Value )
encodeNCDFamilyPlanning =
    encodeNCDMeasurement (encodeFamilyPlanningValueWithType "ncd_family_planning")


encodeNCDHealthEducation : NCDHealthEducation -> List ( String, Value )
encodeNCDHealthEducation =
    encodeNCDMeasurement encodeNCDHealthEducationValue


encodeNCDHealthEducationValue : NCDHealthEducationValue -> List ( String, Value )
encodeNCDHealthEducationValue value =
    [ ( "ncd_health_education_signs", encodeEverySet encodeNCDHealthEducationSign value )
    , ( "deleted", bool False )
    , ( "type", string "ncd_health_education" )
    ]


encodeNCDHealthEducationSign : NCDHealthEducationSign -> Value
encodeNCDHealthEducationSign sign =
    string <|
        case sign of
            EducationHypertension ->
                "hypertension"

            NoNCDHealthEducationSigns ->
                "none"


encodeNCDHIVTest : NCDHIVTest -> List ( String, Value )
encodeNCDHIVTest =
    encodeNCDMeasurement (encodeHIVTestValue "ncd_hiv_test")


encodeNCDLabsResults : NCDLabsResults -> List ( String, Value )
encodeNCDLabsResults =
    encodeNCDMeasurement (encodeLabsResultsValue "ncd_labs_results")


encodeNCDLiverFunctionTest : NCDLiverFunctionTest -> List ( String, Value )
encodeNCDLiverFunctionTest =
    encodeNCDMeasurement encodeLiverFunctionTestValue


encodeLiverFunctionTestValue : LiverFunctionTestValue -> List ( String, Value )
encodeLiverFunctionTestValue value =
    let
        executionDate =
            Maybe.map
                (\date -> [ ( "execution_date", Gizra.NominalDate.encodeYYYYMMDD date ) ])
                value.executionDate
                |> Maybe.withDefault []

        result =
            Maybe.map2
                (\altResult astResult ->
                    [ ( "alt_result", float altResult )
                    , ( "ast_result", float astResult )
                    ]
                )
                value.altResult
                value.astResult
                |> Maybe.withDefault []
    in
    ( "test_execution_note", encodeTestExecutionNote value.executionNote )
        :: executionDate
        ++ result
        ++ [ ( "deleted", bool False )
           , ( "type", string "ncd_liver_function_test" )
           ]


encodeNCDMedicationDistribution : NCDMedicationDistribution -> List ( String, Value )
encodeNCDMedicationDistribution =
    encodeNCDMeasurement encodeNCDMedicationDistributionValue


encodeNCDMedicationDistributionValue : NCDMedicationDistributionValue -> List ( String, Value )
encodeNCDMedicationDistributionValue value =
    [ ( "recommended_treatment", encodeEverySet encodeRecommendedTreatmentSign value.recommendedTreatmentSigns )
    , ( "ncd_guidance", encodeEverySet encodeNCDGuidanceSign value.guidanceSigns )
    , ( "deleted", bool False )
    , ( "type", string "ncd_medication_distribution" )
    ]


encodeNCDGuidanceSign : NCDGuidanceSign -> Value
encodeNCDGuidanceSign sign =
    string <|
        case sign of
            ReturnInOneMonth ->
                "return-1m"

            NoNCDGuidanceSigns ->
                "none"


encodeNCDMedicationHistory : NCDMedicationHistory -> List ( String, Value )
encodeNCDMedicationHistory =
    encodeNCDMeasurement encodeNCDMedicationHistoryValue


encodeNCDMedicationHistoryValue : NCDMedicationHistoryValue -> List ( String, Value )
encodeNCDMedicationHistoryValue value =
    [ ( "causing_hypertension", encodeEverySet encodeMedicationCausingHypertension value.medicationsCausingHypertension )
    , ( "treating_hypertension", encodeEverySet encodeMedicationTreatingHypertension value.medicationsTreatingHypertension )
    , ( "treating_diabetes", encodeEverySet encodeMedicationTreatingDiabetes value.medicationsTreatingDiabetes )
    , ( "deleted", bool False )
    , ( "type", string "ncd_medication_history" )
    ]


encodeMedicationCausingHypertension : MedicationCausingHypertension -> Value
encodeMedicationCausingHypertension =
    medicationCausingHypertensionToString >> string


encodeMedicationTreatingHypertension : MedicationTreatingHypertension -> Value
encodeMedicationTreatingHypertension =
    medicationTreatingHypertensionToString >> string


encodeMedicationTreatingDiabetes : MedicationTreatingDiabetes -> Value
encodeMedicationTreatingDiabetes =
    medicationTreatingDiabetesToString >> string


encodeNCDOutsideCare : NCDOutsideCare -> List ( String, Value )
encodeNCDOutsideCare =
    encodeNCDMeasurement (encodeOutsideCareValue "medical_conditions" encodeMedicalCondition "ncd_outside_care")


encodeNCDPregnancyTest : NCDPregnancyTest -> List ( String, Value )
encodeNCDPregnancyTest =
    encodeNCDMeasurement encodePregnancyTestValue


encodePregnancyTestValue : PregnancyTestValue -> List ( String, Value )
encodePregnancyTestValue value =
    let
        executionDate =
            Maybe.map
                (\date -> [ ( "execution_date", Gizra.NominalDate.encodeYYYYMMDD date ) ])
                value.executionDate
                |> Maybe.withDefault []

        result =
            Maybe.map
                (\testResult -> [ ( "test_result", encodeTestResult testResult ) ])
                value.testResult
                |> Maybe.withDefault []
    in
    ( "test_execution_note", encodeTestExecutionNote value.executionNote )
        :: executionDate
        ++ result
        ++ [ ( "deleted", bool False )
           , ( "type", string "ncd_pregnancy_test" )
           ]


encodeNCDRandomBloodSugarTest : NCDRandomBloodSugarTest -> List ( String, Value )
encodeNCDRandomBloodSugarTest =
    encodeNCDMeasurement (encodeRandomBloodSugarTestValue "ncd_random_blood_sugar_test")


encodeNCDReferral : NCDReferral -> List ( String, Value )
encodeNCDReferral =
    encodeNCDMeasurement encodeReferralValue


encodeReferralValue : ReferralValue -> List ( String, Value )
encodeReferralValue value =
    let
        nonReferralReasons =
            Maybe.map (\reason -> [ ( "reasons_for_non_referrals", encodeEverySet encodeNonReferralSign reason ) ])
                value.nonReferralReasons
                |> Maybe.withDefault []
    in
    (( "referrals", encodeEverySet encodeReferToFacilitySign value.referralSigns ) :: nonReferralReasons)
        ++ [ ( "deleted", bool False )
           , ( "type", string "ncd_referral" )
           ]


encodeNCDSocialHistory : NCDSocialHistory -> List ( String, Value )
encodeNCDSocialHistory =
    encodeNCDMeasurement encodeNCDSocialHistoryValue


encodeNCDSocialHistoryValue : NCDSocialHistoryValue -> List ( String, Value )
encodeNCDSocialHistoryValue value =
    let
        beveragesPerWeek =
            Maybe.map
                (\perWeek ->
                    [ ( "beverages_per_week", int perWeek ) ]
                )
                value.beveragesPerWeek
                |> Maybe.withDefault []

        cigarettesPerWeek =
            Maybe.map
                (\perWeek ->
                    [ ( "cigarettes_per_week", int perWeek ) ]
                )
                value.cigarettesPerWeek
                |> Maybe.withDefault []
    in
    [ ( "ncd_social_history_signs", encodeEverySet encodeNCDSocialHistorySign value.signs )
    , ( "food_group", encodeFoodGroup value.foodGroup )
    , ( "deleted", bool False )
    , ( "type", string "ncd_social_history" )
    ]
        ++ beveragesPerWeek
        ++ cigarettesPerWeek


encodeNCDSocialHistorySign : NCDSocialHistorySign -> Value
encodeNCDSocialHistorySign =
    ncdSocialHistorySignToString >> string


encodeFoodGroup : FoodGroup -> Value
encodeFoodGroup =
    foodGroupToString >> string


encodeNCDSymptomReview : NCDSymptomReview -> List ( String, Value )
encodeNCDSymptomReview =
    encodeNCDMeasurement encodeNCDSymptomReviewValue


encodeNCDSymptomReviewValue : NCDSymptomReviewValue -> List ( String, Value )
encodeNCDSymptomReviewValue value =
    [ ( "ncd_group1_symptoms", encodeEverySet encodeNCDGroup1Symptom value.group1Symptoms )
    , ( "ncd_group2_symptoms", encodeEverySet encodeNCDGroup2Symptom value.group2Symptoms )
    , ( "ncd_pain_symptoms", encodeEverySet encodeNCDPainSymptom value.painSymptoms )
    , ( "deleted", bool False )
    , ( "type", string "ncd_symptom_review" )
    ]


encodeNCDGroup1Symptom : NCDGroup1Symptom -> Value
encodeNCDGroup1Symptom =
    ncdGroup1SymptomToString >> string


encodeNCDGroup2Symptom : NCDGroup2Symptom -> Value
encodeNCDGroup2Symptom =
    ncdGroup2SymptomToString >> string


encodeNCDPainSymptom : NCDPainSymptom -> Value
encodeNCDPainSymptom =
    ncdPainSymptomToString >> string


encodeNCDUrineDipstickTest : NCDUrineDipstickTest -> List ( String, Value )
encodeNCDUrineDipstickTest =
    encodeNCDMeasurement (encodeUrineDipstickTestValue "ncd_urine_dipstick_test")


encodeNCDVitals : NCDVitals -> List ( String, Value )
encodeNCDVitals =
    encodeNCDMeasurement (encodeVitalsValueWithType "ncd_vitals")


encodeGroupNCDA : GroupNCDA -> List ( String, Value )
encodeGroupNCDA =
    encodeGroupMeasurement (encodeNCDAValueWithType "group_ncda")


encodeNutritionNCDA : NutritionNCDA -> List ( String, Value )
encodeNutritionNCDA =
    encodeNutritionMeasurement (encodeNCDAValueWithType "nutrition_ncda")


encodeWellChildNCDA : WellChildNCDA -> List ( String, Value )
encodeWellChildNCDA =
    encodeWellChildMeasurement (encodeNCDAValueWithType "well_child_ncda")


encodeNCDAValueWithType : String -> NCDAValue -> List ( String, Value )
encodeNCDAValueWithType type_ value =
    let
        birthWeight =
            Maybe.map (\(WeightInGrm weight) -> [ ( "weight", float weight ) ])
                value.birthWeight
                |> Maybe.withDefault []
    in
    [ ( "ncda_signs", encodeEverySet encodeNCDASign value.signs )
    , ( "deleted", bool False )
    , ( "type", string type_ )
    ]
        ++ birthWeight


encodeNCDASign : NCDASign -> Value
encodeNCDASign =
    ncdaSignToString >> string


encodeNCDLipidPanelTest : NCDLipidPanelTest -> List ( String, Value )
encodeNCDLipidPanelTest =
    encodeNCDMeasurement encodeLipidPanelTestValue


encodeLipidPanelTestValue : LipidPanelTestValue -> List ( String, Value )
encodeLipidPanelTestValue value =
    let
        executionDate =
            Maybe.map
                (\date -> [ ( "execution_date", Gizra.NominalDate.encodeYYYYMMDD date ) ])
                value.executionDate
                |> Maybe.withDefault []

        result =
            Maybe.map5
                (\unitOfMeasurement totalCholesterolResult ldlCholesterolResult hdlCholesterolResult triglycerides ->
                    [ ( "unit_of_measurement", encodeUnitOfMeasurement unitOfMeasurement )
                    , ( "total_cholesterol", float totalCholesterolResult )
                    , ( "ldl_cholesterol", float ldlCholesterolResult )
                    , ( "hdl_cholesterol", float hdlCholesterolResult )
                    , ( "triglycerides", float triglycerides )
                    ]
                )
                value.unitOfMeasurement
                value.totalCholesterolResult
                value.ldlCholesterolResult
                value.hdlCholesterolResult
                value.triglyceridesResult
                |> Maybe.withDefault []
    in
    ( "test_execution_note", encodeTestExecutionNote value.executionNote )
        :: executionDate
        ++ result
        ++ [ ( "deleted", bool False )
           , ( "type", string "ncd_lipid_panel_test" )
           ]


encodeUnitOfMeasurement : UnitOfMeasurement -> Value
encodeUnitOfMeasurement =
    unitOfMeasurementToString >> string


encodeNCDHbA1cTest : NCDHbA1cTest -> List ( String, Value )
encodeNCDHbA1cTest =
    encodeNCDMeasurement encodeHbA1cTestValue


encodeHbA1cTestValue : HbA1cTestValue -> List ( String, Value )
encodeHbA1cTestValue value =
    let
        executionDate =
            Maybe.map
                (\date -> [ ( "execution_date", Gizra.NominalDate.encodeYYYYMMDD date ) ])
                value.executionDate
                |> Maybe.withDefault []

        result =
            Maybe.map
                (\hba1cResult ->
                    [ ( "hba1c_result", float hba1cResult ) ]
                )
                value.hba1cResult
                |> Maybe.withDefault []
    in
    ( "test_execution_note", encodeTestExecutionNote value.executionNote )
        :: executionDate
        ++ result
        ++ [ ( "deleted", bool False )
           , ( "type", string "ncd_hba1c_test" )
           ]
