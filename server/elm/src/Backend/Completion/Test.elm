module Backend.Completion.Test exposing (all)

import Backend.Completion.Decoder exposing (decodeSyncResponse)
import Backend.Completion.Model exposing (..)
import Backend.Completion.Utils
    exposing
        ( acuteIllnessActivityFromMapping
        , childScoreboardActivityFromMapping
        , hivActivityFromMapping
        , homeVisitActivityFromMapping
        , ncdActivityFromMapping
        , nutritionChildActivityFromMapping
        , nutritionMotherActivityFromMapping
        , prenatalActivityFromMapping
        , tuberculosisActivityFromMapping
        , wellChildActivityFromMapping
        )
import Expect
import Json.Decode exposing (decodeString, errorToString)
import Test exposing (Test, describe, test)


{-| Written from `hedley_reports_generate_completion_data_for_nutrition_individual_encounter()`.
-}
nutritionIndividualLetters : List ( String, NutritionChildActivity )
nutritionIndividualLetters =
    [ ( "a", NutritionHeight )
    , ( "b", NutritionNutrition )
    , ( "c", NutritionPhoto )
    , ( "d", NutritionWeight )
    , ( "e", NutritionMUAC )
    , ( "f", NutritionContributingFactors )
    , ( "g", NutritionFollowUp )
    , ( "h", NutritionHealthEducation )
    , ( "i", NutritionSendToHC )
    , ( "j", NutritionNCDA )
    ]


{-| Written from `hedley_reports_generate_completion_data_for_nutrition_group_encounter() (`$mapping\_child`)`.
-}
nutritionGroupChildLetters : List ( String, NutritionChildActivity )
nutritionGroupChildLetters =
    [ ( "a", NutritionHeight )
    , ( "b", NutritionNutrition )
    , ( "c", NutritionPhoto )
    , ( "d", NutritionWeight )
    , ( "e", NutritionMUAC )
    , ( "f", NutritionContributingFactors )
    , ( "g", NutritionFollowUp )
    , ( "h", NutritionHealthEducation )
    , ( "i", NutritionSendToHC )
    , ( "j", NutritionNCDA )
    , ( "k", NutritionChildFbf )
    ]


{-| Written from `hedley_reports_generate_completion_data_for_nutrition_group_encounter() (`$mapping\_mother`)`.
-}
nutritionGroupMotherLetters : List ( String, NutritionMotherActivity )
nutritionGroupMotherLetters =
    [ ( "a", NutritionFamilyPlanning )
    , ( "b", NutritionMotherFbf )
    , ( "c", NutritionLactation )
    ]


{-| Written from `hedley_reports_generate_completion_data_for_acute_illness()`.
-}
acuteIllnessLetters : List ( String, AcuteIllnessActivity )
acuteIllnessLetters =
    [ ( "a", AcuteIllnessAcuteFindings )
    , ( "b", AcuteIllnessContactsTracing )
    , ( "c", AcuteIllnessCoreExam )
    , ( "d", AcuteIllnessDangerSigns )
    , ( "e", AcuteIllnessFollowUp )
    , ( "f", AcuteIllnessMUAC )
    , ( "g", AcuteIllnessNutrition )
    , ( "h", AcuteIllnessVitals )
    , ( "i", AcuteIllnessCall114 )
    , ( "j", AcuteIllnessCOVIDTesting )
    , ( "k", AcuteIllnessExposure )
    , ( "l", AcuteIllnessContactHC )
    , ( "m", AcuteIllnessHealthEducation )
    , ( "n", AcuteIllnessIsolation )
    , ( "o", AcuteIllnessMalariaTesting )
    , ( "p", AcuteIllnessMedicationDistribution )
    , ( "q", AcuteIllnessSendToHC )
    , ( "r", AcuteIllnessSymptomsGeneral )
    , ( "s", AcuteIllnessSymptomsGI )
    , ( "t", AcuteIllnessSymptomsRespiratory )
    , ( "u", AcuteIllnessTravelHistory )
    , ( "v", AcuteIllnessPriorTreatment )
    , ( "w", AcuteIllnessOngoingTreatment )
    ]


{-| Written from `hedley_reports_generate_completion_data_for_well_child()`.
-}
wellChildLetters : List ( String, WellChildActivity )
wellChildLetters =
    [ ( "a", WellChildAlbendazole )
    , ( "b", WellChildBCGImmunisation )
    , ( "c", WellChildCaring )
    , ( "d", WellChildContributingFactors )
    , ( "e", WellChildDTPImmunisation )
    , ( "f", WellChildECD )
    , ( "g", WellChildFeeding )
    , ( "h", WellChildFollowUp )
    , ( "i", WellChildFoodSecurity )
    , ( "j", WellChildHeadCircumference )
    , ( "k", WellChildHealthEducation )
    , ( "l", WellChildHeight )
    , ( "m", WellChildHygiene )
    , ( "n", WellChildIPVImmunisation )
    , ( "o", WellChildMebendezole )
    , ( "p", WellChildMRImmunisation )
    , ( "q", WellChildMUAC )
    , ( "r", WellChildNCDA )
    , ( "s", WellChildNextVisit )
    , ( "t", WellChildNutrition )
    , ( "u", WellChildOPVImmunisation )
    , ( "v", WellChildPCV13Immunisation )
    , ( "w", WellChildPhoto )
    , ( "x", WellChildPregnancySummary )
    , ( "y", WellChildRotarixImmunisation )
    , ( "z", WellChildSendToHC )
    , ( "1", WellChildSymptomsReview )
    , ( "2", WellChildVitals )
    , ( "3", WellChildVitaminA )
    , ( "4", WellChildWeight )
    , ( "5", WellChildHPVImmunisation )
    , ( "6", WellChildDTPSAImmunisation )
    ]


{-| Written from `hedley_reports_generate_completion_data_for_home_visit_encounter()`.
-}
homeVisitLetters : List ( String, HomeVisitActivity )
homeVisitLetters =
    [ ( "a", HomeVisitCaring )
    , ( "b", HomeVisitFeeding )
    , ( "c", HomeVisitFoodSecurity )
    , ( "d", HomeVisitHygiene )
    ]


{-| Written from `hedley_reports_generate_completion_data_for_child_scoreboard_encounter()`.
-}
childScoreboardLetters : List ( String, ChildScoreboardActivity )
childScoreboardLetters =
    [ ( "a", ChildScoreboardNCDA )
    , ( "b", ChildScoreboardBCGImmunisation )
    , ( "c", ChildScoreboardDTPImmunisation )
    , ( "d", ChildScoreboardIPVImmunisation )
    , ( "e", ChildScoreboardMRImmunisation )
    , ( "f", ChildScoreboardOPVImmunisation )
    , ( "g", ChildScoreboardPCV13Immunisation )
    , ( "h", ChildScoreboardRotarixImmunisation )
    , ( "i", ChildScoreboardDTPSAImmunisation )
    ]


{-| Written from `hedley_reports_generate_completion_data_for_ncd()`.
-}
ncdLetters : List ( String, NCDActivity )
ncdLetters =
    [ ( "a", NCDCoreExam )
    , ( "b", NCDCoMorbidities )
    , ( "c", NCDCreatinineTest )
    , ( "d", NCDDangerSigns )
    , ( "e", NCDFamilyHistory )
    , ( "f", NCDFamilyPlanning )
    , ( "g", NCDHba1cTest )
    , ( "h", NCDHealthEducation )
    , ( "i", NCDHIVTest )
    , ( "j", NCDLipidPanelTest )
    , ( "k", NCDLiverFunctionTest )
    , ( "l", NCDMedicationDistribution )
    , ( "m", NCDMedicationHistory )
    , ( "n", NCDOutsideCare )
    , ( "o", NCDPregnancyTest )
    , ( "p", NCDRandomBloodSugarTest )
    , ( "q", NCDReferral )
    , ( "r", NCDSocialHistory )
    , ( "s", NCDSymptomReview )
    , ( "t", NCDUrineDipstickTest )
    , ( "u", NCDVitals )
    , ( "v", NCDCreatinineTestResult )
    , ( "w", NCDLipidPanelTestResult )
    , ( "x", NCDLiverFunctionTestResult )
    , ( "y", NCDRandomBloodSugarTestResult )
    , ( "z", NCDUrineDipstickTestResult )
    ]


{-| Written from `hedley_reports_generate_completion_data_for_hiv()`.
-}
hivLetters : List ( String, HIVActivity )
hivLetters =
    [ ( "a", HIVDiagnostics )
    , ( "b", HIVFollowUp )
    , ( "c", HIVHealthEducation )
    , ( "d", HIVMedication )
    , ( "e", HIVReferral )
    , ( "f", HIVSymptomReview )
    , ( "g", HIVTreatmentReview )
    ]


{-| Written from `hedley_reports_generate_completion_data_for_tuberculosis()`.
-}
tuberculosisLetters : List ( String, TuberculosisActivity )
tuberculosisLetters =
    [ ( "a", TuberculosisDiagnostics )
    , ( "b", TuberculosisDOT )
    , ( "c", TuberculosisFollowUp )
    , ( "d", TuberculosisHealthEducation )
    , ( "e", TuberculosisMedication )
    , ( "f", TuberculosisReferral )
    , ( "g", TuberculosisSymptomReview )
    , ( "h", TuberculosisTreatmentReview )
    ]


{-| Written from `hedley_reports_generate_completion_data_for_prenatal()`.
-}
prenatalLetters : List ( String, PrenatalActivity )
prenatalLetters =
    [ ( "a", PrenatalAppointmentConfirmation )
    , ( "b", PrenatalBirthPlan )
    , ( "c", PrenatalBreastExam )
    , ( "d", PrenatalCorePhysicalExam )
    , ( "e", PrenatalDangerSigns )
    , ( "f", PrenatalLastMenstrualPeriod )
    , ( "g", PrenatalMedicalHistory )
    , ( "h", PrenatalMedication )
    , ( "i", PrenatalObstetricHistory )
    , ( "j", PrenatalObstetricHistoryStep2 )
    , ( "k", PrenatalObstetricalExam )
    , ( "l", PrenatalPregnancyTesting )
    , ( "m", PrenatalBloodGprsTest )
    , ( "n", PrenatalBreastfeeding )
    , ( "o", PrenatalFamilyPlanning )
    , ( "p", PrenatalFollowUp )
    , ( "q", PrenatalGuExam )
    , ( "r", PrenatalHealthEducation )
    , ( "s", PrenatalHemoglobinTest )
    , ( "t", PrenatalHepatitisBTest )
    , ( "u", PrenatalHIVPCRTest )
    , ( "v", PrenatalHIVTest )
    , ( "w", PrenatalMalariaTest )
    , ( "x", PrenatalMedicationDistribution )
    , ( "y", PrenatalMentalHealth )
    , ( "z", PrenatalNutrition )
    , ( "0", PrenatalOutsideCare )
    , ( "1", PrenatalPartnerHIVTest )
    , ( "2", PrenatalPhoto )
    , ( "3", PrenatalRandomBloodSugarTest )
    , ( "4", PrenatalSendToHC )
    , ( "5", PrenatalSpecialityCare )
    , ( "6", PrenatalSymptomReview )
    , ( "7", PrenatalSyphilisTest )
    , ( "8", PrenatalTetanusImmunisation )
    , ( "9", PrenatalUrineDipstickTest )
    , ( "a1", PrenatalCalcium )
    , ( "b1", PrenatalFolate )
    , ( "c1", PrenatalIron )
    , ( "d1", PrenatalMMS )
    , ( "e1", PrenatalMebendazole )
    , ( "f1", PrenatalAspirin )
    , ( "g1", PrenatalFefol )
    ]


letterTableTest : String -> (String -> Maybe a) -> List ( String, a ) -> Test
letterTableTest name fromMapping letters =
    test name <|
        \_ ->
            List.map (Tuple.first >> fromMapping) letters
                |> Expect.equal (List.map (Tuple.second >> Just) letters)


letterTablesTest : Test
letterTablesTest =
    describe "each activity letter PHP sends reads as the activity PHP meant"
        [ letterTableTest "Acute Illness" acuteIllnessActivityFromMapping acuteIllnessLetters
        , letterTableTest "Child Scoreboard" childScoreboardActivityFromMapping childScoreboardLetters
        , letterTableTest "HIV" hivActivityFromMapping hivLetters
        , letterTableTest "Home Visit" homeVisitActivityFromMapping homeVisitLetters
        , letterTableTest "NCD" ncdActivityFromMapping ncdLetters
        , letterTableTest "Nutrition, group child" nutritionChildActivityFromMapping nutritionGroupChildLetters
        , letterTableTest "Nutrition, group mother" nutritionMotherActivityFromMapping nutritionGroupMotherLetters
        , letterTableTest "Nutrition, individual" nutritionChildActivityFromMapping nutritionIndividualLetters
        , letterTableTest "Prenatal" prenatalActivityFromMapping prenatalLetters
        , letterTableTest "Tuberculosis" tuberculosisActivityFromMapping tuberculosisLetters
        , letterTableTest "Well Child" wellChildActivityFromMapping wellChildLetters
        ]


{-| Decodes a sync response whose batch holds the given encounters under
one key, and nothing under the others.
-}
decodeBatch : String -> String -> Result String SyncResponse
decodeBatch key encounters =
    let
        keys =
            [ "acute_illness", "child_scoreboard", "hiv", "home_visit", "ncd", "nutrition_individual", "nutrition_group", "prenatal", "tuberculosis", "well_child" ]

        batch =
            List.map
                (\batchKey ->
                    if batchKey == key then
                        "\"" ++ batchKey ++ "\": [" ++ encounters ++ "]"

                    else
                        "\"" ++ batchKey ++ "\": []"
                )
                keys
                |> String.join ", "
    in
    ("{\"data\": {\"total_remaining\": 0, \"last\": 1, \"batch\": {" ++ batch ++ "}}}")
        |> decodeString decodeSyncResponse
        |> Result.mapError errorToString


{-| The expected and completed activities of HIV encounters with the
given completion strings.
-}
hivCompletion : String -> Result String (List ( List HIVActivity, List HIVActivity ))
hivCompletion completion =
    decodeBatch "hiv" ("{\"start_date\": \"2025-05-10\", \"completion\": \"" ++ completion ++ "\"}")
        |> Result.map (.hivData >> List.map (\encounter -> ( encounter.completion.expectedActivities, encounter.completion.completedActivities )))


takenByOf : String -> Result String (List (Maybe TakenBy))
takenByOf fields =
    decodeBatch "hiv" ("{\"start_date\": \"2025-05-10\", \"completion\": \"a\"" ++ fields ++ "}")
        |> Result.map (.hivData >> List.map .takenBy)


payloadsTest : Test
payloadsTest =
    describe "decodeSyncResponse reads the completion strings PHP sends"
        [ test "expected, then completed" <|
            \_ -> Expect.equal (Ok [ ( [ HIVDiagnostics, HIVFollowUp ], [ HIVDiagnostics ] ) ]) (hivCompletion "a,b|a")
        , test "expected only" <|
            \_ -> Expect.equal (Ok [ ( [ HIVDiagnostics, HIVFollowUp ], [] ) ]) (hivCompletion "a,b")
        , test "nothing expected" <|
            \_ -> Expect.equal (Ok [ ( [], [] ) ]) (hivCompletion "")
        , test "surrounding spaces are trimmed" <|
            \_ -> Expect.equal (Ok [ ( [ HIVDiagnostics ], [ HIVFollowUp ] ) ]) (hivCompletion " a|b ")
        , test "unknown letters are skipped" <|
            \_ -> Expect.equal (Ok [ ( [ HIVDiagnostics ], [] ) ]) (hivCompletion "a,zz|")
        , test "three parts fail the batch" <|
            \_ -> Expect.equal Nothing (Result.toMaybe (hivCompletion "a|b|c"))
        , test "the children of a group encounter are separated by $" <|
            \_ ->
                decodeBatch "nutrition_group" "{\"start_date\": \"2025-05-10\", \"taken_by\": \"chw\", \"mother\": \"a|a\", \"children\": \"a,d|a$a,d|d\"}"
                    |> Result.map
                        (.nutritionGroupData
                            >> List.map
                                (\encounter ->
                                    ( encounter.takenBy
                                    , encounter.motherData
                                    , List.map (\child -> ( child.expectedActivities, child.completedActivities )) encounter.childrenData
                                    )
                                )
                        )
                    |> Expect.equal
                        (Ok
                            [ ( Just TakenByCHW
                              , Just { expectedActivities = [ NutritionFamilyPlanning ], completedActivities = [ NutritionFamilyPlanning ] }
                              , [ ( [ NutritionHeight, NutritionWeight ], [ NutritionHeight ] ), ( [ NutritionHeight, NutritionWeight ], [ NutritionWeight ] ) ]
                              )
                            ]
                        )
        , test "a group encounter with no children string has no children" <|
            \_ ->
                decodeBatch "nutrition_group" "{\"start_date\": \"2025-05-10\", \"taken_by\": null, \"children\": null}"
                    |> Result.map (.nutritionGroupData >> List.map (\encounter -> ( encounter.takenBy, encounter.motherData, encounter.childrenData )))
                    |> Expect.equal (Ok [ ( Nothing, Nothing, [] ) ])
        , test "taken by nurse" <|
            \_ -> Expect.equal (Ok [ Just TakenByNurse ]) (takenByOf ", \"taken_by\": \"nurse\"")
        , test "an unknown taken by reads as unknown" <|
            \_ -> Expect.equal (Ok [ Just TakenByUnknown ]) (takenByOf ", \"taken_by\": \"midwife\"")
        , test "a missing or null taken by is none" <|
            \_ -> Expect.equal ( Ok [ Nothing ], Ok [ Nothing ] ) ( takenByOf "", takenByOf ", \"taken_by\": null" )
        , test "well-child encounter types" <|
            \_ ->
                [ "pediatric-care", "pediatric-care-chw", "newborn-exam" ]
                    |> List.map (\encounterType -> "{\"start_date\": \"2025-05-10\", \"encounter_type\": \"" ++ encounterType ++ "\", \"completion\": \"\"}")
                    |> String.join ", "
                    |> decodeBatch "well_child"
                    |> Result.map (.wellChildData >> List.map .encounterType)
                    |> Expect.equal (Ok [ PediatricCare, PediatricCareChw, NewbornExam ])
        , test "an unknown well-child encounter type fails the batch" <|
            \_ ->
                decodeBatch "well_child" "{\"start_date\": \"2025-05-10\", \"encounter_type\": \"school-visit\", \"completion\": \"\"}"
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


all : Test
all =
    describe "Backend.Completion"
        [ letterTablesTest
        , payloadsTest
        ]
