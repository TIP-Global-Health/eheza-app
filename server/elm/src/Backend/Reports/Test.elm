module Backend.Reports.Test exposing (all)

import AssocList as Dict
import Backend.Reports.Decoder
    exposing
        ( acuteIllnessDiagnosisFromMapping
        , decodeSyncResponse
        , prenatalDiagnosisFromMapping
        , prenatalIndicatorFromMapping
        , vaccineTypeFromMapping
        )
import Backend.Reports.Model
    exposing
        ( AcuteIllnessDiagnosis(..)
        , AcuteIllnessEncounterType(..)
        , Gender(..)
        , PatientData
        , PrenatalDiagnosis(..)
        , PrenatalEncounterType(..)
        , PrenatalIndicator(..)
        )
import Backend.Scoreboard.Model exposing (VaccineType(..))
import Date
import EverySet
import Expect
import Json.Decode exposing (decodeString, errorToString)
import Test exposing (Test, describe, test)
import Time exposing (Month(..))


{-| Written from `hedley_reports_ai_diagnosis_mapping()`.
-}
acuteIllnessDiagnosisLetters : List ( String, AcuteIllnessDiagnosis )
acuteIllnessDiagnosisLetters =
    [ ( "a", DiagnosisCovid19Suspect )
    , ( "b", DiagnosisSevereCovid19 )
    , ( "c", DiagnosisPneuminialCovid19 )
    , ( "d", DiagnosisLowRiskCovid19 )
    , ( "e", DiagnosisMalariaComplicated )
    , ( "f", DiagnosisMalariaUncomplicated )
    , ( "g", DiagnosisMalariaUncomplicatedAndPregnant )
    , ( "h", DiagnosisGastrointestinalInfectionComplicated )
    , ( "i", DiagnosisGastrointestinalInfectionUncomplicated )
    , ( "j", DiagnosisRespiratoryInfectionComplicated )
    , ( "k", DiagnosisRespiratoryInfectionUncomplicated )
    , ( "l", DiagnosisSimpleColdAndCough )
    , ( "m", DiagnosisFeverOfUnknownOrigin )
    , ( "n", DiagnosisUndeterminedMoreEvaluationNeeded )
    , ( "o", DiagnosisTuberculosisSuspect )
    ]


{-| Written from `hedley_reports_prenatal_diagnosis_mapping()`.
-}
prenatalDiagnosisLetters : List ( String, PrenatalDiagnosis )
prenatalDiagnosisLetters =
    [ ( "a", DiagnosisChronicHypertension )
    , ( "b", DiagnosisGestationalHypertension )
    , ( "c", DiagnosisModeratePreeclampsia )
    , ( "d", DiagnosisSeverePreeclampsia )
    , ( "g", DiagnosisEclampsia )
    , ( "h", DiagnosisHIV )
    , ( "i", DiagnosisHIVDetectableViralLoad )
    , ( "j", DiagnosisDiscordantPartnership )
    , ( "k", DiagnosisSyphilis )
    , ( "l", DiagnosisSyphilisWithComplications )
    , ( "m", DiagnosisNeurosyphilis )
    , ( "n", DiagnosisHepatitisB )
    , ( "o", DiagnosisMalaria )
    , ( "p", DiagnosisMalariaWithAnemia )
    , ( "q", DiagnosisMalariaWithSevereAnemia )
    , ( "r", DiagnosisModerateAnemia )
    , ( "s", DiagnosisSevereAnemia )
    , ( "t", DiagnosisSevereAnemiaWithComplications )
    , ( "u", DiagnosisMiscarriage )
    , ( "v", DiagnosisMolarPregnancy )
    , ( "w", DiagnosisPlacentaPrevia )
    , ( "x", DiagnosisPlacentalAbruption )
    , ( "y", DiagnosisUterineRupture )
    , ( "z", DiagnosisObstructedLabor )
    , ( "a1", DiagnosisPostAbortionSepsis )
    , ( "b1", DiagnosisEctopicPregnancy )
    , ( "c1", DiagnosisPROM )
    , ( "d1", DiagnosisPPROM )
    , ( "e1", DiagnosisHyperemesisGravidum )
    , ( "f1", DiagnosisSevereVomiting )
    , ( "g1", DiagnosisMaternalComplications )
    , ( "h1", DiagnosisInfection )
    , ( "i1", DiagnosisImminentDelivery )
    , ( "j1", DiagnosisLaborAndDelivery )
    , ( "k1", DiagnosisHeartburn )
    , ( "l1", DiagnosisDeepVeinThrombosis )
    , ( "m1", DiagnosisPelvicPainIntense )
    , ( "n1", DiagnosisUrinaryTractInfection )
    , ( "o1", DiagnosisPyelonephritis )
    , ( "p1", DiagnosisCandidiasis )
    , ( "q1", DiagnosisGonorrhea )
    , ( "r1", DiagnosisTrichomonasOrBacterialVaginosis )
    , ( "s1", DiagnosisTuberculosis )
    , ( "t1", DiagnosisDiabetes )
    , ( "u1", DiagnosisGestationalDiabetes )
    , ( "v1", DiagnosisRhesusNegative )
    , ( "w1", DiagnosisDepressionNotLikely )
    , ( "x1", DiagnosisDepressionPossible )
    , ( "y1", DiagnosisDepressionHighlyPossible )
    , ( "z1", DiagnosisDepressionProbable )
    , ( "a2", DiagnosisSuicideRisk )
    , ( "b2", DiagnosisOther )
    , ( "c2", DiagnosisPostpartumAbdominalPain )
    , ( "d2", DiagnosisPostpartumUrinaryIncontinence )
    , ( "e2", DiagnosisPostpartumHeadache )
    , ( "f2", DiagnosisPostpartumFatigue )
    , ( "g2", DiagnosisPostpartumFever )
    , ( "h2", DiagnosisPostpartumPerinealPainOrDischarge )
    , ( "i2", DiagnosisPostpartumInfection )
    , ( "j2", DiagnosisPostpartumExcessiveBleeding )
    , ( "k2", DiagnosisPostpartumEarlyMastitisOrEngorgment )
    , ( "l2", DiagnosisPostpartumMastitis )
    , ( "m2", DiagnosisHighRiskOfPreeclampsia )
    , ( "n2", DiagnosisModerateRiskOfPreeclampsia )
    ]


{-| Written from `hedley_reports_prenatal_indicator_mapping()`.
-}
prenatalIndicatorLetters : List ( String, PrenatalIndicator )
prenatalIndicatorLetters =
    [ ( "a", IndicatorAdequateGWG )
    , ( "b", IndicatorReceivedMMS )
    , ( "c", IndicatorReferredToUltrasound )
    , ( "d", IndicatorReceivedAspirin )
    , ( "e", IndicatorReceivedCalcium )
    , ( "f", IndicatorPretermBirth )
    , ( "g", IndicatorAbortion )
    , ( "h", IndicatorIntrauterineDeath )
    , ( "i", IndicatorStillbirth )
    , ( "j", IndicatorReceivedAzithromycin )
    , ( "k", IndicatorAnemiaTest )
    , ( "l", IndicatorBreastfedFirstHour )
    , ( "m", IndicatorPrematureOnsetContractions )
    ]


{-| Written from `hedley_reports_vaccine_type_mapping()`.
-}
vaccineTypeLetters : List ( String, VaccineType )
vaccineTypeLetters =
    [ ( "a", VaccineBCG )
    , ( "b", VaccineOPV )
    , ( "c", VaccineDTP )
    , ( "d", VaccineDTPStandalone )
    , ( "e", VaccinePCV13 )
    , ( "f", VaccineRotarix )
    , ( "g", VaccineIPV )
    , ( "h", VaccineMR )
    , ( "i", VaccineHPV )
    ]


letterTableTest : String -> (String -> Maybe a) -> List ( String, a ) -> Test
letterTableTest name fromMapping letters =
    test name <|
        \_ ->
            List.map (Tuple.first >> fromMapping) letters
                |> Expect.equal (List.map (Tuple.second >> Just) letters)


letterTablesTest : Test
letterTablesTest =
    describe "each letter PHP sends reads as the value PHP meant"
        [ letterTableTest "Acute Illness diagnoses" acuteIllnessDiagnosisFromMapping acuteIllnessDiagnosisLetters
        , letterTableTest "prenatal diagnoses" prenatalDiagnosisFromMapping prenatalDiagnosisLetters
        , letterTableTest "prenatal indicators" prenatalIndicatorFromMapping prenatalIndicatorLetters
        , letterTableTest "vaccine types" vaccineTypeFromMapping vaccineTypeLetters
        , test "the Acute Illness 'none' letter reads as no diagnosis" <|
            \_ -> Expect.equal Nothing (acuteIllnessDiagnosisFromMapping "z")
        ]


{-| Decodes one patient, given the JSON fields that follow the
required id, created, birth\_date and gender.
-}
decodePatient : String -> String -> Result String PatientData
decodePatient gender fields =
    ("{\"data\": {\"total_remaining\": 0, \"last\": 1, \"batch\": [{\"id\": 1, \"created\": \"2024-01-01\", \"birth_date\": \"2023-03-01\", \"gender\": \"" ++ gender ++ "\"" ++ fields ++ "}]}}")
        |> decodeString decodeSyncResponse
        |> Result.mapError errorToString
        |> Result.andThen (.records >> List.head >> Result.fromMaybe "no record")


{-| Decodes the given CHW group visits of one patient.
-}
decodeChwVisits : String -> Result String PatientData
decodeChwVisits visits =
    decodePatient "male" (""", "group_nutrition": {"chw": [""" ++ visits ++ "]}")


payloadsTest : Test
payloadsTest =
    describe "decodeSyncResponse reads the visit strings PHP sends"
        [ test "the anthropometry payload: stunting, underweight, wasting, MUAC, edema" <|
            \_ ->
                decodeChwVisits "\"2025-05-10 -2.5,-1,-3.5,13,1\""
                    |> Result.map (.groupNutritionChwData >> Maybe.map (List.map (\visit -> ( visit.nutritionData, visit.muacCm, visit.hasEdema ))))
                    |> Expect.equal (Ok (Just [ ( Just { stunting = Just -2.5, underweight = Just -1, wasting = Just -3.5 }, Just 13, True ) ]))
        , test "an edema token other than 1 is no edema" <|
            \_ ->
                decodeChwVisits "\"2025-05-10 ,,,13,0\""
                    |> Result.map (.groupNutritionChwData >> Maybe.map (List.map .hasEdema))
                    |> Expect.equal (Ok (Just [ False ]))
        , test "an FBF amount with empty anthropometry" <|
            \_ ->
                decodeChwVisits "\"2025-05-10 ,,,,|2.5\""
                    |> Result.map (.groupNutritionChwData >> Maybe.map (List.map (\visit -> ( visit.nutritionData, visit.muacCm, visit.fbfAmount ))))
                    |> Expect.equal (Ok (Just [ ( Just { stunting = Nothing, underweight = Nothing, wasting = Nothing }, Nothing, Just 2.5 ) ]))
        , test "the legacy three-score payload has no MUAC and no edema" <|
            \_ ->
                decodeChwVisits "\"2025-05-10 -1,-2,-3\""
                    |> Result.map (.groupNutritionChwData >> Maybe.map (List.map (\visit -> ( visit.nutritionData, visit.muacCm, visit.hasEdema ))))
                    |> Expect.equal (Ok (Just [ ( Just { stunting = Just -1, underweight = Just -2, wasting = Just -3 }, Nothing, False ) ]))
        , test "a date-only visit has no measurements" <|
            \_ ->
                decodeChwVisits "\"2025-05-10\""
                    |> Result.map (.groupNutritionChwData >> Maybe.map (List.map (\visit -> ( visit.nutritionData, visit.muacCm ))))
                    |> Expect.equal (Ok (Just [ ( Nothing, Nothing ) ]))
        , test "an AHEZA amount with no MUAC" <|
            \_ ->
                decodePatient "male" """, "individual": {"family-nutrition-muac": [["2025-05-10 |500", "2025-06-10 12.1"]]}"""
                    |> Result.map (.familyNutritionMuacData >> Maybe.map (List.concat >> List.map (\visit -> ( visit.muacCm, visit.ahezaAmount ))))
                    |> Expect.equal (Ok (Just [ ( Nothing, Just 500 ), ( Just 12.1, Nothing ) ]))
        , test "the well-child immunisation segment: letters, then dates joined by +" <|
            \_ ->
                decodePatient "male" """, "individual": {"well-child": [["2025-05-10|-1,,,,|a:2025-05-10,b:2025-05-10+2025-06-10,x:2025-05-10,c:not-a-date"]]}"""
                    |> Result.map (.wellChildData >> Maybe.map (List.concat >> List.map (.immunisationData >> Maybe.map (Dict.toList >> List.map (Tuple.mapSecond EverySet.toList) >> List.sortBy (Tuple.second >> List.length)))))
                    |> Expect.equal
                        (Ok
                            (Just
                                [ Just
                                    [ ( VaccineBCG, [ Date.fromCalendarDate 2025 May 10 ] )
                                    , ( VaccineOPV, [ Date.fromCalendarDate 2025 Jun 10, Date.fromCalendarDate 2025 May 10 ] )
                                    ]
                                ]
                            )
                        )
        , test "a well-child visit with no immunisation has none" <|
            \_ ->
                decodePatient "male" """, "individual": {"well-child": [["2025-05-10|-1,,,,|"]]}"""
                    |> Result.map (.wellChildData >> Maybe.map (List.concat >> List.map .immunisationData))
                    |> Expect.equal (Ok (Just [ Nothing ]))
        , test "prenatal visits: empty diagnosis and indicator chunks, unknown letters skipped" <|
            \_ ->
                decodePatient "female" """, "individual": {"antenatal": [{"created": "2025-01-01", "encounters": ["2025-02-01|nurse||", "2025-03-01|chw-2|a,zz,b1|a"]}]}"""
                    |> Result.map (.prenatalData >> Maybe.map (List.concatMap .encounters >> List.map (\visit -> ( visit.encounterType, visit.diagnoses, visit.indicators ))))
                    |> Expect.equal
                        (Ok
                            (Just
                                [ ( NurseEncounter, [], [] )
                                , ( ChwSecondEncounter, [ DiagnosisChronicHypertension, DiagnosisEctopicPregnancy ], [ IndicatorAdequateGWG ] )
                                ]
                            )
                        )
        , test "unknown encounter types and gender fall back" <|
            \_ ->
                decodePatient "other" """, "individual": {"antenatal": [{"created": "2025-01-01", "encounters": ["2025-02-01|midwife||"]}], "acute-illness": [["2025-02-01|pharmacy-encounter|f"]]}"""
                    |> Result.map
                        (\patient ->
                            ( patient.gender
                            , Maybe.map (List.concatMap .encounters >> List.map .encounterType) patient.prenatalData
                            , Maybe.map (List.concat >> List.map (\visit -> ( visit.encounterType, visit.diagnosis ))) patient.acuteIllnessData
                            )
                        )
                    |> Expect.equal (Ok ( Female, Just [ NurseEncounter ], Just [ ( AcuteIllnessEncounterCHW, Just DiagnosisMalariaUncomplicated ) ] ))
        , test "a malformed visit date fails the whole batch" <|
            \_ ->
                decodeChwVisits "\"2025-05-10 -1,,,,\", \"2025-13-40 -1,,,,\""
                    |> Result.toMaybe
                    |> Expect.equal Nothing
        ]


all : Test
all =
    describe "Backend.Reports"
        [ letterTablesTest
        , payloadsTest
        ]
