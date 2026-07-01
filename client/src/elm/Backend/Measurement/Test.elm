module Backend.Measurement.Test exposing (all)

import Backend.Measurement.Encoder exposing (encodeMedicalHistoryValue)
import Backend.Measurement.Model exposing (ColorAlertIndication(..), GlucoseValue(..), HeadCircumferenceInCm(..), MuacInCm(..), OccursInFamilySign(..), RandomBloodSugarTestValue, TestExecutionNote(..), TestPrerequisite(..), UrineDipstickTestValue)
import Backend.Measurement.Utils exposing (diabetesBySugarCount, diabetesByUrineGlucose, headCircumferenceIndication, muacIndicationForAdult, muacIndicationForChild)
import EverySet
import Expect
import Json.Decode
import Json.Encode
import Test exposing (Test, describe, test)


muacIndicationTest : Test
muacIndicationTest =
    describe "MuacIndication tests"
        [ describe "muacIndicationForChild"
            [ test "red" <|
                \_ ->
                    muacIndicationForChild (MuacInCm 11.5)
                        |> Expect.equal ColorAlertRed
            , test "yellow1" <|
                \_ ->
                    muacIndicationForChild (MuacInCm 11.6)
                        |> Expect.equal ColorAlertYellow
            , test "yellow2" <|
                \_ ->
                    muacIndicationForChild (MuacInCm 12.5)
                        |> Expect.equal ColorAlertYellow
            , test "green" <|
                \_ ->
                    muacIndicationForChild (MuacInCm 12.6)
                        |> Expect.equal ColorAlertGreen
            ]
        , describe "muacIndicationForAdult"
            [ test "red" <|
                \_ ->
                    muacIndicationForAdult (MuacInCm 18.4)
                        |> Expect.equal ColorAlertRed
            , test "yellow boundary" <|
                \_ ->
                    muacIndicationForAdult (MuacInCm 18.5)
                        |> Expect.equal ColorAlertYellow
            , test "yellow" <|
                \_ ->
                    muacIndicationForAdult (MuacInCm 21.9)
                        |> Expect.equal ColorAlertYellow
            , test "green" <|
                \_ ->
                    muacIndicationForAdult (MuacInCm 22)
                        |> Expect.equal ColorAlertGreen
            ]
        ]


headCircumferenceIndicationTest : Test
headCircumferenceIndicationTest =
    -- WHO: a head-circumference z-score outside +/-3 SD is a red flag.
    -- Note: despite the `HeadCircumferenceInCm` constructor name, the value
    -- compared by `headCircumferenceIndication` is a z-score (SD) -- which is
    -- why these inputs are small and can be negative, not centimeters.
    describe "headCircumferenceIndication"
        [ test "-3.5 -> red" <|
            \_ ->
                headCircumferenceIndication (HeadCircumferenceInCm -3.5)
                    |> Expect.equal ColorAlertRed
        , test "-3.0 boundary -> red" <|
            \_ ->
                headCircumferenceIndication (HeadCircumferenceInCm -3.0)
                    |> Expect.equal ColorAlertRed
        , test "0.0 -> green" <|
            \_ ->
                headCircumferenceIndication (HeadCircumferenceInCm 0.0)
                    |> Expect.equal ColorAlertGreen
        , test "2.9 -> green" <|
            \_ ->
                headCircumferenceIndication (HeadCircumferenceInCm 2.9)
                    |> Expect.equal ColorAlertGreen
        , test "3.0 boundary -> red" <|
            \_ ->
                headCircumferenceIndication (HeadCircumferenceInCm 3.0)
                    |> Expect.equal ColorAlertRed
        , test "3.5 -> red" <|
            \_ ->
                headCircumferenceIndication (HeadCircumferenceInCm 3.5)
                    |> Expect.equal ColorAlertRed
        ]


{-| A Random Blood Sugar test value with the given fasting flag and sugar count;
all other fields are defaulted (the classifier reads only these two).
-}
rbsValue : Bool -> Float -> RandomBloodSugarTestValue ()
rbsValue fasting sugar =
    { executionNote = TestNoteRunToday
    , executionDate = Nothing
    , testPrerequisites =
        Just
            (if fasting then
                EverySet.singleton PrerequisiteFastFor12h

             else
                -- Match the app's convention of an explicit NoTestPrerequisites
                -- rather than an empty set for the non-fasting case.
                EverySet.singleton NoTestPrerequisites
            )
    , sugarCount = Just sugar
    , originatingEncounter = Nothing
    }


{-| A Urine Dipstick test value with the given glucose reading; all other fields
are defaulted (the classifier reads only `.glucose`).
-}
urineValue : GlucoseValue -> UrineDipstickTestValue
urineValue glucose =
    { testVariant = Nothing
    , executionNote = TestNoteRunToday
    , executionDate = Nothing
    , testPrerequisites = Nothing
    , protein = Nothing
    , ph = Nothing
    , glucose = Just glucose
    , leukocytes = Nothing
    , nitrite = Nothing
    , urobilinogen = Nothing
    , haemoglobin = Nothing
    , ketone = Nothing
    , bilirubin = Nothing
    }


diabetesBySugarCountTest : Test
diabetesBySugarCountTest =
    -- Clinical (ADA) oracle: fasting plasma glucose >= 126 mg/dL is diabetes;
    -- random/non-fasting >= 200. The code uses fasting STRICTLY > 126, so the
    -- "fasting 126 -> not diabetes" case below is NOT the oracle value -- it
    -- pins the current code discrepancy against the ADA >= 126 cutoff.
    -- FINDING: the Labs tab has the fasting/non-fasting thresholds SWAPPED
    -- (it lists non-fasting normal up to 126 and fasting up to 200). The code
    -- matches the clinical convention; the last two cases pin that.
    describe "diabetesBySugarCount"
        [ test "fasting 130 -> diabetes" <|
            \_ -> diabetesBySugarCount (rbsValue True 130) |> Expect.equal True
        , test "fasting 100 -> not diabetes" <|
            \_ -> diabetesBySugarCount (rbsValue True 100) |> Expect.equal False
        , test "fasting 127 -> diabetes (just over 126)" <|
            \_ -> diabetesBySugarCount (rbsValue True 127) |> Expect.equal True
        , test "fasting 126 -> not diabetes [code uses >126; ADA is >=126]" <|
            \_ -> diabetesBySugarCount (rbsValue True 126) |> Expect.equal False
        , test "non-fasting 200 -> diabetes (boundary)" <|
            \_ -> diabetesBySugarCount (rbsValue False 200) |> Expect.equal True
        , test "non-fasting 199 -> not diabetes" <|
            \_ -> diabetesBySugarCount (rbsValue False 199) |> Expect.equal False
        , test "fasting 150 -> diabetes [Labs tab's >200 fasting would wrongly miss this]" <|
            \_ -> diabetesBySugarCount (rbsValue True 150) |> Expect.equal True
        , test "non-fasting 150 -> not diabetes [Labs tab's >126 non-fasting would wrongly flag this]" <|
            \_ -> diabetesBySugarCount (rbsValue False 150) |> Expect.equal False
        ]


diabetesByUrineGlucoseTest : Test
diabetesByUrineGlucoseTest =
    -- Oracle: glucosuria of +2 or more indicates diabetes.
    describe "diabetesByUrineGlucose"
        [ test "Glucose0 -> not diabetes" <|
            \_ -> diabetesByUrineGlucose (urineValue Glucose0) |> Expect.equal False
        , test "GlucosePlus1 -> not diabetes" <|
            \_ -> diabetesByUrineGlucose (urineValue GlucosePlus1) |> Expect.equal False
        , test "GlucosePlus2 -> diabetes (threshold)" <|
            \_ -> diabetesByUrineGlucose (urineValue GlucosePlus2) |> Expect.equal True
        , test "GlucosePlus3 -> diabetes" <|
            \_ -> diabetesByUrineGlucose (urineValue GlucosePlus3) |> Expect.equal True
        , test "GlucosePlus4 -> diabetes" <|
            \_ -> diabetesByUrineGlucose (urineValue GlucosePlus4) |> Expect.equal True
        ]


preeclampsiaInFamilyEncodeTest : Test
preeclampsiaInFamilyEncodeTest =
    -- Regression: preeclampsia_in_family must be encoded (with the string the
    -- decoder expects), else the nurse's "family history of preeclampsia"
    -- answer is silently dropped on every save.
    let
        encodedSign sign =
            encodeMedicalHistoryValue
                { signs = EverySet.empty
                , physicalConditions = EverySet.empty
                , infectiousDiseases = EverySet.empty
                , mentalHealthIssues = EverySet.empty
                , preeclampsiaInFamily = sign
                }
                |> Json.Encode.object
                |> Json.Decode.decodeValue (Json.Decode.field "preeclampsia_in_family" Json.Decode.string)
    in
    describe "encodeMedicalHistoryValue emits preeclampsia_in_family"
        [ test "DoesOccur -> \"yes\"" <|
            \_ -> encodedSign DoesOccur |> Expect.equal (Ok "yes")
        , test "DoesNotOccur -> \"no\"" <|
            \_ -> encodedSign DoesNotOccur |> Expect.equal (Ok "no")
        , test "NotKnownIfOccurs -> \"do-not-know\"" <|
            \_ -> encodedSign NotKnownIfOccurs |> Expect.equal (Ok "do-not-know")
        ]


all : Test
all =
    describe "Measurement data tests"
        [ muacIndicationTest
        , headCircumferenceIndicationTest
        , diabetesBySugarCountTest
        , diabetesByUrineGlucoseTest
        , preeclampsiaInFamilyEncodeTest
        ]
