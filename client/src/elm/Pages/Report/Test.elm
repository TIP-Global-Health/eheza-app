module Pages.Report.Test exposing (all)

import Backend.Measurement.Model
    exposing
        ( BloodSmearResult(..)
        , MalariaTestValue
        , TestExecutionNote(..)
        , TestResult(..)
        )
import Date
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Pages.Report.Utils exposing (generateBloodSmearTestResults, malariaRapidTestValues)
import Test exposing (Test, describe, test)
import Time



-- Issue #632 sets the rule the pane follows: a blood smear "will only show up
-- on the progress report if it has been run".


dummyDate : NominalDate
dummyDate =
    Date.fromCalendarDate 2020 Time.Jun 1


{-| The date a measurement was recorded, which stands in for a smear that
carries no execution date of its own. Deliberately not `dummyDate`, so a test
cannot pass by confusing the two.
-}
dateMeasured : NominalDate
dateMeasured =
    Date.fromCalendarDate 2020 Time.Jul 15


{-| A malaria test where the rapid test was not performed and a blood smear was
taken instead, which is the only way a smear is recorded.
-}
smearValue : Maybe NominalDate -> BloodSmearResult -> ( NominalDate, MalariaTestValue )
smearValue executionDate bloodSmearResult =
    ( dateMeasured
    , { executionNote = TestNoteNotIndicated
      , executionDate = executionDate
      , testPrerequisites = Nothing
      , testResult = Nothing
      , bloodSmearResult = bloodSmearResult
      , bloodSmearOrdered = True
      }
    )


{-| A malaria test that was run as a rapid test, which is the only way a rapid
test result is recorded.
-}
rapidTestValue : TestResult -> ( NominalDate, MalariaTestValue )
rapidTestValue testResult =
    ( dateMeasured
    , { executionNote = TestNoteRunToday
      , executionDate = Just dummyDate
      , testPrerequisites = Nothing
      , testResult = Just testResult
      , bloodSmearResult = BloodSmearNotTaken
      , bloodSmearOrdered = False
      }
    )


malariaRapidTestValuesTest : Test
malariaRapidTestValuesTest =
    describe "malariaRapidTestValues"
        [ test "a rapid test is kept" <|
            \_ ->
                malariaRapidTestValues [ rapidTestValue TestNegative ]
                    |> Expect.equal [ rapidTestValue TestNegative ]
        , -- Once the lab confirms the run, a smear carries an execution note
          -- that reads as performed, and a date. Left in, it shows on the rapid
          -- test's history as an entry with no result.
          test "a blood smear is not a rapid test" <|
            \_ ->
                malariaRapidTestValues [ smearValue (Just dummyDate) BloodSmearNegative ]
                    |> Expect.equal []
        , -- Records made before `bloodSmearOrdered` existed decode as not
          -- ordered, so the result is what says the record is a smear.
          test "a smear from before the ordered flag is not a rapid test" <|
            \_ ->
                malariaRapidTestValues
                    [ ( dateMeasured
                      , { executionNote = TestNoteNotIndicated
                        , executionDate = Nothing
                        , testPrerequisites = Nothing
                        , testResult = Nothing
                        , bloodSmearResult = BloodSmearNegative
                        , bloodSmearOrdered = False
                        }
                      )
                    ]
                    |> Expect.equal []
        , test "a smear is dropped from among rapid tests" <|
            \_ ->
                malariaRapidTestValues
                    [ rapidTestValue TestNegative
                    , smearValue (Just dummyDate) BloodSmearPlus
                    ]
                    |> Expect.equal [ rapidTestValue TestNegative ]
        ]


bloodSmearTestResultsTest : Test
bloodSmearTestResultsTest =
    describe "generateBloodSmearTestResults"
        [ test "a smear that was read is listed" <|
            \_ ->
                generateBloodSmearTestResults [ smearValue (Just dummyDate) BloodSmearNegative ]
                    |> Expect.equal [ ( dummyDate, Just BloodSmearNegative ) ]
        , test "a positive smear is listed" <|
            \_ ->
                generateBloodSmearTestResults [ smearValue (Just dummyDate) BloodSmearPlus ]
                    |> Expect.equal [ ( dummyDate, Just BloodSmearPlus ) ]
        , test "a smear still awaiting its result is not listed" <|
            \_ ->
                generateBloodSmearTestResults [ smearValue (Just dummyDate) BloodSmearPendingInput ]
                    |> Expect.equal []
        , test "a test with no smear is not listed" <|
            \_ ->
                generateBloodSmearTestResults [ smearValue (Just dummyDate) BloodSmearNotTaken ]
                    |> Expect.equal []
        , -- A smear the lab read carries no execution date, and every record
          -- made before the smear was given a date of its own carries none.
          test "a smear with no date of its own is listed under the date it was recorded" <|
            \_ ->
                generateBloodSmearTestResults [ smearValue Nothing BloodSmearNegative ]
                    |> Expect.equal [ ( dateMeasured, Just BloodSmearNegative ) ]
        , test "smears are listed most recent first" <|
            \_ ->
                generateBloodSmearTestResults
                    [ smearValue (Just <| Date.add Date.Months -2 dummyDate) BloodSmearNegative
                    , smearValue (Just dummyDate) BloodSmearPlusPlus
                    ]
                    |> Expect.equal
                        [ ( dummyDate, Just BloodSmearPlusPlus )
                        , ( Date.add Date.Months -2 dummyDate, Just BloodSmearNegative )
                        ]
        ]


all : Test
all =
    describe "Progress report lab results"
        [ bloodSmearTestResultsTest
        , malariaRapidTestValuesTest
        ]
