module Pages.Report.Test exposing (all)

import Backend.Measurement.Model
    exposing
        ( BloodSmearResult(..)
        , MalariaTestValue
        , TestExecutionNote(..)
        )
import Date
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Pages.Report.Utils exposing (generateBloodSmearTestResults)
import Test exposing (Test, describe, test)
import Time



-- Issue #632 sets the rule the pane follows: a blood smear "will only show up
-- on the progress report if it has been run".


dummyDate : NominalDate
dummyDate =
    Date.fromCalendarDate 2020 Time.Jun 1


{-| A malaria test where the rapid test was not performed and a blood smear was
taken instead, which is the only way a smear is recorded.
-}
smearValue : Maybe NominalDate -> BloodSmearResult -> MalariaTestValue
smearValue executionDate bloodSmearResult =
    { executionNote = TestNoteNotIndicated
    , executionDate = executionDate
    , testPrerequisites = Nothing
    , testResult = Nothing
    , bloodSmearResult = bloodSmearResult
    , bloodSmearOrdered = True
    }


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
        , -- Records made before the smear was given a date of its own carry
          -- none, and there is nothing to list them under.
          test "a smear with no date is not listed" <|
            \_ ->
                generateBloodSmearTestResults [ smearValue Nothing BloodSmearNegative ]
                    |> Expect.equal []
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
        [ bloodSmearTestResultsTest ]
