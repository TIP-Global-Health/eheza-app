module Pages.WellChild.Activity.Test exposing (all)

import Backend.Measurement.Model
    exposing
        ( HeightInCm(..)
        , PregnancySummarySign(..)
        , PregnancySummaryValue
        , WeightInGrm(..)
        )
import Date
import EverySet
import Expect
import Pages.WellChild.Activity.Model exposing (emptyPregnancySummaryForm)
import Pages.WellChild.Activity.Utils
    exposing
        ( pregnancySummaryFormWithDefault
        , resolveFirstEncounterDateAfterMilestone
        )
import Test exposing (Test, describe, test)
import Time


all : Test
all =
    describe "Pages.WellChild.Activity"
        [ resolveFirstEncounterDateAfterMilestoneTests
        , pregnancySummaryFormWithDefaultTests
        ]


resolveFirstEncounterDateAfterMilestoneTests : Test
resolveFirstEncounterDateAfterMilestoneTests =
    describe "resolveFirstEncounterDateAfterMilestone"
        [ test "picks the earliest encounter strictly AFTER the milestone, not an earlier one before it" <|
            \_ ->
                let
                    milestone =
                        Date.fromCalendarDate 2024 Time.Jul 1
                in
                resolveFirstEncounterDateAfterMilestone milestone
                    [ Date.fromCalendarDate 2024 Time.Apr 1
                    , Date.fromCalendarDate 2024 Time.Jun 20
                    , Date.fromCalendarDate 2024 Time.Jul 15
                    , Date.fromCalendarDate 2024 Time.Sep 1
                    ]
                    |> Expect.equal (Just (Date.fromCalendarDate 2024 Time.Jul 15))
        , test "returns Nothing when every encounter is on or before the milestone" <|
            \_ ->
                let
                    milestone =
                        Date.fromCalendarDate 2024 Time.Jul 1
                in
                resolveFirstEncounterDateAfterMilestone milestone
                    [ Date.fromCalendarDate 2024 Time.Apr 1
                    , Date.fromCalendarDate 2024 Time.Jun 20
                    , Date.fromCalendarDate 2024 Time.Jul 1
                    ]
                    |> Expect.equal Nothing
        , test "treats an encounter exactly on the milestone date as not after it" <|
            \_ ->
                resolveFirstEncounterDateAfterMilestone (Date.fromCalendarDate 2024 Time.Jul 1)
                    [ Date.fromCalendarDate 2024 Time.Jul 1 ]
                    |> Expect.equal Nothing
        ]


{-| A saved Pregnancy Summary with every numeric filled in, so the tests can
check what happens when the nurse clears one and re-opens the encounter.
-}
savedPregnancySummary : PregnancySummaryValue
savedPregnancySummary =
    { expectedDateConcluded = Date.fromCalendarDate 2024 Time.Jan 1
    , deliveryComplications = EverySet.empty
    , signs = EverySet.fromList [ ApgarScores, BirthLength ]
    , apgarOneMin = Just 8
    , apgarFiveMin = Just 9
    , birthWeight = Just (WeightInGrm 3000)
    , birthLength = Just (HeightInCm 50)
    , birthDefects = EverySet.empty
    }


{-| Clearing a value (for example answering "Apgar scores available? No", which
empties the scores and marks them dirty) must stay cleared when the form is
rebuilt from the saved value. An untouched field still loads from the saved
value.
-}
pregnancySummaryFormWithDefaultTests : Test
pregnancySummaryFormWithDefaultTests =
    let
        resolve form =
            pregnancySummaryFormWithDefault form (Just savedPregnancySummary)
    in
    describe "pregnancySummaryFormWithDefault"
        [ test "cleared Apgar scores stay cleared when the field is dirty" <|
            \_ ->
                let
                    resolved =
                        resolve { emptyPregnancySummaryForm | apgarDirty = True }
                in
                ( resolved.apgarOneMin, resolved.apgarFiveMin )
                    |> Expect.equal ( Nothing, Nothing )
        , test "untouched Apgar scores load from the saved value" <|
            \_ ->
                let
                    resolved =
                        resolve emptyPregnancySummaryForm
                in
                ( resolved.apgarOneMin, resolved.apgarFiveMin )
                    |> Expect.equal ( Just 8, Just 9 )
        , test "an edited Apgar score wins over the saved value" <|
            \_ ->
                resolve { emptyPregnancySummaryForm | apgarDirty = True, apgarOneMin = Just 5 }
                    |> .apgarOneMin
                    |> Expect.equal (Just 5)
        , test "a cleared birth weight stays cleared when the field is dirty" <|
            \_ ->
                resolve { emptyPregnancySummaryForm | birthWeightDirty = True }
                    |> .birthWeight
                    |> Expect.equal Nothing
        , test "an untouched birth weight loads from the saved value" <|
            \_ ->
                resolve emptyPregnancySummaryForm
                    |> .birthWeight
                    |> Expect.equal (Just (WeightInGrm 3000))
        , test "a cleared birth length stays cleared when the field is dirty" <|
            \_ ->
                resolve { emptyPregnancySummaryForm | birthLengthDirty = True }
                    |> .birthLength
                    |> Expect.equal Nothing
        ]
