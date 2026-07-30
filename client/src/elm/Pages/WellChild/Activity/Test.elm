module Pages.WellChild.Activity.Test exposing (all)

import Backend.Measurement.Model
    exposing
        ( HeightInCm(..)
        , PregnancySummarySign(..)
        , PregnancySummaryValue
        , WeightInGrm(..)
        )
import Backend.Model exposing (emptyModelIndexedDb)
import Date
import EverySet
import Expect
import Measurement.Model exposing (RangedMeasurement(..), emptyHeightForm, emptyMuacForm, emptyWeightForm)
import Pages.WellChild.Activity.Model exposing (Msg(..), WarningPopupType(..), emptyModel, emptyPregnancySummaryForm)
import Pages.WellChild.Activity.Update exposing (update)
import Pages.WellChild.Activity.Utils
    exposing
        ( pregnancySummaryFormWithDefault
        , pregnancySummaryMeasurementsOutOfRange
        , resolveFirstEncounterDateAfterMilestone
        , resolveNextDateForECDVisit
        )
import Restful.Endpoint exposing (toEntityUuid)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)
import Time


all : Test
all =
    describe "Pages.WellChild.Activity"
        [ resolveFirstEncounterDateAfterMilestoneTests
        , resolveNextDateForECDVisitTests
        , pregnancySummaryFormWithDefaultTests
        , pregnancySummaryMeasurementsOutOfRangeTests
        , nutritionAssessmentGateTests
        ]


{-| What the Save action of a Nutrition Assessment task does with a measurement
that is outside the range it can take.

The measurement is named on a popup and nothing is saved. The task is left as it
is, so the measurement can be entered again.

-}
nutritionAssessmentGateTests : Test
nutritionAssessmentGateTests =
    let
        modelWith height muac weight =
            let
                data =
                    emptyModel.nutritionAssessmentData
            in
            { emptyModel
                | nutritionAssessmentData =
                    { data
                        | heightForm = { emptyHeightForm | height = height }
                        , muacForm = { emptyMuacForm | muac = muac }
                        , weightForm = { emptyWeightForm | weight = weight }
                    }
            }

        preSave model msg =
            let
                ( updatedModel, _, appMsgs ) =
                    update (Date.fromCalendarDate 2026 Time.Jul 27)
                        SiteRwanda
                        (toEntityUuid "encounter")
                        emptyModelIndexedDb
                        msg
                        model
            in
            ( updatedModel.warningPopupState, List.isEmpty appMsgs )

        person =
            toEntityUuid "person"
    in
    describe "the Nutrition Assessment save gate"
        [ test "a height outside the range names it and saves nothing" <|
            \_ ->
                preSave (modelWith (Just 1050) Nothing Nothing)
                    (PreSaveHeight EverySet.empty person Nothing Nothing)
                    |> Expect.equal ( Just (PopupMeasurementOutOfRange [ MeasurementHeight ]), True )
        , test "a weight outside the range names it and saves nothing" <|
            \_ ->
                preSave (modelWith Nothing Nothing (Just 850))
                    (PreSaveWeight EverySet.empty person Nothing Nothing)
                    |> Expect.equal ( Just (PopupMeasurementOutOfRange [ MeasurementWeight ]), True )
        , test "a MUAC outside the range names it and saves nothing" <|
            \_ ->
                preSave (modelWith Nothing (Just 125) Nothing)
                    (PreSaveMuac person Nothing Nothing)
                    |> Expect.equal ( Just (PopupMeasurementOutOfRange [ MeasurementMuac ]), True )
        , test "a height within the range shows no popup and goes on to save" <|
            \_ ->
                preSave (modelWith (Just 105) Nothing Nothing)
                    (PreSaveHeight EverySet.empty person Nothing Nothing)
                    |> Expect.equal ( Nothing, False )
        , test "a MUAC within the range shows no popup and goes on to save" <|
            \_ ->
                preSave (modelWith Nothing (Just 12.5) Nothing)
                    (PreSaveMuac person Nothing Nothing)
                    |> Expect.equal ( Nothing, False )
        ]


pregnancySummaryMeasurementsOutOfRangeTests : Test
pregnancySummaryMeasurementsOutOfRangeTests =
    let
        lengthForm available length =
            { emptyPregnancySummaryForm
                | birthLengthAvailable = available
                , birthLength = Maybe.map HeightInCm length
            }

        apgarForm available one five =
            { emptyPregnancySummaryForm
                | apgarScoresAvailable = available
                , apgarOneMin = one
                , apgarFiveMin = five
            }

        outOfRange =
            pregnancySummaryMeasurementsOutOfRange SiteRwanda
    in
    describe "pregnancySummaryMeasurementsOutOfRange"
        [ describe "the birth length"
            -- A newborn's length is around 50cm. The height range used elsewhere
            -- runs to 250cm, which is a grown adult, so birth length has its own.
            [ test "an ordinary birth length is in range" <|
                \_ ->
                    outOfRange (lengthForm (Just True) (Just 50))
                        |> Expect.equal []
            , test "a length entered in metres is out of range" <|
                \_ ->
                    outOfRange (lengthForm (Just True) (Just 0.5))
                        |> Expect.equal [ MeasurementBirthLength ]
            , test "a length entered in millimetres is out of range" <|
                \_ ->
                    outOfRange (lengthForm (Just True) (Just 500))
                        |> Expect.equal [ MeasurementBirthLength ]
            , test "the ends of the range are accepted" <|
                \_ ->
                    ( outOfRange (lengthForm (Just True) (Just 15))
                    , outOfRange (lengthForm (Just True) (Just 60))
                    )
                        |> Expect.equal ( [], [] )
            , test "just outside either end is refused" <|
                \_ ->
                    ( outOfRange (lengthForm (Just True) (Just 14))
                    , outOfRange (lengthForm (Just True) (Just 61))
                    )
                        |> Expect.equal ( [ MeasurementBirthLength ], [ MeasurementBirthLength ] )
            , test "a length that has not been entered is not reported" <|
                \_ ->
                    outOfRange (lengthForm (Just True) Nothing)
                        |> Expect.equal []
            , test "a length is not reported when the form does not ask for one" <|
                -- The input is hidden then, so stopping on it would leave the
                -- form with nothing on screen to correct.
                \_ ->
                    outOfRange (lengthForm (Just False) (Just 0.5))
                        |> Expect.equal []
            ]
        , describe "the Apgar scores"
            -- A score out of 10. A quarter of the scores already stored are not
            -- one: they run to 36, 246, and 30000.
            [ test "an ordinary pair of scores is in range" <|
                \_ ->
                    outOfRange (apgarForm (Just True) (Just 8) (Just 9))
                        |> Expect.equal []
            , test "the ends of the range are accepted" <|
                \_ ->
                    outOfRange (apgarForm (Just True) (Just 0) (Just 10))
                        |> Expect.equal []
            , test "a score above 10 is refused, and says which one" <|
                \_ ->
                    outOfRange (apgarForm (Just True) (Just 36) (Just 9))
                        |> Expect.equal [ MeasurementApgarOneMinute ]
            , test "the five minute score is named on its own" <|
                \_ ->
                    outOfRange (apgarForm (Just True) (Just 8) (Just 30000))
                        |> Expect.equal [ MeasurementApgarFiveMinutes ]
            , test "both are named when both are wrong" <|
                \_ ->
                    outOfRange (apgarForm (Just True) (Just 11) (Just 246))
                        |> Expect.equal [ MeasurementApgarOneMinute, MeasurementApgarFiveMinutes ]
            , test "a score below zero is refused" <|
                \_ ->
                    outOfRange (apgarForm (Just True) (Just -1) (Just 9))
                        |> Expect.equal [ MeasurementApgarOneMinute ]
            , test "scores that have not been entered are not reported" <|
                \_ ->
                    outOfRange (apgarForm (Just True) Nothing Nothing)
                        |> Expect.equal []
            , test "scores are not reported when the form does not ask for them" <|
                -- The inputs are hidden then, the same as the birth length.
                \_ ->
                    outOfRange (apgarForm (Just False) (Just 36) (Just 30000))
                        |> Expect.equal []
            ]
        , describe "the birth weight, which is asked for every time"
            [ test "a weight in grams is in range" <|
                \_ ->
                    outOfRange { emptyPregnancySummaryForm | birthWeight = Just (WeightInGrm 3200) }
                        |> Expect.equal []
            , test "a weight in kilograms is refused, though no question guards it" <|
                \_ ->
                    outOfRange { emptyPregnancySummaryForm | birthWeight = Just (WeightInGrm 3) }
                        |> Expect.equal [ MeasurementBirthWeight ]
            ]
        , test "every measurement that is wrong is named, in the order the form asks them" <|
            -- So that the nurse corrects them together rather than being sent
            -- back once for each.
            \_ ->
                outOfRange
                    { emptyPregnancySummaryForm
                        | birthWeight = Just (WeightInGrm 3)
                        , apgarScoresAvailable = Just True
                        , apgarOneMin = Just 36
                        , apgarFiveMin = Just 36
                        , birthLengthAvailable = Just True
                        , birthLength = Just (HeightInCm 0.5)
                    }
                    |> Expect.equal
                        [ MeasurementApgarOneMinute
                        , MeasurementApgarFiveMinutes
                        , MeasurementBirthWeight
                        , MeasurementBirthLength
                        ]
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


{-| The ECD sign group starting at 18 months means a child seen between 15 and
17 months should return at 18 months, not skip to 24 months.
-}
resolveNextDateForECDVisitTests : Test
resolveNextDateForECDVisitTests =
    let
        birthDate =
            Date.fromCalendarDate 2024 Time.Jan 1

        resolve current =
            resolveNextDateForECDVisit current birthDate True
    in
    describe "resolveNextDateForECDVisit"
        [ test "a 15-month-old is scheduled for the 18-month visit, not 24 months" <|
            \_ ->
                resolve (Date.fromCalendarDate 2025 Time.Apr 1)
                    |> Expect.equal (Just (Date.fromCalendarDate 2025 Time.Jul 1))
        , test "a 17-month-old is still scheduled for the 18-month visit" <|
            \_ ->
                resolve (Date.fromCalendarDate 2025 Time.Jun 1)
                    |> Expect.equal (Just (Date.fromCalendarDate 2025 Time.Jul 1))
        , test "an 18-month-old moves on to the 24-month visit" <|
            \_ ->
                resolve (Date.fromCalendarDate 2025 Time.Jul 1)
                    |> Expect.equal (Just (Date.fromCalendarDate 2026 Time.Jan 1))
        , test "the 15-month rung is unchanged for a younger child" <|
            \_ ->
                resolve (Date.fromCalendarDate 2024 Time.Nov 1)
                    |> Expect.equal (Just (Date.fromCalendarDate 2025 Time.Apr 1))
        , test "the years ladder is unchanged for a two-year-old" <|
            \_ ->
                resolve (Date.fromCalendarDate 2026 Time.Jan 1)
                    |> Expect.equal (Just (Date.fromCalendarDate 2027 Time.Jan 1))
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
