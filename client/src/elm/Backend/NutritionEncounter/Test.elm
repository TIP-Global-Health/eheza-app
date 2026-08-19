module Backend.NutritionEncounter.Test exposing (all)

import AssocList as Dict
import Backend.Entities exposing (PersonId)
import Backend.Measurement.Model
    exposing
        ( ChildNutritionSign(..)
        , MuacInCm(..)
        , NutritionAssessment(..)
        )
import Backend.Model exposing (ModelIndexedDb, emptyModelIndexedDb)
import Backend.NutritionEncounter.Utils
    exposing
        ( generateNutritionAssessment
        , resolveLatestValue
        , zScoreWeightForAgeModerate
        , zScoreWeightForAgeSevere
        )
import Backend.Person.Model exposing (Person)
import Date
import EverySet exposing (EverySet)
import Expect
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import TestFixtures exposing (testChild)
import Time
import ZScore.Model



-- FIXTURES


{-| The reference "current date" used across the tests.
-}
currentDate : NominalDate
currentDate =
    Date.fromCalendarDate 2020 Time.Jun 1


{-| The shared child fixture, 3 months old by default; `birthDate` is varied
per test via `childAgedMonths`. `gender` does not affect the paths under test
(the weight-for-age path is kept inert by passing `Nothing` for weight and an
empty `ZScore.Model`).
-}
baseChild : Person
baseChild =
    testChild (Date.add Date.Months -3 currentDate)


{-| A child whose birth date places it `months` months before `currentDate`.
-}
childAgedMonths : Int -> Person
childAgedMonths months =
    { baseChild | birthDate = Just (Date.add Date.Months -months currentDate) }



-- PART 1 — PURE UNDERWEIGHT CLASSIFIERS
--
-- Oracle = WHO weight-for-age z-score:
--   severe   z <= -3
--   moderate -3 < z <= -2


zScoreWeightForAgeSevereTest : Test
zScoreWeightForAgeSevereTest =
    describe "zScoreWeightForAgeSevere (WHO: severe = z <= -3)"
        [ test "-3.5 -> True" <|
            \_ ->
                zScoreWeightForAgeSevere -3.5
                    |> Expect.equal True
        , test "-3.0 -> True (boundary)" <|
            \_ ->
                zScoreWeightForAgeSevere -3.0
                    |> Expect.equal True
        , test "-2.5 -> False" <|
            \_ ->
                zScoreWeightForAgeSevere -2.5
                    |> Expect.equal False
        ]


zScoreWeightForAgeModerateUnderSixMonthsTest : Test
zScoreWeightForAgeModerateUnderSixMonthsTest =
    let
        -- Under 6 months: classification uses the current z-score only; the
        -- previous z-score is ignored.
        child =
            childAgedMonths 3

        classify zScore previousZScore =
            zScoreWeightForAgeModerate currentDate child zScore previousZScore
    in
    describe "zScoreWeightForAgeModerate, child under 6mo (WHO: moderate = -3 < z <= -2)"
        [ test "-2.5 -> True" <|
            \_ ->
                classify -2.5 Nothing
                    |> Expect.equal True
        , test "-2.0 -> True (boundary)" <|
            \_ ->
                classify -2.0 Nothing
                    |> Expect.equal True
        , test "-1.5 -> False (above moderate band)" <|
            \_ ->
                classify -1.5 Nothing
                    |> Expect.equal False
        , test "-3.0 -> False (that is severe, not moderate)" <|
            \_ ->
                classify -3.0 Nothing
                    |> Expect.equal False
        ]


zScoreWeightForAgeModerateSixMonthsPlusTest : Test
zScoreWeightForAgeModerateSixMonthsPlusTest =
    let
        -- 6+ months: classification requires BOTH the current and the previous
        -- encounter's z-score to be in the moderate band.
        child =
            childAgedMonths 12

        classify zScore previousZScore =
            zScoreWeightForAgeModerate currentDate child zScore previousZScore
    in
    describe "zScoreWeightForAgeModerate, child 6+ months (requires two consecutive moderate readings)"
        [ test "current -2.5 + previous Just -2.5 -> True" <|
            \_ ->
                classify -2.5 (Just -2.5)
                    |> Expect.equal True
        , test "current -2.5 + previous Just -1.5 -> False (previous not moderate)" <|
            \_ ->
                classify -2.5 (Just -1.5)
                    |> Expect.equal False
        , test "[FINDING] current -2.5 + previous Nothing -> False (no previous encounter)" <|
            \_ ->
                -- For a child 6+ months, a single moderate reading with no
                -- previous encounter is NOT classified as moderate: the code
                -- requires two consecutive moderate encounters, which goes
                -- beyond the tab's plain "z between -2 and -3" definition.
                classify -2.5 Nothing
                    |> Expect.equal False
        ]



-- PART 2 — generateNutritionAssessment (orchestrator)
--
-- Scoped to the MUAC and nutrition-signs paths, which do NOT need WHO z-score
-- data: we pass `ZScore.Model.emptyModel` for `zscores` and `Nothing` for the
-- weight arg, so the weight-for-age path is inert.
--
-- Oracle (Nutrition tab MUAC): normal > 12.5, moderate 11.5-12.5, severe <= 11.5.


childId : PersonId
childId =
    toEntityUuid "child-1"


{-| A `ModelIndexedDb` with `child` inserted into `db.people` as a successful
WebData value (read via `Dict.get childId db.people |> Maybe.andThen
RemoteData.toMaybe`). `db.people` is an `AssocList.Dict`.
-}
dbWithChild : Person -> ModelIndexedDb
dbWithChild child =
    { emptyModelIndexedDb
        | people = Dict.insert childId (Success child) emptyModelIndexedDb.people
    }


{-| Run `generateNutritionAssessment` with the weight-for-age path inert.
-}
assess : Person -> Maybe MuacInCm -> Maybe (EverySet ChildNutritionSign) -> List NutritionAssessment
assess child muacValue nutritionValue =
    generateNutritionAssessment
        currentDate
        ZScore.Model.emptyModel
        childId
        muacValue
        nutritionValue
        Nothing
        False
        (dbWithChild child)


generateNutritionAssessmentMuacTest : Test
generateNutritionAssessmentMuacTest =
    let
        -- A 12-month-old so neither the under-6mo nor weight paths interfere.
        child =
            childAgedMonths 12
    in
    describe "generateNutritionAssessment - MUAC path (severe <= 11.5, moderate 11.5-12.5, normal > 12.5)"
        [ test "MUAC 11.0 (severe) -> AssesmentAcuteMalnutritionSevere" <|
            \_ ->
                assess child (Just (MuacInCm 11.0)) Nothing
                    |> List.member AssesmentAcuteMalnutritionSevere
                    |> Expect.equal True
        , test "MUAC 12.0 (moderate) -> AssesmentAcuteMalnutritionModerate" <|
            \_ ->
                assess child (Just (MuacInCm 12.0)) Nothing
                    |> List.member AssesmentAcuteMalnutritionModerate
                    |> Expect.equal True
        , test "MUAC 13.0 (normal) -> not severe" <|
            \_ ->
                assess child (Just (MuacInCm 13.0)) Nothing
                    |> List.member AssesmentAcuteMalnutritionSevere
                    |> Expect.equal False
        , test "MUAC 13.0 (normal) -> not moderate" <|
            \_ ->
                assess child (Just (MuacInCm 13.0)) Nothing
                    |> List.member AssesmentAcuteMalnutritionModerate
                    |> Expect.equal False
        ]


generateNutritionAssessmentSignsTest : Test
generateNutritionAssessmentSignsTest =
    let
        -- Under 6 months, with no MUAC and no weight, the nutrition-signs path
        -- lists all danger signs (everything except NormalChildNutrition).
        -- Edema is a danger sign.
        child =
            childAgedMonths 3
    in
    describe "generateNutritionAssessment - nutrition-signs path (child under 6mo)"
        [ test "danger sign (Edema) -> AssesmentMalnutritionSigns [Edema]" <|
            \_ ->
                assess child Nothing (Just (EverySet.singleton Edema))
                    |> List.member (AssesmentMalnutritionSigns [ Edema ])
                    |> Expect.equal True
        ]



-- PART 3 — resolveLatestValue
--
-- The "previous value" reference shown next to height/MUAC/weight/head-
-- circumference inputs must be the MOST RECENT measurement before today.
-- Selection must not depend on the input list being pre-sorted: head
-- circumference was concatenated oldest-first, and the old `List.head`-only
-- selection showed the oldest value. resolveLatestValue sorts internally.


resolveLatestValueTest : Test
resolveLatestValueTest =
    describe "resolveLatestValue (most recent value strictly before currentDate; order-independent)"
        [ test "picks the most recent value when the list is oldest-first (reproduces the head-circumference bug)" <|
            \_ ->
                resolveLatestValue currentDate
                    [ ( Date.fromCalendarDate 2020 Time.Jan 1, 100 )
                    , ( Date.fromCalendarDate 2020 Time.Mar 1, 200 )
                    , ( Date.fromCalendarDate 2020 Time.May 1, 300 )
                    ]
                    |> Expect.equal (Just 300)
        , test "picks the most recent value regardless of input order" <|
            \_ ->
                resolveLatestValue currentDate
                    [ ( Date.fromCalendarDate 2020 Time.Mar 1, 200 )
                    , ( Date.fromCalendarDate 2020 Time.May 1, 300 )
                    , ( Date.fromCalendarDate 2020 Time.Jan 1, 100 )
                    ]
                    |> Expect.equal (Just 300)
        , test "ignores measurements dated on or after currentDate" <|
            \_ ->
                resolveLatestValue currentDate
                    [ ( Date.fromCalendarDate 2020 Time.Apr 1, 200 )
                    , ( Date.fromCalendarDate 2020 Time.May 15, 300 )
                    , ( Date.fromCalendarDate 2020 Time.Jun 1, 999 )
                    , ( Date.fromCalendarDate 2020 Time.Jul 1, 888 )
                    ]
                    |> Expect.equal (Just 300)
        , test "returns Nothing for an empty list" <|
            \_ ->
                let
                    noMeasurements : List ( NominalDate, Int )
                    noMeasurements =
                        []
                in
                resolveLatestValue currentDate noMeasurements
                    |> Expect.equal Nothing
        , test "returns Nothing when every measurement is dated on or after currentDate" <|
            \_ ->
                resolveLatestValue currentDate
                    [ ( Date.fromCalendarDate 2020 Time.Jun 1, 999 )
                    , ( Date.fromCalendarDate 2020 Time.Aug 1, 888 )
                    ]
                    |> Expect.equal Nothing
        ]


all : Test
all =
    describe "Nutrition assessment tests"
        [ zScoreWeightForAgeSevereTest
        , zScoreWeightForAgeModerateUnderSixMonthsTest
        , zScoreWeightForAgeModerateSixMonthsPlusTest
        , generateNutritionAssessmentMuacTest
        , generateNutritionAssessmentSignsTest
        , resolveLatestValueTest
        ]
