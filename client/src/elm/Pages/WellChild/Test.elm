module Pages.WellChild.Test exposing (all)

import Backend.Measurement.Model exposing (VaccineDose(..), WellChildVaccineType(..))
import Date exposing (Unit(..))
import Expect
import Measurement.Model exposing (emptyHeightForm, emptyMuacForm, emptyWeightForm)
import Pages.WellChild.Activity.Types exposing (NutritionAssessmentTask(..))
import Pages.WellChild.Activity.Utils exposing (nutritionAssessmentSaveDisabled, resolveNextDateForImmunisationVisit)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)
import Time


all : Test
all =
    describe "Pages.WellChild"
        [ resolveNextDateForImmunisationVisitTests
        , nutritionAssessmentSaveDisabledTests
        ]


{-| The scenarios mirror issue #1896: an on-schedule child used to get
Nothing at essentially every encounter after 14 weeks of age, because
completed vaccine cycles still emit (vaccineType, Nothing) tuples and
the within-a-month filter had no fallback.
-}
resolveNextDateForImmunisationVisitTests : Test
resolveNextDateForImmunisationVisitTests =
    let
        -- An on-schedule child born 2024-01-01, at the 14 weeks encounter.
        fourteenWeeksDate =
            Date.fromCalendarDate 2024 Time.Apr 8

        nineMonthsDate =
            Date.fromCalendarDate 2024 Time.Oct 1

        fifteenMonthsDate =
            Date.fromCalendarDate 2025 Time.Apr 1
    in
    describe "resolveNextDateForImmunisationVisit"
        [ test "suggests the next milestone date when no dose is due within a month" <|
            \_ ->
                resolveNextDateForImmunisationVisit fourteenWeeksDate
                    [ ( VaccineBCG, Nothing )
                    , ( VaccineOPV, Nothing )
                    , ( VaccineDTP, Nothing )
                    , ( VaccinePCV13, Nothing )
                    , ( VaccineRotarix, Nothing )
                    , ( VaccineIPV, Just ( VaccineDoseSecond, nineMonthsDate ) )
                    , ( VaccineMR, Just ( VaccineDoseFirst, nineMonthsDate ) )
                    ]
                    |> Expect.equal (Just nineMonthsDate)
        , test "suggests the earliest MR/HPV date when only long-interval doses remain" <|
            \_ ->
                resolveNextDateForImmunisationVisit (Date.fromCalendarDate 2024 Time.Oct 5)
                    [ ( VaccineBCG, Nothing )
                    , ( VaccineOPV, Nothing )
                    , ( VaccineDTP, Nothing )
                    , ( VaccinePCV13, Nothing )
                    , ( VaccineRotarix, Nothing )
                    , ( VaccineIPV, Nothing )
                    , ( VaccineMR, Just ( VaccineDoseSecond, fifteenMonthsDate ) )
                    , ( VaccineHPV, Nothing )
                    ]
                    |> Expect.equal (Just fifteenMonthsDate)
        , test "falls back to the earliest pending dose across both interval groups" <|
            \_ ->
                resolveNextDateForImmunisationVisit fourteenWeeksDate
                    [ ( VaccineIPV, Just ( VaccineDoseSecond, Date.add Days 60 fourteenWeeksDate ) )
                    , ( VaccineMR, Just ( VaccineDoseFirst, Date.add Days 40 fourteenWeeksDate ) )
                    ]
                    |> Expect.equal (Just (Date.add Days 40 fourteenWeeksDate))
        , test "bundles doses due within a month on the latest of their dates" <|
            \_ ->
                resolveNextDateForImmunisationVisit fourteenWeeksDate
                    [ ( VaccineOPV, Just ( VaccineDoseThird, Date.add Days 10 fourteenWeeksDate ) )
                    , ( VaccineDTP, Just ( VaccineDoseThird, Date.add Days 20 fourteenWeeksDate ) )
                    , ( VaccineMR, Nothing )
                    ]
                    |> Expect.equal (Just (Date.add Days 20 fourteenWeeksDate))
        , test "clamps a passed long-interval suggestion to one interval from today" <|
            \_ ->
                resolveNextDateForImmunisationVisit (Date.fromCalendarDate 2025 Time.May 1)
                    [ ( VaccineOPV, Nothing )
                    , ( VaccineMR, Just ( VaccineDoseSecond, fifteenMonthsDate ) )
                    ]
                    |> Expect.equal (Just (Date.fromCalendarDate 2025 Time.Nov 1))
        ]


{-| The Nutrition Assessment save action used to be gated on task completeness
alone, so a mistyped height/weight/MUAC saved silently and fed the z-score
calculations - while the very same shared forms are range-gated in the Nutrition
encounter, the group sessions and Family Nutrition.
-}
nutritionAssessmentSaveDisabledTests : Test
nutritionAssessmentSaveDisabledTests =
    let
        heightForm value =
            { emptyHeightForm | height = value }

        muacForm value =
            { emptyMuacForm | muac = value }

        weightForm value =
            { emptyWeightForm | weight = value }

        saveDisabled site tasksIncomplete height muac weight task =
            nutritionAssessmentSaveDisabled site
                tasksIncomplete
                (heightForm height)
                (muacForm muac)
                (weightForm weight)
                task
    in
    describe "nutritionAssessmentSaveDisabled"
        [ test "Height: a plausible value with every task answered enables Save" <|
            \_ ->
                saveDisabled SiteRwanda False (Just 105) Nothing Nothing TaskHeight
                    |> Expect.equal False
        , test "Height: a mistyped 1050 cm keeps Save disabled" <|
            \_ ->
                saveDisabled SiteRwanda False (Just 1050) Nothing Nothing TaskHeight
                    |> Expect.equal True
        , test "Height: 'unable to take measurement' enables Save with no value" <|
            \_ ->
                nutritionAssessmentSaveDisabled SiteRwanda
                    False
                    { emptyHeightForm | height = Nothing, measurementNotTaken = Just True }
                    emptyMuacForm
                    emptyWeightForm
                    TaskHeight
                    |> Expect.equal False
        , test "Weight: a mistyped 850 kg keeps Save disabled" <|
            \_ ->
                saveDisabled SiteRwanda False Nothing Nothing (Just 850) TaskWeight
                    |> Expect.equal True
        , test "Weight: 'unable to take measurement' enables Save with no value" <|
            \_ ->
                nutritionAssessmentSaveDisabled SiteRwanda
                    False
                    emptyHeightForm
                    emptyMuacForm
                    { emptyWeightForm | weight = Nothing, measurementNotTaken = Just True }
                    TaskWeight
                    |> Expect.equal False
        , test "Muac: Burundi stores cm, so 12.5 cm (125 mm) enables Save" <|
            \_ ->
                saveDisabled SiteBurundi False Nothing (Just 12.5) Nothing TaskMuac
                    |> Expect.equal False
        , test "Muac: Rwanda rejects 125, a mm value typed into a cm field" <|
            \_ ->
                saveDisabled SiteRwanda False Nothing (Just 125) Nothing TaskMuac
                    |> Expect.equal True
        , test "Muac: an unset value keeps Save disabled" <|
            \_ ->
                saveDisabled SiteRwanda False Nothing Nothing Nothing TaskMuac
                    |> Expect.equal True
        , test "an unanswered task keeps Save disabled even with a valid value" <|
            \_ ->
                saveDisabled SiteRwanda True (Just 105) Nothing Nothing TaskHeight
                    |> Expect.equal True
        , test "HeadCircumference and Nutrition remain gated on task completeness only" <|
            \_ ->
                ( saveDisabled SiteRwanda False Nothing Nothing Nothing TaskHeadCircumference
                , saveDisabled SiteRwanda True Nothing Nothing Nothing TaskHeadCircumference
                , saveDisabled SiteRwanda False Nothing Nothing Nothing TaskNutrition
                )
                    |> Expect.equal ( False, True, False )
        ]
