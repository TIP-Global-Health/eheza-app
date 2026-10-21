module Pages.Scoreboard.Test exposing (all)

import App.Types exposing (Site(..))
import AssocList as Dict exposing (Dict)
import Backend.Scoreboard.Model exposing (PatientData, VaccinationProgressDict, VaccineDose(..), VaccineType(..), emptyNCDAData, emptyNutritionCriterionsData)
import Backend.Scoreboard.Utils exposing (generateVaccinationProgressForVaccine)
import Date exposing (Unit(..))
import EverySet
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Pages.Reports.Utils
import Pages.Scoreboard.Utils
    exposing
        ( allVaccineTypes
        , countChildrenUnder2
        , datesForMonth
        , foldRecordsByMonth
        , formatValues
        , generateFutureVaccinationsData
        , generateMonthsGap
        , isOnTrackForImmunisation
        , isPregnancyMonth
        , resolveTargetDateForMonth
        )
import Test exposing (Test, describe, test)
import Time exposing (Month(..))


birthDate : NominalDate
birthDate =
    Date.fromCalendarDate 2024 Jan 1


{-| A date this many weeks after birth.
-}
weeksOld : Int -> NominalDate
weeksOld weeks =
    Date.add Weeks weeks birthDate


{-| The doses given, as the dates of each vaccine.
-}
progress : List ( VaccineType, List NominalDate ) -> VaccinationProgressDict
progress =
    List.map (Tuple.mapSecond (EverySet.fromList >> generateVaccinationProgressForVaccine))
        >> Dict.fromList


{-| The next dose of a vaccine and its date, or Nothing once the course
is complete.
-}
nextDose : Site -> List ( VaccineType, List NominalDate ) -> VaccineType -> Maybe ( VaccineDose, NominalDate )
nextDose site given vaccineType =
    generateFutureVaccinationsData site birthDate (progress given) [ vaccineType ]
        |> List.head
        |> Maybe.andThen Tuple.second


firstDosesTest : Test
firstDosesTest =
    describe "the first dose of each vaccine is due at its age"
        [ test "BCG and OPV at birth, DTP, PCV13 and Rotarix at 6 weeks, IPV at 14 weeks, MR at 36 weeks" <|
            \_ ->
                [ VaccineBCG, VaccineOPV, VaccineDTP, VaccinePCV13, VaccineRotarix, VaccineIPV, VaccineMR ]
                    |> List.map (nextDose SiteRwanda [])
                    |> Expect.equal
                        (List.map (Tuple.pair VaccineDoseFirst >> Just)
                            [ birthDate, birthDate, weeksOld 6, weeksOld 6, weeksOld 6, weeksOld 14, weeksOld 36 ]
                        )
        ]


opvTest : Test
opvTest =
    describe "OPV"
        [ test "without the birth dose, the second dose is 4 weeks after the first" <|
            \_ ->
                nextDose SiteRwanda [ ( VaccineOPV, [ weeksOld 6 ] ) ] VaccineOPV
                    |> Expect.equal (Just ( VaccineDoseSecond, weeksOld 10 ))
        , test "without the birth dose, three doses complete the course" <|
            \_ ->
                nextDose SiteRwanda [ ( VaccineOPV, [ weeksOld 6, weeksOld 10, weeksOld 14 ] ) ] VaccineOPV
                    |> Expect.equal Nothing
        , test "after the birth dose, the second dose is at 6 weeks of age" <|
            \_ ->
                nextDose SiteRwanda [ ( VaccineOPV, [ Date.add Days 4 birthDate ] ) ] VaccineOPV
                    |> Expect.equal (Just ( VaccineDoseSecond, weeksOld 6 ))
        , test "a first dose at 14 days is not a birth dose, so three doses complete the course" <|
            \_ ->
                nextDose SiteRwanda [ ( VaccineOPV, [ weeksOld 2, weeksOld 6, weeksOld 10 ] ) ] VaccineOPV
                    |> Expect.equal Nothing
        , test "after the birth dose, a fourth dose follows the third" <|
            \_ ->
                nextDose SiteRwanda [ ( VaccineOPV, [ birthDate, weeksOld 6, weeksOld 10 ] ) ] VaccineOPV
                    |> Expect.equal (Just ( VaccineDoseFourth, weeksOld 14 ))
        , test "after the birth dose, four doses complete the course" <|
            \_ ->
                nextDose SiteRwanda [ ( VaccineOPV, [ birthDate, weeksOld 6, weeksOld 10, weeksOld 14 ] ) ] VaccineOPV
                    |> Expect.equal Nothing
        ]


ipvTest : Test
ipvTest =
    describe "IPV"
        [ test "Rwanda: the second dose is at 36 weeks of age, when that is later" <|
            \_ ->
                nextDose SiteRwanda [ ( VaccineIPV, [ weeksOld 14 ] ) ] VaccineIPV
                    |> Expect.equal (Just ( VaccineDoseSecond, weeksOld 36 ))
        , test "Rwanda: the second dose is 28 days after the first, when that is later" <|
            \_ ->
                nextDose SiteRwanda [ ( VaccineIPV, [ weeksOld 34 ] ) ] VaccineIPV
                    |> Expect.equal (Just ( VaccineDoseSecond, weeksOld 38 ))
        , test "Rwanda: two doses complete the course" <|
            \_ ->
                nextDose SiteRwanda [ ( VaccineIPV, [ weeksOld 14, weeksOld 36 ] ) ] VaccineIPV
                    |> Expect.equal Nothing
        , test "Burundi: one dose completes the course" <|
            \_ ->
                nextDose SiteBurundi [ ( VaccineIPV, [ weeksOld 14 ] ) ] VaccineIPV
                    |> Expect.equal Nothing
        ]


dtpStandaloneTest : Test
dtpStandaloneTest =
    describe "DTP standalone"
        [ test "is due at 18 months of age, when that is later than 28 days after the third DTP" <|
            \_ ->
                nextDose SiteBurundi [ ( VaccineDTP, [ weeksOld 6, weeksOld 10, weeksOld 14 ] ) ] VaccineDTPStandalone
                    |> Expect.equal (Just ( VaccineDoseFirst, Date.fromCalendarDate 2025 Jul 1 ))
        , test "is due 28 days after the third DTP, when that is later" <|
            \_ ->
                nextDose SiteBurundi [ ( VaccineDTP, [ weeksOld 6, weeksOld 10, Date.fromCalendarDate 2025 Jun 20 ] ) ] VaccineDTPStandalone
                    |> Expect.equal (Just ( VaccineDoseFirst, Date.fromCalendarDate 2025 Jul 18 ))
        , test "is never due before the third DTP" <|
            \_ ->
                nextDose SiteBurundi [ ( VaccineDTP, [ weeksOld 6, weeksOld 10 ] ) ] VaccineDTPStandalone
                    |> Expect.equal (Just ( VaccineDoseFirst, Date.add Years 999 birthDate ))
        , test "one dose completes the course" <|
            \_ ->
                nextDose SiteBurundi [ ( VaccineDTPStandalone, [ Date.fromCalendarDate 2025 Jul 1 ] ) ] VaccineDTPStandalone
                    |> Expect.equal Nothing
        ]


mrTest : Test
mrTest =
    describe "MR"
        [ test "Rwanda: the second dose is 6 months after the first" <|
            \_ ->
                nextDose SiteRwanda [ ( VaccineMR, [ weeksOld 36 ] ) ] VaccineMR
                    |> Expect.equal (Just ( VaccineDoseSecond, Date.add Months 6 (weeksOld 36) ))
        , test "Burundi: the second dose is 9 months after the first" <|
            \_ ->
                nextDose SiteBurundi [ ( VaccineMR, [ weeksOld 36 ] ) ] VaccineMR
                    |> Expect.equal (Just ( VaccineDoseSecond, Date.add Months 9 (weeksOld 36) ))
        , test "two doses complete the course" <|
            \_ ->
                nextDose SiteBurundi [ ( VaccineMR, [ weeksOld 36, weeksOld 75 ] ) ] VaccineMR
                    |> Expect.equal Nothing
        ]


vaccineListsTest : Test
vaccineListsTest =
    describe "the vaccines each list schedules"
        [ test "the scoreboard: DTP standalone only on Burundi, and no HPV" <|
            \_ ->
                ( allVaccineTypes SiteRwanda, allVaccineTypes SiteBurundi )
                    |> Expect.equal
                        ( [ VaccineBCG, VaccineOPV, VaccineDTP, VaccinePCV13, VaccineRotarix, VaccineIPV, VaccineMR ]
                        , [ VaccineBCG, VaccineOPV, VaccineDTP, VaccinePCV13, VaccineRotarix, VaccineIPV, VaccineMR, VaccineDTPStandalone ]
                        )
        , test "the reports: DTP standalone on Burundi, HPV elsewhere" <|
            \_ ->
                ( Pages.Reports.Utils.allVaccineTypes SiteRwanda, Pages.Reports.Utils.allVaccineTypes SiteBurundi )
                    |> Expect.equal
                        ( [ VaccineBCG, VaccineOPV, VaccineDTP, VaccinePCV13, VaccineRotarix, VaccineIPV, VaccineMR, VaccineHPV ]
                        , [ VaccineBCG, VaccineOPV, VaccineDTP, VaccinePCV13, VaccineRotarix, VaccineIPV, VaccineMR, VaccineDTPStandalone ]
                        )
        ]


{-| The scoreboard is looked at on 2026-10-20.
-}
today : NominalDate
today =
    Date.fromCalendarDate 2026 Oct 20


{-| The value of each month column, January first.
-}
columnValues : Dict Int Int -> List Int
columnValues monthsGap =
    List.filterMap (\index -> Dict.get index monthsGap) (List.range 0 11)


monthColumnsTest : Test
monthColumnsTest =
    describe "the month columns"
        [ test "this year: each column's distance in months from this month" <|
            \_ ->
                generateMonthsGap today 0
                    |> columnValues
                    |> Expect.equal [ 9, 8, 7, 6, 5, 4, 3, 2, 1, 0, -1, -2 ]
        , test "last year: twelve months further back" <|
            \_ ->
                generateMonthsGap today -1
                    |> columnValues
                    |> Expect.equal [ 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10 ]
        , test "a column's date is today for this month, else the last day of its month" <|
            \_ ->
                List.map (\gap -> resolveTargetDateForMonth gap (Date.fromCalendarDate 2026 Mar 31)) [ 0, 1, 2, -2 ]
                    |> Expect.equal
                        [ Date.fromCalendarDate 2026 Mar 31
                        , Date.fromCalendarDate 2026 Feb 28
                        , Date.fromCalendarDate 2026 Jan 31
                        , Date.fromCalendarDate 2026 May 31
                        ]
        , test "each column folds its records against its own month's date" <|
            \_ ->
                foldRecordsByMonth today (generateMonthsGap today 0) (\targetDate _ dates -> targetDate :: dates) [] [ () ]
                    |> List.concat
                    |> Expect.equal
                        (List.map (\month -> Date.add Days -1 (Date.fromCalendarDate 2026 month 1 |> Date.add Months 1)) [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep ]
                            ++ [ today, Date.fromCalendarDate 2026 Nov 30, Date.fromCalendarDate 2026 Dec 31 ]
                        )
        , test "the dates that fall in a column's month" <|
            \_ ->
                datesForMonth (Date.fromCalendarDate 2026 Mar 31) [ Date.fromCalendarDate 2026 Feb 28, Date.fromCalendarDate 2026 Mar 1, Date.fromCalendarDate 2025 Mar 15 ]
                    |> Expect.equal [ Date.fromCalendarDate 2026 Mar 1 ]
        , test "this year shows values up to this month, and nothing after" <|
            \_ ->
                formatValues today 0 (List.repeat 12 "1")
                    |> Expect.equal (List.repeat 10 "1" ++ [ "", "" ])
        , test "2023 shows values only from November" <|
            \_ ->
                formatValues today -3 (List.repeat 12 "1")
                    |> Expect.equal (List.repeat 10 "-" ++ [ "1", "1" ])
        , test "2024 on shows every month" <|
            \_ ->
                formatValues today -2 (List.repeat 12 "1")
                    |> Expect.equal (List.repeat 12 "1")
        ]


scoreboardChild : NominalDate -> NominalDate -> PatientData
scoreboardChild created birth =
    { created = created
    , birthDate = birth
    , eddDate = birth
    , lowBirthWeight = Nothing
    , nutrition = emptyNutritionCriterionsData
    , ncda = emptyNCDAData
    }


childrenUnder2Test : Test
childrenUnder2Test =
    describe "countChildrenUnder2"
        [ test "a child counts from the month it is registered until it turns two" <|
            \_ ->
                [ scoreboardChild (Date.fromCalendarDate 2026 Mar 15) (Date.fromCalendarDate 2026 Mar 15)
                , scoreboardChild (Date.fromCalendarDate 2024 Jun 10) (Date.fromCalendarDate 2024 Jun 10)
                ]
                    |> countChildrenUnder2 today (generateMonthsGap today 0)
                    |> Expect.equal [ 1, 1, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1 ]
        , test "a child registered after the month's date does not count in it" <|
            \_ ->
                [ scoreboardChild (Date.fromCalendarDate 2026 Mar 31) (Date.fromCalendarDate 2025 Jan 1) ]
                    |> countChildrenUnder2 today (generateMonthsGap today 0)
                    |> List.take 4
                    |> Expect.equal [ 0, 0, 0, 1 ]
        ]


pregnancyMonthTest : Test
pregnancyMonthTest =
    describe "isPregnancyMonth"
        [ test "a month in the middle of the pregnancy counts" <|
            \_ -> Expect.equal True (isPregnancyMonth (Date.fromCalendarDate 2024 May 15) (Date.fromCalendarDate 2024 Jan 31))
        , test "a month after the EDD does not" <|
            \_ -> Expect.equal False (isPregnancyMonth (Date.fromCalendarDate 2024 May 15) (Date.fromCalendarDate 2024 Jun 30))
        , test "a month a year before the EDD does not" <|
            \_ -> Expect.equal False (isPregnancyMonth (Date.fromCalendarDate 2024 May 15) (Date.fromCalendarDate 2023 May 31))
        ]


{-| A child born on the birth date of the vaccination tests, given the
doses listed.
-}
vaccinatedChild : List ( VaccineType, List NominalDate ) -> PatientData
vaccinatedChild given =
    let
        child =
            scoreboardChild birthDate birthDate

        ncda =
            child.ncda

        universalIntervention =
            ncda.universalIntervention
    in
    { child | ncda = { ncda | universalIntervention = { universalIntervention | row1 = progress given } } }


{-| The birth doses and the 6-week doses.
-}
dosesTo6Weeks : List ( VaccineType, List NominalDate )
dosesTo6Weeks =
    [ ( VaccineBCG, [ birthDate ] )
    , ( VaccineOPV, [ birthDate, weeksOld 6 ] )
    , ( VaccineDTP, [ weeksOld 6 ] )
    , ( VaccinePCV13, [ weeksOld 6 ] )
    , ( VaccineRotarix, [ weeksOld 6 ] )
    ]


immunisationOnTrackTest : Test
immunisationOnTrackTest =
    describe "isOnTrackForImmunisation"
        [ test "a child missing the 10-week doses is off track at the end of that month of age" <|
            \_ ->
                vaccinatedChild dosesTo6Weeks
                    |> isOnTrackForImmunisation SiteRwanda (Date.fromCalendarDate 2024 Mar 31)
                    |> Expect.equal False
        , test "a child given the 10-week doses is on track" <|
            \_ ->
                vaccinatedChild
                    (dosesTo6Weeks
                        ++ [ ( VaccineOPV, [ birthDate, weeksOld 6, weeksOld 10 ] )
                           , ( VaccineDTP, [ weeksOld 6, weeksOld 10 ] )
                           , ( VaccinePCV13, [ weeksOld 6, weeksOld 10 ] )
                           , ( VaccineRotarix, [ weeksOld 6, weeksOld 10 ] )
                           ]
                    )
                    |> isOnTrackForImmunisation SiteRwanda (Date.fromCalendarDate 2024 Mar 31)
                    |> Expect.equal True
        , test "a dose given after the month is not counted for it" <|
            \_ ->
                vaccinatedChild [ ( VaccineBCG, [ Date.fromCalendarDate 2024 Feb 2 ] ), ( VaccineOPV, [ birthDate ] ) ]
                    |> isOnTrackForImmunisation SiteRwanda (Date.fromCalendarDate 2024 Jan 31)
                    |> Expect.equal False
        , test "a child on track is not counted before it is registered" <|
            \_ ->
                let
                    child =
                        vaccinatedChild [ ( VaccineBCG, [ birthDate ] ), ( VaccineOPV, [ birthDate ] ) ]
                in
                [ child, { child | created = Date.fromCalendarDate 2024 Feb 1 } ]
                    |> List.map (isOnTrackForImmunisation SiteRwanda (Date.fromCalendarDate 2024 Jan 31))
                    |> Expect.equal [ True, False ]
        , test "a child with every dose is counted until it turns two" <|
            \_ ->
                let
                    child =
                        vaccinatedChild
                            [ ( VaccineBCG, [ birthDate ] )
                            , ( VaccineOPV, [ birthDate, weeksOld 6, weeksOld 10, weeksOld 14 ] )
                            , ( VaccineDTP, [ weeksOld 6, weeksOld 10, weeksOld 14 ] )
                            , ( VaccinePCV13, [ weeksOld 6, weeksOld 10, weeksOld 14 ] )
                            , ( VaccineRotarix, [ weeksOld 6, weeksOld 10 ] )
                            , ( VaccineIPV, [ weeksOld 14, weeksOld 36 ] )
                            , ( VaccineMR, [ weeksOld 36, weeksOld 62 ] )
                            ]
                in
                [ Date.fromCalendarDate 2025 Dec 31, Date.fromCalendarDate 2026 Jan 31 ]
                    |> List.map (\targetDate -> isOnTrackForImmunisation SiteRwanda targetDate child)
                    |> Expect.equal [ True, False ]
        ]


all : Test
all =
    describe "Pages.Scoreboard"
        [ childrenUnder2Test
        , dtpStandaloneTest
        , firstDosesTest
        , immunisationOnTrackTest
        , ipvTest
        , monthColumnsTest
        , mrTest
        , opvTest
        , pregnancyMonthTest
        , vaccineListsTest
        ]
