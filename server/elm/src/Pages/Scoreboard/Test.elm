module Pages.Scoreboard.Test exposing (all)

import App.Types exposing (Site(..))
import AssocList as Dict
import Backend.Scoreboard.Model exposing (VaccinationProgressDict, VaccineDose(..), VaccineType(..))
import Backend.Scoreboard.Utils exposing (generateVaccinationProgressForVaccine)
import Date exposing (Unit(..))
import EverySet
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Pages.Reports.Utils
import Pages.Scoreboard.Utils exposing (allVaccineTypes, generateFutureVaccinationsData)
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


all : Test
all =
    describe "Pages.Scoreboard"
        [ dtpStandaloneTest
        , firstDosesTest
        , ipvTest
        , mrTest
        , opvTest
        , vaccineListsTest
        ]
