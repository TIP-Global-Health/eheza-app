module Measurement.Test exposing (all)

import AssocList as Dict
import Backend.Measurement.Model
    exposing
        ( ColorAlertIndication(..)
        , VaccineDose(..)
        , WellChildVaccineType(..)
        )
import Date exposing (Unit(..))
import Expect
import Measurement.Utils
    exposing
        ( getAllDosesForVaccine
        , getIntervalForVaccine
        , initialVaccinationDateByBirthDate
        )
import Measurement.View exposing (viewColorAlertIndication)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (classes, text)
import Time
import Translate.Model exposing (Language(..))


viewChildFormsTest : Test
viewChildFormsTest =
    test "Re-implement viewChildFormsTest" <|
        always Expect.pass


viewMotherFormsTest : Test
viewMotherFormsTest =
    test "Re-implement viewMotherFormsTest" <|
        always Expect.pass


viewColorAlertIndicationTest : Test
viewColorAlertIndicationTest =
    describe "viewColorAlertIndication"
        [ test "red" <|
            \_ ->
                viewColorAlertIndication English ColorAlertRed
                    |> Query.fromHtml
                    |> Query.has
                        [ classes [ "label-red" ]
                        , text "RED"
                        ]
        , test "yellow" <|
            \_ ->
                viewColorAlertIndication English ColorAlertYellow
                    |> Query.fromHtml
                    |> Query.has
                        [ classes [ "label-yellow" ]
                        , text "YELLOW"
                        ]
        , test "green" <|
            \_ ->
                viewColorAlertIndication English ColorAlertGreen
                    |> Query.fromHtml
                    |> Query.has
                        [ classes [ "label-green" ]
                        , text "GREEN"
                        ]
        ]


{-| Vaccine scheduling tests.

ORACLE: the "Standard Pediatric Visit" tab of the clinical sheet (Rwanda
schedule). Each [sheet]-marked expectation is taken from that sheet, not from
the code. [CODE]-marked cases are not covered by the sheet; they pin current
code behavior (findings, not oracle matches).

-}
getIntervalForVaccineTest : Test
getIntervalForVaccineTest =
    describe "getIntervalForVaccine"
        [ test "Rwanda OPV - sheet: no sooner than 28 days between doses (= 4 weeks)" <|
            \_ ->
                getIntervalForVaccine SiteRwanda VaccineOPV
                    |> Expect.equal ( 4, Weeks )
        , test "Rwanda DTP - sheet: 28 days between doses (= 4 weeks)" <|
            \_ ->
                getIntervalForVaccine SiteRwanda VaccineDTP
                    |> Expect.equal ( 4, Weeks )
        , test "Rwanda PCV13 - sheet: 28 days between doses (= 4 weeks)" <|
            \_ ->
                getIntervalForVaccine SiteRwanda VaccinePCV13
                    |> Expect.equal ( 4, Weeks )
        , test "Rwanda Rotarix - sheet: 28 days between doses (= 4 weeks)" <|
            \_ ->
                getIntervalForVaccine SiteRwanda VaccineRotarix
                    |> Expect.equal ( 4, Weeks )
        , test "Rwanda HPV - sheet: 6 months between doses" <|
            \_ ->
                getIntervalForVaccine SiteRwanda VaccineHPV
                    |> Expect.equal ( 6, Months )
        , test "Rwanda MR - sheet: doses 9mo -> 15mo (= 6 months apart)" <|
            \_ ->
                getIntervalForVaccine SiteRwanda VaccineMR
                    |> Expect.equal ( 6, Months )
        , test "Rwanda BCG - sheet: single dose (interval 0)" <|
            \_ ->
                getIntervalForVaccine SiteRwanda VaccineBCG
                    |> Expect.equal ( 0, Days )
        , test "[CODE] Burundi MR - sheet is Rwanda; Burundi interval differs (9 months)" <|
            \_ ->
                getIntervalForVaccine SiteBurundi VaccineMR
                    |> Expect.equal ( 9, Months )
        , test "[CODE] Rwanda IPV - placeholder 0; real 2nd-dose interval is special-cased per issue #1426" <|
            \_ ->
                getIntervalForVaccine SiteRwanda VaccineIPV
                    |> Expect.equal ( 0, Days )
        ]


getAllDosesForVaccineTest : Test
getAllDosesForVaccineTest =
    describe "getAllDosesForVaccine (Rwanda)"
        [ test "OPV, initialOpvAdministered=True - sheet: 4 doses" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda True VaccineOPV
                    |> Expect.equal
                        [ VaccineDoseFirst
                        , VaccineDoseSecond
                        , VaccineDoseThird
                        , VaccineDoseFourth
                        ]
        , test "[CODE] OPV, initialOpvAdministered=False - without the birth dose, 3 doses" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda False VaccineOPV
                    |> Expect.equal
                        [ VaccineDoseFirst
                        , VaccineDoseSecond
                        , VaccineDoseThird
                        ]
        , test "DTP - sheet: 3 doses" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda True VaccineDTP
                    |> Expect.equal
                        [ VaccineDoseFirst
                        , VaccineDoseSecond
                        , VaccineDoseThird
                        ]
        , test "PCV13 - sheet: 3 doses" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda True VaccinePCV13
                    |> Expect.equal
                        [ VaccineDoseFirst
                        , VaccineDoseSecond
                        , VaccineDoseThird
                        ]
        , test "Rotarix - sheet: 2 doses" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda True VaccineRotarix
                    |> Expect.equal
                        [ VaccineDoseFirst
                        , VaccineDoseSecond
                        ]
        , test "MR - sheet: 2 doses" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda True VaccineMR
                    |> Expect.equal
                        [ VaccineDoseFirst
                        , VaccineDoseSecond
                        ]
        , test "HPV - sheet: 2 doses" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda True VaccineHPV
                    |> Expect.equal
                        [ VaccineDoseFirst
                        , VaccineDoseSecond
                        ]
        , test "BCG - sheet: 1 dose" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda True VaccineBCG
                    |> Expect.equal
                        [ VaccineDoseFirst ]
        , test "[CODE] IPV - sheet shows only 1 IPV dose at 14wk; code adds a 2nd dose for Rwanda per issue #1426" <|
            \_ ->
                getAllDosesForVaccine SiteRwanda True VaccineIPV
                    |> Expect.equal
                        [ VaccineDoseFirst
                        , VaccineDoseSecond
                        ]
        ]


initialVaccinationDateByBirthDateTest : Test
initialVaccinationDateByBirthDateTest =
    let
        birthDate =
            Date.fromCalendarDate 2020 Time.Jan 1

        -- For dose-1 cases dosesInterval is 0, so the empty progress dict is
        -- never consulted. VaccinationProgressDict is an AssocList.Dict.
        emptyProgress =
            Dict.empty

        firstDoseDate vaccineType =
            initialVaccinationDateByBirthDate SiteRwanda birthDate True emptyProgress ( vaccineType, VaccineDoseFirst )
    in
    describe "initialVaccinationDateByBirthDate (Rwanda, dose 1 start age)"
        [ test "BCG - sheet: birth" <|
            \_ ->
                firstDoseDate VaccineBCG
                    |> Expect.equal birthDate
        , test "OPV - sheet: birth" <|
            \_ ->
                firstDoseDate VaccineOPV
                    |> Expect.equal birthDate
        , test "DTP - sheet: 6 weeks" <|
            \_ ->
                firstDoseDate VaccineDTP
                    |> Expect.equal (Date.add Weeks 6 birthDate)
        , test "PCV13 - sheet: 6 weeks" <|
            \_ ->
                firstDoseDate VaccinePCV13
                    |> Expect.equal (Date.add Weeks 6 birthDate)
        , test "Rotarix - sheet: 6 weeks" <|
            \_ ->
                firstDoseDate VaccineRotarix
                    |> Expect.equal (Date.add Weeks 6 birthDate)
        , test "IPV - sheet: 14 weeks" <|
            \_ ->
                firstDoseDate VaccineIPV
                    |> Expect.equal (Date.add Weeks 14 birthDate)
        , test "MR - sheet: 36 weeks / 9 months" <|
            \_ ->
                firstDoseDate VaccineMR
                    |> Expect.equal (Date.add Weeks 36 birthDate)
        , test "HPV - sheet: 12 years" <|
            \_ ->
                firstDoseDate VaccineHPV
                    |> Expect.equal (Date.add Years 12 birthDate)
        , test "[CODE] MR dose 2 - 36wk + 6mo (~14.3mo); sheet says 15 months (~3-week precision gap)" <|
            \_ ->
                initialVaccinationDateByBirthDate SiteRwanda birthDate True emptyProgress ( VaccineMR, VaccineDoseSecond )
                    |> Expect.equal (Date.add Months 6 (Date.add Weeks 36 birthDate))
        ]


all : Test
all =
    describe "Measurement of children: form tests"
        [ viewChildFormsTest
        , viewMotherFormsTest
        , viewColorAlertIndicationTest
        , getIntervalForVaccineTest
        , getAllDosesForVaccineTest
        , initialVaccinationDateByBirthDateTest
        ]
