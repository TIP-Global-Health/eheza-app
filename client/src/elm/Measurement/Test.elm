module Measurement.Test exposing (all)

import AssocList as Dict
import Backend.Measurement.Model
    exposing
        ( ColorAlertIndication(..)
        , MuacInCm(..)
        , VaccineDose(..)
        , WellChildVaccineType(..)
        )
import Date exposing (Unit(..))
import Expect
import Measurement.Model exposing (MsgChild(..), emptyModelChild)
import Measurement.Update exposing (updateChild)
import Measurement.Utils
    exposing
        ( getAllDosesForVaccine
        , getInputConstraintsHeight
        , getInputConstraintsWeight
        , getIntervalForVaccine
        , initialVaccinationDateByBirthDate
        , muacOutsideConstraints
        , outsideConstraints
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


updateChildSetMuacTest : Test
updateChildSetMuacTest =
    -- The NCDA MUAC field stores cm. At Burundi the nurse enters mm, so the
    -- group-session input handler must divide by 10 (like every other MUAC
    -- field) rather than store the typed value verbatim.
    describe "updateChild SetMuac (group NCDA MUAC input is site-aware)"
        [ test "Burundi: entering 125 (mm) stores 12.5 cm" <|
            \_ ->
                let
                    ( model, _, _ ) =
                        updateChild SiteBurundi (SetMuac "125") emptyModelChild
                in
                model.ncdaData.form.muac
                    |> Expect.equal (Just (MuacInCm 12.5))
        , test "Rwanda: entering 12.5 (cm) stores 12.5 cm unchanged" <|
            \_ ->
                let
                    ( model, _, _ ) =
                        updateChild SiteRwanda (SetMuac "12.5") emptyModelChild
                in
                model.ncdaData.form.muac
                    |> Expect.equal (Just (MuacInCm 12.5))
        ]


outsideConstraintsTest : Test
outsideConstraintsTest =
    -- Guards the Save actions of every measurement form: a value that is absent,
    -- or outside the range printed above the input, must keep Save disabled.
    describe "outsideConstraints"
        [ test "a plausible height is inside the constraints" <|
            \_ ->
                outsideConstraints getInputConstraintsHeight (Just 105)
                    |> Expect.equal False
        , test "a mistyped height (1050 cm) is outside the constraints" <|
            \_ ->
                outsideConstraints getInputConstraintsHeight (Just 1050)
                    |> Expect.equal True
        , test "a mistyped weight (85 kg for a child) is outside the constraints" <|
            \_ ->
                outsideConstraints getInputConstraintsWeight (Just 850)
                    |> Expect.equal True
        , test "the range bounds themselves are inside the constraints" <|
            \_ ->
                ( outsideConstraints getInputConstraintsHeight (Just 25)
                , outsideConstraints getInputConstraintsHeight (Just 250)
                )
                    |> Expect.equal ( False, False )
        , test "a value just below the minimum is outside the constraints" <|
            \_ ->
                outsideConstraints getInputConstraintsHeight (Just 24.9)
                    |> Expect.equal True
        , test "an unset value is outside the constraints, so Save stays disabled" <|
            \_ ->
                outsideConstraints getInputConstraintsHeight Nothing
                    |> Expect.equal True
        ]


muacOutsideConstraintsTest : Test
muacOutsideConstraintsTest =
    -- MUAC is stored in cm, but its constraints are expressed in the unit shown
    -- to the nurse - mm at Burundi. Comparing the stored value directly would
    -- reject every legitimate Burundi measurement.
    describe "muacOutsideConstraints (site-aware)"
        [ test "Burundi: a stored 12.5 cm (125 mm) is inside the 50-999 mm range" <|
            \_ ->
                muacOutsideConstraints SiteBurundi (Just 12.5)
                    |> Expect.equal False
        , test "Burundi: a stored 0.4 cm (4 mm) is below the 50 mm minimum" <|
            \_ ->
                muacOutsideConstraints SiteBurundi (Just 0.4)
                    |> Expect.equal True
        , test "Rwanda: a stored 12.5 cm is inside the 5-99 cm range" <|
            \_ ->
                muacOutsideConstraints SiteRwanda (Just 12.5)
                    |> Expect.equal False
        , test "Rwanda: a stored 125 cm (mm typed into a cm field) is above the 99 cm maximum" <|
            \_ ->
                muacOutsideConstraints SiteRwanda (Just 125)
                    |> Expect.equal True
        , test "an unset MUAC is outside the constraints at either site" <|
            \_ ->
                ( muacOutsideConstraints SiteBurundi Nothing
                , muacOutsideConstraints SiteRwanda Nothing
                )
                    |> Expect.equal ( True, True )
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
        , updateChildSetMuacTest
        , outsideConstraintsTest
        , muacOutsideConstraintsTest
        ]
