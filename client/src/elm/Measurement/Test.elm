module Measurement.Test exposing (all)

import AssocList as Dict
import Backend.Measurement.Model
    exposing
        ( ColorAlertIndication(..)
        , CreatinineTestValue
        , HeightInCm(..)
        , LiverFunctionTestValue
        , MuacInCm(..)
        , SkippedForm(..)
        , TestExecutionNote(..)
        , VaccineDose(..)
        , WeightInGrm(..)
        , WellChildVaccineType(..)
        )
import Date exposing (Unit(..))
import EverySet
import Expect
import Measurement.Model exposing (MsgChild(..), NCDAStep(..), emptyCreatinineResultForm, emptyHeightForm, emptyLiverFunctionResultForm, emptyModelChild)
import Measurement.Update exposing (updateChild)
import Measurement.Utils
    exposing
        ( birthWeightBlocksNCDAForm
        , birthWeightOutsideConstraints
        , creatinineResultFormWithDefault
        , getAllDosesForVaccine
        , getInputConstraintsHeight
        , getInputConstraintsWeight
        , getIntervalForVaccine
        , heightFormWithDefault
        , initialVaccinationDateByBirthDate
        , liverFunctionResultFormWithDefault
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


birthWeightBlocksNCDAFormTest : Test
birthWeightBlocksNCDAFormTest =
    -- The form may only be stopped over a weight the nurse can actually reach.
    -- The weight is asked on the Antenatal Care step, and only when the newborn
    -- exam did not already record it; a form stopped over a weight it does not
    -- show could not be left at all.
    let
        allSteps =
            [ NCDAStepAntenatalCare, NCDAStepUniversalInterventions ]

        withoutAntenatalCare =
            [ NCDAStepUniversalInterventions ]

        newbornExamWithoutBirthWeight =
            Nothing
    in
    describe "birthWeightBlocksNCDAForm"
        [ test "a weight in kilograms on a form that asks for it is stopped" <|
            \_ ->
                birthWeightBlocksNCDAForm SiteRwanda allSteps newbornExamWithoutBirthWeight (Just (WeightInGrm 3))
                    |> Expect.equal True
        , test "a weight in grams is not stopped" <|
            \_ ->
                birthWeightBlocksNCDAForm SiteRwanda allSteps newbornExamWithoutBirthWeight (Just (WeightInGrm 3000))
                    |> Expect.equal False
        , test "a form without the Antenatal Care step is never stopped" <|
            -- The step is dropped once an NCDA was filled before, and the weight
            -- saved then is still on the form. There is no field to correct it,
            -- so stopping here would leave the form impossible to save.
            \_ ->
                birthWeightBlocksNCDAForm SiteRwanda withoutAntenatalCare newbornExamWithoutBirthWeight (Just (WeightInGrm 3))
                    |> Expect.equal False
        , test "a form with no weight entered is not stopped" <|
            \_ ->
                birthWeightBlocksNCDAForm SiteRwanda allSteps newbornExamWithoutBirthWeight Nothing
                    |> Expect.equal False
        ]


birthWeightOutsideConstraintsTest : Test
birthWeightOutsideConstraintsTest =
    -- Birth weight is stored in grams, but is often typed in kilograms. The
    -- kilogram value lands inside the range a weight in grams cannot be, which
    -- is what this reports so that the form can say so and refuse to save.
    describe "birthWeightOutsideConstraints"
        [ test "an ordinary birth weight in grams is inside the range" <|
            \_ ->
                birthWeightOutsideConstraints SiteRwanda (Just (WeightInGrm 3000))
                    |> Expect.equal False
        , test "a genuinely low birth weight is still inside the range" <|
            \_ ->
                birthWeightOutsideConstraints SiteRwanda (Just (WeightInGrm 1200))
                    |> Expect.equal False
        , test "the same weight typed in kilograms is outside the range" <|
            \_ ->
                birthWeightOutsideConstraints SiteRwanda (Just (WeightInGrm 3))
                    |> Expect.equal True
        , test "a weight far above what a newborn can be is outside the range" <|
            \_ ->
                birthWeightOutsideConstraints SiteRwanda (Just (WeightInGrm 350022))
                    |> Expect.equal True
        , test "the extremes that do occur are accepted" <|
            -- Babies have survived under 500g, and a very large baby can be
            -- over 6000g. Both have to be recordable as they are.
            \_ ->
                ( birthWeightOutsideConstraints SiteRwanda (Just (WeightInGrm 300))
                , birthWeightOutsideConstraints SiteRwanda (Just (WeightInGrm 7000))
                )
                    |> Expect.equal ( False, False )
        , test "just outside either end is refused" <|
            \_ ->
                ( birthWeightOutsideConstraints SiteRwanda (Just (WeightInGrm 299))
                , birthWeightOutsideConstraints SiteRwanda (Just (WeightInGrm 7001))
                )
                    |> Expect.equal ( True, True )
        , test "a weight that has not been entered is not reported" <|
            -- Whether the measurement still has to be taken is answered by the
            -- task count, not here.
            \_ ->
                birthWeightOutsideConstraints SiteRwanda Nothing
                    |> Expect.equal False
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
        , test "a grossly mistyped weight (850 kg) is outside the constraints" <|
            \_ ->
                outsideConstraints getInputConstraintsWeight (Just 850)
                    |> Expect.equal True
        , test "the weight range is wide (0.5-200 kg), so an implausible 85 kg for a child still passes" <|
            -- The constraints are a typo guard, not a plausibility check: the
            -- weight form is shared with adult encounters. A dropped decimal
            -- (8.5 -> 85) is therefore NOT caught here.
            \_ ->
                outsideConstraints getInputConstraintsWeight (Just 85)
                    |> Expect.equal False
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


{-| A saved creatinine test with both results filled in, so the tests can check
what happens when the nurse clears one and re-opens the recurrent encounter.
-}
savedCreatinineTest : CreatinineTestValue
savedCreatinineTest =
    { executionNote = TestNoteRunToday
    , executionDate = Just (Date.fromCalendarDate 2024 Time.Jan 1)
    , creatinineResult = Just 1.2
    , bunResult = Just 15
    }


creatinineResultFormWithDefaultTest : Test
creatinineResultFormWithDefaultTest =
    let
        resolve form =
            creatinineResultFormWithDefault form (Just savedCreatinineTest)
    in
    describe "creatinineResultFormWithDefault"
        [ test "a cleared creatinine result stays cleared when the field is dirty" <|
            \_ ->
                resolve { emptyCreatinineResultForm | creatinineResultDirty = True }
                    |> .creatinineResult
                    |> Expect.equal Nothing
        , test "an untouched creatinine result loads from the saved value" <|
            \_ ->
                resolve emptyCreatinineResultForm
                    |> .creatinineResult
                    |> Expect.equal (Just 1.2)
        , test "an edited creatinine result wins over the saved value" <|
            \_ ->
                resolve { emptyCreatinineResultForm | creatinineResultDirty = True, creatinineResult = Just 2.5 }
                    |> .creatinineResult
                    |> Expect.equal (Just 2.5)
        , test "a cleared BUN result stays cleared when the field is dirty" <|
            \_ ->
                resolve { emptyCreatinineResultForm | bunResultDirty = True }
                    |> .bunResult
                    |> Expect.equal Nothing
        ]


{-| A saved liver function test with both results filled in, for the same
clear-then-reopen check.
-}
savedLiverFunctionTest : LiverFunctionTestValue
savedLiverFunctionTest =
    { executionNote = TestNoteRunToday
    , executionDate = Just (Date.fromCalendarDate 2024 Time.Jan 1)
    , altResult = Just 30
    , astResult = Just 25
    }


liverFunctionResultFormWithDefaultTest : Test
liverFunctionResultFormWithDefaultTest =
    let
        resolve form =
            liverFunctionResultFormWithDefault form (Just savedLiverFunctionTest)
    in
    describe "liverFunctionResultFormWithDefault"
        [ test "a cleared ALT result stays cleared when the field is dirty" <|
            \_ ->
                resolve { emptyLiverFunctionResultForm | altResultDirty = True }
                    |> .altResult
                    |> Expect.equal Nothing
        , test "an untouched ALT result loads from the saved value" <|
            \_ ->
                resolve emptyLiverFunctionResultForm
                    |> .altResult
                    |> Expect.equal (Just 30)
        , test "a cleared AST result stays cleared when the field is dirty" <|
            \_ ->
                resolve { emptyLiverFunctionResultForm | astResultDirty = True }
                    |> .astResult
                    |> Expect.equal Nothing
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
        , birthWeightBlocksNCDAFormTest
        , birthWeightOutsideConstraintsTest
        , outsideConstraintsTest
        , muacOutsideConstraintsTest
        , heightFormWithDefaultSkippedTest
        , creatinineResultFormWithDefaultTest
        , liverFunctionResultFormWithDefaultTest
        ]


{-| The range check asks the form the nurse is looking at, which is why it does
not have to ask separately whether the measurement could be taken: a form that
was skipped holds no height to be out of range.
-}
heightFormWithDefaultSkippedTest : Test
heightFormWithDefaultSkippedTest =
    describe "heightFormWithDefault, on a measurement that could not be taken"
        [ test "holds no height when the encounter says the form was skipped" <|
            \_ ->
                heightFormWithDefault (EverySet.singleton SkippedHeight)
                    emptyHeightForm
                    (Just (HeightInCm 1050))
                    |> Expect.equal
                        { height = Nothing
                        , heightDirty = False
                        , measurementNotTaken = Just True
                        }
        , test "holds no height when the nurse said so on the form" <|
            \_ ->
                heightFormWithDefault EverySet.empty
                    { emptyHeightForm | height = Just 1050, measurementNotTaken = Just True }
                    Nothing
                    |> Expect.equal
                        { height = Nothing
                        , heightDirty = False
                        , measurementNotTaken = Just True
                        }
        ]
