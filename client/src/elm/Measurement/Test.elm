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
        , StuntingLevel(..)
        , TestExecutionNote(..)
        , VaccineDose(..)
        , WeightInGrm(..)
        , WeightInKg(..)
        , WellChildVaccineType(..)
        )
import Date exposing (Unit(..))
import EverySet
import Expect
import Measurement.Model exposing (AnthropometricMeasurement(..), MsgChild(..), NCDAStep(..), emptyCreatinineResultForm, emptyHeightForm, emptyLiverFunctionResultForm, emptyModelChild, emptyNCDAData, emptyNCDAForm)
import Measurement.Update exposing (updateChild)
import Measurement.Utils
    exposing
        ( birthWeightOutsideConstraints
        , creatinineResultFormWithDefault
        , getAllDosesForVaccine
        , getInputConstraintsHeight
        , getInputConstraintsMuac
        , getInputConstraintsWeight
        , getIntervalForVaccine
        , heightFormWithDefault
        , initialVaccinationDateByBirthDate
        , liverFunctionResultFormWithDefault
        , ncdaFormWithDefault
        , ncdaMeasurementsOutOfRange
        , outOfRangeAsEntered
        , setNCDAStep
        , showNCDAMeasurementOutOfRange
        )
import Measurement.View exposing (viewColorAlertIndication)
import SyncManager.Model exposing (Site(..))
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (classes, text)
import Time exposing (Month(..))
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


ncdaMeasurementsOutOfRangeTest : Test
ncdaMeasurementsOutOfRangeTest =
    -- The form may only be stopped over a measurement the nurse can actually
    -- reach. Each of these questions is asked only in some circumstances, and a
    -- value saved earlier stays on the form even once its question is gone, so
    -- a form stopped over one it does not show could not be left at all. Every
    -- condition is checked on its own here for that reason.
    let
        allSteps =
            [ NCDAStepAntenatalCare
            , NCDAStepNutritionAssessment
            , NCDAStepUniversalInterventions
            ]

        newbornExamWithoutBirthWeight =
            Nothing

        newbornExamWithBirthWeight =
            Just
                { expectedDateConcluded = Date.fromCalendarDate 2020 Jan 1
                , deliveryComplications = EverySet.empty
                , signs = EverySet.empty
                , apgarOneMin = Nothing
                , apgarFiveMin = Nothing
                , birthWeight = Just (WeightInGrm 3000)
                , birthLength = Nothing
                , birthDefects = EverySet.empty
                }
    in
    describe "ncdaMeasurementsOutOfRange"
        [ describe "the birth weight, asked on the Antenatal Care step"
            [ test "a weight in kilograms is named, on the step that asks it" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight True { emptyNCDAForm | birthWeight = Just (WeightInGrm 3) }
                        |> Expect.equal [ ( MeasurementBirthWeight, NCDAStepAntenatalCare ) ]
            , test "a weight in grams is not named" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight True { emptyNCDAForm | birthWeight = Just (WeightInGrm 3000) }
                        |> Expect.equal []
            , test "without that step it is not named, though the form still holds it" <|
                -- The step is dropped once an NCDA was filled before, and the
                -- weight saved then is still on the form. There is no field to
                -- correct it, so stopping here would leave the form unsavable.
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda [ NCDAStepNutritionAssessment ] newbornExamWithoutBirthWeight True { emptyNCDAForm | birthWeight = Just (WeightInGrm 3) }
                        |> Expect.equal []
            , test "when the newborn exam already recorded one, it is not asked and not named" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithBirthWeight True { emptyNCDAForm | birthWeight = Just (WeightInGrm 3) }
                        |> Expect.equal []
            ]
        , describe "the weight, asked on the Nutrition Assessment step"
            [ test "a weight in grams is named, on the step that asks it" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight True { emptyNCDAForm | weight = Just (WeightInKg 8500) }
                        |> Expect.equal [ ( MeasurementWeight, NCDAStepNutritionAssessment ) ]
            , test "a weight in kilograms is not named" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight True { emptyNCDAForm | weight = Just (WeightInKg 8.5) }
                        |> Expect.equal []
            , test "at a health centre that step is not asked, so it is not named" <|
                -- The step is only part of the form when the NCDA is filled by
                -- a CHW, and a weight saved earlier is still on the form.
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda [ NCDAStepAntenatalCare ] newbornExamWithoutBirthWeight True { emptyNCDAForm | weight = Just (WeightInKg 8500) }
                        |> Expect.equal []
            , test "a form holding no weight is not named" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight True emptyNCDAForm
                        |> Expect.equal []
            ]
        , describe "the MUAC, asked from six months of age"
            [ test "a MUAC in millimetres is named, on the step that asks it" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight True { emptyNCDAForm | muac = Just (MuacInCm 125) }
                        |> Expect.equal [ ( MeasurementMuac, NCDAStepNutritionAssessment ) ]
            , test "a MUAC in centimetres is not named" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight True { emptyNCDAForm | muac = Just (MuacInCm 12.5) }
                        |> Expect.equal []
            , test "under six months it is not asked, so it is not named" <|
                -- The exact regression this had three times before: the MUAC
                -- input is only drawn from six months, and gating on a value
                -- the form never shows deadened the button with nothing on
                -- screen to correct.
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight False { emptyNCDAForm | muac = Just (MuacInCm 125) }
                        |> Expect.equal []
            , test "Burundi: a form holding 12.5 cm is 125 mm there, so it is not named" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteBurundi allSteps newbornExamWithoutBirthWeight True { emptyNCDAForm | muac = Just (MuacInCm 12.5) }
                        |> Expect.equal []
            , test "Burundi: a form holding 1.25 cm is 12.5 mm there, below the range" <|
                \_ ->
                    ncdaMeasurementsOutOfRange SiteBurundi allSteps newbornExamWithoutBirthWeight True { emptyNCDAForm | muac = Just (MuacInCm 1.25) }
                        |> Expect.equal [ ( MeasurementMuac, NCDAStepNutritionAssessment ) ]
            ]
        , describe "saying a measurement could not be taken"
            [ test "empties it, so there is nothing left to be out of range" <|
                -- Ticking the box clears the value, and hydrating a saved form
                -- keeps it cleared. Were either to leave the old value behind,
                -- the form would be stopped by a measurement whose input is not
                -- even drawn.
                \_ ->
                    ncdaFormWithDefault
                        { emptyNCDAForm | weightNotTaken = Just True }
                        (Just
                            { signs = EverySet.empty
                            , birthWeight = Nothing
                            , ancVisitsDates = EverySet.empty
                            , receivesVitaminA = Nothing
                            , stuntingLevel = Nothing
                            , weight = Just (WeightInKg 8500)
                            , muac = Nothing
                            }
                        )
                        |> ncdaMeasurementsOutOfRange SiteRwanda allSteps newbornExamWithoutBirthWeight True
                        |> Expect.equal []
            ]
        , describe "several at once"
            [ test "all are named, in the order the form asks them" <|
                -- The nurse is told about every one, rather than correcting one
                -- and being stopped again by the next.
                \_ ->
                    ncdaMeasurementsOutOfRange SiteRwanda
                        allSteps
                        newbornExamWithoutBirthWeight
                        True
                        { emptyNCDAForm
                            | birthWeight = Just (WeightInGrm 3)
                            , weight = Just (WeightInKg 8500)
                            , muac = Just (MuacInCm 125)
                        }
                        |> Expect.equal
                            [ ( MeasurementBirthWeight, NCDAStepAntenatalCare )
                            , ( MeasurementWeight, NCDAStepNutritionAssessment )
                            , ( MeasurementMuac, NCDAStepNutritionAssessment )
                            ]
            ]
        ]


{-| The warning has to leave the nurse looking at the input it names.

Four pages hold this form and each moves it the same way, which is why the move
is said once and asked for here: dropping it on any one of them would name a
measurement asked on a step she is not on, and nothing else would notice.

-}
showNCDAMeasurementOutOfRangeTest : Test
showNCDAMeasurementOutOfRangeTest =
    let
        onStep step data =
            { data | form = (\form -> { form | step = Just step }) data.form }

        shownAndStep data =
            ( data.showMeasurementOutOfRangePopup, data.form.step )
    in
    describe "showNCDAMeasurementOutOfRange"
        [ test "showing it opens the form on the step that asks for the measurement" <|
            \_ ->
                emptyNCDAData
                    |> onStep NCDAStepInfrastructureEnvironment
                    |> showNCDAMeasurementOutOfRange (Just NCDAStepNutritionAssessment)
                    |> shownAndStep
                    |> Expect.equal ( True, Just NCDAStepNutritionAssessment )
        , test "hiding it leaves the form where it is" <|
            \_ ->
                emptyNCDAData
                    |> onStep NCDAStepNutritionAssessment
                    |> showNCDAMeasurementOutOfRange Nothing
                    |> shownAndStep
                    |> Expect.equal ( False, Just NCDAStepNutritionAssessment )
        , test "hiding it on a form that was never moved leaves it unmoved" <|
            \_ ->
                emptyNCDAData
                    |> showNCDAMeasurementOutOfRange Nothing
                    |> shownAndStep
                    |> Expect.equal ( False, Nothing )
        , test "setNCDAStep moves the form and touches nothing else" <|
            \_ ->
                setNCDAStep NCDAStepTargetedInterventions emptyNCDAData
                    |> shownAndStep
                    |> Expect.equal ( False, Just NCDAStepTargetedInterventions )
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
        , ncdaFormWithDefaultNotTakenTest
        , outOfRangeAsEnteredTest
        , updateChildOutOfRangePopupTest
        , ncdaMeasurementsOutOfRangeTest
        , showNCDAMeasurementOutOfRangeTest
        , birthWeightOutsideConstraintsTest
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


{-| The group session forms hold what the nurse typed, in the unit they show:
millimetres for MUAC at Burundi, centimetres elsewhere. The range they are
compared against is the one shown above the input, so the value is compared as
it is - converting here as well would report every MUAC at Burundi.
-}
outOfRangeAsEnteredTest : Test
outOfRangeAsEnteredTest =
    describe "outOfRangeAsEntered"
        [ test "Burundi: 125 mm is within the range shown there" <|
            \_ ->
                outOfRangeAsEntered (getInputConstraintsMuac SiteBurundi) MeasurementMuac 125
                    |> Expect.equal []
        , test "Rwanda: 125 is outside the range shown there, being millimetres" <|
            \_ ->
                outOfRangeAsEntered (getInputConstraintsMuac SiteRwanda) MeasurementMuac 125
                    |> Expect.equal [ MeasurementMuac ]
        , test "Rwanda: 12.5 cm is within the range shown there" <|
            \_ ->
                outOfRangeAsEntered (getInputConstraintsMuac SiteRwanda) MeasurementMuac 12.5
                    |> Expect.equal []
        , test "Burundi: 12.5 is outside the range shown there, being centimetres" <|
            \_ ->
                outOfRangeAsEntered (getInputConstraintsMuac SiteBurundi) MeasurementMuac 12.5
                    |> Expect.equal [ MeasurementMuac ]
        , test "a height of 1050 is outside its range, which is the same everywhere" <|
            \_ ->
                outOfRangeAsEntered getInputConstraintsHeight MeasurementHeight 1050
                    |> Expect.equal [ MeasurementHeight ]
        , test "a weight of 850 is outside its range" <|
            \_ ->
                outOfRangeAsEntered getInputConstraintsWeight MeasurementWeight 850
                    |> Expect.equal [ MeasurementWeight ]
        , test "both ends of a range are within it" <|
            \_ ->
                ( outOfRangeAsEntered getInputConstraintsHeight MeasurementHeight 25
                , outOfRangeAsEntered getInputConstraintsHeight MeasurementHeight 250
                )
                    |> Expect.equal ( [], [] )
        ]


{-| What the group session form asks for is named on a warning until the nurse
closes it.
-}
updateChildOutOfRangePopupTest : Test
updateChildOutOfRangePopupTest =
    describe "updateChild SetMeasurementOutOfRangePopupState"
        [ test "names the measurement the form asked for" <|
            \_ ->
                let
                    ( model, _, _ ) =
                        updateChild SiteRwanda
                            (SetMeasurementOutOfRangePopupState [ MeasurementMuac ])
                            emptyModelChild
                in
                model.measurementOutOfRangePopupState
                    |> Expect.equal [ MeasurementMuac ]
        , test "closing it names nothing, and saves nothing on the way" <|
            \_ ->
                let
                    ( model, _, outMsg ) =
                        updateChild SiteRwanda
                            (SetMeasurementOutOfRangePopupState [])
                            { emptyModelChild | measurementOutOfRangePopupState = [ MeasurementWeight ] }
                in
                ( model.measurementOutOfRangePopupState, outMsg )
                    |> Expect.equal ( [], Nothing )
        ]


{-| The Child Scorecard is filled over several steps and saved only at the end,
so what the form holds between them is what gets saved. Ticking "measurement
not taken" empties the input and hides it; the measurement saved at an earlier
encounter must not come back in its place, or it is live behind a box saying it
was not taken, and saved again at the end.
-}
ncdaFormWithDefaultNotTakenTest : Test
ncdaFormWithDefaultNotTakenTest =
    let
        saved =
            { signs = EverySet.empty
            , birthWeight = Nothing
            , ancVisitsDates = EverySet.empty
            , receivesVitaminA = Nothing
            , stuntingLevel = Just LevelYellow
            , weight = Just (WeightInKg 12)
            , muac = Just (MuacInCm 14)
            }

        emptyForm =
            emptyNCDAData.form

        hydrate form =
            ncdaFormWithDefault form (Just saved)
    in
    describe "ncdaFormWithDefault, on a measurement said not to have been taken"
        [ test "the weight saved earlier does not come back" <|
            \_ ->
                hydrate { emptyForm | weightNotTaken = Just True }
                    |> .weight
                    |> Expect.equal Nothing
        , test "the MUAC saved earlier does not come back" <|
            \_ ->
                hydrate { emptyForm | muacNotTaken = Just True }
                    |> .muac
                    |> Expect.equal Nothing
        , test "the stunting level saved earlier does not come back" <|
            \_ ->
                hydrate { emptyForm | stuntingLevelNotTaken = Just True }
                    |> .stuntingLevel
                    |> Expect.equal Nothing
        , test "saying one was not taken leaves the others alone" <|
            \_ ->
                let
                    form =
                        hydrate { emptyForm | weightNotTaken = Just True }
                in
                ( form.weight, form.muac, form.stuntingLevel )
                    |> Expect.equal ( Nothing, Just (MuacInCm 14), Just LevelYellow )
        , test "a form nobody has touched still inherits what was saved" <|
            \_ ->
                let
                    form =
                        hydrate emptyForm
                in
                ( form.weight, form.muac, form.stuntingLevel )
                    |> Expect.equal ( Just (WeightInKg 12), Just (MuacInCm 14), Just LevelYellow )
        , test "what the nurse has typed is kept over what was saved" <|
            \_ ->
                hydrate { emptyForm | weight = Just (WeightInKg 13) }
                    |> .weight
                    |> Expect.equal (Just (WeightInKg 13))
        ]
