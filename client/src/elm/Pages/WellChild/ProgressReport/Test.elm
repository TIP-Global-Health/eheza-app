module Pages.WellChild.ProgressReport.Test exposing (all)

import AssocList as Dict
import Backend.Measurement.Model exposing (NCDASign(..), NCDAValue, ReceiveOption(..))
import Backend.Person.Model exposing (Person)
import Date
import EverySet
import Expect
import Gizra.NominalDate exposing (NominalDate)
import List.Extra
import Pages.WellChild.ProgressReport.Model exposing (NCDACellValue(..))
import Pages.WellChild.ProgressReport.View exposing (generateUniversalInterventionsValues)
import Test exposing (Test, describe, test)
import TestFixtures exposing (testChild)
import Time


all : Test
all =
    describe "Pages.WellChild.ProgressReport"
        [ universalInterventionsValuesTests ]



-- FIXTURES


currentDate : NominalDate
currentDate =
    Date.fromCalendarDate 2020 Time.Jun 1


{-| A child that is 24 months old at `currentDate`, so that all 25 columns of
the scorecard hold a resolved value, rather than being blank for not being
reached yet.
-}
child : Person
child =
    testChild (Date.add Date.Months -24 currentDate)


emptyNCDAValue : NCDAValue
emptyNCDAValue =
    { signs = EverySet.empty
    , birthWeight = Nothing
    , ancVisitsDates = EverySet.empty
    , receivesVitaminA = Nothing
    , stuntingLevel = Nothing
    , weight = Nothing
    , muac = Nothing
    }


{-| Questionnaire as nurse fills it at health center. At universal
interventions, nurse is asked only whether Ongera-MNP was distributed, so
this one reports no intervention at all. The nutrition behavior sign is there
to make the point that the questionnaire is not empty - it simply holds no
answer for Vitamin A, Dewormer and ECD, because these questions were not asked.
-}
nurseQuestionnaire : NCDAValue
nurseQuestionnaire =
    { emptyNCDAValue | signs = EverySet.singleton AppropriateComplementaryFeeding }


{-| Questionnaire as CHW fills it at Child Scoreboard encounter, reporting that
all universal interventions were provided.
-}
chwQuestionnaire : NCDAValue
chwQuestionnaire =
    { emptyNCDAValue
        | signs = EverySet.fromList [ ChildReceivesDewormer, ChildReceivesECD ]
        , receivesVitaminA = Just OptionReceive
    }


cellAtMonth : Int -> List NCDACellValue -> Maybe NCDACellValue
cellAtMonth =
    List.Extra.getAt



-- TESTS


{-| The scenarios mirror issue #1925: Vitamin A, Dewormer and ECD are asked
only at the CHW questionnaire, but Dewormer and ECD used to be resolved from
the nurse + CHW merge that Ongera-MNP legitimately needs. A month holding a
nurse questionnaire alone therefore reported the interventions as not provided
(red X), where Vitamin A - resolved from CHW questionnaires alone - correctly
reported no data (dash).
-}
universalInterventionsValuesTests : Test
universalInterventionsValuesTests =
    let
        valuesFor nurseQuestionnaires chwQuestionnaires =
            generateUniversalInterventionsValues currentDate child (Just nurseQuestionnaires) (Just chwQuestionnaires)
    in
    describe "generateUniversalInterventionsValues"
        [ test "a month holding a nurse questionnaire alone reports no data for the interventions nurse is not asked about" <|
            \_ ->
                let
                    values =
                        valuesFor (Dict.singleton 18 nurseQuestionnaire) Dict.empty
                in
                [ cellAtMonth 18 values.vitaminA
                , cellAtMonth 18 values.dewormer
                , cellAtMonth 18 values.ecd
                ]
                    |> Expect.equal
                        [ Just NCDACellValueDash
                        , Just NCDACellValueDash
                        , Just NCDACellValueDash
                        ]
        , test "a month holding a nurse questionnaire alone does report Ongera-MNP, which nurse is asked about" <|
            \_ ->
                valuesFor (Dict.singleton 18 nurseQuestionnaire) Dict.empty
                    |> .ongeraMNP
                    |> cellAtMonth 18
                    |> Expect.equal (Just NCDACellValueX)
        , test "a nurse questionnaire reporting Ongera-MNP distribution is filtered out, since we can't tell whether it was consumed" <|
            \_ ->
                valuesFor
                    (Dict.singleton 18 { nurseQuestionnaire | signs = EverySet.singleton OngeraMNP })
                    Dict.empty
                    |> .ongeraMNP
                    |> cellAtMonth 18
                    |> Expect.equal (Just NCDACellValueDash)
        , test "a month holding a CHW questionnaire reports the interventions it recorded as provided" <|
            \_ ->
                let
                    values =
                        valuesFor Dict.empty (Dict.singleton 20 chwQuestionnaire)
                in
                [ cellAtMonth 20 values.vitaminA
                , cellAtMonth 20 values.dewormer
                , cellAtMonth 20 values.ecd
                ]
                    |> Expect.equal
                        [ Just NCDACellValueV
                        , Just NCDACellValueV
                        , Just NCDACellValueV
                        ]
        , test "a CHW questionnaire recording no intervention still reports them as not provided" <|
            \_ ->
                let
                    values =
                        valuesFor Dict.empty
                            (Dict.singleton 20 { emptyNCDAValue | receivesVitaminA = Just OptionNotReceive })
                in
                [ cellAtMonth 20 values.vitaminA
                , cellAtMonth 20 values.dewormer
                , cellAtMonth 20 values.ecd
                ]
                    |> Expect.equal
                        [ Just NCDACellValueX
                        , Just NCDACellValueX
                        , Just NCDACellValueX
                        ]
        , test "for a month holding both questionnaires, CHW data is the one reported" <|
            \_ ->
                let
                    values =
                        valuesFor (Dict.singleton 20 nurseQuestionnaire)
                            (Dict.singleton 20 chwQuestionnaire)
                in
                [ cellAtMonth 20 values.dewormer
                , cellAtMonth 20 values.ecd
                ]
                    |> Expect.equal
                        [ Just NCDACellValueV
                        , Just NCDACellValueV
                        ]
        , test "eligibility by age is preserved - Vitamin A before 6 months, Dewormer before 12" <|
            \_ ->
                let
                    values =
                        valuesFor Dict.empty
                            (Dict.singleton 3 { emptyNCDAValue | receivesVitaminA = Just OptionNotReceive })
                in
                [ cellAtMonth 3 values.vitaminA
                , cellAtMonth 3 values.dewormer
                ]
                    |> Expect.equal
                        [ Just NCDACellValueDash
                        , Just NCDACellValueDash
                        ]
        ]
