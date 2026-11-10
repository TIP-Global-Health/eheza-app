module Pages.WellChild.ProgressReport.Test exposing (all)

import AssocList as Dict
import Backend.Entities exposing (WellChildEncounterId)
import Backend.IndividualEncounterParticipant.Model
import Backend.Measurement.Model exposing (HeadCircumferenceInCm(..), HeadCircumferenceValue, HeightInCm(..), MeasurementNote(..), MuacInCm(..), NCDASign(..), NCDAValue, ReceiveOption(..), WeightInKg(..), WellChildMeasurements)
import Backend.Person.Model exposing (Person)
import Backend.WellChildEncounter.Encoder exposing (encodeEncounterWarning)
import Backend.WellChildEncounter.Model exposing (EncounterWarning(..), PediatricCareMilestone(..), WellChildEncounter, WellChildEncounterType(..), emptyWellChildEncounter)
import Date
import EverySet
import Expect
import Gizra.NominalDate exposing (NominalDate)
import Json.Encode
import List.Extra
import Pages.WellChild.Encounter.Model exposing (AssembledData)
import Pages.WellChild.ProgressReport.Model exposing (NCDACellValue(..))
import Pages.WellChild.ProgressReport.View exposing (generatePartitionedWarningEntries, generateUniversalInterventionsValues, heightCellValuesByAgeInMonths, malnutritionTreatmentsByAgeInMonthsFromQuestionnaires, weightCellValuesByAgeInMonths)
import Restful.Endpoint exposing (toEntityUuid)
import Test exposing (Test, describe, test)
import TestFixtures exposing (emptyWellChildMeasurements, testChild, testParticipant, wrapMeasurement)
import Time
import ZScore.Test


all : Test
all =
    describe "Pages.WellChild.ProgressReport"
        [ ecdWarningEntriesTests
        , fillTheBlanksAgeTests
        , headCircumferenceWarningEntriesTests
        , malnutritionTreatmentValuesTests
        , universalInterventionsValuesTests
        ]


{-| Fill the Blanks grades a measurement against the child's age on the day it
was taken, the way the growth charts do. Both values below are typical for a
girl of three months, so both cells are green - and stay green as the child
grows. Read at the child's age today (24 months), the same values would be
far below -3SD, and both cells would come out red.
-}
fillTheBlanksAgeTests : Test
fillTheBlanksAgeTests =
    let
        dateMeasured =
            Date.add Date.Months 3 birthDate
    in
    describe "fill the blanks cell values"
        [ test "height typical for the age it was taken at is green" <|
            \_ ->
                heightCellValuesByAgeInMonths ZScore.Test.testModel child [ ( dateMeasured, { height = Just (HeightInCm 59.8) } ) ]
                    |> Maybe.andThen (Dict.get 3)
                    |> Maybe.map Tuple.second
                    |> Expect.equal (Just NCDACellValueC)
        , test "weight typical for the age it was taken at is green" <|
            \_ ->
                weightCellValuesByAgeInMonths ZScore.Test.testModel child [ ( dateMeasured, { weight = Just (WeightInKg 5.8) } ) ]
                    |> Maybe.andThen (Dict.get 3)
                    |> Maybe.map Tuple.second
                    |> Expect.equal (Just NCDACellValueC)
        ]



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
    testChild birthDate


birthDate : NominalDate
birthDate =
    Date.add Date.Months -24 currentDate


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


{-| The CHW is asked whether the child is treated for acute malnutrition only
when the MUAC typed on the same questionnaire is off. A questionnaire that
never asked holds no answer, and is no data rather than "not treated".
-}
malnutritionTreatmentValuesTests : Test
malnutritionTreatmentValuesTests =
    let
        monthTen =
            Date.add Date.Months 10 birthDate

        cellFor questionnaires =
            malnutritionTreatmentsByAgeInMonthsFromQuestionnaires child questionnaires
                |> Maybe.andThen (Dict.get 10)
                |> Maybe.map Tuple.second

        greenMuac =
            Just (MuacInCm 14)

        offMuac =
            Just (MuacInCm 11)

        treated =
            EverySet.singleton TreatedForAcuteMalnutrition
    in
    describe "malnutritionTreatmentsByAgeInMonthsFromQuestionnaires"
        [ test "a green MUAC never asked the question, so the month has no data" <|
            \_ ->
                cellFor [ ( monthTen, { emptyNCDAValue | muac = greenMuac } ) ]
                    |> Expect.equal Nothing
        , test "a questionnaire without a MUAC never asked the question, so the month has no data" <|
            \_ ->
                cellFor [ ( monthTen, emptyNCDAValue ) ]
                    |> Expect.equal Nothing
        , test "an off MUAC with no treatment recorded reads as not treated" <|
            \_ ->
                cellFor [ ( monthTen, { emptyNCDAValue | muac = offMuac } ) ]
                    |> Expect.equal (Just NCDACellValueX)
        , test "a recorded treatment reads as treated" <|
            \_ ->
                cellFor [ ( monthTen, { emptyNCDAValue | muac = offMuac, signs = treated } ) ]
                    |> Expect.equal (Just NCDACellValueV)
        , test "a treatment recorded before the MUAC was corrected to green reads as not asked" <|
            \_ ->
                cellFor [ ( monthTen, { emptyNCDAValue | muac = greenMuac, signs = treated } ) ]
                    |> Expect.equal Nothing
        , test "a month's answer is kept when a later questionnaire that month did not ask" <|
            \_ ->
                cellFor
                    [ ( Date.add Date.Days 2 monthTen, { emptyNCDAValue | muac = offMuac } )
                    , ( Date.add Date.Days 19 monthTen, { emptyNCDAValue | muac = greenMuac } )
                    ]
                    |> Expect.equal (Just NCDACellValueX)
        ]


{-| One Well Child visit, as the progress report sees it. A visit is placed by
the child's age at it, which sets its date, and that same date is used
everywhere the visit shows up: the encounter's start date, the date the head
circumference was measured, and the date in the previous-measurements rows.

`headCircumference` is what the visit stored for that activity:

  - `Nothing` - the visit did not include head circumference at all.
  - a value with a real reading - the nurse measured it. Whether it was in range
    is told by the visit's `warnings`: a reading within range records no head
    circumference warning.
  - `notTakenValue` - the nurse marked it "not taken", which stores 0 cm and a
    note, alongside a `NoHeadCircumferenceWarning` on the visit.

-}
type alias Visit =
    { monthsOld : Int
    , warnings : List EncounterWarning
    , headCircumference : Maybe HeadCircumferenceValue
    }


readingValue : Maybe HeadCircumferenceValue
readingValue =
    Just
        { headCircumference = HeadCircumferenceInCm 45
        , notes = EverySet.singleton NoMeasurementNotes
        }


notTakenValue : Maybe HeadCircumferenceValue
notTakenValue =
    Just
        { headCircumference = HeadCircumferenceInCm 0
        , notes = EverySet.singleton NoteNotTaken
        }


microcephalyAt : Int -> Visit
microcephalyAt monthsOld =
    Visit monthsOld [ WarningHeadCircumferenceMicrocephaly ] readingValue


macrocephalyAt : Int -> Visit
macrocephalyAt monthsOld =
    Visit monthsOld [ WarningHeadCircumferenceMacrocephaly ] readingValue


{-| The encounter keeps its initial "no warnings" value: the reading was within
range, and ECD was not done at this visit.
-}
inRangeAt : Int -> Visit
inRangeAt monthsOld =
    Visit monthsOld [ NoEncounterWarnings ] readingValue


withoutHeadCircumferenceAt : Int -> Visit
withoutHeadCircumferenceAt monthsOld =
    Visit monthsOld [ NoEncounterWarnings ] Nothing


notTakenAt : Int -> Visit
notTakenAt monthsOld =
    Visit monthsOld [ NoHeadCircumferenceWarning ] notTakenValue


visitDate : Visit -> NominalDate
visitDate visit =
    monthsOldDate visit.monthsOld


monthsOldDate : Int -> NominalDate
monthsOldDate monthsOld =
    Date.add Date.Months monthsOld birthDate


visitId : Visit -> WellChildEncounterId
visitId visit =
    toEntityUuid ("encounter-at-" ++ String.fromInt visit.monthsOld)


visitEncounter : Visit -> ( WellChildEncounterId, WellChildEncounter )
visitEncounter visit =
    let
        encounter =
            emptyWellChildEncounter (toEntityUuid "participant") (visitDate visit) PediatricCare Nothing
    in
    ( visitId visit, { encounter | encounterWarnings = EverySet.fromList visit.warnings } )


visitMeasurements : Visit -> WellChildMeasurements
visitMeasurements visit =
    { emptyWellChildMeasurements
        | headCircumference = Maybe.andThen (wrapMeasurement (visitDate visit)) visit.headCircumference
    }


{-| Assembled data of the visit the report is opened from. Its measurements are
the "current" ones, every other visit lands in the previous measurements most
recent first - the shape `generatePreviousMeasurements` produces.
-}
assembledFrom : Visit -> List Visit -> AssembledData
assembledFrom reportVisit visits =
    let
        ( id, encounter ) =
            visitEncounter reportVisit
    in
    { id = id
    , encounter = encounter
    , participant = testParticipant birthDate Backend.IndividualEncounterParticipant.Model.WellChildEncounter
    , person = child
    , measurements = visitMeasurements reportVisit
    , previousMeasurementsWithDates =
        List.filter (\visit -> visit.monthsOld /= reportVisit.monthsOld) visits
            |> List.sortBy (.monthsOld >> negate)
            |> List.map (\visit -> ( visitDate visit, ( visitId visit, visitMeasurements visit ) ))
    , vaccinationHistory = Dict.empty
    , vaccinationProgress = Dict.empty
    }


{-| Partition with the report opened from the latest visit, which is what the
report does when opened from the patient record.
-}
partitionVisits : List Visit -> ( List WarningEntry, List WarningEntry )
partitionVisits visits =
    generatePartitionedWarningEntries
        (List.map visitEncounter visits)
        (List.Extra.maximumBy .monthsOld visits
            |> Maybe.map (\latest -> assembledFrom latest visits)
        )


type alias WarningEntry =
    ( NominalDate, PediatricCareMilestone, EncounterWarning )


{-| Comparable form of an entry: date and warning. Entries are compared
regardless of their order, and the milestone of head circumference entries has
no meaning, so it is left out. Tests about ECD assert the milestone directly.
-}
entryKey : WarningEntry -> ( String, String )
entryKey ( date, _, warning ) =
    ( Date.toIsoString date, Json.Encode.encode 0 (encodeEncounterWarning warning) )


expectedKey : ( Int, EncounterWarning ) -> ( String, String )
expectedKey ( monthsOld, warning ) =
    entryKey ( monthsOldDate monthsOld, Milestone4Years, warning )


expectPlacement :
    { active : List ( Int, EncounterWarning ), prior : List ( Int, EncounterWarning ) }
    -> ( List WarningEntry, List WarningEntry )
    -> Expect.Expectation
expectPlacement expected ( active, prior ) =
    Expect.equal
        ( List.sort (List.map expectedKey expected.active)
        , List.sort (List.map expectedKey expected.prior)
        )
        ( List.sort (List.map entryKey active)
        , List.sort (List.map entryKey prior)
        )


{-| The first list of the result is Active Diagnosis, shown as Ongoing, and the
second is Prior Diagnosis, shown as Resolved. A head circumference warning
stays Active until a later visit answers it, with a reading in range or a
"not taken" mark. The scenarios are placed by the child's age in months.
-}
headCircumferenceWarningEntriesTests : Test
headCircumferenceWarningEntriesTests =
    describe "generatePartitionedWarningEntries - head circumference warnings"
        [ describe "A child whose latest head circumference gave macrocephaly or microcephaly shows that warning under Active Diagnosis, as Ongoing"
            [ test "microcephaly at the child's first visit is under Active" <|
                \_ ->
                    partitionVisits [ microcephalyAt 6 ]
                        |> expectPlacement
                            { active = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ], prior = [] }
            , test "macrocephaly at the child's first visit is under Active" <|
                \_ ->
                    partitionVisits [ macrocephalyAt 6 ]
                        |> expectPlacement
                            { active = [ ( 6, WarningHeadCircumferenceMacrocephaly ) ], prior = [] }
            , test "a reading in range before the warning does not touch it" <|
                \_ ->
                    partitionVisits [ inRangeAt 3, microcephalyAt 6 ]
                        |> expectPlacement
                            { active = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ], prior = [] }
            , test "a warning whose reading did not get saved is under Active" <|
                -- The warning is written when the popup opens, and the
                -- measurement is saved only when that popup is closed.
                \_ ->
                    partitionVisits [ Visit 6 [ WarningHeadCircumferenceMicrocephaly ] Nothing ]
                        |> expectPlacement
                            { active = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ], prior = [] }
            , test "a head circumference warning needs no birth date to be shown" <|
                \_ ->
                    let
                        visit =
                            microcephalyAt 6

                        assembled =
                            assembledFrom visit [ visit ]
                    in
                    generatePartitionedWarningEntries [ visitEncounter visit ]
                        (Just { assembled | person = { child | birthDate = Nothing } })
                        |> expectPlacement
                            { active = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ], prior = [] }
            ]
        , describe "If a later visit records a reading in the normal range, the earlier warning moves to Prior Diagnosis as Resolved, and Active no longer lists it"
            [ test "microcephaly moves to Prior" <|
                \_ ->
                    partitionVisits [ microcephalyAt 6, inRangeAt 9 ]
                        |> expectPlacement
                            { active = [], prior = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ] }
            , test "macrocephaly moves to Prior" <|
                \_ ->
                    partitionVisits [ macrocephalyAt 6, inRangeAt 9 ]
                        |> expectPlacement
                            { active = [], prior = [ ( 6, WarningHeadCircumferenceMacrocephaly ) ] }
            , test "every earlier warning moves to Prior" <|
                \_ ->
                    partitionVisits [ microcephalyAt 3, macrocephalyAt 6, inRangeAt 9 ]
                        |> expectPlacement
                            { active = []
                            , prior =
                                [ ( 3, WarningHeadCircumferenceMicrocephaly )
                                , ( 6, WarningHeadCircumferenceMacrocephaly )
                                ]
                            }
            , test "the visits may arrive in any order" <|
                \_ ->
                    partitionVisits [ inRangeAt 9, microcephalyAt 6, inRangeAt 3 ]
                        |> expectPlacement
                            { active = [], prior = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ] }
            , test "a report opened from the visit of the warning still sees the later reading" <|
                -- The report of an earlier visit has that visit as the current
                -- one, and the later visits among the previous measurements.
                \_ ->
                    let
                        warningVisit =
                            microcephalyAt 6

                        visits =
                            [ warningVisit, inRangeAt 9 ]
                    in
                    generatePartitionedWarningEntries
                        (List.map visitEncounter visits)
                        (Just (assembledFrom warningVisit visits))
                        |> expectPlacement
                            { active = [], prior = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ] }
            , test "a reading whose notes were stored empty counts as a reading" <|
                \_ ->
                    partitionVisits
                        [ microcephalyAt 6
                        , Visit 9
                            [ NoEncounterWarnings ]
                            (Just { headCircumference = HeadCircumferenceInCm 45, notes = EverySet.empty })
                        ]
                        |> expectPlacement
                            { active = [], prior = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ] }
            , describe "whatever else the in-range visit stored" <|
                List.map
                    (\( label, warnings ) ->
                        test label <|
                            \_ ->
                                partitionVisits [ microcephalyAt 6, Visit 9 warnings readingValue ]
                                    |> Tuple.second
                                    |> List.map entryKey
                                    |> Expect.equal [ expectedKey ( 6, WarningHeadCircumferenceMicrocephaly ) ]
                    )
                    [ ( "an empty set of warnings", [] )
                    , ( "ECD completed with no warning", [ NoECDMilstoneWarning ] )
                    , ( "a missed ECD milestone", [ WarningECDMilestoneBehind ] )
                    ]
            ]
        , describe "If the warning comes back at a later visit, that visit's warning is under Active and the older ones are under Prior"
            [ test "the same warning comes back" <|
                \_ ->
                    partitionVisits [ microcephalyAt 6, inRangeAt 9, microcephalyAt 12 ]
                        |> expectPlacement
                            { active = [ ( 12, WarningHeadCircumferenceMicrocephaly ) ]
                            , prior = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ]
                            }
            , test "the other warning comes back" <|
                \_ ->
                    partitionVisits [ microcephalyAt 6, inRangeAt 9, macrocephalyAt 12 ]
                        |> expectPlacement
                            { active = [ ( 12, WarningHeadCircumferenceMacrocephaly ) ]
                            , prior = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ]
                            }
            , test "the warning that came back stays under Active through a visit without head circumference" <|
                \_ ->
                    partitionVisits [ microcephalyAt 6, inRangeAt 9, microcephalyAt 12, withoutHeadCircumferenceAt 15 ]
                        |> expectPlacement
                            { active = [ ( 12, WarningHeadCircumferenceMicrocephaly ) ]
                            , prior = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ]
                            }
            , test "a warning that came back and then got a reading in range leaves nothing under Active" <|
                \_ ->
                    partitionVisits [ microcephalyAt 6, inRangeAt 9, macrocephalyAt 12, inRangeAt 15 ]
                        |> expectPlacement
                            { active = []
                            , prior =
                                [ ( 6, WarningHeadCircumferenceMicrocephaly )
                                , ( 12, WarningHeadCircumferenceMacrocephaly )
                                ]
                            }
            ]
        , describe "With several warnings at different visits and no in-range reading since, only the most recent is under Active"
            [ test "the same warning at three visits" <|
                \_ ->
                    partitionVisits [ microcephalyAt 3, microcephalyAt 6, microcephalyAt 9 ]
                        |> expectPlacement
                            { active = [ ( 9, WarningHeadCircumferenceMicrocephaly ) ]
                            , prior =
                                [ ( 3, WarningHeadCircumferenceMicrocephaly )
                                , ( 6, WarningHeadCircumferenceMicrocephaly )
                                ]
                            }
            , test "different warnings at different visits" <|
                \_ ->
                    partitionVisits [ microcephalyAt 3, macrocephalyAt 6 ]
                        |> expectPlacement
                            { active = [ ( 6, WarningHeadCircumferenceMacrocephaly ) ]
                            , prior = [ ( 3, WarningHeadCircumferenceMicrocephaly ) ]
                            }
            , test "a later visit without head circumference does not change which is the most recent" <|
                \_ ->
                    partitionVisits [ microcephalyAt 3, macrocephalyAt 6, withoutHeadCircumferenceAt 9 ]
                        |> expectPlacement
                            { active = [ ( 6, WarningHeadCircumferenceMacrocephaly ) ]
                            , prior = [ ( 3, WarningHeadCircumferenceMicrocephaly ) ]
                            }
            , test "a not-taken visit between two warnings does not change which is the most recent" <|
                \_ ->
                    partitionVisits [ microcephalyAt 3, notTakenAt 6, microcephalyAt 9 ]
                        |> expectPlacement
                            { active = [ ( 9, WarningHeadCircumferenceMicrocephaly ) ]
                            , prior = [ ( 3, WarningHeadCircumferenceMicrocephaly ) ]
                            }
            ]
        , describe "A later visit that didn't include head circumference leaves the warning where it was"
            [ test "an Active warning stays under Active" <|
                \_ ->
                    partitionVisits [ microcephalyAt 6, withoutHeadCircumferenceAt 9 ]
                        |> expectPlacement
                            { active = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ], prior = [] }
            , test "an Active warning stays under Active when the later visit stored an empty set of warnings" <|
                \_ ->
                    partitionVisits [ microcephalyAt 6, Visit 9 [] Nothing ]
                        |> expectPlacement
                            { active = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ], prior = [] }
            , test "an Active warning stays under Active when the later visit did ECD only" <|
                \_ ->
                    partitionVisits [ microcephalyAt 6, Visit 9 [ NoECDMilstoneWarning ] Nothing ]
                        |> expectPlacement
                            { active = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ], prior = [] }
            , test "an Active warning stays under Active when the later visit raised an ECD warning" <|
                \_ ->
                    partitionVisits [ microcephalyAt 6, Visit 9 [ WarningECDMilestoneBehind ] Nothing ]
                        |> expectPlacement
                            { active =
                                [ ( 6, WarningHeadCircumferenceMicrocephaly )
                                , ( 9, WarningECDMilestoneBehind )
                                ]
                            , prior = []
                            }
            , test "a warning already moved to Prior by a reading in range stays under Prior" <|
                \_ ->
                    partitionVisits [ microcephalyAt 6, inRangeAt 9, withoutHeadCircumferenceAt 12 ]
                        |> expectPlacement
                            { active = [], prior = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ] }
            , test "a warning already moved to Prior by a not-taken visit stays under Prior" <|
                \_ ->
                    partitionVisits [ microcephalyAt 6, notTakenAt 9, withoutHeadCircumferenceAt 12 ]
                        |> expectPlacement
                            { active = [], prior = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ] }
            ]
        , describe "A later visit marked \"not taken\" moves the warning to Prior, as it does today"
            [ test "microcephaly moves to Prior" <|
                \_ ->
                    partitionVisits [ microcephalyAt 6, notTakenAt 9 ]
                        |> expectPlacement
                            { active = [], prior = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ] }
            , test "macrocephaly moves to Prior" <|
                \_ ->
                    partitionVisits [ macrocephalyAt 6, notTakenAt 9 ]
                        |> expectPlacement
                            { active = [], prior = [ ( 6, WarningHeadCircumferenceMacrocephaly ) ] }
            , test "a visit carrying the not-taken mark with no stored measurement moves it too" <|
                \_ ->
                    partitionVisits [ microcephalyAt 6, Visit 9 [ NoHeadCircumferenceWarning ] Nothing ]
                        |> expectPlacement
                            { active = [], prior = [ ( 6, WarningHeadCircumferenceMicrocephaly ) ] }
            , test "a not-taken visit is not itself listed anywhere" <|
                \_ ->
                    partitionVisits [ notTakenAt 9 ]
                        |> expectPlacement
                            { active = [], prior = [] }
            ]
        , describe "when there is nothing to report"
            [ test "no assembled data gives empty lists" <|
                \_ ->
                    generatePartitionedWarningEntries [ visitEncounter (microcephalyAt 6) ] Nothing
                        |> expectPlacement { active = [], prior = [] }
            , test "no encounters gives empty lists" <|
                \_ ->
                    let
                        visit =
                            inRangeAt 6
                    in
                    generatePartitionedWarningEntries [] (Just (assembledFrom visit [ visit ]))
                        |> expectPlacement { active = [], prior = [] }
            , test "readings in range only give empty lists" <|
                \_ ->
                    partitionVisits [ inRangeAt 3, inRangeAt 6, withoutHeadCircumferenceAt 9 ]
                        |> expectPlacement { active = [], prior = [] }
            ]
        ]


{-| Missed ECD milestone warnings follow the ECD activity alone. Head
circumference has no say on them, in either direction. At 7 months the child is
at the 6 months milestone, and at 13 months at the 12 months milestone.
-}
ecdWarningEntriesTests : Test
ecdWarningEntriesTests =
    let
        behindAt monthsOld =
            Visit monthsOld [ WarningECDMilestoneBehind ] Nothing

        entryAt monthsOld milestone warning =
            ( monthsOldDate monthsOld, milestone, warning )

        expectEntries expectedActive expectedPrior ( active, prior ) =
            Expect.equal ( expectedActive, expectedPrior )
                ( List.sortBy (\( date, _, _ ) -> Date.toRataDie date) active
                , List.sortBy (\( date, _, _ ) -> Date.toRataDie date) prior
                )
    in
    describe "generatePartitionedWarningEntries - Missed ECD milestone warnings behave as they do today"
        [ test "the latest visit with an ECD warning is under Active, the older ones under Prior, each with the milestone of its visit" <|
            \_ ->
                partitionVisits [ behindAt 7, behindAt 13 ]
                    |> expectEntries
                        [ entryAt 13 Milestone12Months WarningECDMilestoneBehind ]
                        [ entryAt 7 Milestone6Months WarningECDMilestoneBehind ]
        , test "a warning to refer to a specialist is placed the same way" <|
            \_ ->
                partitionVisits
                    [ behindAt 7
                    , Visit 13 [ WarningECDMilestoneReferToSpecialist ] Nothing
                    ]
                    |> expectEntries
                        [ entryAt 13 Milestone12Months WarningECDMilestoneReferToSpecialist ]
                        [ entryAt 7 Milestone6Months WarningECDMilestoneBehind ]
        , test "a later visit completing ECD with no warning moves it to Prior" <|
            \_ ->
                partitionVisits [ behindAt 7, Visit 13 [ NoECDMilstoneWarning ] Nothing ]
                    |> expectEntries
                        []
                        [ entryAt 7 Milestone6Months WarningECDMilestoneBehind ]
        , test "a later visit without ECD leaves it under Active" <|
            \_ ->
                partitionVisits [ behindAt 7, withoutHeadCircumferenceAt 13 ]
                    |> expectEntries
                        [ entryAt 7 Milestone6Months WarningECDMilestoneBehind ]
                        []
        , test "a later head circumference reading in range leaves it under Active" <|
            \_ ->
                partitionVisits [ behindAt 7, inRangeAt 13 ]
                    |> expectEntries
                        [ entryAt 7 Milestone6Months WarningECDMilestoneBehind ]
                        []
        , test "a later head circumference marked not taken leaves it under Active" <|
            \_ ->
                partitionVisits [ behindAt 7, notTakenAt 13 ]
                    |> expectEntries
                        [ entryAt 7 Milestone6Months WarningECDMilestoneBehind ]
                        []
        , test "a head circumference warning at a later visit leaves it under Active" <|
            \_ ->
                partitionVisits [ behindAt 7, microcephalyAt 13 ]
                    |> Tuple.mapBoth
                        (List.filter (\( _, _, warning ) -> warning == WarningECDMilestoneBehind))
                        (List.filter (\( _, _, warning ) -> warning == WarningECDMilestoneBehind))
                    |> expectEntries
                        [ entryAt 7 Milestone6Months WarningECDMilestoneBehind ]
                        []
        , test "a visit with both warnings: a later reading in range moves head circumference to Prior and leaves ECD under Active" <|
            \_ ->
                partitionVisits
                    [ Visit 7 [ WarningECDMilestoneBehind, WarningHeadCircumferenceMicrocephaly ] readingValue
                    , inRangeAt 13
                    ]
                    |> expectPlacement
                        { active = [ ( 7, WarningECDMilestoneBehind ) ]
                        , prior = [ ( 7, WarningHeadCircumferenceMicrocephaly ) ]
                        }
        , test "an ECD warning from before the first milestone is not listed" <|
            -- At one month the child has not reached the 6 weeks milestone.
            \_ ->
                partitionVisits [ behindAt 1 ]
                    |> expectEntries [] []
        , test "an ECD warning for a child without a birth date is not listed" <|
            \_ ->
                let
                    visit =
                        behindAt 7

                    assembled =
                        assembledFrom visit [ visit ]
                in
                generatePartitionedWarningEntries [ visitEncounter visit ]
                    (Just { assembled | person = { child | birthDate = Nothing } })
                    |> expectEntries [] []
        ]
