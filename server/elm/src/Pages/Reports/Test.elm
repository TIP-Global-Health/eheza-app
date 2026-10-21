module Pages.Reports.Test exposing (all)

import App.Types exposing (Language(..), Site(..), SiteFeature(..))
import AssocList as Dict exposing (Dict)
import Backend.Components.Model exposing (PersonId)
import Backend.Reports.Model
    exposing
        ( AcuteIllnessDiagnosis(..)
        , AcuteIllnessEncounterData
        , AcuteIllnessEncounterType(..)
        , Gender(..)
        , NutritionData
        , NutritionEncounterData
        , PatientData
        , PregnancyOutcome(..)
        , PrenatalEncounterData
        , PrenatalEncounterType(..)
        , PrenatalIndicator(..)
        , PrenatalParticipantData
        , WellChildEncounterData
        )
import Backend.Scoreboard.Model exposing (VaccineType(..))
import Date exposing (Unit(..))
import EverySet
import Expect exposing (FloatingPointTolerance(..))
import Gizra.NominalDate exposing (NominalDate)
import Pages.Reports.Model exposing (NutritionMetrics, NutritionMetricsResults, NutritionReportData, emptyNutritionMetrics)
import Pages.Reports.Utils
    exposing
        ( calculateNutritionReportData
        , csvRow
        , generateIncidenceNutritionMetricsResults
        , generatePrevalenceNutritionMetricsResults
        , resolveDataSetForMonth
        , resolveDataSetForQuarter
        , resolveDataSetForYear
        , resolvePreviousDataSetForMonth
        , resolveQuarterForIndex
        )
import Pages.Reports.View
    exposing
        ( generateAcuteIllnessReportData
        , generateDemographicsReportEncountersData
        , generateDemographicsReportPatientsData
        , generatePeripartumReportData
        , generatePostnatalCareReportData
        )
import Test exposing (Test, describe, test)
import Time exposing (Month(..))
import Translate exposing (translate)


{-| The report is calculated on 2026-10-20, so it shows visits from
1 January 2023 on.
-}
reportDate : NominalDate
reportDate =
    Date.fromCalendarDate 2026 Oct 20


{-| A person with no visits of any kind.
-}
emptyPatient : PersonId -> NominalDate -> PatientData
emptyPatient id birthDate =
    { id = id
    , created = birthDate
    , birthDate = birthDate
    , gender = Male
    , acuteIllnessData = Nothing
    , prenatalData = Nothing
    , familyNutritionData = Nothing
    , familyNutritionMuacData = Nothing
    , homeVisitData = Nothing
    , wellChildData = Nothing
    , childScorecardData = Nothing
    , ncdData = Nothing
    , hivData = Nothing
    , tuberculosisData = Nothing
    , individualNutritionData = Nothing
    , groupNutritionPmtctData = Nothing
    , groupNutritionFbfData = Nothing
    , groupNutritionSorwatheData = Nothing
    , groupNutritionChwData = Nothing
    , groupNutritionAchiData = Nothing
    , groupNutritionFbfMotherData = Nothing
    }


{-| A CHW group visit whose stunting z-score is moderate, so the visit
shows up in the stuntingModerate bucket of its month.
-}
stuntedVisit : NominalDate -> NutritionEncounterData
stuntedVisit date =
    { startDate = date
    , nutritionData = Just { stunting = Just -2.5, underweight = Nothing, wasting = Nothing }
    , muacCm = Nothing
    , hasEdema = False
    , fbfAmount = Nothing
    }


childWithGroupVisits : PersonId -> NominalDate -> List NominalDate -> PatientData
childWithGroupVisits id birthDate dates =
    let
        patient =
            emptyPatient id birthDate
    in
    { patient | groupNutritionChwData = Just (List.map stuntedVisit dates) }


{-| Every (year, month, person) the report counted, from the stunting
and acute malnutrition buckets.
-}
countedVisits : NutritionReportData -> List ( ( Int, Int ), PersonId )
countedVisits report =
    Dict.toList report.encountersByMonth
        |> List.concatMap
            (\( yearMonth, metrics ) ->
                List.map (Tuple.pair yearMonth) (metrics.stuntingModerate ++ metrics.acuteMalnutritionMam)
            )
        |> List.sortBy (\( ( year, month ), id ) -> ( year, month, id ))


calculate : List PatientData -> NutritionReportData
calculate =
    calculateNutritionReportData reportDate


calculateNutritionReportDataTest : Test
calculateNutritionReportDataTest =
    describe "calculateNutritionReportData counts a visit only if the child was under six on that day"
        [ test "a child who is six today keeps the visits made before turning six" <|
            \_ ->
                [ childWithGroupVisits 1 (Date.fromCalendarDate 2019 Mar 1) [ Date.fromCalendarDate 2024 May 2, Date.fromCalendarDate 2025 Apr 2 ] ]
                    |> calculate
                    |> countedVisits
                    |> Expect.equal [ ( ( 2024, 5 ), 1 ) ]
        , test "a visit the day before the sixth birthday counts, one on the birthday does not" <|
            \_ ->
                [ childWithGroupVisits 1 (Date.fromCalendarDate 2019 Jun 15) [ Date.fromCalendarDate 2025 Jun 14, Date.fromCalendarDate 2025 Jun 15 ] ]
                    |> calculate
                    |> countedVisits
                    |> Expect.equal [ ( ( 2025, 6 ), 1 ) ]
        , test "a child born on 29 February is six on 1 March" <|
            \_ ->
                [ childWithGroupVisits 1 (Date.fromCalendarDate 2020 Feb 29) [ Date.fromCalendarDate 2026 Feb 28, Date.fromCalendarDate 2026 Mar 1 ] ]
                    |> calculate
                    |> countedVisits
                    |> Expect.equal [ ( ( 2026, 2 ), 1 ) ]
        , test "a child under six today keeps every visit and counts as impacted" <|
            \_ ->
                let
                    report =
                        calculate [ childWithGroupVisits 1 (Date.fromCalendarDate 2022 Jan 10) [ Date.fromCalendarDate 2025 Nov 6, Date.fromCalendarDate 2026 Mar 3 ] ]
                in
                Expect.equal ( [ ( ( 2025, 11 ), 1 ), ( ( 2026, 3 ), 1 ) ], [ 1 ] )
                    ( countedVisits report, report.impacted )
        , test "impacted counts only the visits made before six" <|
            \_ ->
                [ childWithGroupVisits 1 (Date.fromCalendarDate 2019 Mar 1) [ Date.fromCalendarDate 2025 Feb 15, Date.fromCalendarDate 2025 Mar 15 ] ]
                    |> calculate
                    |> .impacted
                    |> Expect.equal []
        , test "a family-nutrition MUAC visit counts only before six" <|
            \_ ->
                let
                    patient =
                        emptyPatient 1 (Date.fromCalendarDate 2019 Mar 1)

                    muacVisit date =
                        { startDate = date, muacCm = Just 12, ahezaAmount = Nothing }
                in
                [ { patient | familyNutritionMuacData = Just [ [ muacVisit (Date.fromCalendarDate 2024 Jul 1), muacVisit (Date.fromCalendarDate 2025 Jul 1) ] ] } ]
                    |> calculate
                    |> countedVisits
                    |> Expect.equal [ ( ( 2024, 7 ), 1 ) ]
        , test "someone six by 1 January of the starting year adds nothing" <|
            \_ ->
                -- Child 1 made two visits before six, both before 2023: only
                -- the starting-year skip keeps the child out of impacted.
                [ childWithGroupVisits 1 (Date.fromCalendarDate 2016 Jun 1) [ Date.fromCalendarDate 2021 Mar 5, Date.fromCalendarDate 2022 Mar 5 ]
                , childWithGroupVisits 2 (Date.fromCalendarDate 1990 Jan 1) [ Date.fromCalendarDate 2025 Nov 3 ]
                ]
                    |> calculate
                    |> Expect.equal { impacted = [], encountersByMonth = Dict.empty }
        ]


{-| The metrics of the one month in the report, or Nothing when the
report has no month, or more than one.
-}
singleMonthMetrics : NutritionReportData -> Maybe NutritionMetrics
singleMonthMetrics report =
    case Dict.values report.encountersByMonth of
        [ metrics ] ->
            Just metrics

        _ ->
            Nothing


{-| The bands of a two-year-old's CHW group visit on 2025-05-10.
-}
bandOf : Maybe NutritionData -> Maybe Float -> Bool -> Maybe NutritionMetrics
bandOf nutritionData muacCm hasEdema =
    let
        patient =
            emptyPatient 1 (Date.fromCalendarDate 2023 Mar 1)

        visit =
            { startDate = Date.fromCalendarDate 2025 May 10
            , nutritionData = nutritionData
            , muacCm = muacCm
            , hasEdema = hasEdema
            , fbfAmount = Nothing
            }
    in
    calculate [ { patient | groupNutritionChwData = Just [ visit ] } ]
        |> singleMonthMetrics


stuntingBand : Float -> Maybe NutritionMetrics
stuntingBand score =
    bandOf (Just { stunting = Just score, underweight = Nothing, wasting = Nothing }) Nothing False


muacBand : Float -> Bool -> Maybe NutritionMetrics
muacBand muacCm hasEdema =
    bandOf Nothing (Just muacCm) hasEdema


bandsTest : Test
bandsTest =
    describe "a visit lands in the band of its z-score, or of its MUAC and edema"
        [ test "z-score -3.0 is severe" <|
            \_ -> Expect.equal (Just { emptyNutritionMetrics | stuntingSevere = [ 1 ] }) (stuntingBand -3.0)
        , test "z-score -2.99 is moderate" <|
            \_ -> Expect.equal (Just { emptyNutritionMetrics | stuntingModerate = [ 1 ] }) (stuntingBand -2.99)
        , test "z-score -2.0 is moderate" <|
            \_ -> Expect.equal (Just { emptyNutritionMetrics | stuntingModerate = [ 1 ] }) (stuntingBand -2.0)
        , test "z-score -1.99 is normal" <|
            \_ -> Expect.equal (Just { emptyNutritionMetrics | stuntingNormal = [ 1 ] }) (stuntingBand -1.99)
        , test "each z-score goes to its own indicator" <|
            \_ ->
                bandOf (Just { stunting = Nothing, underweight = Just -3.5, wasting = Just -2.5 }) Nothing False
                    |> Expect.equal (Just { emptyNutritionMetrics | underweightSevere = [ 1 ], wastingModerate = [ 1 ] })
        , test "a visit with no z-score and no MUAC is in no band" <|
            \_ -> Expect.equal (Just emptyNutritionMetrics) (bandOf Nothing Nothing False)
        , test "MUAC 11.4 is SAM" <|
            \_ -> Expect.equal (Just { emptyNutritionMetrics | acuteMalnutritionSam = [ 1 ] }) (muacBand 11.4 False)
        , test "MUAC 11.5 is MAM" <|
            \_ -> Expect.equal (Just { emptyNutritionMetrics | acuteMalnutritionMam = [ 1 ] }) (muacBand 11.5 False)
        , test "MUAC 12.49 is MAM" <|
            \_ -> Expect.equal (Just { emptyNutritionMetrics | acuteMalnutritionMam = [ 1 ] }) (muacBand 12.49 False)
        , test "MUAC 12.5 is normal" <|
            \_ -> Expect.equal (Just { emptyNutritionMetrics | acuteMalnutritionNormal = [ 1 ] }) (muacBand 12.5 False)
        , test "edema with MUAC 13 is SAM" <|
            \_ -> Expect.equal (Just { emptyNutritionMetrics | acuteMalnutritionSam = [ 1 ] }) (muacBand 13 True)
        , test "edema without MUAC is in no band" <|
            \_ -> Expect.equal (Just emptyNutritionMetrics) (bandOf Nothing Nothing True)
        , test "edema at a well-child visit is SAM" <|
            \_ ->
                let
                    patient =
                        emptyPatient 1 (Date.fromCalendarDate 2023 Mar 1)

                    visit =
                        { startDate = Date.fromCalendarDate 2025 May 10
                        , nutritionData = Nothing
                        , muacCm = Just 13
                        , hasEdema = True
                        , immunisationData = Nothing
                        }
                in
                calculate [ { patient | wellChildData = Just [ [ visit ] ] } ]
                    |> singleMonthMetrics
                    |> Expect.equal (Just { emptyNutritionMetrics | acuteMalnutritionSam = [ 1 ] })
        ]


expectPercentage : Float -> Float -> Expect.Expectation
expectPercentage expected =
    Expect.within (Absolute 0.001) expected


prevalenceTest : Test
prevalenceTest =
    describe "generatePrevalenceNutritionMetricsResults counts each child once"
        [ test "child 1 moderate twice and child 2 normal is 50%" <|
            \_ ->
                { emptyNutritionMetrics | stuntingModerate = [ 1, 1 ], stuntingNormal = [ 2 ] }
                    |> generatePrevalenceNutritionMetricsResults
                    |> .stuntingModerate
                    |> expectPercentage 50
        , test "a child moderate at one visit and normal at another is 100% moderate" <|
            \_ ->
                { emptyNutritionMetrics | stuntingModerate = [ 1 ], stuntingNormal = [ 1 ] }
                    |> generatePrevalenceNutritionMetricsResults
                    |> .stuntingModerate
                    |> expectPercentage 100
        , test "MAM and SAM share the MUAC denominator" <|
            \_ ->
                let
                    results =
                        { emptyNutritionMetrics | acuteMalnutritionMam = [ 1 ], acuteMalnutritionSam = [ 2 ], acuteMalnutritionNormal = [ 3, 4 ] }
                            |> generatePrevalenceNutritionMetricsResults
                in
                Expect.all
                    [ \_ -> expectPercentage 25 results.acuteMalnutritionMam
                    , \_ -> expectPercentage 25 results.acuteMalnutritionSam
                    ]
                    ()
        , test "no measured children is 0%" <|
            \_ ->
                generatePrevalenceNutritionMetricsResults emptyNutritionMetrics
                    |> .stuntingModerate
                    |> expectPercentage 0
        ]


{-| Incidence of moderate and severe stunting, for one child's
previous and current bands.
-}
incidence : NutritionMetrics -> NutritionMetrics -> NutritionMetricsResults
incidence current previous =
    generateIncidenceNutritionMetricsResults current previous


incidenceTest : Test
incidenceTest =
    describe "generateIncidenceNutritionMetricsResults counts children newly moderate or severe"
        [ test "normal before, moderate now, counts" <|
            \_ ->
                incidence { emptyNutritionMetrics | stuntingModerate = [ 1 ] } { emptyNutritionMetrics | stuntingNormal = [ 1 ] }
                    |> .stuntingModerate
                    |> expectPercentage 100
        , test "moderate before, moderate now, does not count" <|
            \_ ->
                incidence { emptyNutritionMetrics | stuntingModerate = [ 1 ] } { emptyNutritionMetrics | stuntingModerate = [ 1 ] }
                    |> .stuntingModerate
                    |> expectPercentage 0
        , test "severe before, moderate now, does not count" <|
            \_ ->
                incidence { emptyNutritionMetrics | stuntingModerate = [ 1 ] } { emptyNutritionMetrics | stuntingSevere = [ 1 ] }
                    |> .stuntingModerate
                    |> expectPercentage 0
        , test "moderate before, severe now, counts" <|
            \_ ->
                incidence { emptyNutritionMetrics | stuntingSevere = [ 1 ] } { emptyNutritionMetrics | stuntingModerate = [ 1 ] }
                    |> .stuntingSevere
                    |> expectPercentage 100
        , test "a child not tested before does not count" <|
            \_ ->
                incidence { emptyNutritionMetrics | stuntingModerate = [ 1 ] } { emptyNutritionMetrics | stuntingNormal = [ 2 ] }
                    |> .stuntingModerate
                    |> expectPercentage 0
        , test "the denominator is the children tested before, each once" <|
            \_ ->
                incidence { emptyNutritionMetrics | stuntingModerate = [ 1 ] } { emptyNutritionMetrics | stuntingNormal = [ 1, 2, 2, 3, 4 ] }
                    |> .stuntingModerate
                    |> expectPercentage 25
        , test "MUAC: normal before, MAM now, counts" <|
            \_ ->
                incidence { emptyNutritionMetrics | acuteMalnutritionMam = [ 1 ] } { emptyNutritionMetrics | acuteMalnutritionNormal = [ 1, 2 ] }
                    |> .acuteMalnutritionMam
                    |> expectPercentage 50
        ]


{-| Every month from January 2025 to December 2027, each holding one
"child" whose id is the month as YYYYMM, so the ids show which months a
period took.
-}
monthsByKey : Dict ( Int, Int ) NutritionMetrics
monthsByKey =
    List.range 2025 2027
        |> List.concatMap
            (\year ->
                List.range 1 12
                    |> List.map (\month -> ( ( year, month ), { emptyNutritionMetrics | stuntingNormal = [ year * 100 + month ] } ))
            )
        |> Dict.fromList


monthsOf : NutritionMetrics -> List Int
monthsOf =
    .stuntingNormal >> List.sort


periodWindowsTest : Test
periodWindowsTest =
    describe "the months each period column takes"
        [ test "on 31 March, month 1 is February" <|
            \_ ->
                resolveDataSetForMonth (Date.fromCalendarDate 2026 Mar 31) 1 monthsByKey
                    |> monthsOf
                    |> Expect.equal [ 202602 ]
        , test "in January, month 1 is the December before" <|
            \_ ->
                resolveDataSetForMonth (Date.fromCalendarDate 2027 Jan 15) 1 monthsByKey
                    |> monthsOf
                    |> Expect.equal [ 202612 ]
        , test "month 12 is a year before month 0" <|
            \_ ->
                resolveDataSetForMonth (Date.fromCalendarDate 2027 Jan 15) 12 monthsByKey
                    |> monthsOf
                    |> Expect.equal [ 202601 ]
        , test "the previous set of a month is the three months before it" <|
            \_ ->
                resolvePreviousDataSetForMonth (Date.fromCalendarDate 2027 Jan 15) 1 monthsByKey
                    |> monthsOf
                    |> Expect.equal [ 202609, 202610, 202611 ]
        , test "the previous set of a month counts back from the 31st without skipping a month" <|
            \_ ->
                resolvePreviousDataSetForMonth (Date.fromCalendarDate 2026 May 31) 1 monthsByKey
                    |> monthsOf
                    |> Expect.equal [ 202601, 202602, 202603 ]
        , test "in January, quarter 1 is the last quarter of the year before" <|
            \_ ->
                resolveDataSetForQuarter (Date.fromCalendarDate 2027 Jan 15) 1 monthsByKey
                    |> monthsOf
                    |> Expect.equal [ 202610, 202611, 202612 ]
        , test "on 31 March, quarter 2 is the third quarter of the year before" <|
            \_ ->
                resolveDataSetForQuarter (Date.fromCalendarDate 2026 Mar 31) 2 monthsByKey
                    |> monthsOf
                    |> Expect.equal [ 202507, 202508, 202509 ]
        , test "year 1 is every month of the year before" <|
            \_ ->
                resolveDataSetForYear (Date.fromCalendarDate 2027 Jan 15) 1 monthsByKey
                    |> monthsOf
                    |> Expect.equal (List.range 202601 202612)
        , test "the quarter columns, on the last day of each month of 2026" <|
            \_ ->
                List.range 1 12
                    |> List.map
                        (\month ->
                            let
                                lastDay =
                                    Date.fromCalendarDate 2026 (Date.numberToMonth month) 1
                                        |> Date.add Date.Months 1
                                        |> Date.add Date.Days -1
                            in
                            List.map (resolveQuarterForIndex lastDay) [ 1, 2, 3, 4 ]
                        )
                    |> Expect.equal
                        [ [ ( 2025, 4 ), ( 2025, 3 ), ( 2025, 2 ), ( 2025, 1 ) ]
                        , [ ( 2025, 4 ), ( 2025, 3 ), ( 2025, 2 ), ( 2025, 1 ) ]
                        , [ ( 2025, 4 ), ( 2025, 3 ), ( 2025, 2 ), ( 2025, 1 ) ]
                        , [ ( 2026, 1 ), ( 2025, 4 ), ( 2025, 3 ), ( 2025, 2 ) ]
                        , [ ( 2026, 1 ), ( 2025, 4 ), ( 2025, 3 ), ( 2025, 2 ) ]
                        , [ ( 2026, 1 ), ( 2025, 4 ), ( 2025, 3 ), ( 2025, 2 ) ]
                        , [ ( 2026, 2 ), ( 2026, 1 ), ( 2025, 4 ), ( 2025, 3 ) ]
                        , [ ( 2026, 2 ), ( 2026, 1 ), ( 2025, 4 ), ( 2025, 3 ) ]
                        , [ ( 2026, 2 ), ( 2026, 1 ), ( 2025, 4 ), ( 2025, 3 ) ]
                        , [ ( 2026, 3 ), ( 2026, 2 ), ( 2026, 1 ), ( 2025, 4 ) ]
                        , [ ( 2026, 3 ), ( 2026, 2 ), ( 2026, 1 ), ( 2025, 4 ) ]
                        , [ ( 2026, 3 ), ( 2026, 2 ), ( 2026, 1 ), ( 2025, 4 ) ]
                        ]
        ]


underSixGateTest : Test
underSixGateTest =
    describe "calculateNutritionReportData: which visits enter the report"
        [ test "a visit on 31 December before the starting year is dropped, one on 1 January is kept" <|
            \_ ->
                [ childWithGroupVisits 1 (Date.fromCalendarDate 2021 Jun 1) [ Date.fromCalendarDate 2022 Dec 31, Date.fromCalendarDate 2023 Jan 1 ] ]
                    |> calculate
                    |> countedVisits
                    |> Expect.equal [ ( ( 2023, 1 ), 1 ) ]
        , test "well-child and individual nutrition visits count only before six" <|
            \_ ->
                let
                    patient =
                        emptyPatient 1 (Date.fromCalendarDate 2019 Mar 1)

                    wellChildVisit date =
                        { startDate = date
                        , nutritionData = Just { stunting = Just -2.5, underweight = Nothing, wasting = Nothing }
                        , muacCm = Nothing
                        , hasEdema = False
                        , immunisationData = Nothing
                        }
                in
                [ { patient
                    | wellChildData = Just [ [ wellChildVisit (Date.fromCalendarDate 2024 May 2), wellChildVisit (Date.fromCalendarDate 2025 Apr 2) ] ]
                    , individualNutritionData = Just [ [ stuntedVisit (Date.fromCalendarDate 2024 Jun 2), stuntedVisit (Date.fromCalendarDate 2025 May 2) ] ]
                  }
                ]
                    |> calculate
                    |> countedVisits
                    |> Expect.equal [ ( ( 2024, 5 ), 1 ), ( ( 2024, 6 ), 1 ) ]
        , test "one group visit and one family-nutrition MUAC visit make a child impacted" <|
            \_ ->
                let
                    patient =
                        childWithGroupVisits 1 (Date.fromCalendarDate 2023 Mar 1) [ Date.fromCalendarDate 2025 May 2 ]
                in
                [ { patient | familyNutritionMuacData = Just [ [ { startDate = Date.fromCalendarDate 2025 Jun 2, muacCm = Just 13, ahezaAmount = Nothing } ] ] } ]
                    |> calculate
                    |> .impacted
                    |> Expect.equal [ 1 ]
        , test "a mother with only family-nutrition visits adds nothing" <|
            \_ ->
                let
                    patient =
                        mother 1 []

                    visit date =
                        { startDate = date, ahezaAmount = Just 500 }
                in
                [ { patient | familyNutritionData = Just [ [ visit (Date.fromCalendarDate 2025 May 2), visit (Date.fromCalendarDate 2025 Jun 2) ] ] } ]
                    |> calculate
                    |> Expect.equal { impacted = [], encountersByMonth = Dict.empty }
        ]


{-| The FROM and TO dates of the date-range reports.
-}
fromDate : NominalDate
fromDate =
    Date.fromCalendarDate 2026 Mar 1


toDate : NominalDate
toDate =
    Date.fromCalendarDate 2026 Mar 31


{-| The numbers of each row of a table, without its label.
-}
rowValues : { a | rows : List (List String) } -> List (List String)
rowValues =
    .rows >> List.map (List.drop 1)


pregnancy : List PrenatalEncounterData -> PrenatalParticipantData
pregnancy encounters =
    { created = Date.fromCalendarDate 2025 Jul 1
    , eddDate = Nothing
    , dateConcluded = Nothing
    , outcome = Nothing
    , deliveryLocation = Nothing
    , encounters = encounters
    }


prenatalVisit : PrenatalEncounterType -> List PrenatalIndicator -> PrenatalEncounterData
prenatalVisit encounterType indicators =
    { startDate = Date.fromCalendarDate 2026 Mar 10, encounterType = encounterType, diagnoses = [], indicators = indicators }


mother : PersonId -> List PrenatalParticipantData -> PatientData
mother id pregnancies =
    let
        patient =
            emptyPatient id (Date.fromCalendarDate 1995 Jan 1)
    in
    { patient | gender = Female, prenatalData = Just pregnancies }


delivered : PersonId -> NominalDate -> PregnancyOutcome -> PatientData
delivered id date outcome =
    let
        concluded =
            pregnancy []
    in
    mother id [ { concluded | dateConcluded = Just date, outcome = Just outcome } ]


peripartumTest : Test
peripartumTest =
    describe "generatePeripartumReportData"
        [ test "deliveries on FROM and on TO count, the days outside do not" <|
            \_ ->
                generatePeripartumReportData English
                    fromDate
                    toDate
                    [ delivered 1 (Date.fromCalendarDate 2026 Feb 28) OutcomeLiveAtTerm
                    , delivered 2 fromDate OutcomeLiveAtTerm
                    , delivered 3 toDate OutcomeLiveAtTerm
                    , delivered 4 (Date.fromCalendarDate 2026 Apr 1) OutcomeLiveAtTerm
                    ]
                    []
                    |> rowValues
                    |> Expect.equal [ [ "2" ], [ "2" ], [ "0" ], [ "0" ], [ "0" ] ]
        , test "a delivery with no visit in the range counts, by its outcome" <|
            \_ ->
                generatePeripartumReportData English
                    fromDate
                    toDate
                    [ delivered 1 (Date.fromCalendarDate 2026 Mar 5) OutcomeLiveAtTerm
                    , delivered 2 (Date.fromCalendarDate 2026 Mar 6) OutcomeLivePreTerm
                    , delivered 3 (Date.fromCalendarDate 2026 Mar 7) OutcomeStillAtTerm
                    , delivered 4 (Date.fromCalendarDate 2026 Mar 8) OutcomeAbortions
                    ]
                    []
                    |> rowValues
                    |> Expect.equal [ [ "3" ], [ "2" ], [ "1" ], [ "0" ], [ "0" ] ]
        , test "the visit rows count pregnancies with a visit in the range that recorded them" <|
            \_ ->
                generatePeripartumReportData English
                    fromDate
                    toDate
                    []
                    [ mother 1 [ pregnancy [ prenatalVisit NurseEncounter [ IndicatorBreastfedFirstHour ], prenatalVisit NurseEncounter [ IndicatorBreastfedFirstHour ] ] ]
                    , mother 2 [ pregnancy [ prenatalVisit NurseEncounter [ IndicatorPrematureOnsetContractions ] ] ]
                    , mother 3 [ pregnancy [ prenatalVisit NurseEncounter [] ] ]
                    ]
                    |> rowValues
                    |> Expect.equal [ [ "0" ], [ "0" ], [ "0" ], [ "1" ], [ "1" ] ]
        ]


{-| A child born this many days before TO.
-}
bornDaysBeforeTo : Int -> NominalDate
bornDaysBeforeTo days =
    Date.add Days -days toDate


{-| A well-child visit that gave the vaccines on the dates listed.
-}
spv : NominalDate -> List ( VaccineType, List NominalDate ) -> WellChildEncounterData
spv date doses =
    { startDate = date
    , nutritionData = Nothing
    , muacCm = Nothing
    , hasEdema = False
    , immunisationData =
        if List.isEmpty doses then
            Nothing

        else
            Just (Dict.fromList (List.map (Tuple.mapSecond EverySet.fromList) doses))
    }


childWithSpvs : PersonId -> NominalDate -> List WellChildEncounterData -> PatientData
childWithSpvs id birthDate visits =
    let
        patient =
            emptyPatient id birthDate
    in
    { patient | wellChildData = Just [ visits ] }


{-| Every dose of the Rwanda schedule, so the child is up to date at any age.
-}
fullCourse : NominalDate -> List ( VaccineType, List NominalDate )
fullCourse birthDate =
    let
        weeks n =
            Date.add Weeks n birthDate
    in
    [ ( VaccineBCG, [ birthDate ] )
    , ( VaccineOPV, [ birthDate, weeks 6, weeks 10, weeks 14 ] )
    , ( VaccineDTP, [ weeks 6, weeks 10, weeks 14 ] )
    , ( VaccinePCV13, [ weeks 6, weeks 10, weeks 14 ] )
    , ( VaccineRotarix, [ weeks 6, weeks 10 ] )
    , ( VaccineIPV, [ weeks 14, weeks 36 ] )
    , ( VaccineMR, [ weeks 36, weeks 62 ] )
    ]


{-| The birth doses, then the 6-week doses, so the next is due at 10 weeks.
-}
dosesTo6Weeks : NominalDate -> ( List ( VaccineType, List NominalDate ), List ( VaccineType, List NominalDate ) )
dosesTo6Weeks birthDate =
    let
        sixWeeks =
            Date.add Weeks 6 birthDate
    in
    ( [ ( VaccineBCG, [ birthDate ] ), ( VaccineOPV, [ birthDate ] ) ]
    , [ ( VaccineOPV, [ sixWeeks ] ), ( VaccineDTP, [ sixWeeks ] ), ( VaccinePCV13, [ sixWeeks ] ), ( VaccineRotarix, [ sixWeeks ] ) ]
    )


postnatalCare : List PatientData -> List (List String)
postnatalCare =
    generatePostnatalCareReportData English SiteRwanda toDate >> rowValues


postnatalCareTest : Test
postnatalCareTest =
    describe "generatePostnatalCareReportData"
        [ test "a first visit on the day of birth or the next counts as within 24 hours" <|
            \_ ->
                [ childWithSpvs 1 (bornDaysBeforeTo 10) [ spv (bornDaysBeforeTo 10) [] ]
                , childWithSpvs 2 (bornDaysBeforeTo 10) [ spv (bornDaysBeforeTo 5) [], spv (bornDaysBeforeTo 9) [] ]
                , childWithSpvs 3 (bornDaysBeforeTo 10) [ spv (bornDaysBeforeTo 8) [] ]
                ]
                    |> postnatalCare
                    |> List.head
                    |> Expect.equal (Just [ "2" ])
        , test "children up to date, by age at TO" <|
            \_ ->
                -- 6 and 7 weeks, 11 and 15 weeks, then 10, 19 and 24 months.
                (List.map bornDaysBeforeTo [ 48, 49, 77, 105 ]
                    ++ [ Date.fromCalendarDate 2025 May 31, Date.fromCalendarDate 2024 Aug 31, Date.fromCalendarDate 2024 Mar 31 ]
                )
                    |> List.indexedMap (\index birthDate -> childWithSpvs index birthDate [ spv toDate (fullCourse birthDate) ])
                    |> postnatalCare
                    |> Expect.equal [ [ "0" ], [ "1" ], [ "1" ], [ "1" ], [ "1" ], [ "1" ] ]
        , test "a dose due on TO is not up to date, one due the day after is" <|
            \_ ->
                [ 70, 69 ]
                    |> List.map
                        (\days ->
                            let
                                birthDate =
                                    bornDaysBeforeTo days

                                ( atBirth, atSixWeeks ) =
                                    dosesTo6Weeks birthDate
                            in
                            childWithSpvs days birthDate [ spv toDate (atBirth ++ atSixWeeks) ]
                        )
                    |> postnatalCare
                    |> List.drop 1
                    |> Expect.equal [ [ "1" ], [ "0" ], [ "0" ], [ "0" ], [ "0" ] ]
        , test "doses are gathered from every visit" <|
            \_ ->
                let
                    birthDate =
                        bornDaysBeforeTo 69

                    ( atBirth, atSixWeeks ) =
                        dosesTo6Weeks birthDate
                in
                [ childWithSpvs 1 birthDate [ spv birthDate atBirth, spv (Date.add Weeks 6 birthDate) atSixWeeks ] ]
                    |> postnatalCare
                    |> List.drop 1
                    |> Expect.equal [ [ "1" ], [ "0" ], [ "0" ], [ "0" ], [ "0" ] ]
        ]


{-| A FROM date before every patient was created.
-}
allTime : NominalDate
allTime =
    Date.fromCalendarDate 1900 Jan 1


registeredRows : NominalDate -> List PatientData -> Maybe (List (List String))
registeredRows limitDate records =
    generateDemographicsReportPatientsData English allTime limitDate records
        |> .tables
        |> List.head
        |> Maybe.map (.rows >> List.map (List.drop 1))


demographicsPatientsTest : Test
demographicsPatientsTest =
    describe "generateDemographicsReportPatientsData"
        [ test "age bands at TO, by gender" <|
            \_ ->
                let
                    girl =
                        emptyPatient 3 (Date.fromCalendarDate 2026 Mar 10)
                in
                [ emptyPatient 1 (Date.fromCalendarDate 2026 Mar 2)
                , emptyPatient 2 (Date.fromCalendarDate 2026 Feb 28)
                , { girl | gender = Female }
                , emptyPatient 4 (Date.fromCalendarDate 2021 Mar 31)
                , emptyPatient 5 (Date.fromCalendarDate 1976 Mar 31)
                ]
                    |> registeredRows toDate
                    |> Expect.equal (Just [ [ "1", "1" ], [ "1", "0" ], [ "0", "0" ], [ "1", "0" ], [ "0", "0" ], [ "0", "0" ], [ "1", "0" ] ])
        , test "a child born on 29 February is under two on 28 February" <|
            \_ ->
                [ emptyPatient 1 (Date.fromCalendarDate 2024 Feb 29) ]
                    |> registeredRows (Date.fromCalendarDate 2026 Feb 28)
                    |> Maybe.map (List.take 3)
                    |> Expect.equal (Just [ [ "0", "0" ], [ "1", "0" ], [ "0", "0" ] ])
        , test "a child born on 29 February is two on 1 March" <|
            \_ ->
                [ emptyPatient 1 (Date.fromCalendarDate 2024 Feb 29) ]
                    |> registeredRows (Date.fromCalendarDate 2026 Mar 1)
                    |> Maybe.map (List.take 3)
                    |> Expect.equal (Just [ [ "0", "0" ], [ "0", "0" ], [ "1", "0" ] ])
        , test "only patients created from FROM on are registered" <|
            \_ ->
                let
                    before =
                        emptyPatient 1 (Date.fromCalendarDate 2020 Jan 1)

                    onFrom =
                        emptyPatient 2 (Date.fromCalendarDate 2020 Jan 1)
                in
                [ { before | created = Date.fromCalendarDate 2026 Feb 28 }, { onFrom | created = fromDate } ]
                    |> generateDemographicsReportPatientsData English fromDate toDate
                    |> .tables
                    |> List.map .totals
                    |> Expect.equal [ ( "Total", "1" ), ( "Total", "0" ) ]
        , test "a patient with visits in two programs is impacted" <|
            \_ ->
                let
                    patient =
                        mother 1 [ pregnancy [ prenatalVisit NurseEncounter [] ] ]

                    illness =
                        { startDate = Date.fromCalendarDate 2026 Mar 12, encounterType = AcuteIllnessEncounterNurse, diagnosis = Nothing }
                in
                [ { patient | acuteIllnessData = Just [ [ illness ] ] }, mother 2 [ pregnancy [ prenatalVisit NurseEncounter [] ] ] ]
                    |> generateDemographicsReportPatientsData English allTime toDate
                    |> .tables
                    |> List.map .totals
                    |> Expect.equal [ ( "Total", "2" ), ( "Total", "1" ) ]
        ]


{-| The (all, unique) counts of the encounters table rows, and its totals.
-}
encounterCounts : List SiteFeature -> List PatientData -> ( List ( String, String, String ), ( String, String ) )
encounterCounts features records =
    let
        data =
            generateDemographicsReportEncountersData English (EverySet.fromList features) records
    in
    ( List.filterMap
        (\( cells, _ ) ->
            case cells of
                [ label, total, unique ] ->
                    Just ( label, total, unique )

                _ ->
                    Nothing
        )
        data.rows
    , ( data.totals.total, data.totals.unique )
    )


demographicsEncountersTest : Test
demographicsEncountersTest =
    describe "generateDemographicsReportEncountersData"
        [ test "all counts the visits, unique the patients" <|
            \_ ->
                let
                    patient =
                        emptyPatient 1 (Date.fromCalendarDate 1990 Jan 1)

                    other =
                        emptyPatient 2 (Date.fromCalendarDate 1990 Jan 1)
                in
                [ { patient | ncdData = Just [ [ fromDate, toDate ] ] }, { other | ncdData = Just [ [ toDate ] ] } ]
                    |> encounterCounts []
                    |> Tuple.mapFirst (List.filter (\( label, _, _ ) -> label == translate English Translate.NCD))
                    |> Expect.equal ( [ ( translate English Translate.NCD, "3", "2" ) ], ( "3", "2" ) )
        , test "postpartum visits count with their health center or CHW row" <|
            \_ ->
                [ mother 1 [ pregnancy [ prenatalVisit NursePostpartumEncounter [] ] ]
                , mother 2 [ pregnancy [ prenatalVisit ChwPostpartumEncounter [], prenatalVisit ChwFirstEncounter [] ] ]
                ]
                    |> encounterCounts []
                    |> Tuple.first
                    |> List.take 3
                    |> List.map (\( _, total, unique ) -> ( total, unique ))
                    |> Expect.equal [ ( "3", "2" ), ( "1", "1" ), ( "2", "1" ) ]
        , test "family-nutrition visits have a row only where the feature is on" <|
            \_ ->
                let
                    patient =
                        mother 1 []

                    visit =
                        { startDate = fromDate, ahezaAmount = Nothing }

                    records =
                        [ { patient | familyNutritionData = Just [ [ visit, visit ] ] } ]

                    familyNutritionRow =
                        Tuple.mapFirst (List.filter (\( label, _, _ ) -> label == translate English Translate.FamilyNutrition))
                in
                Expect.equal
                    ( ( [ ( translate English Translate.FamilyNutrition, "2", "1" ) ], ( "2", "1" ) ), ( [], ( "0", "0" ) ) )
                    ( familyNutritionRow (encounterCounts [ FeatureFamilyNutrition ] records)
                    , familyNutritionRow (encounterCounts [] records)
                    )
        ]


acuteIllnessVisit : Int -> Maybe AcuteIllnessDiagnosis -> AcuteIllnessEncounterData
acuteIllnessVisit day diagnosis =
    { startDate = Date.fromCalendarDate 2026 Mar day, encounterType = AcuteIllnessEncounterNurse, diagnosis = diagnosis }


{-| The Uncomplicated Malaria, total and no diagnosis counts.
-}
acuteIllnessCounts : List (List AcuteIllnessEncounterData) -> List String
acuteIllnessCounts illnesses =
    let
        patient =
            emptyPatient 1 (Date.fromCalendarDate 1990 Jan 1)

        rows =
            generateAcuteIllnessReportData English fromDate [ { patient | acuteIllnessData = Just illnesses } ]
                |> .rows
    in
    List.filterMap
        (\label ->
            List.filter (List.head >> (==) (Just label)) rows
                |> List.head
                |> Maybe.andThen (List.drop 1 >> List.head)
        )
        [ translate English (Translate.AcuteIllnessDiagnosis DiagnosisMalariaUncomplicated)
        , translate English Translate.Total
        , translate English Translate.NoDiagnosis
        ]


acuteIllnessTest : Test
acuteIllnessTest =
    describe "generateAcuteIllnessReportData"
        [ test "each visit with a diagnosis counts in its row and the total" <|
            \_ ->
                acuteIllnessCounts
                    [ [ acuteIllnessVisit 5 (Just DiagnosisMalariaUncomplicated) ]
                    , [ acuteIllnessVisit 6 (Just DiagnosisMalariaUncomplicated), acuteIllnessVisit 9 (Just DiagnosisMalariaUncomplicated) ]
                    ]
                    |> Expect.equal [ "3", "3", "0" ]

        -- What the "no diagnosis" row should count is undecided. These pin
        -- what it counts today: an illness whose latest visit has no diagnosis.
        , test "no diagnosis: an illness whose latest visit has none counts, even when an earlier one had one" <|
            \_ ->
                acuteIllnessCounts
                    [ [ acuteIllnessVisit 5 (Just DiagnosisMalariaUncomplicated), acuteIllnessVisit 9 Nothing ]
                    , [ acuteIllnessVisit 6 Nothing ]
                    ]
                    |> Expect.equal [ "1", "1", "2" ]
        , test "no diagnosis: an illness whose latest visit is before FROM does not count" <|
            \_ ->
                acuteIllnessCounts [ [ { startDate = Date.fromCalendarDate 2026 Feb 20, encounterType = AcuteIllnessEncounterCHW, diagnosis = Nothing } ] ]
                    |> Expect.equal [ "0", "0", "0" ]
        ]


csvRowTest : Test
csvRowTest =
    describe "csvRow"
        [ test "a plain cell is written as it is, a cell with a comma, quote or line break is quoted" <|
            \_ ->
                csvRow [ "plain", "a, b", "say \"hi\"", "line\nbreak", "carriage\u{000D}return", "" ]
                    |> Expect.equal "plain,\"a, b\",\"say \"\"hi\"\"\",\"line\nbreak\",\"carriage\u{000D}return\","
        , test "an empty row is an empty line" <|
            \_ -> Expect.equal "" (csvRow [])
        , test "the ANC Contacts labels that hold commas stay in one column" <|
            \_ ->
                [ IndicatorHistoryOfAdversePregnancyOutcomes, IndicatorHistoryOfAdversePregnancyOutcomesReceivedAzithromycin ]
                    |> List.map (\indicator -> csvRow [ translate English (Translate.PrenatalIndicatorLabel indicator), "3" ])
                    |> Expect.equal
                        [ "\"Pregnant women with a history of preterm births, spontaneous abortion, stillbirth, or intrauterine fetal deaths\",3"
                        , "\"Pregnant women with a history of adverse pregnancy outcomes, who received Azithromycin\",3"
                        ]
        ]


all : Test
all =
    describe "Pages.Reports"
        [ acuteIllnessTest
        , bandsTest
        , calculateNutritionReportDataTest
        , csvRowTest
        , demographicsEncountersTest
        , demographicsPatientsTest
        , incidenceTest
        , peripartumTest
        , periodWindowsTest
        , postnatalCareTest
        , prevalenceTest
        , underSixGateTest
        ]
