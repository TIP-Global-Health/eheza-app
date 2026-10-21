module Pages.Reports.Test exposing (all)

import AssocList as Dict exposing (Dict)
import Backend.Components.Model exposing (PersonId)
import Backend.Reports.Model exposing (Gender(..), NutritionData, NutritionEncounterData, PatientData)
import Date
import Expect exposing (FloatingPointTolerance(..))
import Gizra.NominalDate exposing (NominalDate)
import Pages.Reports.Model exposing (NutritionMetrics, NutritionMetricsResults, NutritionReportData, emptyNutritionMetrics)
import Pages.Reports.Utils
    exposing
        ( calculateNutritionReportData
        , generateIncidenceNutritionMetricsResults
        , generatePrevalenceNutritionMetricsResults
        , resolveDataSetForMonth
        , resolveDataSetForQuarter
        , resolveDataSetForYear
        , resolvePreviousDataSetForMonth
        , resolveQuarterForIndex
        )
import Test exposing (Test, describe, test)
import Time exposing (Month(..))


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
                    mother =
                        emptyPatient 1 (Date.fromCalendarDate 1995 Mar 1)

                    visit date =
                        { startDate = date, ahezaAmount = Just 500 }
                in
                [ { mother | familyNutritionData = Just [ [ visit (Date.fromCalendarDate 2025 May 2), visit (Date.fromCalendarDate 2025 Jun 2) ] ] } ]
                    |> calculate
                    |> Expect.equal { impacted = [], encountersByMonth = Dict.empty }
        ]


all : Test
all =
    describe "Pages.Reports"
        [ bandsTest
        , calculateNutritionReportDataTest
        , incidenceTest
        , periodWindowsTest
        , prevalenceTest
        , underSixGateTest
        ]
