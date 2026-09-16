module Pages.Dashboard.Placeholder exposing
    ( alertsFor
    , coverageFor
    , defaultTrendKpi
    , drillRow
    , drillYears
    , filtersFor
    , kpisFor
    , tilesFor
    , trendSeries
    )

{-| The figures the HealthyStart dashboards render, and the generator that
turns them into monthly series. They are representative maternal and child
health numbers, not data read from this installation: this module stands in
until the computed data arrives, and goes away with it.

The generator is deterministic, so an indicator and a year always draw the same
curve.

-}

import Backend.Components.Model exposing (HealthCenterData)
import Date
import Gizra.NominalDate exposing (NominalDate)
import Pages.Dashboard.Model
    exposing
        ( Coverage
        , Dashboard(..)
        , DashboardLabel(..)
        , Filter
        , FilterKey(..)
        , Kpi
        , Tile
        , Trend(..)
        , YearSel(..)
        )



-- SUMMARY TILES


tilesFor : Dashboard -> List Tile
tilesFor dashboard =
    case dashboard of
        Facility ->
            [ Tile WomenCurrentlyInAncCare "142"
            , Tile PregnanciesFirstTrimester "38"
            , Tile PregnanciesSecondTrimester "57"
            , Tile PregnanciesThirdTrimester "47"
            , Tile AncMissRate "12%"
            , Tile TotalDeliveriesInstitutional "214"
            , Tile TotalChildrenOnRoutineCare "389"
            , Tile GraduatedMotherChildPairs "156"
            , Tile LostToFollowUp "23"
            ]

        Program ->
            [ Tile WomenCurrentlyInAncCare "2,418"
            , Tile PregnanciesFirstTrimester "646"
            , Tile PregnanciesSecondTrimester "921"
            , Tile PregnanciesThirdTrimester "851"
            , Tile AncMissRate "14%"
            , Tile TotalDeliveriesProgram "3,772"
            , Tile TotalChildrenOnRoutineCare "6,540"
            , Tile GraduatedMotherChildPairs "2,689"
            , Tile LostToFollowUp "402"
            ]



-- INDICATOR BLOCKS


kpisFor : Dashboard -> List Kpi
kpisFor dashboard =
    case dashboard of
        Facility ->
            [ Kpi "anc-booking" EarlyAncBookingRate "68%" "80%" (Just "64%") "3.5%" Up 11 70 88 66
            , Kpi "median-ga" MedianGaAtFirstAnc "10.8 wk" "≤ 12 wk" (Just "11.6 wk") "5%" Up 23 62 80 58
            , Kpi "ultrasound" EarlyUltrasoundCoverage "74%" "85%" (Just "70%") "1.5%" Up 37 74 92 78
            , Kpi "high-risk" HighRiskPregnancyRate "16%" "< 15%" (Just "18%") "2.5%" Down 41 20 15 23
            , Kpi "pph" PostpartumHemorrhageRate "3.2%" "< 5%" (Just "4.1%") "6%" Up 53 12 8 16
            , Kpi "complete-anc" CompleteAncAttendanceRate "58%" "70%" (Just "55%") "5%" Up 67 60 80 56
            ]

        Program ->
            [ Kpi "anc-cov80" EarlyAncCoverageFirstTrimester "72%" "80%" Nothing "3.5%" Up 71 72 86 70
            , Kpi "ultrasound-p" EarlyUltrasoundCoverage "66%" "80%" Nothing "1.5%" Down 83 68 88 66
            , Kpi "inadequate-gwg" SitesWithInadequateGwg "18%" "< 10%" Nothing "4.5%" Up 91 24 12 26
            , Kpi "delayed-bw" SitesWithDelayedBirthWeightRecording "14%" "< 5%" Nothing "1.5%" Down 97 16 8 18
            , Kpi "preterm-sga" SitesWithElevatedPretermOrSgaBirths "20%" "< 15%" Nothing "0.5%" Up 103 24 16 26
            , Kpi "poor-growth" SitesWithPoorChildGrowthOutcomes "15%" "< 10%" Nothing "1.2%" Up 109 18 12 20
            ]


{-| The indicator the trend chart plots until the viewer picks another one.
-}
defaultTrendKpi : Dashboard -> String
defaultTrendKpi dashboard =
    case dashboard of
        Facility ->
            "ultrasound"

        Program ->
            "ultrasound-p"



-- COVERAGE BARS


coverageFor : Dashboard -> List Coverage
coverageFor dashboard =
    case dashboard of
        Facility ->
            [ Coverage Aspirin ProphylaxisCoverage 82
            , Coverage Calcium ProphylaxisCoverage 74
            , Coverage Sqlns SupplementCoverage 68
            ]

        Program ->
            [ Coverage Aspirin ProphylaxisCoverage 79
            , Coverage Calcium ProphylaxisCoverage 71
            , Coverage Sqlns SupplementCoverage 65
            ]



-- CRITICAL ALERTS


alertsFor : Dashboard -> List DashboardLabel
alertsFor dashboard =
    case dashboard of
        Facility ->
            [ AlertBirthsMissingBirthWeight 3
            , AlertPretermRateExceeded
            ]

        Program ->
            [ AlertSitesWithDelayedBirthWeightRecording 6
            , AlertSitesWithPretermRate 4
            ]



-- FILTERS


{-| The filter dropdowns. The Intervention Site options are the health centers
this installation holds; the rest are placeholder options, and none of them
change the figures yet.
-}
filtersFor : Dashboard -> List HealthCenterData -> List Filter
filtersFor dashboard healthCenters =
    let
        siteNames =
            if List.isEmpty healthCenters then
                [ "XY Health Center", "Rukara HC", "Nyamata HC", "Gahini HC" ]

            else
                List.map .name healthCenters

        timeFilter =
            Filter FilterTime [ "All", "This month", "This quarter", "This year" ] "All"
    in
    case dashboard of
        Facility ->
            [ Filter FilterSite siteNames (List.head siteNames |> Maybe.withDefault "")
            , timeFilter
            ]

        Program ->
            [ Filter FilterSite ("All" :: siteNames) "All"
            , timeFilter
            , Filter FilterLocation [ "All", "Eastern Province", "Kayonza District", "Rwamagana District" ] "All"
            ]



-- YEARS


{-| The years the drill down table and the year selector offer: this year and
the three before it, most recent first.
-}
drillYears : NominalDate -> List Int
drillYears currentDate =
    let
        currentYear =
            Date.year currentDate
    in
    List.map (\offset -> currentYear - offset) (List.range 0 3)



-- SERIES GENERATION


{-| A small hash into [0,1). Deterministic, so the same indicator and year
always draw the same curve.
-}
rnd : Int -> Float
rnd n =
    toFloat (modBy 233280 (abs n * 9301 + 49297)) / 233280


{-| Monthly performance or average line: wanders around `base` with a gentle
upward drift across the year plus stable noise.
-}
monthly : Int -> Float -> Int -> List Float
monthly seed base yearKey =
    List.map
        (\month ->
            let
                drift =
                    toFloat month * 0.9

                noise =
                    (rnd (seed * 131 + yearKey * 17 + month * 7) - 0.5) * 13
            in
            clamp 0 100 (base - 5 + drift + noise)
        )
        (List.range 0 11)


{-| Target line: rises from a little below `base` early in the year and
flattens near `base`.
-}
targetMonthly : Float -> List Float
targetMonthly base =
    List.map (\month -> clamp 0 100 (base - 8 + toFloat month / 11 * 8))
        (List.range 0 11)


{-| Element wise average of equal length lists, used to collapse the years into
a single series.
-}
elementAvg : List (List Float) -> List Float
elementAvg lists =
    List.map
        (\index ->
            let
                values =
                    List.filterMap (\list -> List.drop index list |> List.head) lists
            in
            if List.isEmpty values then
                0

            else
                List.sum values / toFloat (List.length values)
        )
        (List.range 0 11)


yearKeys : List Int -> YearSel -> List Int
yearKeys years selection =
    case selection of
        AllYears ->
            years

        Year year ->
            [ year ]


{-| The three plotted series for an indicator and the selected year, or the
mean across all years.
-}
trendSeries : List Int -> Kpi -> YearSel -> { current : List Float, target : List Float, avg : List Float }
trendSeries years kpi selection =
    let
        keys =
            yearKeys years selection
    in
    { current = elementAvg (List.map (monthly kpi.seed kpi.baseCurrent) keys)
    , target = targetMonthly kpi.baseTarget
    , avg = elementAvg (List.map (monthly (kpi.seed + 777) kpi.baseAvg) keys)
    }


{-| One (performance, target, third) triple for a drill down cell. `third` is
the inter site average on the Facility dashboard, and performance minus target
on the Program dashboard.
-}
drillRow : Dashboard -> Kpi -> Int -> Int -> ( Float, Float, Float )
drillRow dashboard kpi year monthIndex =
    let
        nth index values =
            List.drop index values |> List.head |> Maybe.withDefault 0

        perf =
            nth monthIndex (monthly kpi.seed kpi.baseCurrent year)

        targ =
            nth monthIndex (targetMonthly kpi.baseTarget)

        third =
            case dashboard of
                Facility ->
                    nth monthIndex (monthly (kpi.seed + 777) kpi.baseAvg year)

                Program ->
                    perf - targ
    in
    ( perf, targ, third )
