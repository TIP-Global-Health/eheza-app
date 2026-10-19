module Pages.Dashboard.Placeholder exposing
    ( alertsFor
    , averageSeries
    , coverageFor
    , defaultTrendKpi
    , drillRow
    , drillYears
    , filtersFor
    , kpisFor
    , performanceSeries
    , targetSeries
    , tilesFor
    )

{-| Placeholder figures, not this installation's data, for the HealthyStart
dashboards. The Time filter scopes only the summary tiles and indicator blocks;
the trend chart follows its own Year selector.
-}

import App.Types exposing (Site)
import AssocList as Dict
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
        , FilterOption(..)
        , Kpi
        , Tile
        , TimePeriod(..)
        , Trend(..)
        )
import Utils.GeoLocation exposing (getGeoDistricts)



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


{-| The filter dropdowns. Intervention Site lists the health centers this
installation holds and Location the site's districts, independently of each
other. None of them change the figures yet.
-}
filtersFor : Dashboard -> Site -> List HealthCenterData -> List Filter
filtersFor dashboard site healthCenters =
    let
        siteOptions =
            List.map OptionName <|
                if List.isEmpty healthCenters then
                    [ "XY Health Center", "Rukara HC", "Nyamata HC", "Gahini HC" ]

                else
                    List.map .name healthCenters

        timeFilter =
            Filter FilterTime
                (List.map OptionPeriod [ PeriodWeek, PeriodMonth, PeriodQuarter, PeriodYear ])
                (OptionPeriod PeriodMonth)
    in
    case dashboard of
        Facility ->
            [ Filter FilterSite siteOptions (List.head siteOptions |> Maybe.withDefault OptionAll)
            , timeFilter
            ]

        Program ->
            let
                districtOptions =
                    Dict.values (getGeoDistricts site)
                        |> List.map .name
                        |> List.sort
                        |> List.map OptionName
            in
            [ Filter FilterLocation (OptionAll :: districtOptions) OptionAll
            , Filter FilterSite (OptionAll :: siteOptions) OptionAll
            , timeFilter
            ]



-- YEARS


{-| The years the drill down table and the year selector offer: from the
first cohort's enrolment year to this year, most recent first. Without that
date, only this year.
-}
drillYears : NominalDate -> Maybe NominalDate -> List Int
drillYears currentDate firstCohortDate =
    let
        currentYear =
            Date.year currentDate

        firstYear =
            Maybe.map (Date.year >> min currentYear) firstCohortDate
                |> Maybe.withDefault currentYear
    in
    List.range firstYear currentYear
        |> List.reverse



-- SERIES GENERATION


{-| A small hash into [0,1). Deterministic, so the same indicator and year
always draw the same curve.
-}
rnd : Int -> Float
rnd n =
    toFloat (modBy 233280 (abs n * 9301 + 49297)) / 233280


{-| Monthly line: wanders around `base` with a gentle upward drift across the
year plus stable noise.
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


{-| An indicator's monthly performance in one year.
-}
performanceSeries : Kpi -> Int -> List Float
performanceSeries kpi =
    monthly kpi.seed kpi.baseCurrent


{-| The monthly inter site average of an indicator in one year.
-}
averageSeries : Kpi -> Int -> List Float
averageSeries kpi =
    monthly (kpi.seed + 777) kpi.baseAvg


{-| An indicator's monthly target: rises from a little below its base early in
the year and flattens near it. The same every year.
-}
targetSeries : Kpi -> List Float
targetSeries kpi =
    List.map (\month -> clamp 0 100 (kpi.baseTarget - 8 + toFloat month / 11 * 8))
        (List.range 0 11)


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
            nth monthIndex (performanceSeries kpi year)

        targ =
            nth monthIndex (targetSeries kpi)

        third =
            case dashboard of
                Facility ->
                    nth monthIndex (averageSeries kpi year)

                Program ->
                    perf - targ
    in
    ( perf, targ, third )
