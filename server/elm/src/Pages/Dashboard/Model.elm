module Pages.Dashboard.Model exposing
    ( Coverage
    , Dashboard(..)
    , DashboardLabel(..)
    , Figures
    , Filter
    , FilterKey(..)
    , FilterOption(..)
    , Kpi
    , KpiSeries
    , KpiUnit(..)
    , Model
    , Msg(..)
    , Screen(..)
    , Tile
    , TimePeriod(..)
    , YearSel(..)
    , emptyModel
    )

{-| The HealthyStart dashboards: Facility Performance (Dashboard II) and
Program Monitoring (Dashboard III). Each has its own path, so the model holds
the one on screen.
-}

import AssocList as Dict exposing (Dict)
import Backend.Components.Model exposing (HealthCenterData)


{-| Facility covers a single intervention site; Program aggregates across all
of them.
-}
type Dashboard
    = Facility
    | Program


{-| Every piece of text the dashboards show. Kept as one type so the data the
dashboards render carries translation ids rather than English strings.
-}
type DashboardLabel
    = AlertBirthsMissingBirthWeight Int
    | AlertPretermRateExceeded
    | AlertSitesWithDelayedBirthWeightRecording Int
    | AlertSitesWithPretermRate Int
    | AncMissRate
    | Aspirin
    | Average
    | Calcium
    | CompleteAncAttendanceRate
    | CriticalAlerts
    | CurrentPerformance
    | DashboardThree
    | DashboardTitleFacility
    | DashboardTitleProgram
    | DashboardTwo
    | Delta
    | EarlyAncBookingRate
    | EarlyAncCoverageFirstTrimester
    | EarlyUltrasoundCoverage
    | Export
    | FilterLabel
    | FromPreviousPeriod TimePeriod
    | GraduatedMotherChildPairs
    | HealthyStart
    | HighRiskPregnancyRate
    | IndicatorColumn
    | Indicators
    | InstitutionalTarget
    | InterventionSite
    | InterventionSiteAverage
    | LogOut
    | LostToFollowUp
    | MedianGaAtFirstAnc
    | MonthByMonthDetail
    | NoActiveAlerts
    | OpenMonthByMonthDetailFor
    | Performance
    | PostpartumHemorrhageRate
    | PregnanciesFirstTrimester
    | PregnanciesSecondTrimester
    | PregnanciesThirdTrimester
    | Print
    | ProgramLevel
    | ProgramTarget
    | ProgramTargetShort
    | ProphylaxisCoverage
    | Quarter
    | Return
    | SelectKpiToPlot
    | SitesWithDelayedBirthWeightRecording
    | SitesWithElevatedPretermOrSgaBirths
    | SitesWithInadequateGwg
    | SitesWithPoorChildGrowthOutcomes
    | Sqlns
    | SupplementCoverage
    | Target
    | Time
    | TotalChildrenOnRoutineCare
    | TotalDeliveriesInstitutional
    | TotalDeliveriesProgram
    | TrendChartDescriptionAllYearsFacility String String
    | TrendChartDescriptionAllYearsProgram String String
    | TrendChartDescriptionFacility String String
    | TrendChartDescriptionProgram String String
    | Trends
    | ValueColumn
    | ValueInWeeks String
    | Week
    | WomenCurrentlyInAncCare


{-| One of the dropdowns above the summary tiles.
-}
type FilterKey
    = FilterLocation
    | FilterSite
    | FilterTime


{-| A dropdown and the options it offers. `selected` is the option shown until
the viewer picks another one.
-}
type alias Filter =
    { key : FilterKey
    , options : List FilterOption
    , selected : FilterOption
    }


{-| One entry of a filter dropdown: "All", a health center, a district name, or
a span of time.
-}
type FilterOption
    = OptionAll
    | OptionHealthCenter HealthCenterData
    | OptionName String
    | OptionPeriod TimePeriod


{-| The spans the Time filter offers. Weeks count from the first cohort's
enrolment date.
-}
type TimePeriod
    = PeriodMonth
    | PeriodQuarter
    | PeriodWeek
    | PeriodYear


{-| What one dashboard shows, worked out for the filters selected.
-}
type alias Figures =
    { tiles : List Tile
    , kpis : List Kpi
    , coverage : List Coverage
    , alerts : List DashboardLabel
    , series : Kpi -> KpiSeries
    }


{-| An indicator block: current value, programme target and, shown on the
Facility dashboard only, the inter site `average`. `delta` is the change from
the previous period.
-}
type alias Kpi =
    { id : String
    , label : DashboardLabel
    , unit : KpiUnit
    , current : Maybe Float
    , target : String
    , average : Maybe Float
    , delta : Maybe Float
    }


{-| What an indicator's numbers count: a percentage, or gestational weeks.
-}
type KpiUnit
    = UnitPercent
    | UnitWeeks


{-| An indicator's monthly values, one per month from January, for the trend
chart and the drill down. `performance` and `average` take the year.
-}
type alias KpiSeries =
    { performance : Int -> List (Maybe Float)
    , target : List (Maybe Float)
    , average : Int -> List (Maybe Float)
    }


{-| A numeric summary tile in the row beneath the filters.
-}
type alias Tile =
    { label : DashboardLabel
    , value : String
    }


{-| A horizontal coverage bar. `pct` is the filled percentage; the rest of the
bar renders as a grey track. Nothing leaves the bar empty.
-}
type alias Coverage =
    { emphasis : DashboardLabel
    , label : DashboardLabel
    , pct : Maybe Float
    }


{-| The year the trend chart plots: one year, or all of them, one line each.
-}
type YearSel
    = AllYears
    | Year Int


{-| What is on screen. A drill down is the month by month table of one
indicator, open on top of the dimmed dashboard.
-}
type Screen
    = DashboardScreen
    | DrillScreen Kpi


type alias Model =
    { screen : Screen
    , selectedFilters : Dict FilterKey FilterOption
    , trendKpi : Maybe String
    , trendYear : YearSel
    }


emptyModel : Model
emptyModel =
    { screen = DashboardScreen
    , selectedFilters = Dict.empty
    , trendKpi = Nothing
    , trendYear = AllYears
    }


type Msg
    = CloseDrill
    | DownloadCSV String String
    | NoOp
    | OpenDrill Kpi
    | PrintPage
    | SelectTrendKpi String
    | SelectTrendYear YearSel
    | SetFilter FilterKey FilterOption
