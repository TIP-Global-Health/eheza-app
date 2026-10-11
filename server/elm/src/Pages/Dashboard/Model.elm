module Pages.Dashboard.Model exposing
    ( Coverage
    , Dashboard(..)
    , DashboardLabel(..)
    , Filter
    , FilterKey(..)
    , Kpi
    , Model
    , Msg(..)
    , Screen(..)
    , Tile
    , Trend(..)
    , YearSel(..)
    , emptyModel
    )

{-| The HealthyStart dashboards: Facility Performance (Dashboard II) and
Program Monitoring (Dashboard III). Each has its own path, so the model holds
the one on screen.
-}

import AssocList as Dict exposing (Dict)


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
    | FromPreviousMonth
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
    | TrendChartDescriptionFacility String String
    | TrendChartDescriptionProgram String String
    | Trends
    | ValueColumn
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
    , options : List String
    , selected : String
    }


{-| An indicator block: current value, programme target and, on the Facility
dashboard only, the inter site `average`. `base*` centre each placeholder
series, so every indicator draws its own curve.
-}
type alias Kpi =
    { id : String
    , label : DashboardLabel
    , current : String
    , target : String
    , average : Maybe String
    , deltaValue : String
    , deltaDir : Trend
    , seed : Int
    , baseCurrent : Float
    , baseTarget : Float
    , baseAvg : Float
    }


{-| A numeric summary tile in the row beneath the filters.
-}
type alias Tile =
    { label : DashboardLabel
    , value : String
    }


{-| A horizontal coverage bar. `pct` is the filled percentage; the rest of the
bar renders as a grey track.
-}
type alias Coverage =
    { emphasis : DashboardLabel
    , label : DashboardLabel
    , pct : Int
    }


{-| Direction of the "x% from prev. month" indicator: Up is a green triangle,
Down a red one. As in the mockups, colour shows direction, not clinical merit.
-}
type Trend
    = Down
    | Up


{-| The year the trend chart plots: one year, or the mean of all of them.
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
    , selectedFilters : Dict FilterKey String
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
    | SetFilter FilterKey String
