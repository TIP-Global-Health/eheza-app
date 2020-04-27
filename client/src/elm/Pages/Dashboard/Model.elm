module Pages.Dashboard.Model exposing
    ( BeneficiariesTableLabels(..)
    , Card
    , CardValueSeverity(..)
    , FamilyPlanningSignsCounter
    , FilterCharts(..)
    , FilterGender(..)
    , FilterPeriod(..)
    , FilterType(..)
    , Model
    , Msg(..)
    , StatsCard
    , emptyModel
    , filterCharts
    , filterGenders
    , filterPeriods
    , filterPeriodsForStatsPage
    )

{-| Filtering by period
-}

import AssocList exposing (Dict)
import Backend.Dashboard.Model exposing (ParticipantStats)
import Backend.Measurement.Model exposing (FamilyPlanningSign)
import Pages.Page exposing (DashboardPage(..), Page(..))


type FilterPeriod
    = ThisMonth
    | LastMonth
    | ThreeMonthsAgo
    | OneYear


type BeneficiariesTableLabels
    = New
    | Missed
    | Malnourished


filterPeriods : List FilterPeriod
filterPeriods =
    [ OneYear ]


filterPeriodsForStatsPage : List FilterPeriod
filterPeriodsForStatsPage =
    [ ThisMonth
    , LastMonth
    ]


{-| We don't use the existing `Gender`, as we want to have an `All` option as-well
-}
type FilterGender
    = Boys
    | Girls


filterGenders : List FilterGender
filterGenders =
    [ Boys
    , Girls
    ]


{-| Define the charts filters.
-}
type FilterCharts
    = Stunting
    | Underweight
    | Wasting
    | MUAC


filterCharts : List FilterCharts
filterCharts =
    [ Stunting
    , Underweight
    , Wasting
    , MUAC
    ]


type alias Model =
    { period : FilterPeriod
    , beneficiariesGender : FilterGender
    , currentBeneficiariesChartsFilter : FilterCharts
    , currentBeneficiariesIncidenceChartsFilter : FilterCharts
    , currentCaseManagementFilter : FilterCharts
    , latestPage : DashboardPage
    , modalTable : List ParticipantStats
    , modalTitle : String
    , modalState : Bool
    }


emptyModel : Model
emptyModel =
    { period = OneYear
    , beneficiariesGender = Boys
    , currentBeneficiariesChartsFilter = Stunting
    , currentBeneficiariesIncidenceChartsFilter = Stunting
    , currentCaseManagementFilter = Stunting
    , latestPage = MainPage
    , modalTable = []
    , modalTitle = ""
    , modalState = False
    }


{-| A record to hold the count of total signs used.
-}
type alias FamilyPlanningSignsCounter =
    Dict FamilyPlanningSign Int


{-| This will define what color the `value` from a `Card` will appear in.
-}
type CardValueSeverity
    = Neutral
    | Good
    | Moderate
    | Severe


{-| A `Card` that will appear in the dashboard.
-}
type alias Card =
    { title : String
    , value : Int
    , valueSeverity : CardValueSeverity
    }


{-| A `Stat Card` that will appear in the dashboard and display a certain statistic with difference from last year.
-}
type alias StatsCard =
    { title : String
    , cardClasses : String
    , cardAction : Maybe Msg
    , value : Int
    , valueSeverity : CardValueSeverity
    , valueIsPercentage : Bool
    , previousPercentage : Int
    , previousPercentageLabel : FilterPeriod
    , newCases : Maybe Int
    }


type FilterType
    = FilterBeneficiariesChart
    | FilterBeneficiariesIncidenceChart
    | FilterCaseManagement


type Msg
    = ModalToggle Bool (List ParticipantStats) String
    | SetFilterGender FilterGender
    | SetFilterPeriod FilterPeriod
    | SetFilterBeneficiariesChart FilterCharts FilterType
    | SetFilterCaseManagement FilterCharts
    | SetActivePage Page
