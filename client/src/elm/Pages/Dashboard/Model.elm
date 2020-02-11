module Pages.Dashboard.Model exposing
    ( Card
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
import Backend.Measurement.Model exposing (FamilyPlanningSign)
import Html exposing (Html)
import Pages.Page exposing (DashboardPage(..), Page(..))


type FilterPeriod
    = ThisMonth
    | LastMonth
    | ThreeMonths
    | OneYear


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
    = All
    | Female
    | Male


filterGenders : List FilterGender
filterGenders =
    [ All
    , Female
    , Male
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
    , currentTotalChartsFilter : FilterCharts
    , currentCaseManagementFilter : FilterCharts
    , latestPage : DashboardPage
    }


emptyModel : Model
emptyModel =
    { period = OneYear
    , beneficiariesGender = All
    , currentTotalChartsFilter = Stunting
    , currentCaseManagementFilter = Stunting
    , latestPage = MainPage
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
    { title : Html Msg
    , value : Int
    , valueSeverity : CardValueSeverity
    }


{-| A `Stat Card` that will appear in the dashboard and display a certain statistic with difference from last year.
-}
type alias StatsCard =
    { title : Html Msg
    , cardClasses : String
    , cardAction : Maybe DashboardPage
    , value : Int
    , valueSeverity : CardValueSeverity
    , valueIsPercentage : Bool
    , previousPercentage : Int
    , previousPercentageLabel : FilterPeriod
    , newCases : Maybe Int
    }


type FilterType
    = FilterTotalsChart
    | FilterCaseManagement


type Msg
    = SetFilterGender FilterGender
    | SetFilterPeriod FilterPeriod
    | SetFilterTotalsChart FilterCharts
    | SetFilterCaseManagement FilterCharts
    | SetActivePage Page
