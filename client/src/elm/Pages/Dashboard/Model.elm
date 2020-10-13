module Pages.Dashboard.Model exposing
    ( BeneficiariesTableLabels(..)
    , Card
    , CardValueSeverity(..)
    , DashboardFilter(..)
    , DashboardSubFilter(..)
    , FamilyPlanningSignsCounter
    , FilterGender(..)
    , FilterPeriod(..)
    , FilterType(..)
    , MalnorishedNutritionData
    , Model
    , MonthlyChartType(..)
    , Msg(..)
    , StatsCard
    , caseManagementFilters
    , caseManagementSubFilters
    , emptyModel
    , filterGenders
    , filterPeriodsForCaseManagementPage
    , filterPeriodsForMainPage
    , filterPeriodsForStatsPage
    , monthlyChartFilters
    )

{-| Filtering by period
-}

import AssocList exposing (Dict)
import Backend.Dashboard.Model exposing (ParticipantStats)
import Backend.Measurement.Model exposing (FamilyPlanningSign)
import Backend.Person.Model exposing (Gender)
import Gizra.NominalDate exposing (NominalDate)
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
    | Total


filterPeriodsForMainPage : List FilterPeriod
filterPeriodsForMainPage =
    []


filterPeriodsForCaseManagementPage : List FilterPeriod
filterPeriodsForCaseManagementPage =
    []


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
type DashboardFilter
    = Stunting
    | Underweight
    | Wasting
    | MUAC
    | MissedSession


type DashboardSubFilter
    = FilterTotal
    | FilterModerate
    | FilterSevere


monthlyChartFilters : List DashboardFilter
monthlyChartFilters =
    [ Stunting
    , Underweight
    , Wasting
    , MUAC
    ]


caseManagementFilters : List DashboardFilter
caseManagementFilters =
    [ Stunting
    , Underweight
    , Wasting
    , MUAC
    , MissedSession
    ]


caseManagementSubFilters : DashboardFilter -> List DashboardSubFilter
caseManagementSubFilters mainFilter =
    case mainFilter of
        MissedSession ->
            []

        _ ->
            [ FilterTotal
            , FilterModerate
            , FilterSevere
            ]


type alias Model =
    { period : FilterPeriod
    , beneficiariesGender : FilterGender
    , currentBeneficiariesChartsFilter : DashboardFilter
    , currentBeneficiariesIncidenceChartsFilter : DashboardFilter
    , currentCaseManagementFilter : DashboardFilter
    , currentCaseManagementSubFilter : DashboardSubFilter
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
    , currentCaseManagementSubFilter = FilterTotal
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


type alias MalnorishedNutritionData =
    { identifier : Int
    , birthDate : NominalDate
    , gender : Gender
    , nutritionStatus : Backend.Dashboard.Model.NutritionStatus
    }


type FilterType
    = FilterBeneficiariesChart
    | FilterBeneficiariesIncidenceChart
    | FilterCaseManagement


type MonthlyChartType
    = MonthlyChartTotals
    | MonthlyChartIncidence


type Msg
    = ModalToggle Bool (List ParticipantStats) String
    | NavigateToStuntingTable DashboardSubFilter
    | SetFilterGender FilterGender
    | SetFilterPeriod FilterPeriod
    | SetFilterBeneficiariesChart DashboardFilter FilterType
    | SetFilterCaseManagement DashboardFilter
    | SetSubFilterCaseManagement DashboardSubFilter
    | SetActivePage Page
