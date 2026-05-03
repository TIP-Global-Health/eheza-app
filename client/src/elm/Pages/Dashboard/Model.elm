module Pages.Dashboard.Model exposing (BeneficiariesTableLabels(..), CardValueSeverity(..), DashboardFilter(..), DashboardSubFilter(..), FamilyPlanningSignsCounter, FeverCause(..), FilterGender(..), FilterPeriod(..), FilterProgramType(..), FilterType(..), MalnorishedNutritionData, ModalState(..), Model, MonthlyChartType(..), Msg(..), StatsCard, allFeverCauses, caseManagementFilters, caseManagementSubFilters, emptyModel, filterGenders, filterPeriodsForStatsPage, maxMonthGap, monthlyChartFilters)

import AssocList exposing (Dict)
import Backend.Dashboard.Model exposing (EducationSessionData, ParticipantStats)
import Backend.Entities exposing (VillageId)
import Backend.Measurement.Model exposing (FamilyPlanningSign, Gender)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isJust)
import Pages.Page exposing (DashboardPage(..), Page)


type FilterPeriod
    = ThisMonth
    | LastMonth
    | ThreeMonthsAgo
    | OneYear


type FilterProgramType
    = FilterAllPrograms
    | FilterProgramAchi
    | FilterProgramFbf
    | FilterProgramPmtct
    | FilterProgramSorwathe
    | FilterProgramCommunity


type BeneficiariesTableLabels
    = New
    | Missed
    | Malnourished
    | Total


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
    , programTypeFilter : FilterProgramType
    , selectedVillageFilter : Maybe VillageId
    , beneficiariesGender : FilterGender
    , currentBeneficiariesChartsFilter : DashboardFilter
    , currentBeneficiariesIncidenceChartsFilter : DashboardFilter
    , currentCaseManagementFilter : DashboardFilter
    , currentCaseManagementSubFilter : DashboardSubFilter
    , educationSessionDrillIn : Maybe EducationSessionData
    , latestPage : DashboardPage
    , modalState : Maybe ModalState

    -- This is used by month selector to determine
    -- the gap from current month. We allow to go back
    -- 6 months, so, valid values are between 0 and 5.
    , monthGap : Int
    }


emptyModel : Maybe VillageId -> Model
emptyModel maybeSelectedVillage =
    let
        ( programTypeFilter, selectedVillage ) =
            if isJust maybeSelectedVillage then
                -- This is CHW Nurse, as only CHW work with villages.
                ( FilterProgramCommunity
                , maybeSelectedVillage
                )

            else
                ( FilterAllPrograms
                , Nothing
                )
    in
    { period = OneYear
    , programTypeFilter = programTypeFilter
    , selectedVillageFilter = selectedVillage
    , beneficiariesGender = Boys
    , currentBeneficiariesChartsFilter = Stunting
    , currentBeneficiariesIncidenceChartsFilter = Stunting
    , currentCaseManagementFilter = Stunting
    , currentCaseManagementSubFilter = FilterTotal
    , educationSessionDrillIn = Nothing
    , latestPage = PageMain
    , modalState = Nothing
    , monthGap = 0
    }


maxMonthGap : Int
maxMonthGap =
    5


type ModalState
    = StatisticsModal String (List ParticipantStats)
    | FiltersModal


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


type FeverCause
    = FeverCauseCovid19
    | FeverCauseMalaria
    | FeverCauseRespiratory
    | FeverCauseGI
    | FeverCauseUnknown


allFeverCauses : List FeverCause
allFeverCauses =
    [ FeverCauseCovid19
    , FeverCauseMalaria
    , FeverCauseRespiratory
    , FeverCauseGI
    , FeverCauseUnknown
    ]


type Msg
    = SetModalState (Maybe ModalState)
    | Reset (Maybe VillageId)
    | ChangeMonthGap Int
    | NavigateToStuntingTable DashboardSubFilter
    | SetFilterGender FilterGender
    | SetFilterPeriod FilterPeriod
    | SetFilterBeneficiariesChart DashboardFilter FilterType
    | SetFilterCaseManagement DashboardFilter
    | SetSubFilterCaseManagement DashboardSubFilter
    | SetFilterProgramType String
    | SetSelectedVillage String
    | SetActivePage Page
    | SetEducationSessionDrillIn (Maybe EducationSessionData)
