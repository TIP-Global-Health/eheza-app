module Pages.Dashboard.Utils exposing
    ( dashboardSlug
    , defaultTrendKpi
    , drillDialogId
    , drillYears
    , filterKeySlug
    , filterLabel
    , filterOptionLabel
    , filterOptionValue
    , filtersFor
    , kpiBlockId
    , kpiById
    , monthLabels
    , selectedFilterValue
    , yearSelLabel
    )

import App.Types exposing (Language, Site)
import AssocList as Dict
import Backend.Components.Model exposing (HealthCenterData)
import Date
import Gizra.NominalDate exposing (NominalDate)
import Pages.Components.Utils exposing (healthCentersByName)
import Pages.Dashboard.Model
    exposing
        ( Dashboard(..)
        , DashboardLabel(..)
        , Filter
        , FilterKey(..)
        , FilterOption(..)
        , Kpi
        , Model
        , TimePeriod(..)
        , YearSel(..)
        )
import Time exposing (Month(..))
import Translate exposing (TranslationId, translate)
import Utils.GeoLocation exposing (getGeoDistricts)


dashboardSlug : Dashboard -> String
dashboardSlug dashboard =
    case dashboard of
        Facility ->
            "facility"

        Program ->
            "program"


{-| The indicator the trend chart plots until the viewer picks another one.
-}
defaultTrendKpi : Dashboard -> String
defaultTrendKpi dashboard =
    case dashboard of
        Facility ->
            "ultrasound"

        Program ->
            "ultrasound-p"


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


{-| The drill down dialog, which takes focus while it is open.
-}
drillDialogId : String
drillDialogId =
    "dashboard-drill-down"


{-| The block that opens an indicator's drill down, and takes focus back when
the drill down closes.
-}
kpiBlockId : Kpi -> String
kpiBlockId kpi =
    "kpi-block-" ++ kpi.id


filterKeySlug : FilterKey -> String
filterKeySlug key =
    case key of
        FilterLocation ->
            "location"

        FilterSite ->
            "site"

        FilterTime ->
            "time"


filterLabel : FilterKey -> TranslationId
filterLabel key =
    case key of
        FilterLocation ->
            Translate.Location

        FilterSite ->
            Translate.DashboardLabel InterventionSite

        FilterTime ->
            Translate.DashboardLabel Time


filterOptionLabel : Language -> FilterOption -> String
filterOptionLabel language option =
    case option of
        OptionAll ->
            translate language Translate.All

        OptionHealthCenter healthCenter ->
            healthCenter.name

        OptionName name ->
            name

        OptionPeriod period ->
            translate language <|
                case period of
                    PeriodMonth ->
                        Translate.MonthLabel

                    PeriodQuarter ->
                        Translate.DashboardLabel Quarter

                    PeriodWeek ->
                        Translate.DashboardLabel Week

                    PeriodYear ->
                        Translate.YearLabel


{-| The `value` of a dropdown option, unique within its filter, so the viewer's
pick maps back to the option.
-}
filterOptionValue : FilterOption -> String
filterOptionValue option =
    case option of
        OptionAll ->
            "all"

        OptionHealthCenter healthCenter ->
            "health-center-" ++ String.fromInt healthCenter.id

        OptionName name ->
            "name-" ++ name

        OptionPeriod period ->
            case period of
                PeriodMonth ->
                    "month"

                PeriodQuarter ->
                    "quarter"

                PeriodWeek ->
                    "week"

                PeriodYear ->
                    "year"


{-| The filter dropdowns. Intervention Site lists the health centers Drupal
hands over, by name; Location lists the site's districts. The two are
independent.
-}
filtersFor : Dashboard -> Site -> List HealthCenterData -> List Filter
filtersFor dashboard site healthCenters =
    let
        siteOptions =
            healthCentersByName healthCenters
                |> List.map OptionHealthCenter

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


kpiById : List Kpi -> String -> Maybe Kpi
kpiById kpis id =
    List.filter (\kpi -> kpi.id == id) kpis
        |> List.head


{-| Short month names, in the order the chart and the drill down table use.
-}
monthLabels : Language -> List String
monthLabels language =
    List.map (\month -> translate language (Translate.ResolveMonth True month))
        [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]


{-| The option a filter shows: what the viewer picked, or its default.
-}
selectedFilterValue : Model -> Filter -> FilterOption
selectedFilterValue model filter =
    Dict.get filter.key model.selectedFilters
        |> Maybe.withDefault filter.selected


yearSelLabel : Language -> YearSel -> String
yearSelLabel language selection =
    case selection of
        AllYears ->
            translate language Translate.All

        Year year ->
            String.fromInt year
