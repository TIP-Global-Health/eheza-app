module Pages.Dashboard.Utils exposing
    ( dashboardSlug
    , filterKeySlug
    , filterLabel
    , kpiById
    , monthLabels
    , selectedFilterValue
    , yearSelLabel
    )

import App.Types exposing (Language)
import AssocList as Dict
import Pages.Dashboard.Model
    exposing
        ( Dashboard(..)
        , DashboardLabel(..)
        , Filter
        , FilterKey(..)
        , Kpi
        , Model
        , YearSel(..)
        )
import Time exposing (Month(..))
import Translate exposing (translate)


dashboardSlug : Dashboard -> String
dashboardSlug dashboard =
    case dashboard of
        Facility ->
            "facility"

        Program ->
            "program"


filterKeySlug : FilterKey -> String
filterKeySlug key =
    case key of
        FilterLocation ->
            "location"

        FilterSite ->
            "site"

        FilterTime ->
            "time"


filterLabel : FilterKey -> DashboardLabel
filterLabel key =
    case key of
        FilterLocation ->
            Location

        FilterSite ->
            InterventionSite

        FilterTime ->
            Time


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
selectedFilterValue : Model -> Filter -> String
selectedFilterValue model filter =
    Dict.get filter.key model.selectedFilters
        |> Maybe.withDefault filter.selected


yearSelLabel : Language -> YearSel -> String
yearSelLabel language selection =
    case selection of
        AllYears ->
            translate language (Translate.DashboardLabel AllYearsLabel)

        Year year ->
            String.fromInt year
