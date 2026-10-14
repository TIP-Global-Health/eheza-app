module Pages.Dashboard.Utils exposing
    ( dashboardSlug
    , drillDialogId
    , filterKeySlug
    , filterLabel
    , kpiBlockId
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
import Translate exposing (TranslationId, translate)


dashboardSlug : Dashboard -> String
dashboardSlug dashboard =
    case dashboard of
        Facility ->
            "facility"

        Program ->
            "program"


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
            translate language Translate.All

        Year year ->
            String.fromInt year
