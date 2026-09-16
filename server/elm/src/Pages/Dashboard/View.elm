module Pages.Dashboard.View exposing (view)

{-| The HealthyStart dashboards: a banner, the filter row, the summary tiles,
the indicator blocks and coverage bars, the trends panel and the critical
alerts, plus the month by month drill down that opens over the dimmed
dashboard.
-}

import App.Types exposing (Language)
import Backend.Components.Model exposing (MenuData)
import Backend.Model exposing (ModelBackend)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (Attribute, Html, a, button, div, h1, h2, label, li, option, p, select, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, colspan, for, href, id, selected, style, tabindex, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode
import Maybe.Extra
import Pages.Components.TrendChart as TrendChart
import Pages.Components.Utils exposing (reportTableDataToCSV, reportTablesDataToCSV)
import Pages.Dashboard.Model
    exposing
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
        )
import Pages.Dashboard.Placeholder as Placeholder
import Pages.Dashboard.Utils
    exposing
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
import Pages.Model exposing (MetricsResultsTableData)
import Pages.Utils exposing (viewBackendData)
import Svg
import Svg.Attributes as SA
import Translate exposing (translate)


view : Language -> NominalDate -> ModelBackend -> Dashboard -> Model -> Html Msg
view language currentDate modelBackend dashboard model =
    viewBackendData modelBackend.dashboardData
        (\data -> viewDashboard language currentDate data dashboard model)


viewDashboard : Language -> NominalDate -> MenuData -> Dashboard -> Model -> Html Msg
viewDashboard language currentDate data dashboard model =
    let
        filters =
            Placeholder.filtersFor dashboard data.healthCenters

        years =
            Placeholder.drillYears currentDate

        tiles =
            Placeholder.tilesFor dashboard

        kpis =
            Placeholder.kpisFor dashboard

        coverage =
            Placeholder.coverageFor dashboard
    in
    div [ class "ehs-dashboard mx-auto w-full max-w-[1440px] px-3 py-4 md:px-6" ]
        [ div
            (class "ehs-dashboard__page overflow-hidden rounded-lg bg-white shadow-sm ring-1 ring-slate-200"
                :: behindDrillDownAttributes model.screen
            )
            [ viewBanner language
                dashboard
                (resolveSiteName language dashboard filters model)
                (dashboardExportMsg language dashboard tiles kpis coverage)
            , div [ class "px-4 py-4 md:px-6" ]
                [ viewFilterRow language dashboard filters model
                , viewTileRow language tiles
                , div [ class "mt-6 grid grid-cols-1 gap-6 lg:grid-cols-2" ]
                    [ viewLeftColumn language kpis coverage
                    , viewRightColumn language dashboard years kpis model
                    ]
                ]
            ]
        , case model.screen of
            DashboardScreen ->
                emptyNode

            DrillScreen kpi ->
                viewDrillDown language dashboard years kpi
        ]


{-| While the drill down is open the dashboard under it is out of reach: the
dialog says it covers the page, so nothing behind it should be reachable by tab
or readable by a screen reader.
-}
behindDrillDownAttributes : Screen -> List (Attribute Msg)
behindDrillDownAttributes screen =
    case screen of
        DashboardScreen ->
            []

        DrillScreen _ ->
            [ attribute "aria-hidden" "true", attribute "inert" "" ]


{-| The Facility dashboard is about the site the filter selects; the Program
dashboard is about all of them at once.
-}
resolveSiteName : Language -> Dashboard -> List Filter -> Model -> String
resolveSiteName language dashboard filters model =
    case dashboard of
        Facility ->
            List.filter (\filter -> filter.key == FilterSite) filters
                |> List.head
                |> Maybe.map (selectedFilterValue model)
                |> Maybe.withDefault ""

        Program ->
            translateLabel language ProgramLevel



-- BANNER


viewBanner : Language -> Dashboard -> String -> Msg -> Html Msg
viewBanner language dashboard siteName exportMsg =
    let
        ( number, title ) =
            case dashboard of
                Facility ->
                    ( DashboardTwo, DashboardTitleFacility )

                Program ->
                    ( DashboardThree, DashboardTitleProgram )
    in
    div [ class "grid grid-cols-1 items-center gap-3 bg-accent px-4 py-4 text-white md:grid-cols-3 md:px-6" ]
        [ div [ class "flex items-center gap-3" ]
            [ div
                [ class "flex h-12 w-12 shrink-0 items-center justify-center rounded bg-white/90 text-accent"
                , attribute "aria-hidden" "true"
                ]
                [ viewDashboardIcon dashboard ]
            , div []
                [ p [ class "text-xl font-extrabold underline decoration-white/60 underline-offset-4" ]
                    [ text <| translateLabel language number ]
                , p [ class "text-sm text-white/90" ] [ text <| translateLabel language title ]
                ]
            ]
        , div [ class "text-center" ]
            [ h1 [ class "border-b border-white/60 pb-1 text-2xl font-extrabold tracking-tight md:text-3xl" ]
                [ text <| translateLabel language HealthyStart ]
            , p [ class "mt-1 text-lg font-semibold" ] [ text siteName ]
            ]
        , div [ class "ehs-dashboard__actions flex items-center justify-start gap-6 md:justify-end" ]
            [ viewBannerAction exportMsg Export language
            , viewBannerAction PrintPage Print language
            , a [ href "/user/logout", class bannerActionClass ]
                [ text <| translateLabel language LogOut ]
            ]
        ]


viewBannerAction : Msg -> DashboardLabel -> Language -> Html Msg
viewBannerAction msg labelId language =
    button
        [ type_ "button"
        , onClick msg
        , class bannerActionClass
        ]
        [ text <| translateLabel language labelId ]


{-| The banner's actions read as one row, whether they act on the page or leave
it.
-}
bannerActionClass : String
bannerActionClass =
    "text-lg font-semibold underline decoration-2 underline-offset-4 hover:text-white/80 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-white"


viewDashboardIcon : Dashboard -> Html Msg
viewDashboardIcon dashboard =
    case dashboard of
        Facility ->
            Svg.svg
                [ SA.viewBox "0 0 24 24", SA.class "h-7 w-7", SA.fill "currentColor" ]
                [ Svg.path [ SA.d "M4 21V7l8-4 8 4v14h-5v-5h-6v5H4Zm7-9h2v-2h2V9h-2V7h-2v2H9v2h2v1Z" ] [] ]

        Program ->
            Svg.svg
                [ SA.viewBox "0 0 24 24", SA.class "h-7 w-7", SA.fill "currentColor" ]
                [ Svg.circle [ SA.cx "12", SA.cy "5", SA.r "2.4" ] []
                , Svg.circle [ SA.cx "5", SA.cy "12", SA.r "2.4" ] []
                , Svg.circle [ SA.cx "19", SA.cy "12", SA.r "2.4" ] []
                , Svg.circle [ SA.cx "12", SA.cy "19", SA.r "2.4" ] []
                , Svg.path [ SA.d "M12 7v10M7 12h10", SA.stroke "currentColor", SA.strokeWidth "1.6" ] []
                ]



-- FILTER ROW


viewFilterRow : Language -> Dashboard -> List Filter -> Model -> Html Msg
viewFilterRow language dashboard filters model =
    div [ class "ehs-dashboard__filters flex flex-wrap items-end gap-x-5 gap-y-3" ]
        (span [ class "rounded bg-accent px-3 py-1 text-sm font-semibold text-white" ]
            [ text <| translateLabel language FilterLabel ]
            :: List.map (viewFilterControl language dashboard model) filters
        )


viewFilterControl : Language -> Dashboard -> Model -> Filter -> Html Msg
viewFilterControl language dashboard model filter =
    let
        selectId =
            "filter-" ++ dashboardSlug dashboard ++ "-" ++ filterKeySlug filter.key

        selectedValue =
            selectedFilterValue model filter
    in
    div [ class "flex flex-col gap-1" ]
        [ label [ for selectId, class "text-sm font-bold text-slate-800" ]
            [ text <| translate language (filterLabel filter.key) ]
        , select
            [ id selectId
            , class "rounded border border-slate-300 bg-white px-2 py-1.5 text-sm text-slate-900 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent"
            , onInput (SetFilter filter.key)
            ]
            (List.map
                (\optionValue ->
                    option [ value optionValue, selected (optionValue == selectedValue) ]
                        [ text optionValue ]
                )
                filter.options
            )
        ]



-- SUMMARY TILE ROW


viewTileRow : Language -> List Tile -> Html Msg
viewTileRow language tiles =
    div [ class "mt-4 grid grid-cols-2 gap-2 sm:grid-cols-3 md:grid-cols-5 lg:grid-cols-9" ]
        (List.map (viewTile language) tiles)


viewTile : Language -> Tile -> Html Msg
viewTile language tile =
    div [ class "flex flex-col overflow-hidden rounded ring-1 ring-slate-200" ]
        [ div [ class "flex min-h-[3.25rem] items-center justify-center bg-accent px-2 py-1.5 text-center text-[11px] font-bold leading-tight text-white" ]
            [ text <| translateLabel language tile.label ]
        , div [ class "bg-white py-1 text-center text-xl font-extrabold text-accent" ]
            [ text tile.value ]
        ]



-- INDICATOR BLOCKS AND COVERAGE BARS


viewLeftColumn : Language -> List Kpi -> List Coverage -> Html Msg
viewLeftColumn language kpis coverage =
    div []
        [ div [ class "grid grid-cols-1 gap-4 sm:grid-cols-2 xl:grid-cols-3" ]
            (List.map (viewKpiBlock language) kpis)
        , div [ class "mt-6 flex flex-col gap-4" ]
            (List.map (viewCoverageBar language) coverage)
        ]


viewKpiBlock : Language -> Kpi -> Html Msg
viewKpiBlock language kpi =
    let
        kpiLabel =
            translateLabel language kpi.label
    in
    button
        [ type_ "button"
        , id (kpiBlockId kpi)
        , onClick (OpenDrill kpi)
        , attribute "aria-label" (translateLabel language OpenMonthByMonthDetailFor ++ " " ++ kpiLabel)
        , class "flex w-full flex-col rounded-lg border border-slate-200 p-3 text-left transition hover:border-accent hover:shadow-sm focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent"
        ]
        [ h2 [ class "min-h-[2.5rem] text-sm font-bold leading-tight text-slate-800" ] [ text kpiLabel ]
        , div [ class "mt-2 flex items-stretch gap-3 border-t border-slate-100 pt-2" ]
            [ div [ class "flex flex-col" ]
                [ span [ class "text-3xl font-extrabold leading-none text-accent" ] [ text kpi.current ]
                , viewTrendIndicator language kpi
                ]
            , div [ class "w-px self-stretch bg-slate-200" ] []
            , div [ class "flex flex-col justify-center gap-1 text-sm" ]
                (viewKpiReference language kpi.target ProgramTargetShort
                    :: (Maybe.map
                            (\average -> [ viewKpiReference language average InterventionSiteAverage ])
                            kpi.average
                            |> Maybe.withDefault []
                       )
                )
            ]
        ]


viewKpiReference : Language -> String -> DashboardLabel -> Html Msg
viewKpiReference language reference labelId =
    div []
        [ span [ class "font-bold text-accent" ] [ text reference ]
        , span [ class "ml-1 text-xs text-slate-600" ] [ text <| translateLabel language labelId ]
        ]


viewTrendIndicator : Language -> Kpi -> Html Msg
viewTrendIndicator language kpi =
    let
        ( glyph, colorClass ) =
            case kpi.deltaDir of
                Down ->
                    ( "▼", "text-alert" )

                Up ->
                    ( "▲", "text-trend-avg" )
    in
    div [ class "mt-1 flex items-center gap-1" ]
        [ span [ class ("text-xs " ++ colorClass), attribute "aria-hidden" "true" ] [ text glyph ]
        , span [ class "text-xs italic text-slate-600" ]
            [ text (kpi.deltaValue ++ " " ++ translateLabel language FromPreviousMonth) ]
        ]


viewCoverageBar : Language -> Coverage -> Html Msg
viewCoverageBar language coverage =
    let
        emphasis =
            translateLabel language coverage.emphasis

        coverageLabel =
            translateLabel language coverage.label

        filled =
            String.fromInt coverage.pct ++ "%"
    in
    div []
        [ p [ class "mb-1 text-sm" ]
            [ span [ class "font-bold text-accent" ] [ text emphasis ]
            , span [ class "ml-1 font-semibold text-slate-800" ] [ text coverageLabel ]
            ]
        , div
            [ class "relative h-6 w-full overflow-hidden rounded bg-bar-track"
            , attribute "role" "img"
            , attribute "aria-label" (emphasis ++ " " ++ coverageLabel ++ ": " ++ filled)
            ]
            [ div
                [ class "flex h-full items-center justify-end rounded bg-accent pr-2"
                , style "width" filled
                ]
                [ span [ class "text-xs font-bold text-white" ] [ text filled ] ]
            ]
        ]



-- TRENDS AND CRITICAL ALERTS


viewRightColumn : Language -> Dashboard -> List Int -> List Kpi -> Model -> Html Msg
viewRightColumn language dashboard years kpis model =
    div []
        [ viewTrendsPanel language dashboard years kpis model
        , viewAlertsPanel language dashboard
        ]


viewTrendsPanel : Language -> Dashboard -> List Int -> List Kpi -> Model -> Html Msg
viewTrendsPanel language dashboard years kpis model =
    let
        selectedId =
            Maybe.withDefault (Placeholder.defaultTrendKpi dashboard) model.trendKpi
    in
    kpiById kpis selectedId
        |> Maybe.Extra.orElse (List.head kpis)
        |> Maybe.map (viewTrends language dashboard years model kpis selectedId)
        |> Maybe.withDefault emptyNode


viewTrends : Language -> Dashboard -> List Int -> Model -> List Kpi -> String -> Kpi -> Html Msg
viewTrends language dashboard years model kpis selectedId selectedKpi =
    let
        series =
            Placeholder.trendSeries years selectedKpi model.trendYear

        chartSeries =
            [ TrendChart.Series "#0273b2" (translateLabel language CurrentPerformance) "" series.current
            , TrendChart.Series "#c25e00" (translateLabel language (targetLegendLabel dashboard)) "8 4" series.target
            ]
                ++ (case dashboard of
                        Facility ->
                            [ TrendChart.Series "#2f7d32" (translateLabel language InterventionSiteAverage) "2 4" series.avg ]

                        Program ->
                            []
                   )

        selectId =
            "trend-kpi-" ++ dashboardSlug dashboard
    in
    div [ class "overflow-hidden rounded-lg border border-slate-200" ]
        [ div [ class "flex flex-wrap items-center gap-3 bg-accent px-3 py-2 text-white" ]
            [ h2 [ class "text-lg font-bold" ] [ text <| translateLabel language Trends ]
            , label [ for selectId, class "sr-only" ] [ text <| translateLabel language SelectKpiToPlot ]
            , select
                [ id selectId
                , class "max-w-full rounded border border-white/40 bg-white px-2 py-1 text-sm font-semibold text-slate-900 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-white"
                , onInput SelectTrendKpi
                ]
                (List.map
                    (\kpi ->
                        option [ value kpi.id, selected (kpi.id == selectedId) ]
                            [ text <| translateLabel language kpi.label ]
                    )
                    kpis
                )
            ]
        , div [ class "bg-white p-3" ]
            [ viewYearSelector language years model.trendYear
            , TrendChart.view
                { xLabels = monthLabels language
                , series = chartSeries
                , ariaLabel = chartDescription language dashboard selectedKpi model.trendYear
                }
            , viewLegend chartSeries
            ]
        ]


targetLegendLabel : Dashboard -> DashboardLabel
targetLegendLabel dashboard =
    case dashboard of
        Facility ->
            InstitutionalTarget

        Program ->
            ProgramTarget


chartDescription : Language -> Dashboard -> Kpi -> YearSel -> String
chartDescription language dashboard kpi selection =
    let
        kpiLabel =
            translateLabel language kpi.label

        yearLabel =
            yearSelLabel language selection
    in
    case dashboard of
        Facility ->
            translateLabel language (TrendChartDescriptionFacility kpiLabel yearLabel)

        Program ->
            translateLabel language (TrendChartDescriptionProgram kpiLabel yearLabel)


viewYearSelector : Language -> List Int -> YearSel -> Html Msg
viewYearSelector language years current =
    div [ class "mb-2 flex flex-wrap items-center gap-3" ]
        (span [ class "font-bold text-slate-900" ] [ text (translate language Translate.YearLabel ++ ":") ]
            :: List.map (viewYearButton language current) (AllYears :: List.map Year years)
        )


viewYearButton : Language -> YearSel -> YearSel -> Html Msg
viewYearButton language current selection =
    let
        isActive =
            current == selection
    in
    button
        [ type_ "button"
        , onClick (SelectTrendYear selection)
        , attribute "aria-pressed"
            (if isActive then
                "true"

             else
                "false"
            )
        , class "rounded px-1.5 text-lg focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent"
        , classList
            [ ( "font-extrabold text-accent underline underline-offset-4", isActive )
            , ( "font-medium text-slate-700 hover:text-accent", not isActive )
            ]
        ]
        [ text <| yearSelLabel language selection ]


viewLegend : List TrendChart.Series -> Html Msg
viewLegend series =
    div [ class "mt-2 flex flex-wrap items-center justify-center gap-x-4 gap-y-1" ]
        (List.map viewLegendItem series)


viewLegendItem : TrendChart.Series -> Html Msg
viewLegendItem series =
    div [ class "flex items-center gap-2" ]
        [ Svg.svg
            [ SA.viewBox "0 0 28 10", SA.class "h-2.5 w-7", attribute "aria-hidden" "true" ]
            [ Svg.line
                [ SA.x1 "0"
                , SA.y1 "5"
                , SA.x2 "28"
                , SA.y2 "5"
                , SA.stroke series.color
                , SA.strokeWidth "3"
                , SA.strokeDasharray series.dash
                ]
                []
            ]
        , span [ class "text-sm text-slate-800" ] [ text series.label ]
        ]


viewAlertsPanel : Language -> Dashboard -> Html Msg
viewAlertsPanel language dashboard =
    let
        alerts =
            Placeholder.alertsFor dashboard
    in
    div [ class "mt-6 overflow-hidden rounded-lg border border-slate-200" ]
        [ h2 [ class "bg-accent px-3 py-2 text-lg font-bold text-white" ]
            [ text <| translateLabel language CriticalAlerts ]
        , div [ class "bg-white p-3" ]
            [ if List.isEmpty alerts then
                p [ class "text-sm text-slate-600" ] [ text <| translateLabel language NoActiveAlerts ]

              else
                ul [ class "flex flex-col gap-2" ] (List.map (viewAlert language) alerts)
            ]
        ]


viewAlert : Language -> DashboardLabel -> Html Msg
viewAlert language alert =
    li [ class "flex items-center gap-2.5" ]
        [ span [ class "inline-block h-3.5 w-3.5 shrink-0 rounded-full bg-alert", attribute "aria-hidden" "true" ] []
        , span [ class "text-base text-slate-800" ] [ text <| translateLabel language alert ]
        ]



-- DRILL DOWN


viewDrillDown : Language -> Dashboard -> List Int -> Kpi -> Html Msg
viewDrillDown language dashboard years kpi =
    let
        kpiLabel =
            translateLabel language kpi.label
    in
    div
        [ class "ehs-dashboard__drill fixed inset-0 z-50 flex items-start justify-center overflow-y-auto bg-slate-900/50 p-4"
        , id drillDialogId
        , tabindex -1
        , onEscape CloseDrill
        , attribute "role" "dialog"
        , attribute "aria-modal" "true"
        , attribute "aria-label" (kpiLabel ++ " — " ++ translateLabel language MonthByMonthDetail)
        ]
        [ div [ class "mt-10 w-full max-w-[1200px] rounded-lg bg-white p-5 shadow-2xl md:p-6" ]
            [ h2 [ class "mb-3 text-2xl font-bold text-accent" ] [ text kpiLabel ]
            , div [ class "overflow-x-auto" ] [ viewDrillTable language dashboard years kpi ]
            , div [ class "mt-4 flex justify-end gap-6" ]
                [ viewDrillAction language (drillExportMsg language dashboard years kpi) Export
                , viewDrillAction language PrintPage Print
                , viewDrillAction language CloseDrill Return
                ]
            ]
        ]


viewDrillAction : Language -> Msg -> DashboardLabel -> Html Msg
viewDrillAction language msg labelId =
    button
        [ type_ "button"
        , onClick msg
        , class "text-lg font-semibold text-accent underline decoration-2 underline-offset-4 hover:text-accent-dark focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent"
        ]
        [ text <| translateLabel language labelId ]


viewDrillTable : Language -> Dashboard -> List Int -> Kpi -> Html Msg
viewDrillTable language dashboard years kpi =
    let
        monthHeader backgroundClass =
            th [ class (backgroundClass ++ " p-2"), attribute "scope" "col" ]
                [ span [ class "sr-only" ] [ text <| translate language Translate.MonthLabel ] ]
    in
    table [ class "w-full min-w-[760px] border-collapse text-sm" ]
        [ thead []
            [ tr []
                (monthHeader "bg-brand-navy"
                    :: List.map viewYearGroupHeader years
                )
            , tr []
                (monthHeader "bg-slate-200"
                    :: List.concatMap (viewSubHeaders language dashboard) years
                )
            ]
        , tbody []
            (List.indexedMap (viewDrillMonthRow dashboard years kpi) (monthLabels language))
        ]


viewYearGroupHeader : Int -> Html Msg
viewYearGroupHeader year =
    th
        [ class "bg-brand-navy p-2 text-lg font-bold text-white"
        , colspan 3
        , attribute "scope" "colgroup"
        ]
        [ text <| String.fromInt year ]


viewSubHeaders : Language -> Dashboard -> Int -> List (Html Msg)
viewSubHeaders language dashboard year =
    List.map (viewSubHeaderCell language year) (drillColumnLabels dashboard)


viewSubHeaderCell : Language -> Int -> DashboardLabel -> Html Msg
viewSubHeaderCell language year labelId =
    th [ class "bg-slate-200 px-3 py-1.5 font-semibold text-slate-800", attribute "scope" "col" ]
        [ span [ class "sr-only" ] [ text (String.fromInt year ++ " ") ]
        , text <| translateLabel language labelId
        ]


{-| The Facility drill down compares against the inter site average; the
Program one shows the distance from the target instead.
-}
drillColumnLabels : Dashboard -> List DashboardLabel
drillColumnLabels dashboard =
    case dashboard of
        Facility ->
            [ Performance, Target, Average ]

        Program ->
            [ Performance, Target, Delta ]


viewDrillMonthRow : Dashboard -> List Int -> Kpi -> Int -> String -> Html Msg
viewDrillMonthRow dashboard years kpi monthIndex monthName =
    let
        zebra =
            if modBy 2 monthIndex == 0 then
                "bg-slate-50"

            else
                "bg-slate-100"
    in
    tr [ class zebra ]
        (th [ class "bg-slate-300 px-3 py-1.5 text-left font-semibold text-slate-900", attribute "scope" "row" ]
            [ text monthName ]
            :: List.concatMap
                (\year ->
                    List.map (\cellValue -> td [ class "px-3 py-1.5 text-center text-slate-800" ] [ text cellValue ])
                        (drillCellValues dashboard kpi year monthIndex)
                )
                years
        )


{-| The three values of one drill down cell group, as they are shown and
exported.
-}
drillCellValues : Dashboard -> Kpi -> Int -> Int -> List String
drillCellValues dashboard kpi year monthIndex =
    let
        ( perf, targ, third ) =
            Placeholder.drillRow dashboard kpi year monthIndex

        thirdValue =
            case dashboard of
                Facility ->
                    percentage third

                Program ->
                    signedValue third
    in
    [ percentage perf, percentage targ, thirdValue ]



-- EXPORT


{-| The dashboard export: the summary tiles and the coverage bars as label and
value pairs, then the indicator blocks with the figures they compare against.
-}
dashboardExportMsg : Language -> Dashboard -> List Tile -> List Kpi -> List Coverage -> Msg
dashboardExportMsg language dashboard tiles kpis coverage =
    let
        labelAndValue labelId value =
            [ translateLabel language labelId, value ]

        summaryRows =
            List.map (\tile -> labelAndValue tile.label tile.value) tiles
                ++ List.map
                    (\bar ->
                        [ translateLabel language bar.emphasis ++ " " ++ translateLabel language bar.label
                        , String.fromInt bar.pct ++ "%"
                        ]
                    )
                    coverage

        kpiCaptions =
            [ IndicatorColumn, CurrentPerformance, ProgramTargetShort ]
                ++ (case dashboard of
                        Facility ->
                            [ InterventionSiteAverage ]

                        Program ->
                            []
                   )

        kpiRow kpi =
            [ translateLabel language kpi.label, kpi.current, kpi.target ]
                ++ (Maybe.map List.singleton kpi.average |> Maybe.withDefault [])
    in
    DownloadCSV
        (exportFileName dashboard "dashboard")
        (reportTablesDataToCSV
            [ MetricsResultsTableData
                (translateLabel language (dashboardTitleLabel dashboard))
                (List.map (translateLabel language) [ IndicatorColumn, ValueColumn ])
                summaryRows
            , MetricsResultsTableData
                (translateLabel language Indicators)
                (List.map (translateLabel language) kpiCaptions)
                (List.map kpiRow kpis)
            ]
        )


{-| The drill down export: one row per month, one column group per year.
-}
drillExportMsg : Language -> Dashboard -> List Int -> Kpi -> Msg
drillExportMsg language dashboard years kpi =
    let
        captions =
            translate language Translate.MonthLabel
                :: List.concatMap
                    (\year ->
                        List.map
                            (\labelId -> String.fromInt year ++ " " ++ translateLabel language labelId)
                            (drillColumnLabels dashboard)
                    )
                    years

        rows =
            List.indexedMap
                (\monthIndex monthName ->
                    monthName
                        :: List.concatMap (\year -> drillCellValues dashboard kpi year monthIndex) years
                )
                (monthLabels language)
    in
    DownloadCSV
        (exportFileName dashboard kpi.id)
        (reportTableDataToCSV
            (MetricsResultsTableData (translateLabel language kpi.label) captions rows)
        )


exportFileName : Dashboard -> String -> String
exportFileName dashboard suffix =
    "healthystart-" ++ dashboardSlug dashboard ++ "-" ++ suffix ++ ".csv"


dashboardTitleLabel : Dashboard -> DashboardLabel
dashboardTitleLabel dashboard =
    case dashboard of
        Facility ->
            DashboardTitleFacility

        Program ->
            DashboardTitleProgram



-- HELPERS


{-| Fires only on Escape; any other key decodes to nothing and no message is
sent.
-}
onEscape : Msg -> Attribute Msg
onEscape msg =
    on "keydown"
        (Json.Decode.field "key" Json.Decode.string
            |> Json.Decode.andThen
                (\key ->
                    if key == "Escape" then
                        Json.Decode.succeed msg

                    else
                        Json.Decode.fail "not Escape"
                )
        )


translateLabel : Language -> DashboardLabel -> String
translateLabel language labelId =
    translate language (Translate.DashboardLabel labelId)


percentage : Float -> String
percentage value =
    String.fromInt (round value) ++ "%"


signedValue : Float -> String
signedValue value =
    let
        rounded =
            round value
    in
    (if rounded > 0 then
        "+"

     else
        ""
    )
        ++ String.fromInt rounded
