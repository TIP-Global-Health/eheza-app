module Pages.Dashboard.View exposing (view)

{-| The HealthyStart dashboards: banner, filters, summary tiles, indicator
blocks, coverage bars, trends, critical alerts, and the monthly drill down that
opens over the dimmed dashboard.
-}

import App.Types exposing (Language)
import Backend.Dashboard.Model exposing (DashboardData)
import Backend.Model exposing (ModelBackend)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (Attribute, Html, a, button, div, h1, h2, label, li, option, p, select, span, table, tbody, td, text, th, thead, tr, ul)
import Html.Attributes exposing (attribute, class, classList, colspan, for, href, id, selected, style, tabindex, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode
import List.Extra
import Maybe.Extra
import Pages.Components.TrendChart as TrendChart
import Pages.Components.Utils exposing (isSyncComplete, reportTableDataToCSV, reportTablesDataToCSV, viewSyncingPlaceholder)
import Pages.Dashboard.Figures exposing (facilityFigures, programFigures)
import Pages.Dashboard.Model
    exposing
        ( Coverage
        , Dashboard(..)
        , DashboardLabel(..)
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
        )
import Pages.Dashboard.Utils
    exposing
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
import Pages.Model exposing (MetricsResultsTableData)
import Pages.Utils exposing (viewBackendData)
import Round
import Svg
import Svg.Attributes as SA
import Translate exposing (translate)


view : Language -> NominalDate -> ModelBackend -> Dashboard -> Model -> Html Msg
view language currentDate modelBackend dashboard model =
    viewBackendData modelBackend.dashboardData
        (\data -> viewDashboard language currentDate data dashboard model)


viewDashboard : Language -> NominalDate -> DashboardData -> Dashboard -> Model -> Html Msg
viewDashboard language currentDate data dashboard model =
    if isSyncComplete data.remainingForDownload then
        viewFigures language currentDate data dashboard model

    else
        div [ class "ehs-dashboard mx-auto w-full max-w-[1440px] px-3 py-4 md:px-6" ]
            [ viewSyncingPlaceholder language (List.length data.records) data.remainingForDownload ]


viewFigures : Language -> NominalDate -> DashboardData -> Dashboard -> Model -> Html Msg
viewFigures language currentDate data dashboard model =
    let
        filters =
            filtersFor dashboard data.menu.site data.menu.healthCenters

        selectedOption key =
            List.Extra.find (\filter -> filter.key == key) filters
                |> Maybe.map (selectedFilterValue model)

        period =
            case selectedOption FilterTime of
                Just (OptionPeriod selected) ->
                    selected

                _ ->
                    PeriodMonth

        years =
            drillYears currentDate data.firstCohortDate

        figures =
            case dashboard of
                Facility ->
                    let
                        site =
                            case selectedOption FilterSite of
                                Just (OptionHealthCenter healthCenter) ->
                                    Just healthCenter.id

                                _ ->
                                    Nothing
                    in
                    facilityFigures currentDate data.firstCohortDate data.records site period

                Program ->
                    programFigures currentDate data.firstCohortDate data.records period
    in
    div [ class "ehs-dashboard mx-auto w-full max-w-[1440px] px-3 py-4 md:px-6" ]
        [ div
            (class "ehs-dashboard__page overflow-hidden rounded-lg bg-white shadow-sm ring-1 ring-slate-200"
                :: behindDrillDownAttributes model.screen
            )
            [ viewBanner language
                dashboard
                (resolveSiteName language dashboard selectedOption)
                (dashboardExportMsg language dashboard figures.tiles figures.kpis figures.coverage)
            , div [ class "px-4 py-4 md:px-6" ]
                [ viewFilterRow language dashboard filters model
                , viewTileRow language figures.tiles
                , div [ class "mt-6 grid grid-cols-1 gap-6 lg:grid-cols-2" ]
                    [ viewLeftColumn language dashboard period figures.kpis figures.coverage
                    , div []
                        [ viewTrendsPanel language dashboard years figures.kpis figures.series model
                        , viewAlertsPanel language figures.alerts
                        ]
                    ]
                ]
            ]
        , case model.screen of
            DashboardScreen ->
                emptyNode

            DrillScreen kpi ->
                viewDrillDown language dashboard years kpi (figures.series kpi)
        ]


{-| While the drill down is open, the dashboard behind it is out of reach: the
dialog covers the page, so tab and screen readers skip what is under it.
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
resolveSiteName : Language -> Dashboard -> (FilterKey -> Maybe FilterOption) -> String
resolveSiteName language dashboard selectedOption =
    case dashboard of
        Facility ->
            selectedOption FilterSite
                |> Maybe.map (filterOptionLabel language)
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

        selectedOption =
            selectedFilterValue model filter

        setFilter optionValue =
            List.filter (\option -> filterOptionValue option == optionValue) filter.options
                |> List.head
                |> Maybe.map (SetFilter filter.key)
                |> Maybe.withDefault NoOp
    in
    div [ class "flex flex-col gap-1" ]
        [ label [ for selectId, class "text-sm font-bold text-slate-800" ]
            [ text <| translate language (filterLabel filter.key) ]
        , select
            [ id selectId
            , class "rounded border border-slate-300 bg-white px-2 py-1.5 text-sm text-slate-900 focus-visible:outline focus-visible:outline-2 focus-visible:outline-offset-2 focus-visible:outline-accent"
            , onInput setFilter
            ]
            (List.map
                (\filterOption ->
                    option
                        [ value (filterOptionValue filterOption)
                        , selected (filterOption == selectedOption)
                        ]
                        [ text <| filterOptionLabel language filterOption ]
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


viewLeftColumn : Language -> Dashboard -> TimePeriod -> List Kpi -> List Coverage -> Html Msg
viewLeftColumn language dashboard period kpis coverage =
    div []
        [ div [ class "grid grid-cols-1 gap-4 sm:grid-cols-2 xl:grid-cols-3" ]
            (List.map (viewKpiBlock language dashboard period) kpis)
        , div [ class "mt-6 flex flex-col gap-4" ]
            (List.map (viewCoverageBar language) coverage)
        ]


viewKpiBlock : Language -> Dashboard -> TimePeriod -> Kpi -> Html Msg
viewKpiBlock language dashboard period kpi =
    let
        kpiLabel =
            translateLabel language kpi.label

        averageReference =
            case dashboard of
                Facility ->
                    [ viewKpiReference language (formatValue language kpi.unit kpi.average) InterventionSiteAverage ]

                Program ->
                    []
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
                [ span [ class "text-3xl font-extrabold leading-none text-accent" ] [ text (formatValue language kpi.unit kpi.current) ]
                , viewTrendIndicator language period kpi
                ]
            , div [ class "w-px self-stretch bg-slate-200" ] []
            , div [ class "flex flex-col justify-center gap-1 text-sm" ]
                (viewKpiReference language kpi.target ProgramTargetShort :: averageReference)
            ]
        ]


viewKpiReference : Language -> String -> DashboardLabel -> Html Msg
viewKpiReference language reference labelId =
    div []
        [ span [ class "font-bold text-accent" ] [ text reference ]
        , span [ class "ml-1 text-xs text-slate-600" ] [ text <| translateLabel language labelId ]
        ]


{-| The change from the previous period. As in the mockups, a green triangle
points up and a red one down: colour shows direction, not clinical merit.
-}
viewTrendIndicator : Language -> TimePeriod -> Kpi -> Html Msg
viewTrendIndicator language period kpi =
    Maybe.map
        (\delta ->
            let
                glyph =
                    if delta > 0 then
                        span [ class "text-xs text-trend-avg", attribute "aria-hidden" "true" ] [ text "▲" ]

                    else if delta < 0 then
                        span [ class "text-xs text-alert", attribute "aria-hidden" "true" ] [ text "▼" ]

                    else
                        emptyNode
            in
            div [ class "mt-1 flex items-center gap-1" ]
                [ glyph
                , span [ class "text-xs italic text-slate-600" ]
                    [ text (formatValue language kpi.unit (Just (abs delta)) ++ " " ++ translateLabel language (FromPreviousPeriod period)) ]
                ]
        )
        kpi.delta
        |> Maybe.withDefault emptyNode


viewCoverageBar : Language -> Coverage -> Html Msg
viewCoverageBar language coverage =
    let
        emphasis =
            translateLabel language coverage.emphasis

        coverageLabel =
            translateLabel language coverage.label

        filled =
            formatValue language UnitPercent coverage.pct

        width =
            Maybe.Extra.unwrap "0%" (\pct -> String.fromInt (round pct) ++ "%") coverage.pct

        -- A short bar has no room for its value, which then follows it.
        valueInside =
            Maybe.Extra.unwrap False (\pct -> pct >= 15) coverage.pct
    in
    div []
        [ p [ class "mb-1 text-sm" ]
            [ span [ class "font-bold text-accent" ] [ text emphasis ]
            , span [ class "ml-1 font-semibold text-slate-800" ] [ text coverageLabel ]
            ]
        , div
            [ class "relative flex h-6 w-full items-center overflow-hidden rounded bg-bar-track"
            , attribute "role" "img"
            , attribute "aria-label" (emphasis ++ " " ++ coverageLabel ++ ": " ++ filled)
            ]
            (if valueInside then
                [ div
                    [ class "flex h-full items-center justify-end rounded bg-accent pr-2"
                    , style "width" width
                    ]
                    [ span [ class "text-xs font-bold text-white" ] [ text filled ] ]
                ]

             else
                [ div [ class "h-full rounded bg-accent", style "width" width ] []
                , span [ class "ml-2 text-xs font-bold text-slate-800" ] [ text filled ]
                ]
            )
        ]



-- TRENDS AND CRITICAL ALERTS


viewTrendsPanel : Language -> Dashboard -> List Int -> List Kpi -> (Kpi -> KpiSeries) -> Model -> Html Msg
viewTrendsPanel language dashboard years kpis seriesOf model =
    let
        selectedId =
            Maybe.withDefault (defaultTrendKpi dashboard) model.trendKpi
    in
    kpiById kpis selectedId
        |> Maybe.Extra.orElse (List.head kpis)
        |> Maybe.map (\kpi -> viewTrends language dashboard years model kpis selectedId kpi (seriesOf kpi))
        |> Maybe.withDefault emptyNode


viewTrends : Language -> Dashboard -> List Int -> Model -> List Kpi -> String -> Kpi -> KpiSeries -> Html Msg
viewTrends language dashboard years model kpis selectedId selectedKpi series =
    let
        performanceColor =
            "#0273b2"

        targetLine =
            TrendChart.Series "#c25e00"
                (translateLabel language (targetLegendLabel dashboard))
                "8 4"
                series.target

        -- Solid lines, none of them close to the target's orange. Repeats
        -- after six years.
        yearColors =
            [ performanceColor, "#7b3294", "#00838f", "#ad1457", "#475569", "#2f7d32" ]

        yearLine index year =
            TrendChart.Series
                (List.drop (modBy (List.length yearColors) index) yearColors
                    |> List.head
                    |> Maybe.withDefault performanceColor
                )
                (String.fromInt year)
                ""
                (series.performance year)

        chartSeries =
            case model.trendYear of
                AllYears ->
                    List.indexedMap yearLine years ++ [ targetLine ]

                Year year ->
                    [ TrendChart.Series performanceColor
                        (translateLabel language CurrentPerformance)
                        ""
                        (series.performance year)
                    , targetLine
                    ]
                        ++ (case dashboard of
                                Facility ->
                                    [ TrendChart.Series "#2f7d32"
                                        (translateLabel language InterventionSiteAverage)
                                        "2 4"
                                        (series.average year)
                                    ]

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
                , ariaLabel = chartDescription language dashboard years selectedKpi model.trendYear
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


chartDescription : Language -> Dashboard -> List Int -> Kpi -> YearSel -> String
chartDescription language dashboard years kpi selection =
    let
        kpiLabel =
            translateLabel language kpi.label

        yearList =
            String.join ", " (List.map String.fromInt years)
    in
    translateLabel language <|
        case ( selection, dashboard ) of
            ( AllYears, Facility ) ->
                TrendChartDescriptionAllYearsFacility kpiLabel yearList

            ( AllYears, Program ) ->
                TrendChartDescriptionAllYearsProgram kpiLabel yearList

            ( Year year, Facility ) ->
                TrendChartDescriptionFacility kpiLabel (String.fromInt year)

            ( Year year, Program ) ->
                TrendChartDescriptionProgram kpiLabel (String.fromInt year)


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


viewAlertsPanel : Language -> List DashboardLabel -> Html Msg
viewAlertsPanel language alerts =
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


viewDrillDown : Language -> Dashboard -> List Int -> Kpi -> KpiSeries -> Html Msg
viewDrillDown language dashboard years kpi series =
    let
        kpiLabel =
            translateLabel language kpi.label

        rows =
            drillRows language dashboard years kpi series
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
            , div [ class "overflow-x-auto" ] [ viewDrillTable language dashboard years rows ]
            , div [ class "mt-4 flex justify-end gap-6" ]
                [ viewDrillAction language (drillExportMsg language dashboard years kpi rows) Export
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


viewDrillTable : Language -> Dashboard -> List Int -> List ( String, List String ) -> Html Msg
viewDrillTable language dashboard years rows =
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
            (List.indexedMap viewDrillMonthRow rows)
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


viewDrillMonthRow : Int -> ( String, List String ) -> Html Msg
viewDrillMonthRow monthIndex ( monthName, cells ) =
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
            :: List.map (\cellValue -> td [ class "px-3 py-1.5 text-center text-slate-800" ] [ text cellValue ]) cells
        )


{-| One row per month: its name, then three values per year, as shown and
exported. The third is the inter site average on Facility, performance minus
target on Program.
-}
drillRows : Language -> Dashboard -> List Int -> Kpi -> KpiSeries -> List ( String, List String )
drillRows language dashboard years kpi series =
    let
        format =
            formatValue language kpi.unit

        yearColumns year =
            let
                performance =
                    series.performance year

                third =
                    case dashboard of
                        Facility ->
                            List.map format (series.average year)

                        Program ->
                            List.map2 (Maybe.map2 (-)) performance series.target
                                |> List.map (Maybe.Extra.unwrap noValue signedValue)
            in
            List.map3 (\perf targ thirdValue -> [ format perf, format targ, thirdValue ])
                performance
                series.target
                third

        byYear =
            List.map yearColumns years
    in
    List.indexedMap
        (\monthIndex monthName ->
            ( monthName
            , List.concatMap (List.Extra.getAt monthIndex >> Maybe.withDefault []) byYear
            )
        )
        (monthLabels language)



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
                        , formatValue language UnitPercent bar.pct
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
            [ translateLabel language kpi.label, formatValue language kpi.unit kpi.current, kpi.target ]
                ++ (case dashboard of
                        Facility ->
                            [ formatValue language kpi.unit kpi.average ]

                        Program ->
                            []
                   )
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
drillExportMsg : Language -> Dashboard -> List Int -> Kpi -> List ( String, List String ) -> Msg
drillExportMsg language dashboard years kpi rows =
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
    in
    DownloadCSV
        (exportFileName dashboard kpi.id)
        (reportTableDataToCSV
            (MetricsResultsTableData (translateLabel language kpi.label)
                captions
                (List.map (\( monthName, cells ) -> monthName :: cells) rows)
            )
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


{-| A figure as the dashboards show it: a whole percentage, or weeks to one
decimal. A dash where there is no figure.
-}
formatValue : Language -> KpiUnit -> Maybe Float -> String
formatValue language unit =
    Maybe.Extra.unwrap noValue
        (\value ->
            case unit of
                UnitPercent ->
                    String.fromInt (round value) ++ "%"

                UnitWeeks ->
                    translateLabel language (ValueInWeeks (Round.round 1 value))
        )


noValue : String
noValue =
    "—"


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
