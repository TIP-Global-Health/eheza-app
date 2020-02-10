module Pages.Dashboard.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Dashboard.Model exposing (CaseManagement, CaseNutrition, DashboardStats, GoodNutrition, Nutrition, NutritionValue, Periods, TotalBeneficiaries)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (FamilyPlanningSign(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model
import Color exposing (Color)
import Date exposing (Unit(..), isBetween, numberToMonth)
import Debug exposing (toString)
import Gizra.Html exposing (emptyNode, showMaybe)
import Gizra.NominalDate exposing (NominalDate, isDiffTruthy)
import Html exposing (..)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick)
import List.Extra
import Pages.Dashboard.GraphUtils exposing (..)
import Pages.Dashboard.Model exposing (..)
import Pages.Page exposing (DashboardPage(..), Page(..), UserPage(..))
import Pages.Utils exposing (calculatePercentage)
import Path
import Scale exposing (BandConfig, BandScale, ContinuousScale)
import Shape exposing (Arc, defaultPieConfig)
import Svg
import Svg.Attributes exposing (cx, cy, r)
import Time exposing (Month(..))
import Translate exposing (Language, translateText)
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes as Explicit exposing (fill, transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Fill(..), Transform(..))


{-| Shows a dashboard page.
-}
view : Language -> DashboardPage -> NominalDate -> HealthCenterId -> Model -> ModelIndexedDb -> Html Msg
view language page currentDate healthCenterId model db =
    let
        stats =
            Dict.get healthCenterId db.computedDashboard
                |> Maybe.withDefault Backend.Dashboard.Model.emptyModel
                -- Filter by period.
                |> filterStatsByPeriod currentDate model

        ( pageView, goBackPage ) =
            case page of
                MainPage ->
                    ( viewMainPage language page currentDate stats model, PinCodePage )

                StatsPage ->
                    ( viewStatsPage language page currentDate stats model, UserPage <| DashboardPage MainPage )

                CaseManagementPage ->
                    ( viewCaseManagementPage language page currentDate healthCenterId stats model db, UserPage <| DashboardPage MainPage )

        header =
            div
                [ class "ui basic head segment" ]
                [ h1 [ class "ui header" ]
                    [ translateText language Translate.DashboardLabel ]
                , a
                    [ class "link-back"
                    , onClick <| SetActivePage goBackPage
                    ]
                    [ span [ class "icon-back" ] [] ]
                ]
    in
    div
        [ class "wrap" ]
        [ header
        , pageView
        ]


viewMainPage : Language -> DashboardPage -> NominalDate -> DashboardStats -> Model -> Html Msg
viewMainPage language currentPage currentDate stats model =
    div [ class "dashboard main" ]
        [ viewPeriodFilter language model
        , div [ class "ui grid" ]
            [ div [ class "eight wide column" ]
                [ viewGoodNutrition language currentPage stats.goodNutrition
                ]
            , div [ class "eight wide column" ]
                [ viewTotalEncounters language currentPage stats.totalEncounters
                ]
            , div [ class "sixteen wide column" ]
                [ viewMonthlyChart language model stats.totalBeneficiaries model.currentTotalChartsFilter
                ]
            , div [ class "sixteen wide column" ]
                [ viewDashboardPagesLinks language
                ]
            ]
        ]


viewStatsPage : Language -> DashboardPage -> NominalDate -> DashboardStats -> Model -> Html Msg
viewStatsPage language currentPage currentDate stats model =
    div [ class "dashboard stats" ]
        [ viewPeriodFilter language model
        , viewAllCards language stats
        , viewBeneficiariesTable language currentDate stats model
        , div
            [ class "ui placeholder segment" ]
            [ div [ class "ui two column stackable center aligned grid" ]
                [ div [ class "middle aligned row" ]
                    [ div [ class "column" ] [ viewDonutChart language stats ]
                    ]
                ]
            ]
        , div [ class "ui segment" ]
            [ text <| Debug.toString <| getFamilyPlanningSignsCounter stats
            ]
        ]


viewCaseManagementPage : Language -> DashboardPage -> NominalDate -> HealthCenterId -> DashboardStats -> Model -> ModelIndexedDb -> Html Msg
viewCaseManagementPage language currentPage currentDate healthCenterId stats model db =
    let
        tableData =
            List.foldl
                (\person accum ->
                    case model.currentCaseManagementFilter of
                        Stunting ->
                            { name = person.name, nutrition = person.nutrition.stunting } :: accum

                        Underweight ->
                            { name = person.name, nutrition = person.nutrition.underweight } :: accum

                        Wasting ->
                            { name = person.name, nutrition = person.nutrition.wasting } :: accum

                        MUAC ->
                            { name = person.name, nutrition = person.nutrition.muac } :: accum
                )
                []
                stats.caseManagement
                |> List.sortWith (\t1 t2 -> compare t1.name t2.name)
    in
    div [ class "dashboard case" ]
        [ viewPeriodFilter language model
        , div [ class "ui segment blue" ]
            [ div [ class "case-management" ]
                [ div [ class "header" ]
                    [ h3 [ class "title left floated column" ] [ translateText language <| Translate.Dashboard Translate.CaseManagementLabel ]
                    , div [ class "filters" ]
                        (List.map (viewFilters FilterCaseManagement model.currentCaseManagementFilter) filterCharts)
                    ]
                , div [ class "content" ]
                    [ viewCaseManagementTable language model tableData
                    ]
                ]
            ]
        ]


viewCaseManagementTable : Language -> Model -> List { name : String, nutrition : Dict Int NutritionValue } -> Html Msg
viewCaseManagementTable language model tableData =
    table [ class "ui very basic collapsing celled table" ]
        [ thead []
            [ tr []
                [ th [ class "name" ] [ translateText language <| Translate.Name ]
                , th [] [ span [] [ translateText language <| Translate.ResolveMonth Jan True ] ]
                , th [] [ span [] [ translateText language <| Translate.ResolveMonth Feb True ] ]
                , th [] [ span [] [ translateText language <| Translate.ResolveMonth Mar True ] ]
                , th [] [ span [] [ translateText language <| Translate.ResolveMonth Apr True ] ]
                , th [] [ span [] [ translateText language <| Translate.ResolveMonth May True ] ]
                , th [] [ span [] [ translateText language <| Translate.ResolveMonth Jun True ] ]
                , th [] [ span [] [ translateText language <| Translate.ResolveMonth Jul True ] ]
                , th [] [ span [] [ translateText language <| Translate.ResolveMonth Aug True ] ]
                , th [] [ span [] [ translateText language <| Translate.ResolveMonth Sep True ] ]
                , th [] [ span [] [ translateText language <| Translate.ResolveMonth Oct True ] ]
                , th [] [ span [] [ translateText language <| Translate.ResolveMonth Nov True ] ]
                , th [] [ span [] [ translateText language <| Translate.ResolveMonth Dec True ] ]
                ]
            ]
        , tbody []
            (List.map viewCaseManagementTableRow tableData)
        ]


viewCaseManagementTableRow : { name : String, nutrition : Dict Int NutritionValue } -> Html Msg
viewCaseManagementTableRow rowData =
    let
        rowList =
            rowData.nutrition
                |> Dict.toList
                |> List.sortWith (\t1 t2 -> compare (Tuple.first t1) (Tuple.first t2))
    in
    tr []
        (List.append
            [ td [ class "name" ] [ text rowData.name ] ]
            (List.map viewMonthCell rowList)
        )


viewMonthCell : ( Int, NutritionValue ) -> Html Msg
viewMonthCell ( month, cellData ) =
    let
        class =
            classList
                [ ( String.toLower <| Debug.toString cellData.class, True )
                , ( String.fromInt month, True )
                ]
    in
    td [ class ] [ span [] [ text cellData.value ] ]


viewPeriodFilter : Language -> Model -> Html Msg
viewPeriodFilter language model =
    let
        renderButton period =
            -- @todo: Translate
            button
                [ classList
                    [ ( "primary", model.period == period )
                    , ( "ui button", True )
                    ]
                , onClick <| SetFilterPeriod period
                ]
                [ translateText language <| Translate.Dashboard Translate.PeriodFilter
                ]
    in
    div [ class "ui segment filters" ]
        (List.map renderButton filterPeriods)


viewGoodNutrition : Language -> DashboardPage -> GoodNutrition -> Html Msg
viewGoodNutrition language currentPage nutrition =
    let
        percentageThisYear =
            calculatePercentage nutrition.all.thisYear nutrition.good.thisYear
                |> round

        percentageLastYear =
            calculatePercentage nutrition.all.lastYear nutrition.good.lastYear
                |> round

        percentageDiff =
            percentageThisYear - percentageLastYear

        statsCard =
            { title = Translate.Dashboard Translate.GoodNutritionLabel
            , cardClasses = "good-nutrition"
            , cardAction = Nothing
            , value = percentageThisYear
            , valueSeverity = Neutral
            , valueIsPercentage = True
            , percentageLastYear = percentageDiff
            , newCases = Nothing
            }
    in
    viewStatsCard language currentPage statsCard


viewTotalEncounters : Language -> DashboardPage -> Periods -> Html Msg
viewTotalEncounters language currentPage encounters =
    let
        diff =
            encounters.thisYear - encounters.lastYear

        percentageDiff =
            calculatePercentage encounters.thisYear diff
                |> round

        statsCard =
            { title = Translate.Dashboard Translate.TotalEncountersLabel
            , cardClasses = "total-encounters"
            , cardAction = Nothing
            , value = encounters.thisYear
            , valueSeverity = Neutral
            , valueIsPercentage = False
            , percentageLastYear = percentageDiff
            , newCases = Nothing
            }
    in
    viewStatsCard language currentPage statsCard


viewAllCards : Language -> DashboardStats -> Html Msg
viewAllCards language stats =
    if List.isEmpty stats.malnourished then
        div [ class "ui segment" ] [ text "No data for the selected period." ]

    else
        div [ class "ui segment" ]
            [ viewMalnourishedCards language stats
            , viewMiscCards language stats
            ]


viewMalnourishedCards : Language -> DashboardStats -> Html Msg
viewMalnourishedCards language stats =
    let
        total =
            stats.malnourished
                |> List.length

        totalCard =
            { title = Translate.Dashboard Translate.TotalMalnourished
            , value = total
            , valueSeverity = Neutral
            }

        severe =
            stats.malnourished
                |> List.filter (\row -> row.zscore <= -2)
                |> List.length

        severeCard =
            { title = Translate.Dashboard Translate.SeverelyMalnourished
            , value = severe
            , valueSeverity = Severe
            }

        moderate =
            stats.malnourished
                |> List.filter (\row -> row.zscore > -2)
                |> List.length

        moderateCard =
            { title = Translate.Dashboard Translate.ModeratelyMalnourished
            , value = moderate
            , valueSeverity = Moderate
            }
    in
    div [ class "ui segment" ]
        [ div [ class "ui cards" ]
            [ viewCard language totalCard
            , viewCard language severeCard
            , viewCard language moderateCard
            ]
        ]


viewMiscCards : Language -> DashboardStats -> Html Msg
viewMiscCards language stats =
    let
        totalNewBeneficiaries =
            stats.childrenBeneficiaries
                |> List.length

        totalNewBeneficiariesCard =
            { title = Translate.Dashboard Translate.NewBeneficiaries
            , value = totalNewBeneficiaries
            , valueSeverity = Neutral
            }
    in
    div [ class "ui segment" ]
        [ div [ class "ui cards" ]
            [ viewCard language totalNewBeneficiariesCard
            ]
        ]


viewStatsCard : Language -> DashboardPage -> StatsCard -> Html Msg
viewStatsCard language currentPage statsCard =
    let
        ( cardAction, cardLinkClass ) =
            case statsCard.cardAction of
                Nothing ->
                    ( SetActivePage <| UserPage <| DashboardPage currentPage
                    , ""
                    )

                Just page ->
                    ( SetActivePage <| UserPage <| DashboardPage <| page
                    , "link"
                    )

        severityClass =
            case statsCard.valueSeverity of
                Neutral ->
                    "neutral"

                Good ->
                    "good"

                Moderate ->
                    "moderate"

                Severe ->
                    "severe"

        valueSuffix =
            if statsCard.valueIsPercentage then
                "%"

            else
                ""

        viewPercentageArrow icon =
            img
                [ class "arrow"
                , src <| "/assets/images/" ++ icon ++ ".svg"
                ]
                []

        percentageArrow =
            if statsCard.percentageLastYear > 0 then
                viewPercentageArrow "icon-up"

            else if statsCard.percentageLastYear < 0 then
                viewPercentageArrow "icon-down"

            else
                emptyNode
    in
    div
        [ class <| "ui segment blue dashboard-cards " ++ statsCard.cardClasses ++ " " ++ cardLinkClass
        , onClick cardAction
        ]
        [ div [ class "content" ]
            [ div [ class "header" ] [ translateText language statsCard.title ]
            , div [ class <| "percentage this-year severity severity-" ++ severityClass ] [ text <| String.fromInt statsCard.value ++ valueSuffix ]
            , div [ class "total last-year" ]
                [ span [ class "percentage" ]
                    [ percentageArrow
                    , i [] [ text <| String.fromInt statsCard.percentageLastYear ++ "%" ]
                    ]
                , span [ class "percentage-label" ] [ translateText language <| Translate.Dashboard Translate.PercentageEncountersLabel ]
                ]
            , statsCard.newCases
                |> Maybe.map
                    (\newCases ->
                        div [ class "new-cases" ]
                            [ span [ class "label" ] [ translateText language <| Translate.Dashboard Translate.NewCasesLabel ]
                            , span [ class "new-cases-value" ] [ text <| String.fromInt newCases ]
                            ]
                    )
                |> showMaybe
            ]
        ]


viewCard : Language -> Card -> Html Msg
viewCard language card =
    let
        severityClass =
            case card.valueSeverity of
                Neutral ->
                    "neutral"

                Good ->
                    "good"

                Moderate ->
                    "moderate"

                Severe ->
                    "severe"
    in
    div [ class "card" ]
        [ div [ class "content" ]
            [ div [ class "header" ] [ translateText language card.title ]
            , div [ class <| "severity severity-" ++ severityClass ] [ text <| String.fromInt card.value ]
            ]
        ]


viewBeneficiariesGenderFilter : Language -> Model -> Html Msg
viewBeneficiariesGenderFilter language model =
    let
        renderButton gender =
            -- @todo: Translate
            button
                [ classList
                    [ ( "primary", model.beneficiariesGender == gender )
                    , ( "ui button", True )
                    ]
                , onClick <| SetFilterGender gender
                ]
                [ text <| Debug.toString gender
                ]
    in
    div []
        (List.map renderButton filterGenders)


viewBeneficiariesTable : Language -> NominalDate -> DashboardStats -> Model -> Html Msg
viewBeneficiariesTable language currentDate stats model =
    let
        statsFilteredByGender =
            stats
                |> filterStatsByGender currentDate model

        filterStatsByAgeDo func =
            filterStatsByAge
                currentDate
                func
                statsFilteredByGender

        stats0_2 =
            filterStatsByAgeDo (\{ months } -> months >= 0 && months <= (2 * 12))

        stats3_7 =
            filterStatsByAgeDo (\{ months } -> months > (2 * 12) && months <= (7 * 12))

        stats8_11 =
            filterStatsByAgeDo (\{ months } -> months > (7 * 12) && months <= (11 * 12))

        stats12_plus =
            filterStatsByAgeDo (\{ months } -> months > (11 * 12))

        getNewBeneficiariesCount stats_ =
            stats_.childrenBeneficiaries
                |> List.length
                |> String.fromInt

        getTotalMalnourishedCount stats_ =
            stats_.malnourished
                |> List.length
                |> String.fromInt
    in
    div [ class "ui blue segment" ]
        [ viewBeneficiariesGenderFilter language model
        , table [ class "ui celled table" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Grouped by age (Years)" ]
                    , th [] [ text "0-2" ]
                    , th [] [ text "3-7" ]
                    , th [] [ text "8-11" ]
                    , th [] [ text "12+" ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td [] [ text "New beneficiaries to program" ]
                    , td [] [ text <| getNewBeneficiariesCount stats0_2 ]
                    , td [] [ text <| getNewBeneficiariesCount stats3_7 ]
                    , td [] [ text <| getNewBeneficiariesCount stats8_11 ]
                    , td [] [ text <| getNewBeneficiariesCount stats12_plus ]
                    ]
                , tr []
                    [ td [] [ text "Malnourished beneficiaries" ]
                    , td [] [ text <| getTotalMalnourishedCount stats0_2 ]
                    , td [] [ text <| getTotalMalnourishedCount stats3_7 ]
                    , td [] [ text <| getTotalMalnourishedCount stats8_11 ]
                    , td [] [ text <| getTotalMalnourishedCount stats12_plus ]
                    ]
                ]
            ]
        ]


viewDashboardPagesLinks : Language -> Html Msg
viewDashboardPagesLinks language =
    div [ class "dashboards-links" ]
        [ div
            [ class "ui segment stats"
            , DashboardPage StatsPage
                |> UserPage
                |> SetActivePage
                |> onClick
            ]
            [ i [ class "icon" ] []
            , span
                []
                [ translateText language <| Translate.Dashboard Translate.StatisticsHelper ]
            , i [ class "arrow" ] []
            ]
        , div
            [ class "ui segment case"
            , DashboardPage CaseManagementPage
                |> UserPage
                |> SetActivePage
                |> onClick
            ]
            [ i [ class "icon" ] []
            , span
                []
                [ translateText language <| Translate.Dashboard Translate.CaseManagementHelper ]
            , i [ class "arrow" ] []
            ]
        ]


filterStatsByAge : NominalDate -> ({ months : Int, days : Int } -> Bool) -> DashboardStats -> DashboardStats
filterStatsByAge currentDate func stats =
    let
        childrenBeneficiaries =
            stats.childrenBeneficiaries
                |> List.filter (\row -> isDiffTruthy row.birthdate currentDate func)

        malnourished =
            stats.malnourished
                |> List.filter (\row -> isDiffTruthy row.created currentDate func)
    in
    { stats
        | childrenBeneficiaries = childrenBeneficiaries
        , malnourished = malnourished
    }


{-| Filter stats to match the selected period.
-}
filterStatsByPeriod : NominalDate -> Model -> DashboardStats -> DashboardStats
filterStatsByPeriod currentDate model stats =
    let
        startDate =
            case model.period of
                OneYear ->
                    Date.add Years -1 currentDate

        filterPartial =
            isBetween startDate currentDate

        childrenBeneficiariesUpdated =
            stats.childrenBeneficiaries
                |> List.filter (\child -> filterPartial child.memberSince)

        familyPlanningUpdated =
            stats.familyPlanning
                |> List.filter (\familyPlanning -> filterPartial familyPlanning.created)

        malnourishedUpdated =
            stats.malnourished
                |> List.filter (\malnourished -> filterPartial malnourished.created)
    in
    { stats
        | childrenBeneficiaries = childrenBeneficiariesUpdated
        , familyPlanning = familyPlanningUpdated
        , malnourished = malnourishedUpdated
    }


{-| Filter stats to match the selected gender.
-}
filterStatsByGender : NominalDate -> Model -> DashboardStats -> DashboardStats
filterStatsByGender currentDate model stats =
    let
        -- Filter by gender
        filterDo data =
            if model.beneficiariesGender == All then
                -- No change
                data

            else
                data
                    |> List.filter
                        (\personStats ->
                            case ( personStats.gender, model.beneficiariesGender ) of
                                ( Backend.Person.Model.Female, Pages.Dashboard.Model.Female ) ->
                                    True

                                ( Backend.Person.Model.Male, Pages.Dashboard.Model.Male ) ->
                                    True

                                _ ->
                                    False
                        )
    in
    { stats
        | childrenBeneficiaries = filterDo stats.childrenBeneficiaries
        , malnourished = filterDo stats.malnourished
    }


getFamilyPlanningSignsCounter : DashboardStats -> FamilyPlanningSignsCounter
getFamilyPlanningSignsCounter stats =
    if List.isEmpty stats.familyPlanning then
        Dict.empty

    else
        List.foldl
            (\familyPlanning accum ->
                let
                    currentCount sign =
                        Dict.get sign accum
                            |> Maybe.withDefault 0

                    incrementCount sign accum_ =
                        Dict.insert
                            sign
                            (currentCount sign + 1)
                            accum_
                in
                if List.isEmpty familyPlanning.signs then
                    accum

                else if List.member NoFamilyPlanning familyPlanning.signs then
                    -- In case we have a `NoFamilyPlanning` we don't need to iterate over signs.
                    incrementCount NoFamilyPlanning accum

                else
                    -- Iterate over existing signs.
                    List.foldl
                        (\sign innerAccum -> incrementCount sign innerAccum)
                        accum
                        familyPlanning.signs
            )
            Dict.empty
            stats.familyPlanning


viewDonutChart : Language -> DashboardStats -> Html Msg
viewDonutChart language stats =
    let
        dict =
            getFamilyPlanningSignsCounter stats
    in
    if Dict.isEmpty dict then
        div [] [ text "No family plannings for the selected period." ]

    else
        let
            -- Remove the No family planning, as it won't be used for the chart
            dictWithoutNoFamilyPlanning =
                dict
                    |> Dict.filter (\k _ -> not (k == NoFamilyPlanning))

            totalCount =
                dictWithoutNoFamilyPlanning
                    |> Dict.values
                    |> List.foldl (\val accum -> val + accum) 0

            totalNoFamilyPlanning =
                Dict.get NoFamilyPlanning dict
                    |> Maybe.withDefault 0

            useFamilyPlanning =
                totalCount - totalNoFamilyPlanning

            totalPercent =
                useFamilyPlanning * 100 // totalCount
        in
        div []
            [ viewChart dictWithoutNoFamilyPlanning
            , div [ class "stats" ]
                [ span [ class "neutral" ] [ text <| String.fromInt totalPercent ++ "%" ]
                , text " "
                , span [] [ translateText language <| Translate.Dashboard Translate.UseFamilyPlanning ]
                ]
            , div []
                [ translateText language <|
                    Translate.Dashboard <|
                        Translate.FamilyPlanningOutOfWomen
                            { total = totalCount
                            , useFamilyPlanning = useFamilyPlanning
                            }
                ]
            , viewFamilyPlanningChartLegend language dictWithoutNoFamilyPlanning
            ]


viewMonthlyChart : Language -> Model -> Dict Int TotalBeneficiaries -> FilterCharts -> Html Msg
viewMonthlyChart language model data currentFilter =
    let
        chartList =
            data
                |> Dict.toList
                |> List.sortWith (\t1 t2 -> compare (Tuple.first t1) (Tuple.first t2))
                |> Dict.fromList

        chartData =
            Dict.foldl
                (\key totalBeneficiaries accum ->
                    let
                        month =
                            numberToMonth key
                    in
                    case currentFilter of
                        Stunting ->
                            Dict.insert month totalBeneficiaries.stunting accum

                        Underweight ->
                            Dict.insert month totalBeneficiaries.underweight accum

                        Wasting ->
                            Dict.insert month totalBeneficiaries.wasting accum

                        MUAC ->
                            Dict.insert month totalBeneficiaries.muac accum
                )
                Dict.empty
                chartList
                |> Dict.toList

        yScaleMaxList =
            let
                choose x y =
                    let
                        chosenX =
                            if x.moderateNutrition > x.severeNutrition then
                                x.moderateNutrition

                            else
                                x.severeNutrition
                    in
                    if chosenX > y then
                        chosenX

                    else
                        y
            in
            List.map (\( key, value ) -> choose value 0) chartData

        yScaleMax =
            List.maximum yScaleMaxList |> Maybe.withDefault 1

        -- Add 20% to the top of the graph above the max
        yScaleMaxEnhanced =
            toFloat yScaleMax + (toFloat yScaleMax * 0.2)
    in
    div [ class "ui segment blue dashboards-monthly-chart" ]
        [ div [ class "header" ]
            [ h3 [ class "title left floated column" ] [ translateText language <| Translate.Dashboard Translate.TotalBeneficiariesWasting ]
            , div [ class "filters" ]
                (List.map (viewFilters FilterTotalsChart currentFilter) filterCharts)
            ]
        , div [ class "content" ]
            [ viewBarsChartLegend language
            , viewBarChart chartData yScaleMaxEnhanced
            ]
        ]


viewBarChart : List ( Month, Nutrition ) -> Float -> Html Msg
viewBarChart data yScaleMax =
    svg [ viewBox 0 0 barChartWidth barChartHeight ]
        [ g [ Explicit.class [ "grid gird-y" ] ] <| List.indexedMap yGridLine <| Scale.ticks gridYScale 4
        , g [ Explicit.class [ "grid gird-x" ] ] <| List.indexedMap xGridLine <| Scale.ticks gridXScale 12
        , g [ transform [ Translate (padding - 1) (barChartHeight - padding) ] ]
            [ xAxis data ]
        , g [ transform [ Translate (padding + 1) padding ] ]
            [ yAxis yScaleMax ]
        , g [ transform [ Translate padding padding ], Explicit.class [ "data" ] ] <|
            List.map (column (xScale data) yScaleMax) data
        ]


viewBarsChartLegend : Language -> Html Msg
viewBarsChartLegend language =
    div [ class "legend" ]
        [ div []
            [ svg [ Svg.Attributes.width "12", Svg.Attributes.height "12", viewBox 0 0 100 100 ]
                [ Svg.circle [ cx "50", cy "50", r "50", Explicit.class [ "moderate" ] ] []
                ]
            , span [] [ translateText language <| Translate.Dashboard Translate.Moderate ]
            ]
        , div []
            [ svg [ Svg.Attributes.width "12", Svg.Attributes.height "12", viewBox 0 0 100 100 ]
                [ Svg.circle [ cx "50", cy "50", r "50", Explicit.class [ "severe" ] ] []
                ]
            , span [] [ translateText language <| Translate.Dashboard Translate.Severe ]
            ]
        ]


viewFilters : FilterType -> FilterCharts -> FilterCharts -> Html Msg
viewFilters filterType currentChartFilter filter =
    let
        filterAction =
            case filterType of
                FilterTotalsChart ->
                    SetFilterTotalsChart filter

                FilterCaseManagement ->
                    SetFilterCaseManagement filter
    in
    span
        [ classList
            [ ( "chart-filters", True )
            , ( "active", filter == currentChartFilter )
            ]
        , onClick <| filterAction
        ]
        [ text <| toString filter ]


viewFamilyPlanningChartLegend : Language -> FamilyPlanningSignsCounter -> Html Msg
viewFamilyPlanningChartLegend language dict =
    let
        listSorted =
            dict
                |> Dict.toList
                |> List.sortBy (\( _, val ) -> val)
                |> List.reverse
    in
    div [ class "legend" ]
        (List.map
            (\( sign, _ ) ->
                div []
                    [ svg [ Svg.Attributes.width "10", Svg.Attributes.height "10", viewBox 0 0 100 100 ]
                        [ Svg.circle [ cx "50", cy "50", r "40", fill <| Fill <| familyPlanningSignToColor sign ] []
                        ]
                    , span [] [ translateText language <| Translate.FamilyPlanningSignLabel sign ]
                    ]
            )
            listSorted
        )


viewChart : FamilyPlanningSignsCounter -> Svg msg
viewChart dict =
    let
        arcs =
            dict
                |> Dict.values
                |> List.map toFloat

        signsList =
            dict
                |> Dict.keys

        pieData =
            arcs
                |> Shape.pie
                    { defaultPieConfig
                        | outerRadius = radius
                        , padAngle = 0
                        , cornerRadius = 0
                    }
    in
    svg [ viewBox 0 0 pieChartWidth pieChartHeight ]
        [ annular signsList pieData ]


annular : List FamilyPlanningSign -> List Arc -> Svg msg
annular signsList arcs =
    let
        getColor index =
            List.Extra.getAt index signsList
                |> Maybe.withDefault NoFamilyPlanning
                |> (\sign -> Dict.get sign colors)
                |> Maybe.withDefault Color.black

        makeSlice index datum =
            Path.element (Shape.arc { datum | innerRadius = radius - 60 })
                [ fill <| Fill <| getColor index ]
    in
    g [ transform [ Translate (3 * radius + 20) radius ] ]
        [ g [] <| List.indexedMap makeSlice arcs
        ]
