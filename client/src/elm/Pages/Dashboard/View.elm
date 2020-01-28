module Pages.Dashboard.View exposing (view)

import AssocList as Dict exposing (Dict)
import Backend.Dashboard.Model exposing (DashboardStats, TotalEncounters)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (FamilyPlanningSign(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model
import Color exposing (Color)
import Date exposing (Unit(..), isBetween)
import Gizra.Html exposing (showIf)
import Gizra.NominalDate exposing (NominalDate, isDiffTruthy)
import Html exposing (..)
import Html.Attributes exposing (class, classList, src)
import Html.Events exposing (onClick)
import List.Extra
import Pages.Dashboard.Model exposing (..)
import Pages.Page exposing (DashboardPage(..), Page(..), UserPage(..))
import Path
import Shape exposing (Arc, defaultPieConfig)
import Svg
import Svg.Attributes exposing (cx, cy, height, r, width)
import Translate exposing (Language, translateText)
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (fill, transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))


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
                    ( viewMainPage language currentDate stats model, PinCodePage )

                StatsPage ->
                    ( viewMainPage language currentDate stats model, UserPage <| DashboardPage MainPage )

                CaseManagementPage ->
                    ( viewCaseManagementPage language currentDate healthCenterId model db, UserPage <| DashboardPage MainPage )

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


viewMainPage : Language -> NominalDate -> DashboardStats -> Model -> Html Msg
viewMainPage language currentDate stats model =
    div [ class "dashboard main" ]
        [ div [ class "ui grid" ]
            [ div [ class "eight wide column" ]
                [-- @todo: Add good nutrition here.
                ]
            , div [ class "eight wide column" ]
                [ viewTotalEncounters language stats.totalEncounters
                ]
            , div [ class "sixteen wide column" ]
                [ viewDashboardPagesLinks language
                ]
            ]
        ]


viewStatsPage : Language -> NominalDate -> DashboardStats -> Model -> Html Msg
viewStatsPage language currentDate stats model =
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


viewCaseManagementPage : Language -> NominalDate -> HealthCenterId -> Model -> ModelIndexedDb -> Html Msg
viewCaseManagementPage language currentDate healthCenterId model db =
    --@todo: Add case management page design.
    div [ class "dashboard management ui segment blue" ] [ text "Case Management Page" ]


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
                [ text <| Debug.toString period
                ]
    in
    div [ class "ui blue segment" ]
        (List.map renderButton filterPeriods)


viewTotalEncounters : Language -> TotalEncounters -> Html Msg
viewTotalEncounters language encounters =
    let
        diff =
            encounters.thisYear - encounters.lastYear

        percentageDiff =
            let
                lastYear =
                    if encounters.lastYear == 0 then
                        -- Avoid dividing by zero.
                        1

                    else
                        encounters.lastYear
            in
            if diff > 0 then
                (diff // lastYear) * 100

            else
                (diff // encounters.thisYear) * 100

        percentageArrow =
            if percentageDiff == 0 then
                ""

            else if percentageDiff > 0 then
                "icon-up"

            else
                "icon-down"
    in
    div [ class "ui segment blue dashboard-cards total-encounters" ]
        [ div [ class "content" ]
            [ div [ class "header" ] [ translateText language <| Translate.Dashboard Translate.TotalEncountersLabel ]
            , div [ class "total this-year severity severity-neutral" ] [ text <| String.fromInt encounters.thisYear ]
            , div [ class "total last-year" ]
                [ span [ class "percentage" ]
                    [ showIf (percentageArrow /= "") <|
                        img
                            [ class "arrow"
                            , src <| "/assets/images/" ++ percentageArrow ++ ".svg"
                            ]
                            []
                    , i [] [ text <| String.fromInt percentageDiff ++ "%" ]
                    ]
                , span [ class "percentage-label" ] [ translateText language <| Translate.Dashboard Translate.PercentageEncountersLabel ]
                ]
            ]
        ]


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


familyPlanningSignToColor : FamilyPlanningSign -> Color
familyPlanningSignToColor sign =
    case sign of
        Condoms ->
            Color.red

        IUD ->
            Color.orange

        Implant ->
            Color.yellow

        Injection ->
            Color.green

        Necklace ->
            Color.blue

        NoFamilyPlanning ->
            Color.black

        Pill ->
            Color.purple



-- From https://code.gampleman.eu/elm-visualization/PadAngle/


w : Float
w =
    990


h : Float
h =
    504


colors : Dict FamilyPlanningSign Color
colors =
    Dict.fromList
        [ ( Condoms, familyPlanningSignToColor Condoms )
        , ( IUD, familyPlanningSignToColor IUD )
        , ( Implant, familyPlanningSignToColor Implant )
        , ( Injection, familyPlanningSignToColor Injection )
        , ( Necklace, familyPlanningSignToColor Necklace )
        , ( NoFamilyPlanning, familyPlanningSignToColor NoFamilyPlanning )
        , ( Pill, familyPlanningSignToColor Pill )
        ]


radius : Float
radius =
    min (w / 2) h / 2 - 10


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
                    [ svg [ width "10", height "10", viewBox 0 0 100 100 ]
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
    svg [ viewBox 0 0 w h ]
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
