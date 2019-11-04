module Pages.Dashboard.View exposing (view)

import Array exposing (Array)
import AssocList as Dict exposing (Dict)
import Backend.Dashboard.Model exposing (DashboardStats)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (FamilyPlanningSign(..))
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model
import Color exposing (Color)
import Date exposing (Unit(..), fromCalendarDate, isBetween)
import Gizra.NominalDate exposing (NominalDate, diffCalendarMonthsAndDays, isDiffTruthy)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import List.Extra
import Pages.Dashboard.Model exposing (..)
import Path
import Shape exposing (Arc, defaultPieConfig)
import Translate exposing (Language)
import TypedSvg exposing (g, svg)
import TypedSvg.Attributes exposing (fill, stroke, transform, viewBox)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (Fill(..), Transform(..))


{-| Shows a dashboard page.
-}
view : Language -> NominalDate -> HealthCenterId -> Model -> ModelIndexedDb -> Html Msg
view language currentDate healthCenterId model db =
    let
        stats =
            Dict.get healthCenterId db.computedDashboard
                |> Maybe.withDefault Backend.Dashboard.Model.emptyModel
                -- Filter by period.
                |> filterStatsByPeriod currentDate model
    in
    div
        []
        [ viewPeriodFilter language model
        , viewBeneficiariesTable language currentDate stats model
        , div
            [ class "ui placeholder segment" ]
            [ div [ class "ui two column stackable center aligned grid" ]
                [ div [ class "middle aligned row" ]
                    [ div [ class "column" ] [ viewDonutChart stats ]
                    ]
                ]
            ]
        , div [ class "ui segment" ]
            [ text <| Debug.toString <| getFamilyPlanningSignsCounter stats
            , details [] [ text <| Debug.toString <| stats ]
            ]
        ]


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
                ThisMonth ->
                    -- Beginning of current month.
                    fromCalendarDate (Date.year currentDate) (Date.month currentDate) 1

                LastMonth ->
                    Date.add Months -1 currentDate

                ThreeMonths ->
                    Date.add Months -3 currentDate

                OneYear ->
                    Date.add Years -1 currentDate

        childrenBeneficiariesUpdated =
            stats.childrenBeneficiaries
                |> List.filter (\child -> isBetween startDate currentDate child.memberSince)

        familyPlanningUpdated =
            stats.familyPlanning
                |> List.filter (\familyPlanning -> isBetween startDate currentDate familyPlanning.created)

        malnourishedUpdated =
            stats.malnourished
                |> List.filter (\malnourished -> isBetween startDate currentDate malnourished.created)
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


rgba255 : Int -> Int -> Int -> Float -> Color
rgba255 r g b a =
    Color.fromRgba { red = toFloat r / 255, green = toFloat g / 255, blue = toFloat b / 255, alpha = a }


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


viewDonutChart : DashboardStats -> Html Msg
viewDonutChart stats =
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
        in
        div []
            [ viewChart dictWithoutNoFamilyPlanning
            , div [] [ text <| "Total family plannings:" ++ String.fromInt totalCount ]
            , div [] [ text <| "Total No family planning:" ++ String.fromInt totalNoFamilyPlanning ]
            ]


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
