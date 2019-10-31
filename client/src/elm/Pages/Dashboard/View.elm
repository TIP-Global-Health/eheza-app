module Pages.Dashboard.View exposing (view)

import Array exposing (Array)
import AssocList as Dict
import Backend.Dashboard.Model exposing (DashboardStats)
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Backend.Person.Model
import Color exposing (Color)
import Date exposing (Unit(..), fromCalendarDate, isBetween)
import Gizra.NominalDate exposing (NominalDate, diffCalendarMonthsAndDays, isDiffTruthy)
import Html exposing (..)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
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
    in
    div
        []
        [ viewPeriodFilter language model
        , viewBeneficiariesTable language currentDate stats model
        , div
            [ class "ui placeholder segment" ]
            [ div [ class "ui two column stackable center aligned grid" ]
                [ div [ class "middle aligned row" ]
                    [ div [ class "column" ] [ viewDonutChart data ]
                    , div [ class "column" ] [ viewDonutChart data ]
                    ]
                ]
            ]
        , details [ class "ui segment" ] [ text <| Debug.toString db.computedDashboard ]
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
        statsFilteredByPeriod =
            filterStatsByPeriodAndGender currentDate stats model

        getRangeCount func =
            getGroupedByAgeCount
                currentDate
                statsFilteredByPeriod
                func
                |> String.fromInt

        range0_2 =
            getRangeCount (\{ months } -> months >= 0 && months <= (2 * 12))

        range3_7 =
            getRangeCount (\{ months } -> months > (2 * 12) && months <= (7 * 12))

        range8_11 =
            getRangeCount (\{ months } -> months > (7 * 12) && months <= (11 * 12))

        range12_plus =
            getRangeCount (\{ months } -> months > (11 * 12))
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
                    , td [] [ text range0_2 ]
                    , td [] [ text range3_7 ]
                    , td [] [ text range8_11 ]
                    , td [] [ text range12_plus ]
                    ]
                ]
            ]
        ]


getGroupedByAgeCount : NominalDate -> DashboardStats -> ({ months : Int, days : Int } -> Bool) -> Int
getGroupedByAgeCount currentDate stats func =
    stats.people
        |> List.filter (\personStats -> isDiffTruthy personStats.birthdate currentDate func)
        |> List.length


{-| Filter stats to match the selected period and gender.
-}
filterStatsByPeriodAndGender : NominalDate -> DashboardStats -> Model -> DashboardStats
filterStatsByPeriodAndGender currentDate stats model =
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

        peopleFilterPeriod =
            stats.people
                |> List.filter (\personStats -> isBetween startDate currentDate personStats.memberSince)

        -- Filter by gender
        peopleUpdated =
            if model.beneficiariesGender == All then
                -- No change
                peopleFilterPeriod

            else
                peopleFilterPeriod
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
    { stats | people = peopleUpdated }



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


colors : Array Color
colors =
    Array.fromList
        [ rgba255 31 119 180 1
        , rgba255 255 127 14 1
        , rgba255 44 159 44 1
        , rgba255 214 39 40 1
        , rgba255 148 103 189 1
        , rgba255 140 86 75 1
        , rgba255 227 119 194 1
        , rgba255 128 128 128 1
        , rgba255 188 189 34 1
        , rgba255 23 190 207 1
        ]


radius : Float
radius =
    min (w / 2) h / 2 - 10


circular : List Arc -> Svg msg
circular arcs =
    let
        makeSlice index datum =
            Path.element (Shape.arc datum)
                [ fill <| Fill <| Maybe.withDefault Color.black <| Array.get index colors
                , stroke Color.black
                ]
    in
    g [ transform [ Translate radius radius ] ]
        [ g [] <| List.indexedMap makeSlice arcs
        ]


annular : List Arc -> Svg msg
annular arcs =
    let
        makeSlice index datum =
            Path.element (Shape.arc { datum | innerRadius = radius - 60 })
                [ fill <| Fill <| Maybe.withDefault Color.black <| Array.get index colors ]
    in
    g [ transform [ Translate (3 * radius + 20) radius ] ]
        [ g [] <| List.indexedMap makeSlice arcs
        ]


viewDonutChart : List Float -> Svg msg
viewDonutChart model =
    let
        pieData =
            model
                |> Shape.pie
                    { defaultPieConfig
                        | outerRadius = radius
                        , padAngle = 0
                        , cornerRadius = 0
                    }
    in
    svg [ viewBox 0 0 w h ]
        [ annular pieData ]


data : List Float
data =
    [ 1, 1, 2, 3, 5, 8, 13 ]
