module Pages.FamilyNutrition.ProgressReport.Svg exposing (viewMuacChart)

import Html exposing (Html)
import Pages.Report.Svg exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Translate exposing (Language, translate)


viewMuacChart : Language -> Bool -> { years : Int, months : Int } -> List ( Int, Float ) -> Html any
viewMuacChart language isAdult anchorAge muacPoints =
    let
        horizontalParts =
            36

        verticalParts =
            18

        ( verticalMin, verticalMax ) =
            if isAdult then
                ( 10, 46 )

            else
                ( 0, 36 )

        verticalStep =
            heightPx / toFloat (verticalMax - verticalMin)

        horizontalStep =
            widthPx / toFloat horizontalParts

        ( redThreshold, yellowThreshold ) =
            if isAdult then
                ( 18.5, 22 )

            else
                ( 11.5, 12.5 )

        verticalMinFloat =
            toFloat verticalMin

        redPoints =
            [ ( dimensionsPx.left, dimensionsPx.bottom )
            , ( dimensionsPx.left, dimensionsPx.bottom - (redThreshold - verticalMinFloat) * verticalStep )
            , ( dimensionsPx.right, dimensionsPx.bottom - (redThreshold - verticalMinFloat) * verticalStep )
            , ( dimensionsPx.right, dimensionsPx.bottom )
            , ( dimensionsPx.left, dimensionsPx.bottom )
            ]

        yellowPoints =
            [ ( dimensionsPx.left, dimensionsPx.bottom - (redThreshold - verticalMinFloat) * verticalStep )
            , ( dimensionsPx.left, dimensionsPx.bottom - (yellowThreshold - verticalMinFloat) * verticalStep )
            , ( dimensionsPx.right, dimensionsPx.bottom - (yellowThreshold - verticalMinFloat) * verticalStep )
            , ( dimensionsPx.right, dimensionsPx.bottom - (redThreshold - verticalMinFloat) * verticalStep )
            , ( dimensionsPx.left, dimensionsPx.bottom - (redThreshold - verticalMinFloat) * verticalStep )
            ]

        greenPoints =
            [ ( dimensionsPx.left, dimensionsPx.bottom - (yellowThreshold - verticalMinFloat) * verticalStep )
            , ( dimensionsPx.left, dimensionsPx.top )
            , ( dimensionsPx.right, dimensionsPx.top )
            , ( dimensionsPx.right, dimensionsPx.bottom - (yellowThreshold - verticalMinFloat) * verticalStep )
            , ( dimensionsPx.left, dimensionsPx.bottom - (yellowThreshold - verticalMinFloat) * verticalStep )
            ]

        verticalMaxFloat =
            toFloat verticalMax

        measurements =
            muacPoints
                |> List.filterMap
                    (\( monthOffset, muacCm ) ->
                        let
                            gridPos =
                                toFloat monthOffset + 3
                        in
                        if
                            withinRange gridPos 0 (toFloat horizontalParts)
                                && withinRange muacCm verticalMinFloat verticalMaxFloat
                        then
                            Just
                                ( dimensionsPx.left + gridPos * horizontalStep
                                , dimensionsPx.bottom - (muacCm - verticalMinFloat) * verticalStep
                                )

                        else
                            Nothing
                    )
    in
    svg
        [ class "z-score"
        , x "0px"
        , y "0px"
        , viewBox "25 25 841.9 595.3"
        ]
        [ frame
        , g []
            [ text_
                [ transform "matrix(1 0 0 1 373 541)"
                , class "z-score-semibold chart-label"
                ]
                [ text <| translate language Translate.AgeAxisLabel ]
            , text_
                [ transform "matrix(0 -1 1 0 81 350)"
                , class "z-score-semibold chart-label"
                ]
                [ text <| translate language Translate.MUAC ]
            ]
        , g [] <|
            [ drawPolygon redPoints "red-area"
            , drawPolygon yellowPoints "yellow-area"
            , drawPolygon greenPoints "green-area"
            , drawPolyline measurements "data"
            ]
                ++ drawPoints "#06B9FF" measurements
        , (referenceVerticalLines verticalParts
            ++ referenceVerticalNumbers verticalParts verticalMin 2 (dimensionsPx.left - 17 |> String.fromFloat)
            ++ referenceVerticalNumbers verticalParts verticalMin 2 (dimensionsPx.right + 7.5 |> String.fromFloat)
          )
            |> g []
        , referenceHorizontalLines horizontalParts ++ referenceHorizontalAgeLabels horizontalParts anchorAge |> g []
        ]


referenceHorizontalAgeLabels : Int -> { years : Int, months : Int } -> List (Svg any)
referenceHorizontalAgeLabels parts anchorAge =
    let
        margin =
            widthPx / toFloat parts

        anchorTotalMonths =
            anchorAge.years * 12 + anchorAge.months
    in
    -- Render labels at every 3rd grid line: indices 2, 5, 8, ... 35
    List.range 0 ((parts - 1) // 3)
        |> List.map
            (\step ->
                let
                    index =
                        2 + step * 3

                    posX =
                        dimensionsPx.left + (toFloat (index + 1) * margin)

                    totalMonths =
                        anchorTotalMonths + (index - 2)

                    labelYears =
                        totalMonths // 12

                    labelMonths =
                        modBy 12 totalMonths

                    label =
                        String.fromInt labelYears ++ "-" ++ String.fromInt labelMonths

                    posX_ =
                        (posX - 7) |> String.fromFloat
                in
                text_ [ transform <| "matrix(1 0 0 1 " ++ posX_ ++ " 520)", class "z-score-semibold st17" ] [ text label ]
            )


frame : Svg any
frame =
    g
        []
        [ rect
            [ class "chart-outer-frame"
            , height "447.9"
            , width "728.5"
            , x "56.7"
            , y "101.1"
            ]
            []
        , rect
            [ height "386.8"
            , width "626.8"
            , x "110.9"
            , y "119.9"
            ]
            []
        ]
