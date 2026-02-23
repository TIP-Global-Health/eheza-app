module Pages.FamilyNutrition.ProgressReport.Svg exposing (viewMuacChart)

import Html exposing (Html)
import Pages.Report.Svg exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Translate exposing (Language, translate)


viewMuacChart : Language -> Bool -> Html any
viewMuacChart language isAdult =
    let
        verticalParts =
            18

        ( verticalMin, verticalMax ) =
            if isAdult then
                ( 10, 46 )

            else
                ( 0, 36 )

        verticalStep =
            heightPx / toFloat (verticalMax - verticalMin)

        ( redThreshold, yellowThreshold ) =
            if isAdult then
                ( 18.5, 22 )

            else
                ( 11.5, 12.5 )

        redPoints =
            [ ( dimensionsPx.left, dimensionsPx.bottom )
            , ( dimensionsPx.left, dimensionsPx.bottom - (redThreshold - verticalMin) * verticalStep )
            , ( dimensionsPx.right, dimensionsPx.bottom - (redThreshold - verticalMin) * verticalStep )
            , ( dimensionsPx.right, dimensionsPx.bottom )
            , ( dimensionsPx.left, dimensionsPx.bottom )
            ]

        yellowPoints =
            [ ( dimensionsPx.left, dimensionsPx.bottom - (redThreshold - verticalMin) * verticalStep )
            , ( dimensionsPx.left, dimensionsPx.bottom - (yellowThreshold - verticalMin) * verticalStep )
            , ( dimensionsPx.right, dimensionsPx.bottom - (yellowThreshold - verticalMin) * verticalStep )
            , ( dimensionsPx.right, dimensionsPx.bottom - (redThreshold - verticalMin) * verticalStep )
            , ( dimensionsPx.left, dimensionsPx.bottom - (redThreshold - verticalMin) * verticalStep )
            ]

        greenPoints =
            [ ( dimensionsPx.left, dimensionsPx.bottom - (yellowThreshold - verticalMin) * verticalStep )
            , ( dimensionsPx.left, dimensionsPx.top )
            , ( dimensionsPx.right, dimensionsPx.top )
            , ( dimensionsPx.right, dimensionsPx.bottom - (yellowThreshold - verticalMin) * verticalStep )
            , ( dimensionsPx.left, dimensionsPx.bottom - (yellowThreshold - verticalMin) * verticalStep )
            ]
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
                [ text <| translate language Translate.EgaWeeks ]
            , text_
                [ transform "matrix(0 -1 1 0 81 350)"
                , class "z-score-semibold chart-label"
                ]
                [ text <| translate language Translate.MUAC ]
            ]
        , g []
            [ drawPolygon redPoints "red-area"
            , drawPolygon yellowPoints "yellow-area"
            , drawPolygon greenPoints "green-area"
            ]
        , (referenceVerticalLines verticalParts
            ++ referenceVerticalNumbers verticalParts verticalMin 2 (dimensionsPx.left - 17 |> String.fromFloat)
            ++ referenceVerticalNumbers verticalParts verticalMin 2 (dimensionsPx.right + 7.5 |> String.fromFloat)
          )
            |> g []
        , referenceHorizontalLines 21 ++ referenceHorizontalNumbers 21 0 2 |> g []
        ]


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
