module Pages.Prenatal.ProgressReport.Svg exposing (viewBMIForEGA, viewFundalHeightForEGA, viewMarkers)

import Html exposing (Html)
import Pages.Report.Svg exposing (..)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Translate exposing (ChartPhrase(..), Language, TranslationId(..), translate)


{-| If you're calling any of the functions that generate charts,
also call this one in order to generate some markers they all use.
-}
viewMarkers : Html any
viewMarkers =
    svg
        [ width "0"
        , height "0"
        ]
        [ defs
            []
            [ marker
                [ id "dot-marker"
                , markerWidth "8"
                , markerHeight "8"
                , refX "4"
                , refY "4"
                , markerUnits "userSpaceOnUse"
                , class "dot-marker"
                ]
                [ circle
                    [ cx "4"
                    , cy "4"
                    , r "3"
                    ]
                    []
                ]
            ]
        ]


viewBMIForEGA : Language -> List ( Int, Float ) -> Html any
viewBMIForEGA language points =
    let
        verticalParts =
            18

        verticalMin =
            10

        verticalMax =
            46

        verticalStep =
            heightPx / toFloat (verticalMax - verticalMin)

        horizontalMin =
            0

        horizontalMax =
            42

        horizontalStep =
            widthPx / toFloat (horizontalMax - horizontalMin)

        bottomRedPoints =
            [ ( dimensionsPx.left, dimensionsPx.bottom )
            , ( dimensionsPx.left, dimensionsPx.bottom - (18.5 - verticalMin) * verticalStep )
            , ( dimensionsPx.right, dimensionsPx.bottom - (18.5 - verticalMin) * verticalStep )
            , ( dimensionsPx.right, dimensionsPx.bottom )
            , ( dimensionsPx.left, dimensionsPx.bottom )
            ]

        greenPoints =
            [ ( dimensionsPx.left, dimensionsPx.bottom - (18.5 - verticalMin) * verticalStep )
            , ( dimensionsPx.left, dimensionsPx.bottom - (25 - verticalMin) * verticalStep )
            , ( dimensionsPx.right, dimensionsPx.bottom - (25 - verticalMin) * verticalStep )
            , ( dimensionsPx.right, dimensionsPx.bottom - (18.5 - verticalMin) * verticalStep )
            , ( dimensionsPx.left, dimensionsPx.bottom - (18.5 - verticalMin) * verticalStep )
            ]

        yellowPoints =
            [ ( dimensionsPx.left, dimensionsPx.bottom - (25 - verticalMin) * verticalStep )
            , ( dimensionsPx.left, dimensionsPx.bottom - (30 - verticalMin) * verticalStep )
            , ( dimensionsPx.right, dimensionsPx.bottom - (30 - verticalMin) * verticalStep )
            , ( dimensionsPx.right, dimensionsPx.bottom - (25 - verticalMin) * verticalStep )
            , ( dimensionsPx.left, dimensionsPx.bottom - (25 - verticalMin) * verticalStep )
            ]

        topRedPoints =
            [ ( dimensionsPx.left, dimensionsPx.bottom - (30 - verticalMin) * verticalStep )
            , ( dimensionsPx.left, dimensionsPx.top )
            , ( dimensionsPx.right, dimensionsPx.top )
            , ( dimensionsPx.right, dimensionsPx.bottom - (30 - verticalMin) * verticalStep )
            , ( dimensionsPx.left, dimensionsPx.bottom - (30 - verticalMin) * verticalStep )
            ]

        measurements =
            points
                |> List.filterMap
                    (\( egaDays, bmi_ ) ->
                        let
                            ega =
                                toFloat egaDays / 7 - horizontalMin

                            bmi =
                                bmi_ - verticalMin
                        in
                        if withinRange ega horizontalMin horizontalMax && withinRange bmi verticalMin verticalMax then
                            Just ( dimensionsPx.left + ega * horizontalStep, dimensionsPx.bottom - bmi * verticalStep )

                        else
                            Nothing
                    )
    in
    svg
        [ class "z-score boys"
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
                [ text <| translate language Translate.BMI ]
            ]
        , g []
            [ drawPolygon topRedPoints "red-area"
            , drawPolygon yellowPoints "yellow-area"
            , drawPolygon greenPoints "green-area"
            , drawPolygon bottomRedPoints "red-area"
            , drawPolyline measurements "data"
            ]
        , (referenceVerticalLines verticalParts
            ++ referenceVerticalNumbers verticalParts verticalMin 2 (dimensionsPx.left - 17 |> String.fromFloat)
            ++ referenceVerticalNumbers verticalParts verticalMin 2 (dimensionsPx.right + 7.5 |> String.fromFloat)
          )
            |> g []
        , referenceHorizontalLines 21 ++ referenceHorizontalNumbers 21 0 2 |> g []
        ]


viewFundalHeightForEGA : Language -> List ( Int, Float ) -> Html any
viewFundalHeightForEGA language points =
    let
        verticalParts =
            15

        verticalMin =
            16

        verticalMax =
            46

        verticalStep =
            heightPx / toFloat (verticalMax - verticalMin)

        horizontalParts =
            14

        horizontalMin =
            16

        horizontalMax =
            44

        horizontalStep =
            widthPx / toFloat (horizontalMax - horizontalMin)

        bottomRedPoints =
            [ ( dimensionsPx.left + (20 - horizontalMin) * horizontalStep, dimensionsPx.bottom )
            , ( dimensionsPx.right, dimensionsPx.bottom - (40 - verticalMin) * verticalStep )
            , ( dimensionsPx.right, dimensionsPx.bottom )
            , ( dimensionsPx.left + (20 - horizontalMin) * horizontalStep, dimensionsPx.bottom )
            ]

        bottomYellowPoints =
            [ ( dimensionsPx.left + (20 - horizontalMin) * horizontalStep, dimensionsPx.bottom )
            , ( dimensionsPx.left + (18 - horizontalMin) * horizontalStep, dimensionsPx.bottom )
            , ( dimensionsPx.right, dimensionsPx.bottom - (42 - verticalMin) * verticalStep )
            , ( dimensionsPx.right, dimensionsPx.bottom - (40 - verticalMin) * verticalStep )
            , ( dimensionsPx.left + (20 - horizontalMin) * horizontalStep, dimensionsPx.bottom )
            ]

        greenPoints =
            [ ( dimensionsPx.left + (18 - horizontalMin) * horizontalStep, dimensionsPx.bottom )
            , ( dimensionsPx.left, dimensionsPx.bottom )
            , ( dimensionsPx.left, dimensionsPx.bottom - (18 - verticalMin) * verticalStep )
            , ( dimensionsPx.right, dimensionsPx.top )
            , ( dimensionsPx.right, dimensionsPx.bottom - (42 - verticalMin) * verticalStep )
            , ( dimensionsPx.left + (18 - horizontalMin) * horizontalStep, dimensionsPx.bottom )
            ]

        topYellowPoints =
            [ ( dimensionsPx.left, dimensionsPx.bottom - (18 - verticalMin) * verticalStep )
            , ( dimensionsPx.left, dimensionsPx.bottom - (20 - verticalMin) * verticalStep )
            , ( dimensionsPx.left + (42 - horizontalMin) * horizontalStep, dimensionsPx.top )
            , ( dimensionsPx.right, dimensionsPx.top )
            , ( dimensionsPx.left, dimensionsPx.bottom - (18 - verticalMin) * verticalStep )
            ]

        topRedPoints =
            [ ( dimensionsPx.left, dimensionsPx.bottom - (20 - verticalMin) * verticalStep )
            , ( dimensionsPx.left, dimensionsPx.top )
            , ( dimensionsPx.left + (42 - horizontalMin) * horizontalStep, dimensionsPx.top )
            , ( dimensionsPx.left, dimensionsPx.bottom - (20 - verticalMin) * verticalStep )
            ]

        measurements =
            points
                |> List.filterMap
                    (\( egaDays, height_ ) ->
                        let
                            ega =
                                toFloat egaDays / 7 - horizontalMin

                            height =
                                height_ - verticalMin
                        in
                        if withinRange ega horizontalMin horizontalMax && withinRange height verticalMin verticalMax then
                            Just ( dimensionsPx.left + ega * horizontalStep, dimensionsPx.bottom - height * verticalStep )

                        else
                            Nothing
                    )
    in
    svg
        [ class "z-score boys"
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
                [ transform "matrix(0 -1 1 0 81 380)"
                , class "z-score-semibold chart-label"
                ]
                [ text <| translate language Translate.FundalHeight ]
            ]
        , g []
            [ drawPolygon bottomRedPoints "red-area"
            , drawPolygon bottomYellowPoints "yellow-area"
            , drawPolygon greenPoints "green-area"
            , drawPolygon topYellowPoints "yellow-area"
            , drawPolygon topRedPoints "red-area"
            , drawPolyline measurements "data"
            ]
        , (referenceVerticalLines verticalParts
            ++ referenceVerticalNumbers verticalParts verticalMin 2 (dimensionsPx.left - 17 |> String.fromFloat)
            ++ referenceVerticalNumbers verticalParts verticalMin 2 (dimensionsPx.right + 7.5 |> String.fromFloat)
          )
            |> g []
        , referenceHorizontalLines horizontalParts ++ referenceHorizontalNumbers horizontalParts horizontalMin 2 |> g []
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
