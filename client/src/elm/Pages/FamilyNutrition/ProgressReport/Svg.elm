module Pages.FamilyNutrition.ProgressReport.Svg exposing (viewMuacChart)

import Backend.Measurement.Utils exposing (muacValueForSite)
import Html exposing (Html)
import Pages.Report.Svg exposing (dimensionsPx, drawPoints, drawPolygon, drawPolyline, frame, heightPx, referenceHorizontalLines, referenceVerticalLines, referenceVerticalNumbers, widthPx, withinRange)
import Pages.Utils exposing (muacUnitTransIdForSite)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import SyncManager.Model exposing (Site)
import Translate exposing (Language, translate)


viewMuacChart : Language -> Site -> Bool -> { years : Int, months : Int } -> List ( Float, Float ) -> Html any
viewMuacChart language site isAdult anchorAge muacPoints =
    let
        -- The chart is drawn in cm. muacValueForSite says what a value reads
        -- as at this site, so what one cm reads as is what to scale by.
        displayFactor =
            round (muacValueForSite site 1)

        horizontalParts =
            36

        verticalParts =
            18

        ( verticalMin, verticalMax ) =
            if isAdult then
                ( 10 * displayFactor, 46 * displayFactor )

            else
                ( 0, 36 * displayFactor )

        verticalStep =
            heightPx / toFloat (verticalMax - verticalMin)

        horizontalStep =
            widthPx / toFloat horizontalParts

        ( redThreshold, yellowThreshold ) =
            if isAdult then
                ( muacValueForSite site 18.5, muacValueForSite site 22 )

            else
                ( muacValueForSite site 11.5, muacValueForSite site 12.5 )

        verticalNumberGap =
            2 * displayFactor

        unitTransId =
            muacUnitTransIdForSite site

        yAxisLabel =
            translate language Translate.MUAC
                ++ " ("
                ++ translate language unitTransId
                ++ ")"

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
                            muacDisplay =
                                muacValueForSite site muacCm

                            gridPos =
                                monthOffset + 3
                        in
                        if
                            withinRange gridPos 0 (toFloat horizontalParts)
                                && withinRange muacDisplay verticalMinFloat verticalMaxFloat
                        then
                            Just
                                ( dimensionsPx.left + gridPos * horizontalStep
                                , dimensionsPx.bottom - (muacDisplay - verticalMinFloat) * verticalStep
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
                [ transform "matrix(1 0 0 1 325 541)"
                , class "z-score-semibold chart-label"
                ]
                [ text <| translate language Translate.AgeAxisLabel ]
            , text_
                [ transform "matrix(0 -1 1 0 81 380)"
                , class "z-score-semibold chart-label"
                ]
                [ text yAxisLabel ]
            ]
        , g [] <|
            [ drawPolygon redPoints "red-area"
            , drawPolygon yellowPoints "yellow-area"
            , drawPolygon greenPoints "green-area"
            , drawPolyline measurements "data"
            ]
                ++ drawPoints "#06B9FF" measurements
        , (referenceVerticalLines verticalParts
            ++ referenceVerticalNumbers verticalParts verticalMin verticalNumberGap (dimensionsPx.left - 17 |> String.fromFloat)
            ++ referenceVerticalNumbers verticalParts verticalMin verticalNumberGap (dimensionsPx.right + 7.5 |> String.fromFloat)
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
