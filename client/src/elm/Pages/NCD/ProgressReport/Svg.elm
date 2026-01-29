module Pages.NCD.ProgressReport.Svg exposing (frame, horizontalLabel, measurementsByTime, measurementsWithInidactorsByTime, verticalLabel, viewBloodGlucoseByTime, viewBloodPressureByTime, viewHbA1cByTime, viewMarkers)

import Html exposing (Html)
import Pages.Report.Model exposing (RandomBloodSugarResult(..))
import Pages.Report.Svg exposing (dimensionsPx, drawPoints, drawPolyline, heightPx, referenceHorizontalLines, referenceHorizontalNumbers, referenceVerticalLines, referenceVerticalNumbers, svgMarker, widthPx, withinRange)
import Pages.Report.Utils exposing (getRandomBloodSugarResultValue)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Translate exposing (Language, TranslationId, translate)


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
            [ svgMarker "dot-marker-green" "#1CBCB2"
            , svgMarker "dot-marker-red" "#D4145A"
            , svgMarker "dot-marker-black" "#000000"
            ]
        ]


viewBloodPressureByTime : Language -> List Float -> List Float -> Html any
viewBloodPressureByTime language sysPoints diaPoints =
    let
        verticalParts =
            14

        verticalMin =
            40

        verticalMax =
            320

        verticalStep =
            heightPx / toFloat (verticalMax - verticalMin)

        horizontalMin =
            0

        horizontalMax =
            13

        horizontalStep =
            widthPx / toFloat (horizontalMax - horizontalMin)

        measurementsSys =
            measurementsByTime verticalMin verticalMax verticalStep horizontalStep sysPoints

        sysHelper =
            viewHelper "Sys" "red" measurementsSys

        diaHelper =
            viewHelper "Dia" "green" measurementsDia

        viewHelper label color points =
            List.reverse points
                |> List.head
                |> Maybe.map
                    (\( left_, top_ ) ->
                        let
                            left =
                                left_ + 10 |> String.fromFloat

                            top =
                                top_ + 7 |> String.fromFloat
                        in
                        [ text_
                            [ transform <| "matrix(1 0 0 1 " ++ left ++ " " ++ top ++ ")"
                            , class <| "z-score-semibold chart-helper " ++ color
                            ]
                            [ text label ]
                        ]
                    )
                |> Maybe.withDefault []

        measurementsDia =
            measurementsByTime verticalMin verticalMax verticalStep horizontalStep diaPoints
    in
    svg
        [ class "chart"
        , x "0px"
        , y "0px"
        , viewBox "25 25 841.9 595.3"
        ]
        [ frame
        , [ horizontalLabel language
          , verticalLabel language Translate.BloodPressure
          ]
            ++ sysHelper
            ++ diaHelper
            |> g []
        , g [] <|
            (drawPolyline measurementsSys "data red" :: drawPoints "#06B9FF" measurementsSys)
                ++ (drawPolyline measurementsDia "data green" :: drawPoints "purple" measurementsDia)
        , (referenceVerticalLines verticalParts
            ++ referenceVerticalNumbers verticalParts verticalMin 20 (dimensionsPx.left - 21.5 |> String.fromFloat)
            ++ referenceVerticalNumbers verticalParts verticalMin 20 (dimensionsPx.right + 7.5 |> String.fromFloat)
          )
            |> g []
        , referenceHorizontalLines 13 ++ referenceHorizontalNumbers 13 0 1 |> g []
        ]


viewBloodGlucoseByTime : Language -> List RandomBloodSugarResult -> Html any
viewBloodGlucoseByTime language results =
    let
        verticalParts =
            14

        verticalMin =
            30

        verticalMax =
            450

        verticalStep =
            heightPx / toFloat (verticalMax - verticalMin)

        horizontalMin =
            0

        horizontalMax =
            13

        horizontalStep =
            widthPx / toFloat (horizontalMax - horizontalMin)

        measurementsWithIndicators =
            measurementsWithInidactorsByTime verticalMin
                verticalMax
                verticalStep
                horizontalStep
                getRandomBloodSugarResultValue
                identity
                results

        measurements =
            List.map Tuple.second measurementsWithIndicators

        indicators =
            List.map
                (\( result, ( left_, top_ ) ) ->
                    let
                        ( indicator, left ) =
                            case result of
                                TestRunBeforeMeal _ ->
                                    ( "F", left_ - 5 |> String.fromFloat )

                                TestRunAfterMeal _ ->
                                    ( "NF", left_ - 12 |> String.fromFloat )

                        top =
                            top_ - 10 |> String.fromFloat
                    in
                    text_
                        [ transform <| "matrix(1 0 0 1 " ++ left ++ " " ++ top ++ ")"
                        , class <| "z-score-semibold chart-helper red"
                        ]
                        [ text indicator ]
                )
                measurementsWithIndicators
    in
    svg
        [ class "chart"
        , x "0px"
        , y "0px"
        , viewBox "25 25 841.9 595.3"
        ]
        [ frame
        , g [] <|
            [ horizontalLabel language
            , verticalLabel language Translate.BloodGlucose
            ]
                ++ indicators
        , g [] <|
            drawPolyline measurements "data black"
                :: drawPoints "#06B9FF" measurements
        , (referenceVerticalLines verticalParts
            ++ referenceVerticalNumbers verticalParts verticalMin 30 (dimensionsPx.left - 21.5 |> String.fromFloat)
            ++ referenceVerticalNumbers verticalParts verticalMin 30 (dimensionsPx.right + 7.5 |> String.fromFloat)
          )
            |> g []
        , referenceHorizontalLines 13 ++ referenceHorizontalNumbers 13 0 1 |> g []
        ]


viewHbA1cByTime : Language -> List Float -> Html any
viewHbA1cByTime language points =
    let
        verticalParts =
            14

        verticalMin =
            0

        verticalMax =
            14

        verticalStep =
            heightPx / toFloat (verticalMax - verticalMin)

        horizontalMin =
            0

        horizontalMax =
            13

        horizontalStep =
            widthPx / toFloat (horizontalMax - horizontalMin)

        measurements =
            measurementsByTime verticalMin verticalMax verticalStep horizontalStep points
    in
    svg
        [ class "chart"
        , x "0px"
        , y "0px"
        , viewBox "25 25 841.9 595.3"
        ]
        [ frame
        , g []
            [ horizontalLabel language
            , verticalLabel language Translate.HbA1cPercentage
            ]
        , g [] <|
            drawPolyline measurements "data black"
                :: drawPoints "#06B9FF" measurements
        , (referenceVerticalLines verticalParts
            ++ referenceVerticalNumbers verticalParts verticalMin 1 (dimensionsPx.left - 21.5 |> String.fromFloat)
            ++ referenceVerticalNumbers verticalParts verticalMin 1 (dimensionsPx.right + 7.5 |> String.fromFloat)
          )
            |> g []
        , referenceHorizontalLines 13 ++ referenceHorizontalNumbers 13 0 1 |> g []
        ]


verticalLabel : Language -> TranslationId -> Html any
verticalLabel language label =
    text_
        [ transform "matrix(0 -1 1 0 70 400)"
        , class "z-score-semibold chart-label"
        ]
        [ text <| translate language label ]


horizontalLabel : Language -> Html any
horizontalLabel language =
    text_
        [ transform "matrix(1 0 0 1 401 545)"
        , class "z-score-semibold chart-label"
        ]
        [ text <| translate language Translate.Time ]


measurementsByTime : Float -> Float -> Float -> Float -> List Float -> List ( Float, Float )
measurementsByTime verticalMin verticalMax verticalStep horizontalStep points =
    measurementsWithInidactorsByTime verticalMin verticalMax verticalStep horizontalStep identity identity points
        |> List.map Tuple.second


measurementsWithInidactorsByTime :
    Float
    -> Float
    -> Float
    -> Float
    -> (a -> Float)
    -> (a -> indicator)
    -> List a
    -> List ( indicator, ( Float, Float ) )
measurementsWithInidactorsByTime verticalMin verticalMax verticalStep horizontalStep toNumberFunc toIndicatorFunc points =
    List.indexedMap (\index value -> ( toFloat <| index + 1, value )) points
        |> List.filterMap
            (\( time, value_ ) ->
                let
                    value =
                        toNumberFunc value_
                in
                if withinRange value verticalMin verticalMax then
                    Just
                        ( toIndicatorFunc value_
                        , ( dimensionsPx.left + time * horizontalStep, dimensionsPx.bottom - (value - verticalMin) * verticalStep )
                        )

                else
                    Nothing
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
            , Svg.Attributes.style "fill:white"
            ]
            []
        ]
