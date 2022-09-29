module Pages.NCD.ProgressReport.Svg exposing (viewBloodGlucoseByTime, viewBloodPressureByTime, viewMarkers)

import Html exposing (Html)
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
                [ id "dot-marker-green"
                , markerWidth "8"
                , markerHeight "8"
                , refX "4"
                , refY "4"
                , markerUnits "userSpaceOnUse"
                ]
                [ circle
                    [ cx "4"
                    , cy "4"
                    , r "3"
                    , Svg.Attributes.style "fill:#1CBCB2"
                    ]
                    []
                ]
            , marker
                [ id "dot-marker-red"
                , markerWidth "8"
                , markerHeight "8"
                , refX "4"
                , refY "4"
                , markerUnits "userSpaceOnUse"
                ]
                [ circle
                    [ cx "4"
                    , cy "4"
                    , r "3"
                    , Svg.Attributes.style "fill:#D4145A"
                    ]
                    []
                ]
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
        , g []
            [ horizontalLabel language
            , verticalLabel language Translate.BloodPressure
            ]
        , g []
            [ drawPolyline measurementsSys "data red"
            , drawPolyline measurementsDia "data green"
            ]
        , (referenceVerticalLines verticalParts
            ++ referenceVerticalNumbers verticalParts verticalMin 20 (dimensionsPx.left - 21.5 |> String.fromFloat)
            ++ referenceVerticalNumbers verticalParts verticalMin 20 (dimensionsPx.right + 7.5 |> String.fromFloat)
          )
            |> g []
        , referenceHorizontalLines 13 ++ referenceHorizontalNumbers 13 0 1 |> g []
        ]


viewBloodGlucoseByTime : Language -> List Float -> Html any
viewBloodGlucoseByTime language points =
    let
        verticalParts =
            20

        verticalMin =
            40

        verticalMax =
            440

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
            , verticalLabel language Translate.BloodGlucose
            ]
        , g []
            [ drawPolyline measurements "data red" ]
        , (referenceVerticalLines verticalParts
            ++ referenceVerticalNumbers verticalParts verticalMin 20 (dimensionsPx.left - 21.5 |> String.fromFloat)
            ++ referenceVerticalNumbers verticalParts verticalMin 20 (dimensionsPx.right + 7.5 |> String.fromFloat)
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
        [ transform "matrix(1 0 0 1 401 541)"
        , class "z-score-semibold chart-label"
        ]
        [ text <| translate language Translate.Time ]


referenceHorizontalLines : Int -> List (Svg any)
referenceHorizontalLines parts =
    let
        margin =
            widthPx / toFloat parts
    in
    List.repeat (parts - 1) ""
        |> List.indexedMap
            (\index _ ->
                let
                    posX =
                        dimensionsPx.left + (toFloat (index + 1) * margin) |> String.fromFloat
                in
                line [ class "refference-line", x1 posX, y1 (String.fromFloat dimensionsPx.bottom), x2 posX, y2 (String.fromFloat dimensionsPx.top) ] []
            )


referenceHorizontalNumbers : Int -> Int -> Int -> List (Svg any)
referenceHorizontalNumbers parts min gap =
    let
        margin =
            widthPx / toFloat parts
    in
    List.repeat (parts - 1) ""
        |> List.indexedMap
            (\index _ ->
                let
                    posX =
                        dimensionsPx.left + (toFloat (index + 1) * margin)

                    number =
                        min + (index + 1) * gap

                    posX_ =
                        (if number > 9 then
                            posX - 4

                         else
                            posX - 2.5
                        )
                            |> String.fromFloat

                    number_ =
                        number |> String.fromInt
                in
                text_ [ transform <| "matrix(1 0 0 1 " ++ posX_ ++ " 520)", class "z-score-semibold st17" ] [ text number_ ]
            )


referenceVerticalLines : Int -> List (Svg any)
referenceVerticalLines parts =
    let
        margin =
            heightPx / toFloat parts
    in
    List.repeat (parts - 1) ""
        |> List.indexedMap
            (\index _ ->
                let
                    posY =
                        dimensionsPx.top + (toFloat (index + 1) * margin) |> String.fromFloat
                in
                line [ class "refference-line", x1 (String.fromFloat dimensionsPx.left), y1 posY, x2 (String.fromFloat dimensionsPx.right), y2 posY ] []
            )


referenceVerticalNumbers : Int -> Int -> Int -> String -> List (Svg any)
referenceVerticalNumbers parts min gap posX =
    let
        margin =
            heightPx / toFloat parts
    in
    List.repeat (parts - 1) ""
        |> List.indexedMap
            (\index _ ->
                let
                    posY =
                        dimensionsPx.top + (toFloat (index + 1) * margin)

                    number =
                        min + (parts - index - 1) * gap

                    posY_ =
                        posY + 2 |> String.fromFloat

                    number_ =
                        number |> String.fromInt
                in
                text_ [ transform <| "matrix(1 0 0 1 " ++ posX ++ " " ++ posY_ ++ ")", class "z-score-semibold st17" ] [ text number_ ]
            )


measurementsByTime : Float -> Float -> Float -> Float -> List Float -> List ( Float, Float )
measurementsByTime verticalMin verticalMax verticalStep horizontalStep points =
    List.indexedMap (\index value -> ( toFloat <| index + 1, value )) points
        |> List.filterMap
            (\( time, value_ ) ->
                let
                    value =
                        value_ - verticalMin
                in
                if withinRange value verticalMin verticalMax then
                    Just ( dimensionsPx.left + time * horizontalStep, dimensionsPx.bottom - value * verticalStep )

                else
                    Nothing
            )


drawPolygon : List ( Float, Float ) -> String -> Svg any
drawPolygon =
    drawPolyshape polygon


drawPolyline : List ( Float, Float ) -> String -> Svg any
drawPolyline =
    drawPolyshape polyline


drawPolyshape shape points_ class_ =
    List.map
        (\( x, y ) ->
            String.fromFloat x ++ "," ++ String.fromFloat y
        )
        points_
        |> String.join " "
        |> points
        |> (\pointList ->
                shape
                    [ class class_
                    , pointList
                    ]
                    []
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


withinRange : number -> number -> number -> Bool
withinRange value min max =
    (value >= min)
        && (value <= max)


widthPx =
    dimensionsPx.right - dimensionsPx.left


heightPx =
    dimensionsPx.bottom - dimensionsPx.top


dimensionsPx =
    { left = 110.9
    , top = 119.9
    , right = 737.7
    , bottom = 506.7
    }
