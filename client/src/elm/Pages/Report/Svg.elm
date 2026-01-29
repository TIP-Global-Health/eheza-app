module Pages.Report.Svg exposing (dimensionsPx, drawPoints, drawPolygon, drawPolyline, drawPolyshape, heightPx, referenceHorizontalLines, referenceHorizontalNumbers, referenceVerticalLines, referenceVerticalNumbers, svgMarker, widthPx, withinRange)

import Svg exposing (..)
import Svg.Attributes exposing (..)


svgMarker identification fill =
    marker
        [ id identification
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
            , Svg.Attributes.style <| "fill:" ++ fill
            ]
            []
        ]


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


drawPoints : String -> List ( Float, Float ) -> List (Svg any)
drawPoints fill =
    List.map
        (\( x, y ) ->
            circle
                [ cx <| String.fromFloat x
                , cy <| String.fromFloat y
                , r "4"
                , Svg.Attributes.style <| "fill:" ++ fill
                ]
                []
        )


withinRange : number -> number -> number -> Bool
withinRange value min max =
    (value >= min)
        && (value <= max)


widthPx : Float
widthPx =
    dimensionsPx.right - dimensionsPx.left


heightPx : Float
heightPx =
    dimensionsPx.bottom - dimensionsPx.top


dimensionsPx =
    { left = 110.9
    , top = 119.9
    , right = 737.7
    , bottom = 506.7
    }
