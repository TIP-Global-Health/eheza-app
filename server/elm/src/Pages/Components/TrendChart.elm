module Pages.Components.TrendChart exposing (Series, view)

{-| A multi-series monthly trend chart drawn as inline SVG. It is parameterised
by an arbitrary set of series holding values between 0 and 100, so it plots any
indicator without knowing what the indicator is.

Series are told apart by colour and by line style (solid / dashed / dotted), so
they stay readable without relying on colour alone. The whole chart is exposed
to assistive tech as a single labelled image.

-}

import Html exposing (Html)
import Html.Attributes
import Svg exposing (Svg)
import Svg.Attributes as SA


{-| One plotted line. `dash` is a stroke-dasharray value; an empty string draws
a solid line. `points` holds one value per month.
-}
type alias Series =
    { color : String
    , label : String
    , dash : String
    , points : List Float
    }



-- PLOT GEOMETRY (SVG user units; the chart scales to its container width).


plotLeft : Float
plotLeft =
    46


plotRight : Float
plotRight =
    556


plotTop : Float
plotTop =
    14


plotBottom : Float
plotBottom =
    216


plotWidth : Float
plotWidth =
    plotRight - plotLeft


plotHeight : Float
plotHeight =
    plotBottom - plotTop


xAt : Int -> Float
xAt index =
    plotLeft + toFloat index * (plotWidth / 11)


yAt : Float -> Float
yAt value =
    plotBottom - (value / 100) * plotHeight


view : { xLabels : List String, series : List Series, ariaLabel : String } -> Html msg
view config =
    Svg.svg
        [ SA.viewBox "0 0 580 250"
        , SA.class "w-full h-auto"
        , Html.Attributes.attribute "role" "img"
        , Html.Attributes.attribute "aria-label" config.ariaLabel
        , SA.preserveAspectRatio "xMidYMid meet"
        ]
        (gridAndYAxis
            ++ List.indexedMap xLabel config.xLabels
            ++ List.concatMap seriesLayer config.series
        )


{-| Horizontal gridlines and y axis value labels at 0/20/40/60/80/100.
-}
gridAndYAxis : List (Svg msg)
gridAndYAxis =
    List.concatMap
        (\value ->
            let
                y =
                    yAt (toFloat value)
            in
            [ Svg.line
                [ SA.x1 (String.fromFloat plotLeft)
                , SA.y1 (String.fromFloat y)
                , SA.x2 (String.fromFloat plotRight)
                , SA.y2 (String.fromFloat y)
                , SA.stroke "#e2e8f0"
                , SA.strokeWidth "1"
                ]
                []
            , Svg.text_
                [ SA.x (String.fromFloat (plotLeft - 10))
                , SA.y (String.fromFloat (y + 4))
                , SA.textAnchor "end"
                , SA.fontSize "12"
                , SA.fill "#475569"
                ]
                [ Svg.text (String.fromInt value) ]
            ]
        )
        [ 0, 20, 40, 60, 80, 100 ]


xLabel : Int -> String -> Svg msg
xLabel index label =
    Svg.text_
        [ SA.x (String.fromFloat (xAt index))
        , SA.y (String.fromFloat (plotBottom + 22))
        , SA.textAnchor "middle"
        , SA.fontSize "12"
        , SA.fill "#334155"
        ]
        [ Svg.text label ]


{-| A polyline plus circular markers for one series.
-}
seriesLayer : Series -> List (Svg msg)
seriesLayer series =
    let
        coordinates =
            List.indexedMap (\index value -> ( xAt index, yAt value )) series.points

        marker ( x, y ) =
            Svg.circle
                [ SA.cx (String.fromFloat x)
                , SA.cy (String.fromFloat y)
                , SA.r "3.5"
                , SA.fill series.color
                ]
                []
    in
    Svg.polyline
        [ SA.points (List.map (\( x, y ) -> String.fromFloat x ++ "," ++ String.fromFloat y) coordinates |> String.join " ")
        , SA.fill "none"
        , SA.stroke series.color
        , SA.strokeWidth "2.5"
        , SA.strokeDasharray series.dash
        , SA.strokeLinejoin "round"
        , SA.strokeLinecap "round"
        ]
        []
        :: List.map marker coordinates
