module Pages.Dashboard.View exposing (view)

import Array exposing (Array)
import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb)
import Color exposing (Color)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (class)
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
        debug =
            ul [ class "ui segment" ]
                (db.computedDashboard
                    |> Dict.values
                    |> List.take 5
                    |> List.map
                        (\row ->
                            li []
                                [ div [] [ text row.computed ]
                                , div [] [ text <| Debug.toString row.healthCenter ]
                                ]
                        )
                )
    in
    div
        []
        [ div
            [ class "ui placeholder segment" ]
            [ div [ class "ui two column stackable center aligned grid" ]
                [ div [ class "middle aligned row" ]
                    [ div [ class "column" ] [ viewDonutChart data ]
                    , div [ class "column" ] [ viewDonutChart data ]
                    ]
                ]
            ]
        , debug
        ]



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
