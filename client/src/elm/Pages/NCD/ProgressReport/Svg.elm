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


dimensionsPx =
    { left = 110.9
    , top = 119.9
    , right = 737.7
    , bottom = 506.7
    }


widthPx =
    dimensionsPx.right - dimensionsPx.left


heightPx =
    dimensionsPx.bottom - dimensionsPx.top


viewBloodPressureByTime : Language -> List Float -> Html any
viewBloodPressureByTime language points =
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

        -- measurements =
        --     points
        --         |> List.filterMap
        --             (\( egaDays, bmi_ ) ->
        --                 let
        --                     ega =
        --                         toFloat egaDays / 7 - horizontalMin
        --
        --                     bmi =
        --                         bmi_ - verticalMin
        --                 in
        --                 if withinRange ega horizontalMin horizontalMax && withinRange bmi verticalMin verticalMax then
        --                     Just ( dimensionsPx.left + ega * horizontalStep, dimensionsPx.bottom - bmi * verticalStep )
        --
        --                 else
        --                     Nothing
        --             )
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
                [ text <| translate language Translate.Time ]
            , text_
                [ transform "matrix(0 -1 1 0 70 400)"
                , class "z-score-semibold chart-label"
                ]
                [ text <| translate language Translate.BloodPressure ]
            ]

        -- , g []
        --     [ drawPolygon topRedPoints "red-area"
        --     , drawPolygon yellowPoints "yellow-area"
        --     , drawPolygon greenPoints "green-area"
        --     , drawPolygon bottomRedPoints "red-area"
        --     , drawPolyline measurements "child-data"
        --     ]
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

        -- measurements =
        --     points
        --         |> List.filterMap
        --             (\( egaDays, bmi_ ) ->
        --                 let
        --                     ega =
        --                         toFloat egaDays / 7 - horizontalMin
        --
        --                     bmi =
        --                         bmi_ - verticalMin
        --                 in
        --                 if withinRange ega horizontalMin horizontalMax && withinRange bmi verticalMin verticalMax then
        --                     Just ( dimensionsPx.left + ega * horizontalStep, dimensionsPx.bottom - bmi * verticalStep )
        --
        --                 else
        --                     Nothing
        --             )
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
                [ text <| translate language Translate.Time ]
            , text_
                [ transform "matrix(0 -1 1 0 70 400)"
                , class "z-score-semibold chart-label"
                ]
                [ text <| translate language Translate.BloodGlucose ]
            ]

        -- , g []
        --     [ drawPolygon topRedPoints "red-area"
        --     , drawPolygon yellowPoints "yellow-area"
        --     , drawPolygon greenPoints "green-area"
        --     , drawPolygon bottomRedPoints "red-area"
        --     , drawPolyline measurements "child-data"
        --     ]
        , (referenceVerticalLines verticalParts
            ++ referenceVerticalNumbers verticalParts verticalMin 20 (dimensionsPx.left - 21.5 |> String.fromFloat)
            ++ referenceVerticalNumbers verticalParts verticalMin 20 (dimensionsPx.right + 7.5 |> String.fromFloat)
          )
            |> g []
        , referenceHorizontalLines 13 ++ referenceHorizontalNumbers 13 0 1 |> g []
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
    points_
        |> List.map
            (\( x, y ) ->
                String.fromFloat x ++ "," ++ String.fromFloat y
            )
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
            ]
            []
        ]


withinRange : number -> number -> number -> Bool
withinRange value min max =
    if value < 0 || value > (max - min) then
        False

    else
        True
