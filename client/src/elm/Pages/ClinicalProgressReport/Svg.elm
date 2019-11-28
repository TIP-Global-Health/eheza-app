module Pages.ClinicalProgressReport.Svg exposing (viewBMIForEGA, viewFundalHeightForEGA, viewMarkers)

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


viewBMIForEGA : Language -> Html any
viewBMIForEGA language =
    let
        bottomRedPoints =
            [ ( 110.9, 506.7 ), ( 110.9, 394.88 ), ( 737.7, 394.88 ), ( 737.7, 506.7 ), ( 110.9, 506.7 ) ]

        greenPoints =
            [ ( 110.9, 394.88 ), ( 110.9, 350.8 ), ( 737.7, 350.8 ), ( 737.7, 394.88 ), ( 110.9, 394.88 ) ]

        yellowPoints =
            [ ( 110.9, 350.8 ), ( 110.9, 316.9 ), ( 737.7, 316.9 ), ( 737.7, 350.8 ), ( 110.9, 350.8 ) ]

        topRedPoints =
            [ ( 110.9, 316.9 ), ( 110.9, 119.9 ), ( 737.7, 119.9 ), ( 737.7, 316.9 ), ( 110.9, 316.9 ) ]
    in
    svg
        [ class "z-score boys"
        , x "0px"
        , y "0px"
        , viewBox "25 25 841.9 595.3"
        ]
        [ frame2 language
        , labels2 language Translate.Yes Translate.Yes
        , (referenceVerticalLines
            ++ referenceVerticalNumbers 55 5 "95.425"
            ++ referenceVerticalNumbers 55 5 "745.0155"
          )
            |> g []
        , referenceHorizontalLines 20 ++ referenceHorizontalNumbers 20 2 |> g []
        , g []
            [ drawPolygon topRedPoints "below-neg-three"
            , drawPolygon yellowPoints "neg-two-to-neg-three"
            , drawPolygon greenPoints "above-neg-two"
            , drawPolygon bottomRedPoints "below-neg-three"
            ]
        ]


viewFundalHeightForEGA : Language -> Html any
viewFundalHeightForEGA language =
    let
        leftXpx =
            110.9

        topYpx =
            119.9

        rightXpx =
            737.7

        bottomYpx =
            506.7

        leftX =
            0

        topY =
            49

        rightX =
            42

        bottomY =
            2

        xSteps =
            rightX - leftX

        ySteps =
            topY - bottomY

        xStepPx =
            (rightXpx - leftXpx) / xSteps

        yStepPx =
            (bottomYpx - topYpx) / ySteps

        leftRedPoints =
            [ ( leftXpx, bottomYpx - 4 * yStepPx )
            , ( leftXpx, topYpx )
            , ( rightXpx, topYpx )
            , ( rightXpx, bottomYpx - 46 * yStepPx )
            , ( leftXpx, bottomYpx - 4 * yStepPx )
            ]

        leftYellowPoints =
            [ ( leftXpx, bottomYpx - 2 * yStepPx )
            , ( leftXpx, bottomYpx - 4 * yStepPx )
            , ( rightXpx, bottomYpx - 46 * yStepPx )
            , ( rightXpx, bottomYpx - 44 * yStepPx )
            , ( leftXpx, bottomYpx - 2 * yStepPx )
            ]

        greenPoints =
            [ ( leftXpx, bottomYpx )
            , ( leftXpx, bottomYpx - 2 * yStepPx )
            , ( rightXpx, bottomYpx - 44 * yStepPx )
            , ( rightXpx, bottomYpx - 40 * yStepPx )
            , ( leftXpx + 2 * xStepPx, bottomYpx )
            , ( leftXpx, bottomYpx )
            ]

        rightYellowPoints =
            [ ( leftXpx + 2 * xStepPx, bottomYpx )
            , ( rightXpx, bottomYpx - 40 * yStepPx )
            , ( rightXpx, bottomYpx - 38 * yStepPx )
            , ( leftXpx + 4 * xStepPx, bottomYpx )
            , ( leftXpx + 2 * xStepPx, bottomYpx )
            ]

        rightRedPoints =
            [ ( leftXpx + 4 * xStepPx, bottomYpx )
            , ( rightXpx, bottomYpx - 38 * yStepPx )
            , ( rightXpx, bottomYpx )
            , ( leftXpx + 4 * xStepPx, bottomYpx )
            ]
    in
    svg
        [ class "z-score boys"
        , x "0px"
        , y "0px"
        , viewBox "25 25 841.9 595.3"
        ]
        [ frame2 language
        , labels2 language Translate.No Translate.No
        , (referenceVerticalLines
            ++ referenceVerticalNumbers 45 4 "95.425"
            ++ referenceVerticalNumbers 45 4 "745.0155"
          )
            |> g []
        , referenceHorizontalLines 20 ++ referenceHorizontalNumbers 20 2 |> g []
        , g []
            [ drawPolygon leftRedPoints "below-neg-three"
            , drawPolygon leftYellowPoints "neg-two-to-neg-three"
            , drawPolygon greenPoints "above-neg-two"
            , drawPolygon rightYellowPoints "neg-two-to-neg-three"
            , drawPolygon rightRedPoints "below-neg-three"
            ]
        ]


drawPolygon : List ( Float, Float ) -> String -> Svg any
drawPolygon points_ class_ =
    points_
        |> List.map
            (\( x, y ) ->
                toString x ++ "," ++ toString y
            )
        |> String.join " "
        |> points
        |> (\pointList ->
                polygon
                    [ class class_
                    , pointList
                    ]
                    []
           )


labels2 : Language -> TranslationId -> TranslationId -> Svg any
labels2 language xAxis yAxis =
    let
        xAxisLabel =
            if xAxis == Translate.Yes then
                "Bmi"

            else
                "Fundal Height"
    in
    g []
        [ text_
            [ transform "matrix(1 0 0 1 400 540.9924)"
            , class "z-score-white z-score-semibold st17"
            ]
            [ text "EGA (Weeks)" ]

        -- [ text <| translate language xAxis ]
        , text_
            [ transform "matrix(0 -1 1 0 80.8497 350)"
            , class "z-score-white z-score-semibold st17"
            ]
            [ text xAxisLabel ]

        -- [ text <| translate language yAxis ]
        ]


frame2 : Language -> Svg any
frame2 language =
    g
        []
        [ rect
            [ class "z-score-grey"
            , height "447.9"
            , width "728.5"
            , x "56.7"
            , y "101.1"
            ]
            []
        , rect
            [ class "z-score-white"
            , height "386.8"
            , width "626.8"
            , x "110.9"
            , y "119.9"
            ]
            []
        ]


referenceVerticalLines : List (Svg any)
referenceVerticalLines =
    [ line [ class "st18", x1 "110.8", y1 "486.3", x2 "737.6", y2 "486.3" ] []
    , line [ class "st18", x1 "110.8", y1 "452.4", x2 "737.6", y2 "452.4" ] []
    , line [ class "st18", x1 "110.8", y1 "418.5", x2 "737.6", y2 "418.5" ] []
    , line [ class "st18", x1 "110.8", y1 "384.5", x2 "737.6", y2 "384.5" ] []
    , line [ class "st18", x1 "110.8", y1 "350.6", x2 "737.6", y2 "350.6" ] []
    , line [ class "st18", x1 "110.8", y1 "316.7", x2 "737.6", y2 "316.7" ] []
    , line [ class "st18", x1 "110.8", y1 "282.8", x2 "737.6", y2 "282.8" ] []
    , line [ class "st18", x1 "110.8", y1 "248.8", x2 "737.6", y2 "248.8" ] []
    , line [ class "st18", x1 "110.8", y1 "214.9", x2 "737.6", y2 "214.9" ] []
    , line [ class "st18", x1 "110.8", y1 "181", x2 "737.6", y2 "181" ] []
    , line [ class "st18", x1 "110.8", y1 "147.1", x2 "737.6", y2 "147.1" ] []
    , line [ class "st19", x1 "110.8", y1 "499.9", x2 "737.6", y2 "499.9" ] []
    , line [ class "st19", x1 "110.8", y1 "493.1", x2 "737.6", y2 "493.1" ] []
    , line [ class "st19", x1 "110.8", y1 "479.5", x2 "737.6", y2 "479.5" ] []
    , line [ class "st19", x1 "110.8", y1 "472.8", x2 "737.6", y2 "472.8" ] []
    , line [ class "st19", x1 "110.8", y1 "466", x2 "737.6", y2 "466" ] []
    , line [ class "st19", x1 "110.8", y1 "459.2", x2 "737.6", y2 "459.2" ] []
    , line [ class "st19", x1 "110.8", y1 "445.6", x2 "737.6", y2 "445.6" ] []
    , line [ class "st19", x1 "110.8", y1 "438.8", x2 "737.6", y2 "438.8" ] []
    , line [ class "st19", x1 "110.8", y1 "432", x2 "737.6", y2 "432" ] []
    , line [ class "st19", x1 "110.8", y1 "425.3", x2 "737.6", y2 "425.3" ] []
    , line [ class "st19", x1 "110.8", y1 "411.7", x2 "737.6", y2 "411.7" ] []
    , line [ class "st19", x1 "110.8", y1 "404.9", x2 "737.6", y2 "404.9" ] []
    , line [ class "st19", x1 "110.8", y1 "398.1", x2 "737.6", y2 "398.1" ] []
    , line [ class "st19", x1 "110.8", y1 "391.3", x2 "737.6", y2 "391.3" ] []
    , line [ class "st19", x1 "110.8", y1 "377.8", x2 "737.6", y2 "377.8" ] []
    , line [ class "st19", x1 "110.8", y1 "371", x2 "737.6", y2 "371" ] []
    , line [ class "st19", x1 "110.8", y1 "364.2", x2 "737.6", y2 "364.2" ] []
    , line [ class "st19", x1 "110.8", y1 "357.4", x2 "737.6", y2 "357.4" ] []
    , line [ class "st19", x1 "110.8", y1 "343.8", x2 "737.6", y2 "343.8" ] []
    , line [ class "st19", x1 "110.8", y1 "337.1", x2 "737.6", y2 "337.1" ] []
    , line [ class "st19", x1 "110.8", y1 "330.3", x2 "737.6", y2 "330.3" ] []
    , line [ class "st19", x1 "110.8", y1 "323.5", x2 "737.6", y2 "323.5" ] []
    , line [ class "st19", x1 "110.8", y1 "309.9", x2 "737.6", y2 "309.9" ] []
    , line [ class "st19", x1 "110.8", y1 "303.1", x2 "737.6", y2 "303.1" ] []
    , line [ class "st19", x1 "110.8", y1 "296.3", x2 "737.6", y2 "296.3" ] []
    , line [ class "st19", x1 "110.8", y1 "289.6", x2 "737.6", y2 "289.6" ] []
    , line [ class "st19", x1 "110.8", y1 "276", x2 "737.6", y2 "276" ] []
    , line [ class "st19", x1 "110.8", y1 "269.2", x2 "737.6", y2 "269.2" ] []
    , line [ class "st19", x1 "110.8", y1 "262.4", x2 "737.6", y2 "262.4" ] []
    , line [ class "st19", x1 "110.8", y1 "255.6", x2 "737.6", y2 "255.6" ] []
    , line [ class "st19", x1 "110.8", y1 "242.1", x2 "737.6", y2 "242.1" ] []
    , line [ class "st19", x1 "110.8", y1 "235.3", x2 "737.6", y2 "235.3" ] []
    , line [ class "st19", x1 "110.8", y1 "228.5", x2 "737.6", y2 "228.5" ] []
    , line [ class "st19", x1 "110.8", y1 "221.7", x2 "737.6", y2 "221.7" ] []
    , line [ class "st19", x1 "110.8", y1 "208.1", x2 "737.6", y2 "208.1" ] []
    , line [ class "st19", x1 "110.8", y1 "201.4", x2 "737.6", y2 "201.4" ] []
    , line [ class "st19", x1 "110.8", y1 "194.6", x2 "737.6", y2 "194.6" ] []
    , line [ class "st19", x1 "110.8", y1 "187.8", x2 "737.6", y2 "187.8" ] []
    , line [ class "st19", x1 "110.8", y1 "174.2", x2 "737.6", y2 "174.2" ] []
    , line [ class "st19", x1 "110.8", y1 "167.4", x2 "737.6", y2 "167.4" ] []
    , line [ class "st19", x1 "110.8", y1 "160.6", x2 "737.6", y2 "160.6" ] []
    , line [ class "st19", x1 "110.8", y1 "153.9", x2 "737.6", y2 "153.9" ] []
    , line [ class "st19", x1 "110.8", y1 "140.3", x2 "737.6", y2 "140.3" ] []
    , line [ class "st19", x1 "110.8", y1 "133.5", x2 "737.6", y2 "133.5" ] []
    , line [ class "st19", x1 "110.8", y1 "126.7", x2 "737.6", y2 "126.7" ] []
    ]


referenceVerticalNumbers : Int -> Int -> String -> List (Svg any)
referenceVerticalNumbers max gap posX =
    List.repeat 11 ""
        |> List.indexedMap
            (\index _ ->
                let
                    number =
                        max - (index * gap) |> toString

                    posY =
                        149.63 + (toFloat index * 33.926) |> toString
                in
                text_ [ transform <| "matrix(1 0 0 1 " ++ posX ++ " " ++ posY ++ ")", class "z-score-white z-score-semibold st16" ] [ text number ]
            )


referenceHorizontalLines : Int -> List (Svg any)
referenceHorizontalLines parts =
    let
        margin =
            604.8 / toFloat parts
    in
    List.repeat parts ""
        |> List.indexedMap
            (\index _ ->
                let
                    posX =
                        136 + (toFloat index * margin) |> toString
                in
                line [ class "month-line", x1 posX, y1 "506.5", x2 posX, y2 "119.5" ] []
            )


referenceHorizontalNumbers : Int -> Int -> List (Svg any)
referenceHorizontalNumbers parts gap =
    let
        margin =
            604.8 / toFloat parts
    in
    List.repeat parts ""
        |> List.indexedMap
            (\index _ ->
                let
                    posX =
                        133.967 + (toFloat index * margin)

                    number =
                        (index + 1) * gap

                    posX_ =
                        (if number > 9 then
                            posX - 1.5

                         else
                            posX
                        )
                            |> toString

                    number_ =
                        number |> toString
                in
                text_ [ transform <| "matrix(1 0 0 1 " ++ posX_ ++ " 516.5436)", class "z-score-white z-score-semibold st16" ] [ text number_ ]
            )
