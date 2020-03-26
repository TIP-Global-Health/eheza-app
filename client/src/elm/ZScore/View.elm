module ZScore.View exposing (Bounds, LabelConfig, PlotConfig, ageLines, frame, heightForAgeBoysLabels, heightForAgeConfig, heightForAgeGirlsLabels, heightLines, labels, plotChildData, plotData, plotReferenceData, referenceLinesHeight, referenceLinesWeight, referenceLinesWeightForHeight, viewHeightForAgeBoys, viewHeightForAgeBoys5To19, viewHeightForAgeGirls, viewMarkers, viewWeightForAgeBoys, viewWeightForAgeGirls, viewWeightForHeightBoys, viewWeightForHeightGirls, weightForAgeBoysLabels, weightForAgeConfig, weightForAgeGirlsLabels, weightForHeightBoysLabels, weightForHeightConfig, weightForHeightGirlsLabels, zScoreLabelsHeightForAgeBoys, zScoreLabelsHeightForAgeGirls, zScoreLabelsWeightForAgeBoys, zScoreLabelsWeightForAgeGirls, zScoreLabelsWeightForHeightBoys, zScoreLabelsWeightForHeightGirls)

{-| Ultimately, the idea is that we've got information in the `Model` that we
can use to draw the elements below more programmatically ... that is, we don't
have to have static lists of points here when we have the numeric values
anyway.

However, to save development time for the moment, we're only applying a limited
amount of intelligence below ... there is more that could be done to simplify
this, use less memory, and make it more flexible.

Some of this ultimately should probably be done lazily in some way ... but so
far it doesn't seem to be a performance problem, so no premature optimization!

-}

import Debug exposing (toString)
import Html exposing (Html)
import RemoteData
import Round
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Translate exposing (ChartPhrase(..), Language, TranslationId(..), translate)
import Utils.AllDict as AllDict exposing (AllDict)
import Utils.NominalDate exposing (Days(..))
import ZScore.Model exposing (..)
import ZScore.Utils exposing (valueForZScore)


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


viewHeightForAgeBoys : Language -> Model -> List ( Days, Centimetres ) -> Html any
viewHeightForAgeBoys language model data =
    svg
        [ class "z-score boys"
        , x "0px"
        , y "0px"
        , viewBox "0 0 841.9 595.3"
        ]
        [ frame language "z-score-grey"
        , labels language heightForAgeBoysLabels
        , referenceLinesHeight
        , ageLines heightForAgeConfig
        , zScoreLabelsHeightForAgeBoys
        , model.lengthHeightForAge
            |> RemoteData.map (.male >> .byDay >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData heightForAgeConfig
        , plotChildData heightForAgeConfig data
        ]


viewHeightForAgeBoys5To19 : Language -> Model -> List ( Days, Centimetres ) -> Html any
viewHeightForAgeBoys5To19 language model data =
    svg
        [ class "z-score boys"
        , x "0px"
        , y "0px"
        , viewBox "0 0 841.9 595.3"
        ]
        [ frame language "z-score-blue"
        , labels language heightForAgeBoysLabels5To19
        , referenceLinesHeight5To19
        , ageLines heightForAgeConfig5To19
        , zScoreLabelsHeightForAgeBoys5To19
        , model.lengthHeightForAge
            |> RemoteData.map (.male >> .byDay >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData heightForAgeConfig5To19
        , plotChildData heightForAgeConfig5To19 data
        ]


{-| Things we need to know to plot stuff.

  - drawSD1 controls whether we draw a line for SD1 and SD1neg

  - input represents the bounds of the input values, in terms of their units

  - output represents the bounds of the output values, in terms of pixels

  - paintLevels controls whether we add colors between the lines or not.

-}
type alias PlotConfig xAxis yAxis =
    { toFloatX : xAxis -> Float
    , toFloatY : yAxis -> Float
    , input : Bounds
    , output : Bounds
    , drawSD1 : Bool
    , paintLevels : Bool
    , xAxis : XAxisConfig
    }


type alias Bounds =
    { minX : Float
    , maxX : Float
    , minY : Float
    , maxY : Float
    }


type alias XAxisConfig =
    { width : Int
    , minYear : Int
    , maxYear : Int
    , monthsList : List Int
    , minLength : Int
    , maxLength : Int
    }


heightForAgeConfig : PlotConfig Days Centimetres
heightForAgeConfig =
    { toFloatX = \(Utils.NominalDate.Days days) -> toFloat days
    , toFloatY = \(Centimetres cm) -> cm
    , input = { minY = 42, maxY = 99, minX = 0, maxX = 365 * 2 }
    , output = { minX = 110.9, maxX = 715.4, minY = 119.9, maxY = 506.7 }
    , drawSD1 = False
    , paintLevels = True
    , xAxis =
        { width = 908
        , minYear = 0
        , maxYear = 2
        , monthsList = List.range 1 11
        , minLength = 0
        , maxLength = 0
        }
    }


heightForAgeConfig5To19 : PlotConfig Days Centimetres
heightForAgeConfig5To19 =
    { toFloatX = \(Utils.NominalDate.Days days) -> toFloat days
    , toFloatY = \(Centimetres cm) -> cm
    , input = { minY = 90, maxY = 200, minX = 365 * 2, maxX = 365 * 19 }
    , output = { minX = 111, maxX = 758, minY = 119.9, maxY = 506.7 }
    , drawSD1 = False
    , paintLevels = False
    , xAxis =
        { width = 647
        , minYear = 5
        , maxYear = 19
        , monthsList = [ 3, 6, 9 ]
        , minLength = 0
        , maxLength = 0
        }
    }


weightForAgeConfig : PlotConfig Days Kilograms
weightForAgeConfig =
    { toFloatX = \(Utils.NominalDate.Days days) -> toFloat days
    , toFloatY = \(Kilograms kg) -> kg
    , input = { minY = 1.4, maxY = 17.8, minX = 0, maxX = 365 * 2 }
    , output = { minX = 110.9, maxX = 715.4, minY = 119.9, maxY = 506.7 }
    , drawSD1 = False
    , paintLevels = True
    , xAxis =
        { width = 908
        , minYear = 0
        , maxYear = 2
        , monthsList = List.range 1 11
        , minLength = 0
        , maxLength = 0
        }
    }


weightForHeightConfig : PlotConfig Length Kilograms
weightForHeightConfig =
    { toFloatX = \(Length cm) -> cm
    , toFloatY = \(Kilograms kg) -> kg
    , input = { minY = 1.0, maxY = 25.0, minX = 45, maxX = 110 }
    , output = { minX = 110.9, maxX = 715.4, minY = 119.9, maxY = 506.7 }
    , drawSD1 = True
    , paintLevels = True
    , xAxis =
        { width = 908
        , minYear = 0
        , maxYear = 0
        , monthsList = []
        , minLength = 45
        , maxLength = 110
        }
    }


plotData : PlotConfig x y -> List { x : Float, y : Float } -> List String
plotData config data =
    let
        scaleX =
            (config.output.maxX - config.output.minX)
                / (config.input.maxX - config.input.minX)

        scaleY =
            (config.output.maxY - config.output.minY)
                / (config.input.maxY - config.input.minY)

        plotX x =
            config.output.minX
                + ((x - config.input.minX) * scaleX)
                |> clamp config.output.minX config.output.maxX

        plotY y =
            -- Y is a bit different, because SVG has its origin at the top
            config.output.maxY
                - ((y - config.input.minY) * scaleY)
                |> clamp config.output.minY config.output.maxY
    in
    data
        |> List.map
            (\{ x, y } ->
                Round.round 1 (plotX x) ++ "," ++ Round.round 1 (plotY y)
            )


plotReferenceData : PlotConfig x y -> List ( x, ZScoreEntry y ) -> Svg any
plotReferenceData config zscoreList =
    let
        getPoints zscore =
            zscoreList
                |> List.filterMap
                    (\( x, entry ) ->
                        let
                            result =
                                { x = config.toFloatX x
                                , y = valueForZScore config.toFloatY zscore entry
                                }
                        in
                        -- We have more data in the numeric tables than we
                        -- chart, so don't plot points that are out of
                        -- bounds
                        if result.x >= config.input.minX && result.x <= config.input.maxX then
                            Just result

                        else
                            Nothing
                    )

        -- We need the neg3 and neg2 points both to draw a line and to draw a polygon
        -- for fill ... so, we do them here.
        neg3points =
            getPoints -3

        neg2points =
            getPoints -2

        makeLine dataPoints lineClass =
            dataPoints
                |> plotData config
                |> String.join " "
                |> points
                |> (\pointList -> polyline [ class lineClass, pointList ] [])

        -- Points for a polygon from neg3 to the bottom of the chart
        fillBelowNegativeThree =
            if config.paintLevels then
                [ { x = config.input.maxX
                  , y = config.input.minY
                  }
                , { x = config.input.minX
                  , y = config.input.minY
                  }
                ]
                    |> List.append neg3points
                    |> plotData config
                    |> String.join " "
                    |> points
                    |> (\pointList ->
                            polygon
                                [ class "below-neg-three"
                                , pointList
                                ]
                                []
                       )
                    |> Just

            else
                Nothing

        -- Points for a polygon from neg2 to the top of the chart
        fillAboveNegativeTwo =
            if config.paintLevels then
                [ { x = config.input.maxX
                  , y = config.input.maxY
                  }
                , { x = config.input.minX
                  , y = config.input.maxY
                  }
                ]
                    |> List.append neg2points
                    |> plotData config
                    |> String.join " "
                    |> points
                    |> (\pointList ->
                            polygon
                                [ class "above-neg-two"
                                , pointList
                                ]
                                []
                       )
                    |> Just

            else
                Nothing

        -- Points for a polygon from neg2 to neg3
        fillBetweenNegTwoAndNegThree =
            if config.paintLevels then
                neg2points
                    |> List.append (List.reverse neg3points)
                    |> plotData config
                    |> String.join " "
                    |> points
                    |> (\pointList ->
                            polygon
                                [ class "neg-two-to-neg-three"
                                , pointList
                                ]
                                []
                       )
                    |> Just

            else
                Nothing
    in
    [ fillBelowNegativeThree
    , fillAboveNegativeTwo
    , fillBetweenNegTwoAndNegThree
    , Just <| makeLine neg3points "three-line-new"
    , Just <| makeLine neg2points "two-line-new"
    , if config.drawSD1 then
        Just <| makeLine (getPoints -1) "one-line-new"

      else
        Nothing
    , Just <| makeLine (getPoints 0) "zero-line-new"
    , if config.drawSD1 then
        Just <| makeLine (getPoints 1) "one-line-new"

      else
        Nothing
    , Just <| makeLine (getPoints 2) "two-line-new"
    , Just <| makeLine (getPoints 3) "three-line-new"
    ]
        |> List.filterMap identity
        |> g []


plotChildData : PlotConfig x y -> List ( x, y ) -> Svg any
plotChildData config data =
    let
        pointList =
            data
                |> List.map
                    (\( x, y ) ->
                        { x = config.toFloatX x
                        , y = config.toFloatY y
                        }
                    )
                |> List.sortBy .x
                |> plotData config
                |> String.join " "
                |> points
    in
    polyline
        [ class "child-data"
        , pointList
        ]
        []


viewWeightForAgeBoys : Language -> Model -> List ( Days, Kilograms ) -> Html any
viewWeightForAgeBoys language model data =
    svg
        [ class "z-score boys"
        , x "0px"
        , y "0px"
        , viewBox "0 0 841.9 595.3"
        ]
        [ frame language "z-score-grey"
        , labels language weightForAgeBoysLabels
        , referenceLinesWeight
        , ageLines weightForAgeConfig
        , zScoreLabelsWeightForAgeBoys
        , model.weightForAge
            |> RemoteData.map (.male >> .byDay >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData weightForAgeConfig
        , plotChildData weightForAgeConfig data
        ]


viewWeightForHeightBoys : Language -> Model -> List ( Length, Kilograms ) -> Html any
viewWeightForHeightBoys language model data =
    svg
        [ class "z-score boys"
        , x "0px"
        , y "0px"
        , viewBox "0 0 841.9 595.3"
        ]
        [ frame language "z-score-grey"
        , labels language weightForHeightBoysLabels
        , referenceLinesWeightForHeight
        , heightLines
        , zScoreLabelsWeightForHeightBoys
        , model.weightForLength
            |> RemoteData.map (.male >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData weightForHeightConfig
        , plotChildData weightForHeightConfig data
        ]


viewWeightForHeightGirls : Language -> Model -> List ( Length, Kilograms ) -> Html any
viewWeightForHeightGirls language model data =
    svg
        [ class "z-score girls"
        , x "0px"
        , y "0px"
        , viewBox "0 0 841.9 595.3"
        ]
        [ frame language "z-score-grey"
        , labels language weightForHeightGirlsLabels
        , referenceLinesWeightForHeight
        , heightLines
        , zScoreLabelsWeightForHeightGirls
        , model.weightForLength
            |> RemoteData.map (.female >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData weightForHeightConfig
        , plotChildData weightForHeightConfig data
        ]


viewHeightForAgeGirls : Language -> Model -> List ( Days, Centimetres ) -> Html any
viewHeightForAgeGirls language model data =
    svg
        [ class "z-score girls"
        , x "0px"
        , y "0px"
        , viewBox "0 0 841.9 595.3"
        ]
        [ frame language "z-score-grey"
        , labels language heightForAgeGirlsLabels
        , referenceLinesHeight
        , ageLines heightForAgeConfig
        , zScoreLabelsHeightForAgeGirls
        , model.lengthHeightForAge
            |> RemoteData.map (.female >> .byDay >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData heightForAgeConfig
        , plotChildData heightForAgeConfig data
        ]


viewWeightForAgeGirls : Language -> Model -> List ( Days, Kilograms ) -> Html any
viewWeightForAgeGirls language model data =
    svg
        [ class "z-score girls"
        , x "0px"
        , y "0px"
        , viewBox "0 0 841.9 595.3"
        ]
        [ frame language "z-score-grey"
        , labels language weightForAgeGirlsLabels
        , referenceLinesWeight
        , ageLines weightForAgeConfig
        , zScoreLabelsWeightForAgeGirls
        , model.weightForAge
            |> RemoteData.map (.female >> .byDay >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData weightForAgeConfig
        , plotChildData weightForAgeConfig data
        ]


zScoreLabelsWeightForHeightBoys : Svg any
zScoreLabelsWeightForHeightBoys =
    g []
        [ circle [ class "z-score-white", cx "726", cy "216.2", r "7.5" ] []
        , circle [ class "z-score-white", cx "726", cy "271.7", r "7.5" ] []
        , circle [ class "z-score-white", cx "726", cy "291.3", r "7.5" ] []
        , circle [ class "z-score-white", cx "726", cy "133.9", r "7.5" ] []
        , circle [ class "z-score-white", cx "726.7", cy "196", r "7.5" ] []
        , circle [ class "z-score-white", cx "726", cy "250.1", r "7.5" ] []
        , circle [ class "z-score-white", cx "726.7", cy "167.9", r "7.5" ] []
        , circle [ class "z-score-white", cx "726.7", cy "224.9", r "7.5" ] []
        , text_ [ transform "matrix(1 0 0 1 720.6406 276.0231)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 720.7002 295.5534)", class "z-score-semibold st23" ] [ text "-3" ]
        , text_ [ transform "matrix(1 0 0 1 722.457 138.0392)", class "z-score-semibold st23" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 723.0352 200.634)", class "one-line z-score-semibold st23" ] [ text "1" ]
        , text_ [ transform "matrix(1 0 0 1 720.498 254.5388)", class "one-line z-score-semibold st23" ] [ text "-1" ]
        , text_ [ transform "matrix(1 0 0 1 722.8945 171.6686)", class "two-line z-score-semibold st23" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 723.5527 229.1389)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        ]


zScoreLabelsWeightForHeightGirls : Svg any
zScoreLabelsWeightForHeightGirls =
    g []
        [ circle [ class "z-score-white", cx "726", cy "216.2", r "7.5" ] []
        , circle [ class "z-score-white", cx "726", cy "274.6", r "7.5" ] []
        , circle [ class "z-score-white", cx "726", cy "295.3", r "7.5" ] []
        , circle [ class "z-score-white", cx "726.7", cy "194.3", r "7.5" ] []
        , circle [ class "z-score-white", cx "726", cy "250.9", r "7.5" ] []
        , circle [ class "z-score-white", cx "726.7", cy "162.5", r "7.5" ] []
        , circle [ class "z-score-white", cx "726.7", cy "224.6", r "7.5" ] []
        , text_ [ transform "matrix(1 0 0 1 720.6406 278.8576)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 720.7002 299.5217)", class "z-score-semibold st23" ] [ text "-3" ]
        , text_ [ transform "matrix(1 0 0 1 722.457 130.1022)", class "z-score-semibold st23" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 723.0352 198.9333)", class "one-line z-score-semibold st23" ] [ text "1" ]
        , text_ [ transform "matrix(1 0 0 1 720.498 255.3893)", class "one-line z-score-semibold st23" ] [ text "-1" ]
        , text_ [ transform "matrix(1 0 0 1 722.8945 166.2829)", class "two-line z-score-semibold st23" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 723.5527 228.8552)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        ]


zScoreLabelsHeightForAgeBoys : Svg any
zScoreLabelsHeightForAgeBoys =
    g
        []
        [ text_ [ transform "matrix(1 0 0 1 720.9237 240.2482)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 720.7001 260.0353)", class "z-score-semibold st23" ] [ text "-3" ]
        , text_ [ transform "matrix(1 0 0 1 722.0057 141.1564)", class "z-score-semibold st23" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 722.4686 200.841)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        , text_ [ transform "matrix(1 0 0 1 722.0448 161.738)", class "two-line z-score-semibold st23" ] [ text "2" ]
        ]


zScoreLabelsHeightForAgeBoys5To19 : Svg any
zScoreLabelsHeightForAgeBoys5To19 =
    g
        []
        [ text_ [ transform "matrix(1 0 0 1 722.0057 141.1564)", class "z-score-semibold st23" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 722.0448 161.738)", class "two-line z-score-semibold st23" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 722.0448 181.738)", class "two-line z-score-semibold st23" ] [ text "1" ]
        , text_ [ transform "matrix(1 0 0 1 722.4686 200.841)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        , text_ [ transform "matrix(1 0 0 1 720.9237 220.2482)", class "two-line z-score-semibold st23" ] [ text "-1" ]
        , text_ [ transform "matrix(1 0 0 1 720.9237 240.2482)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 720.7001 260.0353)", class "z-score-semibold st23" ] [ text "-3" ]
        ]


zScoreLabelsWeightForAgeBoys : Svg any
zScoreLabelsWeightForAgeBoys =
    g []
        [ text_ [ transform "matrix(1 0 0 1 719.923 315.5098)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 719.6994 339.3469)", class "z-score-semibold st23" ] [ text "-3" ]
        , text_ [ transform "matrix(1 0 0 1 721.6105 182.8234)", class "two-line z-score-semibold st23" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 722.4973 254.3636)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        , text_ [ transform "matrix(1 0 0 1 722.1398 136.3553)", class "z-score-semibold st23" ] [ text "3" ]
        ]


zScoreLabelsHeightForAgeGirls : Svg any
zScoreLabelsHeightForAgeGirls =
    g
        []
        [ text_ [ transform "matrix(1 0 0 1 720.9238 251.8845)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 720.7002 273.9982)", class "z-score-semibold st23" ] [ text "-3" ]
        , text_ [ transform "matrix(1 0 0 1 722.29 142.5744)", class "z-score-semibold st23" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 722.4688 208.1066)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        , text_ [ transform "matrix(1 0 0 1 722.0449 165.1994)", class "two-line z-score-semibold st23" ] [ text "2" ]
        ]


zScoreLabelsWeightForAgeGirls : Svg any
zScoreLabelsWeightForAgeGirls =
    g
        []
        [ circle [ class "z-score-white", cx "726.7", cy "186.8", r "7.5" ] []
        , circle [ class "z-score-white", cx "726.7", cy "137.8", r "7.5" ] []
        , circle [ class "z-score-white", cx "726.7", cy "325", r "7.5" ] []
        , circle [ class "z-score-white", cx "726.7", cy "347.8", r "7.5" ] []
        , circle [ class "z-score-white", cx "726.7", cy "268", r "7.5" ] []
        , text_ [ transform "matrix(1 0 0 1 720.9238 329.1916)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 720.7002 351.9709)", class "z-score-semibold st23" ] [ text "-3" ]
        , text_ [ transform "matrix(1 0 0 1 723.707 141.6711)", class "z-score-semibold st23" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 723.4619 190.8845)", class "two-line z-score-semibold st23" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 723.498 272.1838)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        ]


ageLines : PlotConfig x y -> Svg any
ageLines config =
    let
        yearsList =
            List.range config.xAxis.minYear config.xAxis.maxYear

        yearsCount =
            List.length yearsList

        spaceBetweenYears =
            config.xAxis.width // yearsCount

        spaceBetweenMonths =
            -- We add one here because we want to give space before the next year.
            spaceBetweenYears // (List.length config.xAxis.monthsList + 1)

        -- Here we can define the lines as we want.
        lines =
            List.indexedMap
                (\i year ->
                    let
                        yearMargin =
                            if i == 0 then
                                config.output.minX

                            else
                                config.output.minX + toFloat (spaceBetweenYears * i)

                        yearTextPosition =
                            if year < 10 then
                                (yearMargin - 2)
                                    |> toString

                            else
                                (yearMargin - 5)
                                    |> toString

                        yearLinePosition =
                            toString yearMargin

                        monthLinesAndText =
                            List.indexedMap
                                (\monthIndex month ->
                                    let
                                        monthMargin =
                                            yearMargin + toFloat (spaceBetweenMonths * (monthIndex + 1))

                                        monthTextPosition =
                                            if month < 10 then
                                                (monthMargin - 2)
                                                    |> toString

                                            else
                                                (monthMargin - 5)
                                                    |> toString

                                        monthLinePosition =
                                            toString monthMargin
                                    in
                                    if year < config.xAxis.maxYear then
                                        [ line [ class "month-line", x1 monthLinePosition, y1 "506.5", x2 monthLinePosition, y2 "119.5" ] []
                                        , text_ [ transform <| "matrix(1 0 0 1 " ++ monthTextPosition ++ " 516.5436)", class "z-score-white z-score-semibold st16" ] [ text <| toString month ]
                                        ]

                                    else
                                        []
                                )
                                config.xAxis.monthsList
                                |> List.concat
                    in
                    [ line [ class "year-line", x1 yearLinePosition, y1 "514.5", x2 yearLinePosition, y2 "119.5" ] []
                    , text_
                        [ transform <| "matrix(1 0 0 1 " ++ yearTextPosition ++ " 525.9767)"
                        , class "z-score-white z-score-semibold st20"
                        ]
                        [ text <| toString year ]
                    ]
                        |> List.append monthLinesAndText
                )
                yearsList
                |> List.concat
    in
    g []
        lines


heightLines : Svg any
heightLines =
    g []
        [ line [ class "st18", x1 "157.3", y1 "506.6", x2 "157.3", y2 "119.9" ] []
        , line [ class "st18", x1 "203.9", y1 "506.6", x2 "203.9", y2 "119.9" ] []
        , line [ class "st18", x1 "250.4", y1 "506.6", x2 "250.4", y2 "119.9" ] []
        , line [ class "st18", x1 "296.9", y1 "506.6", x2 "296.9", y2 "119.9" ] []
        , line [ class "st18", x1 "343.4", y1 "506.6", x2 "343.4", y2 "119.9" ] []
        , line [ class "st18", x1 "389.9", y1 "506.6", x2 "389.9", y2 "119.9" ] []
        , line [ class "st18", x1 "436.4", y1 "506.6", x2 "436.4", y2 "119.9" ] []
        , line [ class "st18", x1 "482.9", y1 "506.6", x2 "482.9", y2 "119.9" ] []
        , line [ class "st18", x1 "529.4", y1 "506.6", x2 "529.4", y2 "119.9" ] []
        , line [ class "st18", x1 "575.9", y1 "506.6", x2 "575.9", y2 "119.9" ] []
        , line [ class "st18", x1 "622.4", y1 "506.6", x2 "622.4", y2 "119.9" ] []
        , line [ class "st18", x1 "668.9", y1 "506.6", x2 "668.9", y2 "119.9" ] []
        , line [ class "st19", x1 "120.2", y1 "506.6", x2 "120.2", y2 "119.9" ] []
        , line [ class "st19", x1 "129.5", y1 "506.6", x2 "129.5", y2 "119.9" ] []
        , line [ class "st19", x1 "138.8", y1 "506.6", x2 "138.8", y2 "119.9" ] []
        , line [ class "st19", x1 "148.1", y1 "506.6", x2 "148.1", y2 "119.9" ] []
        , line [ class "st19", x1 "166.7", y1 "506.6", x2 "166.7", y2 "119.9" ] []
        , line [ class "st19", x1 "176", y1 "506.6", x2 "176", y2 "119.9" ] []
        , line [ class "st19", x1 "185.3", y1 "506.6", x2 "185.3", y2 "119.9" ] []
        , line [ class "st19", x1 "194.6", y1 "506.6", x2 "194.6", y2 "119.9" ] []
        , line [ class "st19", x1 "213.2", y1 "506.6", x2 "213.2", y2 "119.9" ] []
        , line [ class "st19", x1 "222.5", y1 "506.6", x2 "222.5", y2 "119.9" ] []
        , line [ class "st19", x1 "231.8", y1 "506.6", x2 "231.8", y2 "119.9" ] []
        , line [ class "st19", x1 "241.1", y1 "506.6", x2 "241.1", y2 "119.9" ] []
        , line [ class "st19", x1 "259.7", y1 "506.6", x2 "259.7", y2 "119.9" ] []
        , line [ class "st19", x1 "269", y1 "506.6", x2 "269", y2 "119.9" ] []
        , line [ class "st19", x1 "278.3", y1 "506.6", x2 "278.3", y2 "119.9" ] []
        , line [ class "st19", x1 "287.6", y1 "506.6", x2 "287.6", y2 "119.9" ] []
        , line [ class "st19", x1 "306.2", y1 "506.6", x2 "306.2", y2 "119.9" ] []
        , line [ class "st19", x1 "315.5", y1 "506.6", x2 "315.5", y2 "119.9" ] []
        , line [ class "st19", x1 "324.8", y1 "506.6", x2 "324.8", y2 "119.9" ] []
        , line [ class "st19", x1 "334.1", y1 "506.6", x2 "334.1", y2 "119.9" ] []
        , line [ class "st19", x1 "352.7", y1 "506.6", x2 "352.7", y2 "119.9" ] []
        , line [ class "st19", x1 "362", y1 "506.6", x2 "362", y2 "119.9" ] []
        , line [ class "st19", x1 "371.3", y1 "506.6", x2 "371.3", y2 "119.9" ] []
        , line [ class "st19", x1 "380.6", y1 "506.6", x2 "380.6", y2 "119.9" ] []
        , line [ class "st19", x1 "399.2", y1 "506.6", x2 "399.2", y2 "119.9" ] []
        , line [ class "st19", x1 "408.5", y1 "506.6", x2 "408.5", y2 "119.9" ] []
        , line [ class "st19", x1 "417.8", y1 "506.6", x2 "417.8", y2 "119.9" ] []
        , line [ class "st19", x1 "427.1", y1 "506.6", x2 "427.1", y2 "119.9" ] []
        , line [ class "st19", x1 "445.7", y1 "506.6", x2 "445.7", y2 "119.9" ] []
        , line [ class "st19", x1 "455", y1 "506.6", x2 "455", y2 "119.9" ] []
        , line [ class "st19", x1 "464.3", y1 "506.6", x2 "464.3", y2 "119.9" ] []
        , line [ class "st19", x1 "473.6", y1 "506.6", x2 "473.6", y2 "119.9" ] []
        , line [ class "st19", x1 "492.2", y1 "506.6", x2 "492.2", y2 "119.9" ] []
        , line [ class "st19", x1 "501.5", y1 "506.6", x2 "501.5", y2 "119.9" ] []
        , line [ class "st19", x1 "510.8", y1 "506.6", x2 "510.8", y2 "119.9" ] []
        , line [ class "st19", x1 "520.1", y1 "506.6", x2 "520.1", y2 "119.9" ] []
        , line [ class "st19", x1 "538.7", y1 "506.6", x2 "538.7", y2 "119.9" ] []
        , line [ class "st19", x1 "548", y1 "506.6", x2 "548", y2 "119.9" ] []
        , line [ class "st19", x1 "557.3", y1 "506.6", x2 "557.3", y2 "119.9" ] []
        , line [ class "st19", x1 "566.6", y1 "506.6", x2 "566.6", y2 "119.9" ] []
        , line [ class "st19", x1 "585.2", y1 "506.6", x2 "585.2", y2 "119.9" ] []
        , line [ class "st19", x1 "594.5", y1 "506.6", x2 "594.5", y2 "119.9" ] []
        , line [ class "st19", x1 "603.8", y1 "506.6", x2 "603.8", y2 "119.9" ] []
        , line [ class "st19", x1 "613.1", y1 "506.6", x2 "613.1", y2 "119.9" ] []
        , line [ class "st19", x1 "631.7", y1 "506.6", x2 "631.7", y2 "119.9" ] []
        , line [ class "st19", x1 "641", y1 "506.6", x2 "641", y2 "119.9" ] []
        , line [ class "st19", x1 "650.3", y1 "506.6", x2 "650.3", y2 "119.9" ] []
        , line [ class "st19", x1 "659.6", y1 "506.6", x2 "659.6", y2 "119.9" ] []
        , line [ class "st19", x1 "678.2", y1 "506.6", x2 "678.2", y2 "119.9" ] []
        , line [ class "st19", x1 "687.5", y1 "506.6", x2 "687.5", y2 "119.9" ] []
        , line [ class "st19", x1 "696.8", y1 "506.6", x2 "696.8", y2 "119.9" ] []
        , line [ class "st19", x1 "706.1", y1 "506.6", x2 "706.1", y2 "119.9" ] []
        , line [ class "year-line", x1 "715.4", y1 "506.6", x2 "715.4", y2 "119.9" ] []
        , text_ [ transform "matrix(1 0 0 1 106.77 518.1096)", class "z-score-white z-score-semibold st16" ] [ text "45" ]
        , text_ [ transform "matrix(1 0 0 1 153.2725 518.1096)", class "z-score-white z-score-semibold st16" ] [ text "50" ]
        , text_ [ transform "matrix(1 0 0 1 199.7749 518.1096)", class "z-score-white z-score-semibold st16" ] [ text "55" ]
        , text_ [ transform "matrix(1 0 0 1 246.2773 518.1096)", class "z-score-white z-score-semibold st16" ] [ text "60" ]
        , text_ [ transform "matrix(1 0 0 1 292.7798 518.1096)", class "z-score-white z-score-semibold st16" ] [ text "65" ]
        , text_ [ transform "matrix(1 0 0 1 339.2822 518.1096)", class "z-score-white z-score-semibold st16" ] [ text "70" ]
        , text_ [ transform "matrix(1 0 0 1 385.7847 518.1096)", class "z-score-white z-score-semibold st16" ] [ text "75" ]
        , text_ [ transform "matrix(1 0 0 1 432.2871 518.1096)", class "z-score-white z-score-semibold st16" ] [ text "80" ]
        , text_ [ transform "matrix(1 0 0 1 478.7891 518.1096)", class "z-score-white z-score-semibold st16" ] [ text "85" ]
        , text_ [ transform "matrix(1 0 0 1 525.292 518.1096)", class "z-score-white z-score-semibold st16" ] [ text "90" ]
        , text_ [ transform "matrix(1 0 0 1 571.7939 518.1096)", class "z-score-white z-score-semibold st16" ] [ text "95" ]
        , text_ [ transform "matrix(1 0 0 1 616.2168 518.1096)", class "z-score-white z-score-semibold st16" ] [ text "100" ]
        , text_ [ transform "matrix(1 0 0 1 662.7197 518.1096)", class "z-score-white z-score-semibold st16" ] [ text "105" ]
        , text_ [ transform "matrix(1 0 0 1 709.0928 518.1096)", class "z-score-white z-score-semibold st16" ] [ text "110" ]
        ]


referenceLinesHeight : Svg any
referenceLinesHeight =
    g []
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
        , line [ class "st19", x1 "110.8", y1 "499.9", x2 "715.4", y2 "499.9" ] []
        , line [ class "st19", x1 "110.8", y1 "493.1", x2 "715.4", y2 "493.1" ] []
        , line [ class "st19", x1 "110.8", y1 "479.5", x2 "715.4", y2 "479.5" ] []
        , line [ class "st19", x1 "110.8", y1 "472.8", x2 "715.4", y2 "472.8" ] []
        , line [ class "st19", x1 "110.8", y1 "466", x2 "715.4", y2 "466" ] []
        , line [ class "st19", x1 "110.8", y1 "459.2", x2 "715.4", y2 "459.2" ] []
        , line [ class "st19", x1 "110.8", y1 "445.6", x2 "715.4", y2 "445.6" ] []
        , line [ class "st19", x1 "110.8", y1 "438.8", x2 "715.4", y2 "438.8" ] []
        , line [ class "st19", x1 "110.8", y1 "432", x2 "715.4", y2 "432" ] []
        , line [ class "st19", x1 "110.8", y1 "425.3", x2 "715.4", y2 "425.3" ] []
        , line [ class "st19", x1 "110.8", y1 "411.7", x2 "715.4", y2 "411.7" ] []
        , line [ class "st19", x1 "110.8", y1 "404.9", x2 "715.4", y2 "404.9" ] []
        , line [ class "st19", x1 "110.8", y1 "398.1", x2 "715.4", y2 "398.1" ] []
        , line [ class "st19", x1 "110.8", y1 "391.3", x2 "715.4", y2 "391.3" ] []
        , line [ class "st19", x1 "110.8", y1 "377.8", x2 "715.4", y2 "377.8" ] []
        , line [ class "st19", x1 "110.8", y1 "371", x2 "715.4", y2 "371" ] []
        , line [ class "st19", x1 "110.8", y1 "364.2", x2 "715.4", y2 "364.2" ] []
        , line [ class "st19", x1 "110.8", y1 "357.4", x2 "715.4", y2 "357.4" ] []
        , line [ class "st19", x1 "110.8", y1 "343.8", x2 "715.4", y2 "343.8" ] []
        , line [ class "st19", x1 "110.8", y1 "337.1", x2 "715.4", y2 "337.1" ] []
        , line [ class "st19", x1 "110.8", y1 "330.3", x2 "715.4", y2 "330.3" ] []
        , line [ class "st19", x1 "110.8", y1 "323.5", x2 "715.4", y2 "323.5" ] []
        , line [ class "st19", x1 "110.8", y1 "309.9", x2 "715.4", y2 "309.9" ] []
        , line [ class "st19", x1 "110.8", y1 "303.1", x2 "715.4", y2 "303.1" ] []
        , line [ class "st19", x1 "110.8", y1 "296.3", x2 "715.4", y2 "296.3" ] []
        , line [ class "st19", x1 "110.8", y1 "289.6", x2 "715.4", y2 "289.6" ] []
        , line [ class "st19", x1 "110.8", y1 "276", x2 "715.4", y2 "276" ] []
        , line [ class "st19", x1 "110.8", y1 "269.2", x2 "715.4", y2 "269.2" ] []
        , line [ class "st19", x1 "110.8", y1 "262.4", x2 "715.4", y2 "262.4" ] []
        , line [ class "st19", x1 "110.8", y1 "255.6", x2 "715.4", y2 "255.6" ] []
        , line [ class "st19", x1 "110.8", y1 "242.1", x2 "715.4", y2 "242.1" ] []
        , line [ class "st19", x1 "110.8", y1 "235.3", x2 "715.4", y2 "235.3" ] []
        , line [ class "st19", x1 "110.8", y1 "228.5", x2 "715.4", y2 "228.5" ] []
        , line [ class "st19", x1 "110.8", y1 "221.7", x2 "715.4", y2 "221.7" ] []
        , line [ class "st19", x1 "110.8", y1 "208.1", x2 "715.4", y2 "208.1" ] []
        , line [ class "st19", x1 "110.8", y1 "201.4", x2 "715.4", y2 "201.4" ] []
        , line [ class "st19", x1 "110.8", y1 "194.6", x2 "715.4", y2 "194.6" ] []
        , line [ class "st19", x1 "110.8", y1 "187.8", x2 "715.4", y2 "187.8" ] []
        , line [ class "st19", x1 "110.8", y1 "174.2", x2 "715.4", y2 "174.2" ] []
        , line [ class "st19", x1 "110.8", y1 "167.4", x2 "715.4", y2 "167.4" ] []
        , line [ class "st19", x1 "110.8", y1 "160.6", x2 "715.4", y2 "160.6" ] []
        , line [ class "st19", x1 "110.8", y1 "153.9", x2 "715.4", y2 "153.9" ] []
        , line [ class "st19", x1 "110.8", y1 "140.3", x2 "715.4", y2 "140.3" ] []
        , line [ class "st19", x1 "110.8", y1 "133.5", x2 "715.4", y2 "133.5" ] []
        , line [ class "st19", x1 "110.8", y1 "126.7", x2 "715.4", y2 "126.7" ] []
        , text_ [ transform "matrix(1 0 0 1 95.4252 488.8879)", class "z-score-white z-score-semibold st16" ] [ text "45" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 454.9621)", class "z-score-white z-score-semibold st16" ] [ text "50" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 421.0353)", class "z-score-white z-score-semibold st16" ] [ text "55" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 387.1096)", class "z-score-white z-score-semibold st16" ] [ text "60" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 353.1828)", class "z-score-white z-score-semibold st16" ] [ text "65" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 319.257)", class "z-score-white z-score-semibold st16" ] [ text "70" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 285.3303)", class "z-score-white z-score-semibold st16" ] [ text "75" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 251.404)", class "z-score-white z-score-semibold st16" ] [ text "80" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 217.4777)", class "z-score-white z-score-semibold st16" ] [ text "85" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 183.5515)", class "z-score-white z-score-semibold st16" ] [ text "90" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 149.6296)", class "z-score-white z-score-semibold st16" ] [ text "95" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 488.8879)", class "z-score-white z-score-semibold st16" ] [ text "45" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 454.9621)", class "z-score-white z-score-semibold st16" ] [ text "50" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 421.0353)", class "z-score-white z-score-semibold st16" ] [ text "55" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 387.1096)", class "z-score-white z-score-semibold st16" ] [ text "60" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 353.1828)", class "z-score-white z-score-semibold st16" ] [ text "65" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 319.257)", class "z-score-white z-score-semibold st16" ] [ text "70" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 285.3307)", class "z-score-white z-score-semibold st16" ] [ text "75" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 251.4045)", class "z-score-white z-score-semibold st16" ] [ text "80" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 217.4777)", class "z-score-white z-score-semibold st16" ] [ text "85" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 183.5515)", class "z-score-white z-score-semibold st16" ] [ text "90" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 149.6296)", class "z-score-white z-score-semibold st16" ] [ text "95" ]
        ]


referenceLinesHeight5To19 : Svg any
referenceLinesHeight5To19 =
    g []
        [ line [ class "st19", x1 "110.8", y1 "486.3", x2 "737.6", y2 "486.3" ] []
        , line [ class "st19", x1 "110.8", y1 "452.4", x2 "737.6", y2 "452.4" ] []
        , line [ class "st19", x1 "110.8", y1 "418.5", x2 "737.6", y2 "418.5" ] []
        , line [ class "st19", x1 "110.8", y1 "384.5", x2 "737.6", y2 "384.5" ] []
        , line [ class "st19", x1 "110.8", y1 "350.6", x2 "737.6", y2 "350.6" ] []
        , line [ class "st19", x1 "110.8", y1 "316.7", x2 "737.6", y2 "316.7" ] []
        , line [ class "st19", x1 "110.8", y1 "282.8", x2 "737.6", y2 "282.8" ] []
        , line [ class "st19", x1 "110.8", y1 "248.8", x2 "737.6", y2 "248.8" ] []
        , line [ class "st19", x1 "110.8", y1 "214.9", x2 "737.6", y2 "214.9" ] []
        , line [ class "st19", x1 "110.8", y1 "181", x2 "737.6", y2 "181" ] []
        , line [ class "st19", x1 "110.8", y1 "147.1", x2 "737.6", y2 "147.1" ] []
        , line [ class "st19", x1 "110.8", y1 "499.9", x2 "715.4", y2 "499.9" ] []
        , line [ class "st19", x1 "110.8", y1 "493.1", x2 "715.4", y2 "493.1" ] []
        , line [ class "st19", x1 "110.8", y1 "479.5", x2 "715.4", y2 "479.5" ] []
        , line [ class "st19", x1 "110.8", y1 "472.8", x2 "715.4", y2 "472.8" ] []
        , line [ class "st19", x1 "110.8", y1 "466", x2 "715.4", y2 "466" ] []
        , line [ class "st19", x1 "110.8", y1 "459.2", x2 "715.4", y2 "459.2" ] []
        , line [ class "st19", x1 "110.8", y1 "445.6", x2 "715.4", y2 "445.6" ] []
        , line [ class "st19", x1 "110.8", y1 "438.8", x2 "715.4", y2 "438.8" ] []
        , line [ class "st19", x1 "110.8", y1 "432", x2 "715.4", y2 "432" ] []
        , line [ class "st19", x1 "110.8", y1 "425.3", x2 "715.4", y2 "425.3" ] []
        , line [ class "st19", x1 "110.8", y1 "411.7", x2 "715.4", y2 "411.7" ] []
        , line [ class "st19", x1 "110.8", y1 "404.9", x2 "715.4", y2 "404.9" ] []
        , line [ class "st19", x1 "110.8", y1 "398.1", x2 "715.4", y2 "398.1" ] []
        , line [ class "st19", x1 "110.8", y1 "391.3", x2 "715.4", y2 "391.3" ] []
        , line [ class "st19", x1 "110.8", y1 "377.8", x2 "715.4", y2 "377.8" ] []
        , line [ class "st19", x1 "110.8", y1 "371", x2 "715.4", y2 "371" ] []
        , line [ class "st19", x1 "110.8", y1 "364.2", x2 "715.4", y2 "364.2" ] []
        , line [ class "st19", x1 "110.8", y1 "357.4", x2 "715.4", y2 "357.4" ] []
        , line [ class "st19", x1 "110.8", y1 "343.8", x2 "715.4", y2 "343.8" ] []
        , line [ class "st19", x1 "110.8", y1 "337.1", x2 "715.4", y2 "337.1" ] []
        , line [ class "st19", x1 "110.8", y1 "330.3", x2 "715.4", y2 "330.3" ] []
        , line [ class "st19", x1 "110.8", y1 "323.5", x2 "715.4", y2 "323.5" ] []
        , line [ class "st19", x1 "110.8", y1 "309.9", x2 "715.4", y2 "309.9" ] []
        , line [ class "st19", x1 "110.8", y1 "303.1", x2 "715.4", y2 "303.1" ] []
        , line [ class "st19", x1 "110.8", y1 "296.3", x2 "715.4", y2 "296.3" ] []
        , line [ class "st19", x1 "110.8", y1 "289.6", x2 "715.4", y2 "289.6" ] []
        , line [ class "st19", x1 "110.8", y1 "276", x2 "715.4", y2 "276" ] []
        , line [ class "st19", x1 "110.8", y1 "269.2", x2 "715.4", y2 "269.2" ] []
        , line [ class "st19", x1 "110.8", y1 "262.4", x2 "715.4", y2 "262.4" ] []
        , line [ class "st19", x1 "110.8", y1 "255.6", x2 "715.4", y2 "255.6" ] []
        , line [ class "st19", x1 "110.8", y1 "242.1", x2 "715.4", y2 "242.1" ] []
        , line [ class "st19", x1 "110.8", y1 "235.3", x2 "715.4", y2 "235.3" ] []
        , line [ class "st19", x1 "110.8", y1 "228.5", x2 "715.4", y2 "228.5" ] []
        , line [ class "st19", x1 "110.8", y1 "221.7", x2 "715.4", y2 "221.7" ] []
        , line [ class "st19", x1 "110.8", y1 "208.1", x2 "715.4", y2 "208.1" ] []
        , line [ class "st19", x1 "110.8", y1 "201.4", x2 "715.4", y2 "201.4" ] []
        , line [ class "st19", x1 "110.8", y1 "194.6", x2 "715.4", y2 "194.6" ] []
        , line [ class "st19", x1 "110.8", y1 "187.8", x2 "715.4", y2 "187.8" ] []
        , line [ class "st19", x1 "110.8", y1 "174.2", x2 "715.4", y2 "174.2" ] []
        , line [ class "st19", x1 "110.8", y1 "167.4", x2 "715.4", y2 "167.4" ] []
        , line [ class "st19", x1 "110.8", y1 "160.6", x2 "715.4", y2 "160.6" ] []
        , line [ class "st19", x1 "110.8", y1 "153.9", x2 "715.4", y2 "153.9" ] []
        , line [ class "st19", x1 "110.8", y1 "140.3", x2 "715.4", y2 "140.3" ] []
        , line [ class "st19", x1 "110.8", y1 "133.5", x2 "715.4", y2 "133.5" ] []
        , line [ class "st19", x1 "110.8", y1 "126.7", x2 "715.4", y2 "126.7" ] []
        , text_ [ transform "matrix(1 0 0 1 95.4252 488.8879)", class "z-score-white z-score-semibold st16" ] [ text "45" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 454.9621)", class "z-score-white z-score-semibold st16" ] [ text "50" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 421.0353)", class "z-score-white z-score-semibold st16" ] [ text "55" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 387.1096)", class "z-score-white z-score-semibold st16" ] [ text "60" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 353.1828)", class "z-score-white z-score-semibold st16" ] [ text "65" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 319.257)", class "z-score-white z-score-semibold st16" ] [ text "70" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 285.3303)", class "z-score-white z-score-semibold st16" ] [ text "75" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 251.404)", class "z-score-white z-score-semibold st16" ] [ text "80" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 217.4777)", class "z-score-white z-score-semibold st16" ] [ text "85" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 183.5515)", class "z-score-white z-score-semibold st16" ] [ text "90" ]
        , text_ [ transform "matrix(1 0 0 1 95.4252 149.6296)", class "z-score-white z-score-semibold st16" ] [ text "95" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 488.8879)", class "z-score-white z-score-semibold st16" ] [ text "45" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 454.9621)", class "z-score-white z-score-semibold st16" ] [ text "50" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 421.0353)", class "z-score-white z-score-semibold st16" ] [ text "55" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 387.1096)", class "z-score-white z-score-semibold st16" ] [ text "60" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 353.1828)", class "z-score-white z-score-semibold st16" ] [ text "65" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 319.257)", class "z-score-white z-score-semibold st16" ] [ text "70" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 285.3307)", class "z-score-white z-score-semibold st16" ] [ text "75" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 251.4045)", class "z-score-white z-score-semibold st16" ] [ text "80" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 217.4777)", class "z-score-white z-score-semibold st16" ] [ text "85" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 183.5515)", class "z-score-white z-score-semibold st16" ] [ text "90" ]
        , text_ [ transform "matrix(1 0 0 1 745.0155 149.6296)", class "z-score-white z-score-semibold st16" ] [ text "95" ]
        ]


referenceLinesWeight : Svg any
referenceLinesWeight =
    g []
        [ line [ class "st18", x1 "110.8", y1 "492.5", x2 "737.6", y2 "492.5" ] []
        , line [ class "st18", x1 "110.8", y1 "469", x2 "737.6", y2 "469" ] []
        , line [ class "st18", x1 "110.8", y1 "445.4", x2 "737.6", y2 "445.4" ] []
        , line [ class "st18", x1 "110.8", y1 "421.8", x2 "737.6", y2 "421.8" ] []
        , line [ class "st18", x1 "110.8", y1 "398.2", x2 "737.6", y2 "398.2" ] []
        , line [ class "st18", x1 "110.8", y1 "374.6", x2 "737.6", y2 "374.6" ] []
        , line [ class "st18", x1 "110.8", y1 "351", x2 "737.6", y2 "351" ] []
        , line [ class "st18", x1 "110.8", y1 "327.4", x2 "737.6", y2 "327.4" ] []
        , line [ class "st18", x1 "110.8", y1 "303.9", x2 "737.6", y2 "303.9" ] []
        , line [ class "st18", x1 "110.8", y1 "280.3", x2 "737.6", y2 "280.3" ] []
        , line [ class "st18", x1 "110.8", y1 "256.7", x2 "737.6", y2 "256.7" ] []
        , line [ class "st18", x1 "110.8", y1 "233.1", x2 "737.6", y2 "233.1" ] []
        , line [ class "st18", x1 "110.8", y1 "209.5", x2 "737.6", y2 "209.5" ] []
        , line [ class "st18", x1 "110.8", y1 "185.9", x2 "737.6", y2 "185.9" ] []
        , line [ class "st18", x1 "110.8", y1 "162.4", x2 "737.6", y2 "162.4" ] []
        , line [ class "st18", x1 "110.8", y1 "138.8", x2 "737.6", y2 "138.8" ] []
        , line [ class "st19", x1 "110.8", y1 "502", x2 "715.4", y2 "502" ] []
        , line [ class "st19", x1 "110.8", y1 "497.3", x2 "715.4", y2 "497.3" ] []
        , line [ class "st19", x1 "110.8", y1 "487.8", x2 "715.4", y2 "487.8" ] []
        , line [ class "st19", x1 "110.8", y1 "483.1", x2 "715.4", y2 "483.1" ] []
        , line [ class "st19", x1 "110.8", y1 "478.4", x2 "715.4", y2 "478.4" ] []
        , line [ class "st19", x1 "110.8", y1 "473.7", x2 "715.4", y2 "473.7" ] []
        , line [ class "st19", x1 "110.8", y1 "464.2", x2 "715.4", y2 "464.2" ] []
        , line [ class "st19", x1 "110.8", y1 "459.5", x2 "715.4", y2 "459.5" ] []
        , line [ class "st19", x1 "110.8", y1 "454.8", x2 "715.4", y2 "454.8" ] []
        , line [ class "st19", x1 "110.8", y1 "450.1", x2 "715.4", y2 "450.1" ] []
        , line [ class "st19", x1 "110.8", y1 "440.6", x2 "715.4", y2 "440.6" ] []
        , line [ class "st19", x1 "110.8", y1 "435.9", x2 "715.4", y2 "435.9" ] []
        , line [ class "st19", x1 "110.8", y1 "431.2", x2 "715.4", y2 "431.2" ] []
        , line [ class "st19", x1 "110.8", y1 "426.5", x2 "715.4", y2 "426.5" ] []
        , line [ class "st19", x1 "110.8", y1 "417.1", x2 "715.4", y2 "417.1" ] []
        , line [ class "st19", x1 "110.8", y1 "412.3", x2 "715.4", y2 "412.3" ] []
        , line [ class "st19", x1 "110.8", y1 "407.6", x2 "715.4", y2 "407.6" ] []
        , line [ class "st19", x1 "110.8", y1 "402.9", x2 "715.4", y2 "402.9" ] []
        , line [ class "st19", x1 "110.8", y1 "393.5", x2 "715.4", y2 "393.5" ] []
        , line [ class "st19", x1 "110.8", y1 "388.8", x2 "715.4", y2 "388.8" ] []
        , line [ class "st19", x1 "110.8", y1 "384", x2 "715.4", y2 "384" ] []
        , line [ class "st19", x1 "110.8", y1 "379.3", x2 "715.4", y2 "379.3" ] []
        , line [ class "st19", x1 "110.8", y1 "369.9", x2 "715.4", y2 "369.9" ] []
        , line [ class "st19", x1 "110.8", y1 "365.2", x2 "715.4", y2 "365.2" ] []
        , line [ class "st19", x1 "110.8", y1 "360.5", x2 "715.4", y2 "360.5" ] []
        , line [ class "st19", x1 "110.8", y1 "355.7", x2 "715.4", y2 "355.7" ] []
        , line [ class "st19", x1 "110.8", y1 "346.3", x2 "715.4", y2 "346.3" ] []
        , line [ class "st19", x1 "110.8", y1 "341.6", x2 "715.4", y2 "341.6" ] []
        , line [ class "st19", x1 "110.8", y1 "336.9", x2 "715.4", y2 "336.9" ] []
        , line [ class "st19", x1 "110.8", y1 "332.2", x2 "715.4", y2 "332.2" ] []
        , line [ class "st19", x1 "110.8", y1 "322.7", x2 "715.4", y2 "322.7" ] []
        , line [ class "st19", x1 "110.8", y1 "318", x2 "715.4", y2 "318" ] []
        , line [ class "st19", x1 "110.8", y1 "313.3", x2 "715.4", y2 "313.3" ] []
        , line [ class "st19", x1 "110.8", y1 "308.6", x2 "715.4", y2 "308.6" ] []
        , line [ class "st19", x1 "110.8", y1 "299.1", x2 "715.4", y2 "299.1" ] []
        , line [ class "st19", x1 "110.8", y1 "294.4", x2 "715.4", y2 "294.4" ] []
        , line [ class "st19", x1 "110.8", y1 "289.7", x2 "715.4", y2 "289.7" ] []
        , line [ class "st19", x1 "110.8", y1 "285", x2 "715.4", y2 "285" ] []
        , line [ class "st19", x1 "110.8", y1 "275.6", x2 "715.4", y2 "275.6" ] []
        , line [ class "st19", x1 "110.8", y1 "270.8", x2 "715.4", y2 "270.8" ] []
        , line [ class "st19", x1 "110.8", y1 "266.1", x2 "715.4", y2 "266.1" ] []
        , line [ class "st19", x1 "110.8", y1 "261.4", x2 "715.4", y2 "261.4" ] []
        , line [ class "st19", x1 "110.8", y1 "252", x2 "715.4", y2 "252" ] []
        , line [ class "st19", x1 "110.8", y1 "247.3", x2 "715.4", y2 "247.3" ] []
        , line [ class "st19", x1 "110.8", y1 "242.5", x2 "715.4", y2 "242.5" ] []
        , line [ class "st19", x1 "110.8", y1 "237.8", x2 "715.4", y2 "237.8" ] []
        , line [ class "st19", x1 "110.8", y1 "228.4", x2 "715.4", y2 "228.4" ] []
        , line [ class "st19", x1 "110.8", y1 "223.7", x2 "715.4", y2 "223.7" ] []
        , line [ class "st19", x1 "110.8", y1 "219", x2 "715.4", y2 "219" ] []
        , line [ class "st19", x1 "110.8", y1 "214.2", x2 "715.4", y2 "214.2" ] []
        , line [ class "st19", x1 "110.8", y1 "204.8", x2 "715.4", y2 "204.8" ] []
        , line [ class "st19", x1 "110.8", y1 "200.1", x2 "715.4", y2 "200.1" ] []
        , line [ class "st19", x1 "110.8", y1 "195.4", x2 "715.4", y2 "195.4" ] []
        , line [ class "st19", x1 "110.8", y1 "190.7", x2 "715.4", y2 "190.7" ] []
        , line [ class "st19", x1 "110.8", y1 "181.2", x2 "715.4", y2 "181.2" ] []
        , line [ class "st19", x1 "110.8", y1 "176.5", x2 "715.4", y2 "176.5" ] []
        , line [ class "st19", x1 "110.8", y1 "171.8", x2 "715.4", y2 "171.8" ] []
        , line [ class "st19", x1 "110.8", y1 "167.1", x2 "715.4", y2 "167.1" ] []
        , line [ class "st19", x1 "110.8", y1 "157.6", x2 "715.4", y2 "157.6" ] []
        , line [ class "st19", x1 "110.8", y1 "152.9", x2 "715.4", y2 "152.9" ] []
        , line [ class "st19", x1 "110.8", y1 "148.2", x2 "715.4", y2 "148.2" ] []
        , line [ class "st19", x1 "110.8", y1 "143.5", x2 "715.4", y2 "143.5" ] []
        , line [ class "st19", x1 "110.8", y1 "134.1", x2 "715.4", y2 "134.1" ] []
        , line [ class "st19", x1 "110.8", y1 "129.3", x2 "715.4", y2 "129.3" ] []
        , line [ class "st19", x1 "110.8", y1 "124.6", x2 "715.4", y2 "124.6" ] []
        , text_ [ transform "matrix(1 0 0 1 97.3838 495.0705)", class "z-score-white z-score-semibold st16" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 97.3838 471.4875)", class "z-score-white z-score-semibold st16" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 97.3838 447.9045)", class "z-score-white z-score-semibold st16" ] [ text "4" ]
        , text_ [ transform "matrix(1 0 0 1 97.3838 424.3215)", class "z-score-white z-score-semibold st16" ] [ text "5" ]
        , text_ [ transform "matrix(1 0 0 1 97.3838 400.7385)", class "z-score-white z-score-semibold st16" ] [ text "6" ]
        , text_ [ transform "matrix(1 0 0 1 97.3838 377.1555)", class "z-score-white z-score-semibold st16" ] [ text "7" ]
        , text_ [ transform "matrix(1 0 0 1 97.3838 353.5724)", class "z-score-white z-score-semibold st16" ] [ text "8" ]
        , text_ [ transform "matrix(1 0 0 1 97.3838 329.9904)", class "z-score-white z-score-semibold st16" ] [ text "9" ]
        , text_ [ transform "matrix(1 0 0 1 95.3042 306.4064)", class "z-score-white z-score-semibold st16" ] [ text "10" ]
        , text_ [ transform "matrix(1 0 0 1 95.3042 282.8239)", class "z-score-white z-score-semibold st16" ] [ text "11" ]
        , text_ [ transform "matrix(1 0 0 1 95.3042 259.2409)", class "z-score-white z-score-semibold st16" ] [ text "12" ]
        , text_ [ transform "matrix(1 0 0 1 95.3042 235.6584)", class "z-score-white z-score-semibold st16" ] [ text "13" ]
        , text_ [ transform "matrix(1 0 0 1 95.3042 212.0754)", class "z-score-white z-score-semibold st16" ] [ text "14" ]
        , text_ [ transform "matrix(1 0 0 1 95.3042 188.4924)", class "z-score-white z-score-semibold st16" ] [ text "15" ]
        , text_ [ transform "matrix(1 0 0 1 95.3042 164.9103)", class "z-score-white z-score-semibold st16" ] [ text "16" ]
        , text_ [ transform "matrix(1 0 0 1 95.3042 141.3303)", class "z-score-white z-score-semibold st16" ] [ text "17" ]
        , text_ [ transform "matrix(1 0 0 1 746.6816 495.0705)", class "z-score-white z-score-semibold st16" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 746.6816 471.4875)", class "z-score-white z-score-semibold st16" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 746.6816 447.9045)", class "z-score-white z-score-semibold st16" ] [ text "4" ]
        , text_ [ transform "matrix(1 0 0 1 746.6816 424.3215)", class "z-score-white z-score-semibold st16" ] [ text "5" ]
        , text_ [ transform "matrix(1 0 0 1 746.6816 400.7385)", class "z-score-white z-score-semibold st16" ] [ text "6" ]
        , text_ [ transform "matrix(1 0 0 1 746.6816 377.1555)", class "z-score-white z-score-semibold st16" ] [ text "7" ]
        , text_ [ transform "matrix(1 0 0 1 746.6816 353.5724)", class "z-score-white z-score-semibold st16" ] [ text "8" ]
        , text_ [ transform "matrix(1 0 0 1 746.6816 329.9904)", class "z-score-white z-score-semibold st16" ] [ text "9" ]
        , text_ [ transform "matrix(1 0 0 1 744.6016 306.4064)", class "z-score-white z-score-semibold st16" ] [ text "10" ]
        , text_ [ transform "matrix(1 0 0 1 744.6016 282.8239)", class "z-score-white z-score-semibold st16" ] [ text "11" ]
        , text_ [ transform "matrix(1 0 0 1 744.6016 259.2409)", class "z-score-white z-score-semibold st16" ] [ text "12" ]
        , text_ [ transform "matrix(1 0 0 1 744.6016 235.6584)", class "z-score-white z-score-semibold st16" ] [ text "13" ]
        , text_ [ transform "matrix(1 0 0 1 744.6016 212.0754)", class "z-score-white z-score-semibold st16" ] [ text "14" ]
        , text_ [ transform "matrix(1 0 0 1 744.6016 188.4924)", class "z-score-white z-score-semibold st16" ] [ text "15" ]
        , text_ [ transform "matrix(1 0 0 1 744.6016 164.9103)", class "z-score-white z-score-semibold st16" ] [ text "16" ]
        , text_ [ transform "matrix(1 0 0 1 744.6016 141.3303)", class "z-score-white z-score-semibold st16" ] [ text "17" ]
        ]


referenceLinesWeightForHeight : Svg any
referenceLinesWeightForHeight =
    g []
        [ line [ class "st19", x1 "110.8", y1 "498.6", x2 "715.4", y2 "498.6" ] []
        , line [ class "st19", x1 "110.8", y1 "482.5", x2 "715.4", y2 "482.5" ] []
        , line [ class "st18", x1 "110.8", y1 "474.5", x2 "715.4", y2 "474.5" ] []
        , line [ class "st19", x1 "110.8", y1 "466.4", x2 "715.4", y2 "466.4" ] []
        , line [ class "st19", x1 "110.8", y1 "450.3", x2 "715.4", y2 "450.3" ] []
        , line [ class "st18", x1 "110.8", y1 "442.2", x2 "715.4", y2 "442.2" ] []
        , line [ class "st19", x1 "110.8", y1 "434.2", x2 "715.4", y2 "434.2" ] []
        , line [ class "st19", x1 "110.8", y1 "418.1", x2 "715.4", y2 "418.1" ] []
        , line [ class "st18", x1 "110.8", y1 "410", x2 "715.4", y2 "410" ] []
        , line [ class "st19", x1 "110.8", y1 "401.9", x2 "715.4", y2 "401.9" ] []
        , line [ class "st19", x1 "110.8", y1 "385.8", x2 "715.4", y2 "385.8" ] []
        , line [ class "st18", x1 "110.8", y1 "377.8", x2 "715.4", y2 "377.8" ] []
        , line [ class "st19", x1 "110.8", y1 "369.7", x2 "715.4", y2 "369.7" ] []
        , line [ class "st19", x1 "110.8", y1 "353.6", x2 "715.4", y2 "353.6" ] []
        , line [ class "st18", x1 "110.8", y1 "345.5", x2 "715.4", y2 "345.5" ] []
        , line [ class "st19", x1 "110.8", y1 "337.5", x2 "715.4", y2 "337.5" ] []
        , line [ class "st19", x1 "110.8", y1 "321.4", x2 "715.4", y2 "321.4" ] []
        , line [ class "st18", x1 "110.8", y1 "313.3", x2 "715.4", y2 "313.3" ] []
        , line [ class "st19", x1 "110.8", y1 "305.2", x2 "715.4", y2 "305.2" ] []
        , line [ class "st19", x1 "110.8", y1 "289.1", x2 "715.4", y2 "289.1" ] []
        , line [ class "st18", x1 "110.8", y1 "281.1", x2 "715.4", y2 "281.1" ] []
        , line [ class "st19", x1 "110.8", y1 "273", x2 "715.4", y2 "273" ] []
        , line [ class "st19", x1 "110.8", y1 "256.9", x2 "715.4", y2 "256.9" ] []
        , line [ class "st18", x1 "110.8", y1 "248.8", x2 "715.4", y2 "248.8" ] []
        , line [ class "st19", x1 "110.8", y1 "240.8", x2 "715.4", y2 "240.8" ] []
        , line [ class "st19", x1 "110.8", y1 "224.7", x2 "715.4", y2 "224.7" ] []
        , line [ class "st18", x1 "110.8", y1 "216.6", x2 "715.4", y2 "216.6" ] []
        , line [ class "st19", x1 "110.8", y1 "208.6", x2 "715.4", y2 "208.6" ] []
        , line [ class "st19", x1 "110.8", y1 "192.4", x2 "715.4", y2 "192.4" ] []
        , line [ class "st18", x1 "110.8", y1 "184.4", x2 "715.4", y2 "184.4" ] []
        , line [ class "st19", x1 "110.8", y1 "176.3", x2 "715.4", y2 "176.3" ] []
        , line [ class "st19", x1 "110.8", y1 "160.2", x2 "715.4", y2 "160.2" ] []
        , line [ class "st18", x1 "110.8", y1 "152.2", x2 "715.4", y2 "152.2" ] []
        , line [ class "st19", x1 "110.8", y1 "144.1", x2 "715.4", y2 "144.1" ] []
        , line [ class "st18", x1 "110.8", y1 "490.6", x2 "737.6", y2 "490.6" ] []
        , line [ class "st18", x1 "110.8", y1 "458.3", x2 "737.6", y2 "458.3" ] []
        , line [ class "st18", x1 "110.8", y1 "426.1", x2 "737.6", y2 "426.1" ] []
        , line [ class "st18", x1 "110.8", y1 "393.9", x2 "737.6", y2 "393.9" ] []
        , line [ class "st18", x1 "110.8", y1 "361.7", x2 "737.6", y2 "361.7" ] []
        , line [ class "st18", x1 "110.8", y1 "329.4", x2 "737.6", y2 "329.4" ] []
        , line [ class "st18", x1 "110.8", y1 "297.2", x2 "737.6", y2 "297.2" ] []
        , line [ class "st18", x1 "110.8", y1 "265", x2 "737.6", y2 "265" ] []
        , line [ class "st18", x1 "110.8", y1 "232.7", x2 "737.6", y2 "232.7" ] []
        , line [ class "st18", x1 "110.8", y1 "200.5", x2 "737.6", y2 "200.5" ] []
        , line [ class "st18", x1 "110.8", y1 "168.3", x2 "737.6", y2 "168.3" ] []
        , line [ class "st18", x1 "110.8", y1 "136", x2 "737.6", y2 "136" ] []
        , line [ class "st19", x1 "110.8", y1 "128", x2 "715.4", y2 "128" ] []
        , text_ [ transform "matrix(1 0 0 1 98.0068 493.0851)", class "z-score-white z-score-semibold st16" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 98.0068 460.8557)", class "z-score-white z-score-semibold st16" ] [ text "4" ]
        , text_ [ transform "matrix(1 0 0 1 98.0068 428.6252)", class "z-score-white z-score-semibold st16" ] [ text "6" ]
        , text_ [ transform "matrix(1 0 0 1 98.0068 396.3947)", class "z-score-white z-score-semibold st16" ] [ text "8" ]
        , text_ [ transform "matrix(1 0 0 1 95.9273 364.1642)", class "z-score-white z-score-semibold st16" ] [ text "10" ]
        , text_ [ transform "matrix(1 0 0 1 95.9273 331.9348)", class "z-score-white z-score-semibold st16" ] [ text "12" ]
        , text_ [ transform "matrix(1 0 0 1 95.9273 299.7043)", class "z-score-white z-score-semibold st16" ] [ text "14" ]
        , text_ [ transform "matrix(1 0 0 1 95.9273 267.4748)", class "z-score-white z-score-semibold st16" ] [ text "16" ]
        , text_ [ transform "matrix(1 0 0 1 95.9273 235.2448)", class "z-score-white z-score-semibold st16" ] [ text "18" ]
        , text_ [ transform "matrix(1 0 0 1 95.9273 203.0143)", class "z-score-white z-score-semibold st16" ] [ text "20" ]
        , text_ [ transform "matrix(1 0 0 1 95.9273 170.7853)", class "z-score-white z-score-semibold st16" ] [ text "22" ]
        , text_ [ transform "matrix(1 0 0 1 95.9273 138.5568)", class "z-score-white z-score-semibold st16" ] [ text "24" ]
        , text_ [ transform "matrix(1 0 0 1 746.7832 493.0851)", class "z-score-white z-score-semibold st16" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 746.7832 460.8557)", class "z-score-white z-score-semibold st16" ] [ text "4" ]
        , text_ [ transform "matrix(1 0 0 1 746.7832 428.6252)", class "z-score-white z-score-semibold st16" ] [ text "6" ]
        , text_ [ transform "matrix(1 0 0 1 746.7832 396.3947)", class "z-score-white z-score-semibold st16" ] [ text "8" ]
        , text_ [ transform "matrix(1 0 0 1 744.7031 364.1642)", class "z-score-white z-score-semibold st16" ] [ text "10" ]
        , text_ [ transform "matrix(1 0 0 1 744.7031 331.9348)", class "z-score-white z-score-semibold st16" ] [ text "12" ]
        , text_ [ transform "matrix(1 0 0 1 744.7031 299.7043)", class "z-score-white z-score-semibold st16" ] [ text "14" ]
        , text_ [ transform "matrix(1 0 0 1 744.7031 267.4748)", class "z-score-white z-score-semibold st16" ] [ text "16" ]
        , text_ [ transform "matrix(1 0 0 1 744.7031 235.2448)", class "z-score-white z-score-semibold st16" ] [ text "18" ]
        , text_ [ transform "matrix(1 0 0 1 744.7031 203.0143)", class "z-score-white z-score-semibold st16" ] [ text "20" ]
        , text_ [ transform "matrix(1 0 0 1 744.7031 170.7853)", class "z-score-white z-score-semibold st16" ] [ text "22" ]
        , text_ [ transform "matrix(1 0 0 1 744.7031 138.5568)", class "z-score-white z-score-semibold st16" ] [ text "24" ]
        ]


type alias LabelConfig =
    { title : ChartPhrase
    , subtitle : ChartPhrase
    , xAxis1 : Maybe ChartPhrase
    , xAxis2 : ChartPhrase
    , yAxis : ChartPhrase
    }


heightForAgeBoysLabels : LabelConfig
heightForAgeBoysLabels =
    { title = Translate.LengthForAgeBoys
    , subtitle = Translate.BirthToTwoYears
    , xAxis1 = Just Translate.Months
    , xAxis2 = Translate.AgeCompletedMonthsYears
    , yAxis = Translate.LengthCm
    }


heightForAgeBoysLabels5To19 : LabelConfig
heightForAgeBoysLabels5To19 =
    { title = Translate.HeightForAgeBoys
    , subtitle = Translate.TwoToNineteenYears
    , xAxis1 = Just Translate.Months
    , xAxis2 = Translate.AgeCompletedMonthsYears
    , yAxis = Translate.HeightCm
    }


weightForAgeBoysLabels : LabelConfig
weightForAgeBoysLabels =
    { title = Translate.WeightForAgeBoys
    , subtitle = Translate.BirthToTwoYears
    , xAxis1 = Just Translate.Months
    , xAxis2 = Translate.AgeCompletedMonthsYears
    , yAxis = Translate.WeightKg
    }


weightForHeightBoysLabels : LabelConfig
weightForHeightBoysLabels =
    { title = Translate.WeightForLengthBoys
    , subtitle = Translate.BirthToTwoYears
    , xAxis1 = Nothing
    , xAxis2 = Translate.LengthCm
    , yAxis = Translate.WeightKg
    }


weightForHeightGirlsLabels : LabelConfig
weightForHeightGirlsLabels =
    { title = Translate.WeightForLengthGirls
    , subtitle = Translate.BirthToTwoYears
    , xAxis1 = Nothing
    , xAxis2 = Translate.LengthCm
    , yAxis = Translate.WeightKg
    }


weightForAgeGirlsLabels : LabelConfig
weightForAgeGirlsLabels =
    { title = Translate.WeightForAgeGirls
    , subtitle = Translate.BirthToTwoYears
    , xAxis1 = Just Translate.Months
    , xAxis2 = Translate.AgeCompletedMonthsYears
    , yAxis = Translate.WeightKg
    }


heightForAgeGirlsLabels : LabelConfig
heightForAgeGirlsLabels =
    { title = Translate.LengthForAgeGirls
    , subtitle = Translate.BirthToTwoYears
    , xAxis1 = Just Translate.Months
    , xAxis2 = Translate.AgeCompletedMonthsYears
    , yAxis = Translate.LengthCm
    }


labels : Language -> LabelConfig -> Svg any
labels language config =
    g []
        [ rect
            [ x "110.9"
            , y "119.9"
            , class "month-line"
            , width "626.8"
            , height "386.8"
            ]
            []
        , rect
            [ x "12.9"
            , y "72"
            , class "gender"
            , width "379.4"
            , height "1"
            ]
            []
        , text_
            [ transform "matrix(1 0 0 1 109.2567 62.4895)"
            , class "gender st12 st13"
            ]
            [ text <| translate language (ChartPhrase config.title) ]
        , text_
            [ transform "matrix(1 0 0 1 109.7767 86.491)"
            , class "gender z-score-semibold st15"
            ]
            [ text <| translate language (ChartPhrase config.subtitle) ]
        , text_
            [ transform "matrix(1 0 0 1 62.3622 513.5461)"
            , class "z-score-white z-score-semibold st16"
            ]
            [ config.xAxis1
                |> Maybe.map (translate language << ChartPhrase)
                |> Maybe.withDefault ""
                |> text
            ]
        , text_
            [ transform "matrix(1 0 0 1 325.0975 540.9924)"
            , class "z-score-white z-score-semibold st17"
            ]
            [ text <| translate language (ChartPhrase config.xAxis2) ]
        , text_
            [ transform "matrix(0 -1 1 0 80.8497 345.7814)"
            , class "z-score-white z-score-semibold st17"
            ]
            [ text <| translate language (ChartPhrase config.yAxis) ]
        ]


frame : Language -> String -> Svg any
frame language color =
    g
        []
        [ a
            [ xlinkHref "http://www.who.int/childgrowth/en/"
            , target "_blank"
            ]
            [ text_
                [ transform "matrix(1 0 0 1 500 566.1733)"
                , class "z-score-frame z-score-semibold z-score-font-sm"
                ]
                [ text <| translate language (Translate.ChartPhrase Translate.ZScoreChartsAvailableAt)
                , text " http://www.who.int/childgrowth/en/"
                ]
            ]
        , rect
            [ class color
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
