module ZScore.View exposing (Bounds, LabelConfig, PlotConfig, frame, heightForAgeBoysLabels, heightForAgeConfig, heightForAgeGirlsLabels, labels, plotChildData, plotData, plotReferenceData, viewHeightForAgeBoys, viewHeightForAgeBoys5To19, viewHeightForAgeGirls, viewHeightForAgeGirls2To5, viewHeightForAgeGirls5To19, viewMarkers, viewWeightForAgeBoys, viewWeightForAgeGirls, viewWeightForHeightBoys, viewWeightForHeightGirls, weightForAgeBoysLabels, weightForAgeConfig, weightForAgeGirlsLabels, weightForHeightBoysLabels, weightForHeightConfig, weightForHeightGirlsLabels, zScoreLabelsHeightForAgeBoys, zScoreLabelsHeightForAgeGirls, zScoreLabelsWeightForAgeBoys, zScoreLabelsWeightForAgeGirls, zScoreLabelsWeightForHeightBoys, zScoreLabelsWeightForHeightGirls)

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
import Float.Extra
import Gizra.Html exposing (emptyNode)
import Html exposing (Html)
import RemoteData
import Round
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Translate exposing (ChartPhrase(..), Language, TranslationId(..), translate)
import Utils.AllDict as AllDict exposing (AllDict)
import Utils.NominalDate exposing (Days(..), Months)
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
        , yAxisLinesAndText heightForAgeConfig
        , xAxisLinesAndText heightForAgeConfig
        , zScoreLabelsHeightForAgeBoys
        , model.lengthHeightForAge
            |> RemoteData.map (.male >> .byDay >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData heightForAgeConfig
        , plotChildData heightForAgeConfig data
        ]


viewHeightForAgeGirls2To5 : Language -> Model -> List ( Days, Centimetres ) -> Html any
viewHeightForAgeGirls2To5 language model data =
    svg
        [ class "z-score girls"
        , x "0px"
        , y "0px"
        , viewBox "0 0 841.9 595.3"
        ]
        [ frame language "z-score-grey"
        , labels language heightForAgeGirlsLabels2To5
        , yAxisLinesAndText heightForAgeConfig2To5
        , xAxisLinesAndText heightForAgeConfig2To5
        , zScoreLabelsHeightForAgeBoys5To19
        , model.lengthHeightForAge
            |> RemoteData.map (.female >> .byDay >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData heightForAgeConfig2To5
        , plotChildData heightForAgeConfig2To5 data
        ]


viewHeightForAgeBoys5To19 : Language -> Model -> List ( Months, Centimetres ) -> Html any
viewHeightForAgeBoys5To19 language model data =
    svg
        [ class "z-score boys"
        , x "0px"
        , y "0px"
        , viewBox "0 0 841.9 595.3"
        ]
        [ frame language "z-score-grey"
        , labels language heightForAgeBoysLabels5To19
        , yAxisLinesAndText heightForAgeConfig5To19
        , xAxisLinesAndText heightForAgeConfig5To19
        , zScoreLabelsHeightForAgeBoys5To19
        , model.lengthHeightForAge
            |> RemoteData.map (.male >> .byMonth >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData heightForAgeConfig5To19
        , plotChildData heightForAgeConfig5To19 data
        ]


viewHeightForAgeGirls5To19 : Language -> Model -> List ( Months, Centimetres ) -> Html any
viewHeightForAgeGirls5To19 language model data =
    svg
        [ class "z-score girls"
        , x "0px"
        , y "0px"
        , viewBox "0 0 841.9 595.3"
        ]
        [ frame language "z-score-grey"
        , labels language heightForAgeGirlsLabels5To19
        , yAxisLinesAndText heightForAgeConfig5To19
        , xAxisLinesAndText heightForAgeConfig5To19
        , zScoreLabelsHeightForAgeBoys5To19
        , model.lengthHeightForAge
            |> RemoteData.map (.female >> .byMonth >> AllDict.toList)
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
    , yAxis : YAxisConfig
    }


type alias Bounds =
    { minX : Float
    , maxX : Float
    , minY : Float
    , maxY : Float
    }


type alias XAxisConfig =
    { width : Float
    , minYear : Int
    , maxYear : Int
    , monthsList : List Int
    , innerLinesNumber : Int
    , minLength : Int
    , maxLength : Int
    , xAxisType : XAxisTypes
    }


type alias YAxisConfig =
    { yAxisIntervals : Int
    , innerLinesNumber : Int
    , spaceType : YAxisSpaceType
    }


type alias LabelConfig =
    { title : ChartPhrase
    , subtitle : ChartPhrase
    , xAxis1 : Maybe ChartPhrase
    , xAxis2 : ChartPhrase
    , yAxis : ChartPhrase
    }


type XAxisTypes
    = Age
    | Height


type YAxisSpaceType
    = SpaceAround
    | SpaceAbove
    | SpaceBelow
    | NoSpace


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
        , innerLinesNumber = 0
        , minLength = 0
        , maxLength = 0
        , xAxisType = Age
        }
    , yAxis =
        { yAxisIntervals = 5
        , innerLinesNumber = 4
        , spaceType = SpaceAround
        }
    }


heightForAgeConfig2To5 : PlotConfig Days Centimetres
heightForAgeConfig2To5 =
    { toFloatX = \(Utils.NominalDate.Days days) -> toFloat days
    , toFloatY = \(Centimetres cm) -> cm
    , input = { minY = 76, maxY = 125, minX = 365 * 2, maxX = 365 * 5 }
    , output = { minX = 111, maxX = 715.4, minY = 119.9, maxY = 506.7 }
    , drawSD1 = False
    , paintLevels = True
    , xAxis =
        { width = 806
        , minYear = 2
        , maxYear = 5
        , monthsList = [ 2, 4, 6, 8, 10 ]
        , innerLinesNumber = 0
        , minLength = 0
        , maxLength = 0
        , xAxisType = Age
        }
    , yAxis =
        { yAxisIntervals = 5
        , innerLinesNumber = 4
        , spaceType = SpaceBelow
        }
    }


heightForAgeConfig5To19 : PlotConfig Months Centimetres
heightForAgeConfig5To19 =
    { toFloatX = \(Utils.NominalDate.Months months) -> toFloat months
    , toFloatY = \(Centimetres cm) -> cm
    , input = { minY = 90, maxY = 200, minX = 61, maxX = 228 }
    , output = { minX = 111, maxX = 715.4, minY = 119.9, maxY = 506.7 }
    , drawSD1 = True
    , paintLevels = True
    , xAxis =
        { width = 647
        , minYear = 5
        , maxYear = 19
        , monthsList = [ 3, 6, 9 ]
        , innerLinesNumber = 0
        , minLength = 0
        , maxLength = 0
        , xAxisType = Age
        }
    , yAxis =
        { yAxisIntervals = 10
        , innerLinesNumber = 1
        , spaceType = NoSpace
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
        , innerLinesNumber = 0
        , minLength = 0
        , maxLength = 0
        , xAxisType = Age
        }
    , yAxis =
        { yAxisIntervals = 1
        , innerLinesNumber = 4
        , spaceType = SpaceAround
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
        { width = 650
        , minYear = 0
        , maxYear = 0
        , monthsList = []
        , innerLinesNumber = 4
        , minLength = 45
        , maxLength = 110
        , xAxisType = Height
        }
    , yAxis =
        { yAxisIntervals = 2
        , innerLinesNumber = 1
        , spaceType = SpaceAround
        }
    }


heightForAgeBoysLabels : LabelConfig
heightForAgeBoysLabels =
    { title = Translate.LengthForAgeBoys
    , subtitle = Translate.BirthToTwoYears
    , xAxis1 = Just Translate.Months
    , xAxis2 = Translate.AgeCompletedMonthsYears
    , yAxis = Translate.LengthCm
    }


heightForAgeBoysLabels2To5 : LabelConfig
heightForAgeBoysLabels2To5 =
    { title = Translate.HeightForAgeBoys
    , subtitle = Translate.TwoToFiveYears
    , xAxis1 = Just Translate.Months
    , xAxis2 = Translate.AgeCompletedMonthsYears
    , yAxis = Translate.HeightCm
    }


heightForAgeGirlsLabels2To5 : LabelConfig
heightForAgeGirlsLabels2To5 =
    { title = Translate.HeightForAgeGirls
    , subtitle = Translate.TwoToFiveYears
    , xAxis1 = Just Translate.Months
    , xAxis2 = Translate.AgeCompletedMonthsYears
    , yAxis = Translate.HeightCm
    }


heightForAgeBoysLabels5To19 : LabelConfig
heightForAgeBoysLabels5To19 =
    { title = Translate.HeightForAgeBoys
    , subtitle = Translate.FiveToNineteenYears
    , xAxis1 = Just Translate.Months
    , xAxis2 = Translate.AgeCompletedMonthsYears
    , yAxis = Translate.HeightCm
    }


heightForAgeGirlsLabels5To19 : LabelConfig
heightForAgeGirlsLabels5To19 =
    { title = Translate.HeightForAgeGirls
    , subtitle = Translate.FiveToNineteenYears
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
        , yAxisLinesAndText weightForAgeConfig
        , xAxisLinesAndText weightForAgeConfig
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
        , yAxisLinesAndText weightForHeightConfig
        , xAxisLinesAndText weightForHeightConfig
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
        , yAxisLinesAndText weightForHeightConfig
        , xAxisLinesAndText weightForHeightConfig
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
        , yAxisLinesAndText heightForAgeConfig
        , xAxisLinesAndText heightForAgeConfig
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
        , yAxisLinesAndText weightForAgeConfig
        , xAxisLinesAndText weightForAgeConfig
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


xAxisLinesAndText : PlotConfig x y -> Svg any
xAxisLinesAndText config =
    let
        ( xAxisList, monthList ) =
            case config.xAxis.xAxisType of
                Age ->
                    ( List.range config.xAxis.minYear config.xAxis.maxYear, True )

                Height ->
                    ( List.range config.xAxis.minLength config.xAxis.maxLength
                        -- We want the list to contain only the successive numbers of 5.
                        |> List.filter
                            (\length ->
                                if remainderBy 5 length == 0 then
                                    True

                                else
                                    False
                            )
                    , False
                    )

        listCount =
            List.length xAxisList

        spaceBetweenLines =
            config.xAxis.width / toFloat listCount

        spaceBetweenInnerLines =
            if monthList then
                -- We add one here because we want to give space before the next year.
                spaceBetweenLines / toFloat (List.length config.xAxis.monthsList + 1)

            else
                spaceBetweenLines / toFloat (config.xAxis.innerLinesNumber + 1)

        -- Here we can define the lines as we want.
        lines =
            List.indexedMap
                (\i year ->
                    let
                        index =
                            toFloat i

                        linesMargin =
                            if index == 0 then
                                config.output.minX

                            else
                                config.output.minX + (spaceBetweenLines * index)

                        lineTextPosition =
                            if year < 10 then
                                (linesMargin - 2)
                                    |> toString

                            else
                                (linesMargin - 5)
                                    |> toString

                        linePosition =
                            toString linesMargin

                        innerLinesAndText =
                            if monthList then
                                List.indexedMap
                                    (\ii month ->
                                        let
                                            innerIndex =
                                                toFloat ii

                                            innerMargin =
                                                linesMargin + (spaceBetweenInnerLines * (innerIndex + 1))

                                            innerTextPosition =
                                                if month < 10 then
                                                    (innerMargin - 2)
                                                        |> toString

                                                else
                                                    (innerMargin - 5)
                                                        |> toString

                                            innerLinePosition =
                                                toString innerMargin
                                        in
                                        if year < config.xAxis.maxYear then
                                            [ line [ class "month-line", x1 innerLinePosition, y1 "506.5", x2 innerLinePosition, y2 "119.5" ] []
                                            , text_ [ transform <| "matrix(1 0 0 1 " ++ innerTextPosition ++ " 516.5436)", class "z-score-white z-score-semibold st16" ] [ text <| toString month ]
                                            ]

                                        else
                                            []
                                    )
                                    config.xAxis.monthsList
                                    |> List.concat

                            else if config.xAxis.innerLinesNumber > 0 then
                                List.range 1 config.xAxis.innerLinesNumber
                                    |> List.map
                                        (\innerLine ->
                                            let
                                                innerIndex =
                                                    toFloat innerLine

                                                innerLinePosition =
                                                    toString <| linesMargin + (spaceBetweenInnerLines * innerIndex)
                                            in
                                            if year < config.xAxis.maxLength then
                                                [ line [ class "month-line", x1 innerLinePosition, y1 "506.5", x2 innerLinePosition, y2 "119.5" ] [] ]

                                            else
                                                []
                                        )
                                    |> List.concat

                            else
                                []
                    in
                    [ line [ class "year-line", x1 linePosition, y1 "514.5", x2 linePosition, y2 "119.5" ] []
                    , text_
                        [ transform <| "matrix(1 0 0 1 " ++ lineTextPosition ++ " 525.9767)"
                        , class "z-score-white z-score-semibold st20"
                        ]
                        [ text <| toString year ]
                    ]
                        |> List.append innerLinesAndText
                )
                xAxisList
                |> List.concat
    in
    g []
        lines


yAxisLinesAndText : PlotConfig x y -> Svg any
yAxisLinesAndText config =
    let
        yAxisList =
            Float.Extra.range { start = config.input.minY, end = config.input.maxY, steps = round <| config.input.maxY - config.input.minY }
                -- We only display the successive numbers of the intervals defined in the config.
                |> List.filter
                    (\yInput ->
                        if (yInput == config.input.minY) || (yInput == config.input.maxY) || (remainderBy config.yAxis.yAxisIntervals (round yInput) == 0) then
                            True

                        else
                            False
                    )

        height =
            config.output.maxY - config.output.minY

        listCount =
            List.length yAxisList

        spaceBetweenLines =
            height / toFloat (listCount - 1)

        spaceBetweenInnerLines =
            spaceBetweenLines / (toFloat config.yAxis.innerLinesNumber + 1)

        -- Here we can define the lines as we want.
        lines =
            List.indexedMap
                (\i lineText ->
                    let
                        index =
                            toFloat i

                        linesMargin =
                            if index == 0 then
                                config.output.maxY

                            else
                                config.output.maxY - (spaceBetweenLines * index)

                        lineTextPosition =
                            (linesMargin + 2)
                                |> toString

                        linePosition =
                            toString linesMargin

                        innerLines =
                            if lineText /= config.input.maxY then
                                List.range 1 config.yAxis.innerLinesNumber
                                    |> List.map
                                        (\innerLine ->
                                            let
                                                innerIndex =
                                                    toFloat innerLine

                                                innerLinePosition =
                                                    toString <| linesMargin - (spaceBetweenInnerLines * innerIndex)
                                            in
                                            [ line [ class "st19", x1 "110.8", y1 innerLinePosition, x2 "715.4", y2 innerLinePosition ] [] ]
                                        )
                                    |> List.concat

                            else
                                []

                        ( beginningText, endText ) =
                            ( text_ [ transform <| "matrix(1 0 0 1 95.4252 " ++ lineTextPosition ++ ")", class "z-score-white z-score-semibold st16" ] [ text <| toString lineText ]
                            , text_ [ transform <| "matrix(1 0 0 1 745.0155 " ++ lineTextPosition ++ ")", class "z-score-white z-score-semibold st16" ] [ text <| toString lineText ]
                            )

                        ( beginningTextConditional, endTextConditional ) =
                            -- There're multiple types of showing the text for each graph, in some of them, we don't
                            -- need to show the text on the first line, some on the last and some we don't want to show
                            -- on either side.
                            case config.yAxis.spaceType of
                                SpaceAround ->
                                    if lineText == config.input.minY || lineText == config.input.maxY then
                                        ( emptyNode, emptyNode )

                                    else
                                        ( beginningText, endText )

                                SpaceBelow ->
                                    if lineText == config.input.minY then
                                        ( emptyNode, emptyNode )

                                    else
                                        ( beginningText, endText )

                                SpaceAbove ->
                                    if lineText == config.input.maxY then
                                        ( emptyNode, emptyNode )

                                    else
                                        ( beginningText, endText )

                                NoSpace ->
                                    ( beginningText, endText )
                    in
                    [ line [ class "st18", x1 "110.8", y1 linePosition, x2 "737.6", y2 linePosition ] []
                    , beginningTextConditional
                    , endTextConditional
                    ]
                        |> List.append innerLines
                )
                yAxisList
                |> List.concat
    in
    g []
        lines


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
