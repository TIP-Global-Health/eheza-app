module ZScore.View exposing
    ( AreaColor
    , Bounds
    , LabelConfig
    , PlotConfig
    , XAxisConfig
    , XAxisTypes
    , YAxisConfig
    , YAxisSpaceType
    , viewHeadCircumferenceForAge0To13WeeksBoys
    , viewHeadCircumferenceForAge0To13WeeksGirls
    , viewHeadCircumferenceForAge0To2Boys
    , viewHeadCircumferenceForAge0To2Girls
    , viewHeadCircumferenceForAge0To5Boys
    , viewHeadCircumferenceForAge0To5Girls
    , viewHeightForAgeBoys
    , viewHeightForAgeBoys0To5
    , viewHeightForAgeBoys5To19
    , viewHeightForAgeGirls
    , viewHeightForAgeGirls0To5
    , viewHeightForAgeGirls5To19
    , viewMarkers
    , viewWeightForAgeBoys
    , viewWeightForAgeBoys0To5
    , viewWeightForAgeBoys5To10
    , viewWeightForAgeGirls
    , viewWeightForAgeGirls0To5
    , viewWeightForAgeGirls5To10
    , viewWeightForHeight0To5Boys
    , viewWeightForHeight0To5Girls
    , viewWeightForHeightBoys
    , viewWeightForHeightGirls
    )

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

import Backend.Measurement.Model exposing (Gender(..))
import Float.Extra
import Gizra.Html exposing (emptyNode)
import Html exposing (Html)
import Maybe.Extra
import Pages.Report.Svg exposing (drawPoints)
import RemoteData
import Round
import String exposing (fromInt)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Translate exposing (ChartPhrase, Language, TranslationId(..), translate)
import Utils.AllDict as AllDict
import ZScore.Model exposing (Centimetres(..), ChartAgeRange(..), Days, Height, Kilograms(..), Length(..), Model, Months, ZScoreEntry)
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
    , neg2ToNeg3Color : AreaColor
    , abovePos3Color : AreaColor
    }


type alias Bounds =
    { minX : Float
    , maxX : Float
    , minY : Float
    , maxY : Float
    }


type alias XAxisConfig =
    { width : Float
    , minAgeUnit : Int
    , maxAgeUnit : Int
    , ageUnitBreakdown : List Int
    , innerLinesNumber : Int
    , minLength : Int
    , maxLength : Int
    , xAxisType : XAxisTypes
    }


type alias YAxisConfig =
    { yAxisIntervals : Int
    , innerLinesNumber : Int
    , spaceType : YAxisSpaceType
    , decimalPointsForText : Int
    }


type alias LabelConfig =
    { title : ChartPhrase
    , subtitle : ChartPhrase
    , xAxis1 : Maybe ChartPhrase
    , xAxis2 : ChartPhrase
    , yAxis : ChartPhrase
    }


type XAxisTypes
    = AgeWeeks
    | AgeYears
    | Height


type YAxisSpaceType
    = SpaceAround
    | SpaceBelow
    | NoSpace


type AreaColor
    = AreaGreen
    | AreaOrange
    | AreaRed


areaColorToClass : AreaColor -> String
areaColorToClass areaColor =
    case areaColor of
        AreaGreen ->
            "green"

        AreaOrange ->
            "orange"

        AreaRed ->
            "red"


heightForAgeConfig : PlotConfig Days Centimetres
heightForAgeConfig =
    { toFloatX = \(ZScore.Model.Days days) -> toFloat days
    , toFloatY = \(Centimetres cm) -> cm
    , input = { minY = 42, maxY = 99, minX = 0, maxX = 365 * 2 }
    , output = { minX = 110.9, maxX = 715.4, minY = 119.9, maxY = 506.7 }
    , drawSD1 = False
    , paintLevels = True
    , xAxis =
        { width = 908
        , minAgeUnit = 0
        , maxAgeUnit = 2
        , ageUnitBreakdown = List.range 1 11
        , innerLinesNumber = 0
        , minLength = 0
        , maxLength = 0
        , xAxisType = AgeYears
        }
    , yAxis =
        { yAxisIntervals = 5
        , innerLinesNumber = 4
        , spaceType = SpaceAround
        , decimalPointsForText = 0
        }
    , neg2ToNeg3Color = AreaOrange
    , abovePos3Color = AreaGreen
    }


heightForAgeConfig0To5 : PlotConfig Days Centimetres
heightForAgeConfig0To5 =
    { toFloatX = \(ZScore.Model.Days days) -> toFloat days
    , toFloatY = \(Centimetres cm) -> cm
    , input = { minY = 45, maxY = 125, minX = 0, maxX = 365 * 5 }
    , output = { minX = 111, maxX = 715.4, minY = 119.9, maxY = 506.7 }
    , drawSD1 = False
    , paintLevels = True
    , xAxis =
        { width = 725
        , minAgeUnit = 0
        , maxAgeUnit = 5
        , ageUnitBreakdown = [ 2, 4, 6, 8, 10 ]
        , innerLinesNumber = 0
        , minLength = 0
        , maxLength = 0
        , xAxisType = AgeYears
        }
    , yAxis =
        { yAxisIntervals = 5
        , innerLinesNumber = 4
        , spaceType = SpaceBelow
        , decimalPointsForText = 0
        }
    , neg2ToNeg3Color = AreaOrange
    , abovePos3Color = AreaGreen
    }


heightForAgeConfig5To19 : PlotConfig Months Centimetres
heightForAgeConfig5To19 =
    { toFloatX = \(ZScore.Model.Months months) -> toFloat months
    , toFloatY = \(Centimetres cm) -> cm
    , input = { minY = 90, maxY = 200, minX = 61, maxX = 228 }
    , output = { minX = 111, maxX = 715.4, minY = 119.9, maxY = 506.7 }
    , drawSD1 = True
    , paintLevels = True
    , xAxis =
        { width = 647
        , minAgeUnit = 5
        , maxAgeUnit = 19
        , ageUnitBreakdown = [ 3, 6, 9 ]
        , innerLinesNumber = 0
        , minLength = 0
        , maxLength = 0
        , xAxisType = AgeYears
        }
    , yAxis =
        { yAxisIntervals = 10
        , innerLinesNumber = 1
        , spaceType = NoSpace
        , decimalPointsForText = 0
        }
    , neg2ToNeg3Color = AreaOrange
    , abovePos3Color = AreaGreen
    }


weightForAgeConfig : PlotConfig Days Kilograms
weightForAgeConfig =
    { toFloatX = \(ZScore.Model.Days days) -> toFloat days
    , toFloatY = \(Kilograms kg) -> kg
    , input = { minY = 1.4, maxY = 17.8, minX = 0, maxX = 365 * 2 }
    , output = { minX = 110.9, maxX = 715.4, minY = 119.9, maxY = 506.7 }
    , drawSD1 = False
    , paintLevels = True
    , xAxis =
        { width = 908
        , minAgeUnit = 0
        , maxAgeUnit = 2
        , ageUnitBreakdown = List.range 1 11
        , innerLinesNumber = 0
        , minLength = 0
        , maxLength = 0
        , xAxisType = AgeYears
        }
    , yAxis =
        { yAxisIntervals = 1
        , innerLinesNumber = 4
        , spaceType = SpaceAround
        , decimalPointsForText = 2
        }
    , neg2ToNeg3Color = AreaOrange
    , abovePos3Color = AreaGreen
    }


weightForAge0To5Config : PlotConfig Days Kilograms
weightForAge0To5Config =
    { toFloatX = \(ZScore.Model.Days days) -> toFloat days
    , toFloatY = \(Kilograms kg) -> kg
    , input = { minY = 2, maxY = 30, minX = 0, maxX = 365 * 5 }
    , output = { minX = 110.9, maxX = 715.4, minY = 119.9, maxY = 506.7 }
    , drawSD1 = False
    , paintLevels = True
    , xAxis =
        { width = 725
        , minAgeUnit = 0
        , maxAgeUnit = 5
        , ageUnitBreakdown = [ 2, 4, 6, 8, 10 ]
        , innerLinesNumber = 1
        , minLength = 0
        , maxLength = 0
        , xAxisType = AgeYears
        }
    , yAxis =
        { yAxisIntervals = 1
        , innerLinesNumber = 4
        , spaceType = NoSpace
        , decimalPointsForText = 0
        }
    , neg2ToNeg3Color = AreaOrange
    , abovePos3Color = AreaGreen
    }


weightForAge5To10Config : PlotConfig Months Kilograms
weightForAge5To10Config =
    { toFloatX = \(ZScore.Model.Months months) -> toFloat months
    , toFloatY = \(Kilograms kg) -> kg
    , input = { minY = 10, maxY = 60, minX = 61, maxX = 120 }
    , output = { minX = 110.9, maxX = 715.4, minY = 119.9, maxY = 506.7 }
    , drawSD1 = True
    , paintLevels = True
    , xAxis =
        { width = 725
        , minAgeUnit = 5
        , maxAgeUnit = 10
        , ageUnitBreakdown = [ 3, 6, 9 ]
        , innerLinesNumber = 2
        , minLength = 0
        , maxLength = 0
        , xAxisType = AgeYears
        }
    , yAxis =
        { yAxisIntervals = 5
        , innerLinesNumber = 4
        , spaceType = NoSpace
        , decimalPointsForText = 0
        }
    , neg2ToNeg3Color = AreaOrange
    , abovePos3Color = AreaGreen
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
        , minAgeUnit = 0
        , maxAgeUnit = 0
        , ageUnitBreakdown = []
        , innerLinesNumber = 4
        , minLength = 45
        , maxLength = 110
        , xAxisType = Height
        }
    , yAxis =
        { yAxisIntervals = 2
        , innerLinesNumber = 1
        , spaceType = SpaceAround
        , decimalPointsForText = 0
        }
    , neg2ToNeg3Color = AreaOrange
    , abovePos3Color = AreaGreen
    }


weightForHeight0To5Config : PlotConfig Height Kilograms
weightForHeight0To5Config =
    { toFloatX = \(ZScore.Model.Height cm) -> cm
    , toFloatY = \(Kilograms kg) -> kg
    , input = { minY = 1, maxY = 32, minX = 45, maxX = 120 }
    , output = { minX = 110.9, maxX = 715.4, minY = 119.9, maxY = 506.7 }
    , drawSD1 = True
    , paintLevels = True
    , xAxis =
        { width = 645
        , minAgeUnit = 0
        , maxAgeUnit = 0
        , ageUnitBreakdown = []
        , innerLinesNumber = 4
        , minLength = 45
        , maxLength = 120
        , xAxisType = Height
        }
    , yAxis =
        { yAxisIntervals = 2
        , innerLinesNumber = 2
        , spaceType = SpaceAround
        , decimalPointsForText = 0
        }
    , neg2ToNeg3Color = AreaOrange
    , abovePos3Color = AreaGreen
    }


headCircumferenceForAge0To13WeeksConfig : PlotConfig Days Centimetres
headCircumferenceForAge0To13WeeksConfig =
    { toFloatX = \(ZScore.Model.Days days) -> toFloat days
    , toFloatY = \(Centimetres cm) -> cm
    , input = { minY = 27, maxY = 47, minX = 0, maxX = 7 * 13 }
    , output = { minX = 110.9, maxX = 715.4, minY = 119.9, maxY = 506.7 }
    , drawSD1 = True
    , paintLevels = True
    , xAxis =
        { width = 650
        , minAgeUnit = 0
        , maxAgeUnit = 13
        , ageUnitBreakdown = List.range 1 6
        , innerLinesNumber = 0
        , minLength = 0
        , maxLength = 0
        , xAxisType = AgeWeeks
        }
    , yAxis =
        { yAxisIntervals = 1
        , innerLinesNumber = 19
        , spaceType = SpaceAround
        , decimalPointsForText = 0
        }
    , neg2ToNeg3Color = AreaGreen
    , abovePos3Color = AreaRed
    }


headCircumferenceForAge0To2Config : PlotConfig Days Centimetres
headCircumferenceForAge0To2Config =
    { toFloatX = \(ZScore.Model.Days days) -> toFloat days
    , toFloatY = \(Centimetres cm) -> cm
    , input = { minY = 25, maxY = 55, minX = 0, maxX = 365 * 2 }
    , output = { minX = 110.9, maxX = 715.4, minY = 119.9, maxY = 506.7 }
    , drawSD1 = True
    , paintLevels = True
    , xAxis =
        { width = 908
        , minAgeUnit = 0
        , maxAgeUnit = 2
        , ageUnitBreakdown = List.range 1 11
        , innerLinesNumber = 0
        , minLength = 0
        , maxLength = 0
        , xAxisType = AgeYears
        }
    , yAxis =
        { yAxisIntervals = 1
        , innerLinesNumber = 29
        , spaceType = SpaceAround
        , decimalPointsForText = 0
        }
    , neg2ToNeg3Color = AreaGreen
    , abovePos3Color = AreaRed
    }


headCircumferenceForAge0To5Config : PlotConfig Days Centimetres
headCircumferenceForAge0To5Config =
    { toFloatX = \(ZScore.Model.Days days) -> toFloat days
    , toFloatY = \(Centimetres cm) -> cm
    , input = { minY = 25, maxY = 60, minX = 0, maxX = 365 * 5 }
    , output = { minX = 110.9, maxX = 715.4, minY = 119.9, maxY = 506.7 }
    , drawSD1 = True
    , paintLevels = True
    , xAxis =
        { width = 725
        , minAgeUnit = 0
        , maxAgeUnit = 5
        , ageUnitBreakdown = [ 2, 4, 6, 8, 10 ]
        , innerLinesNumber = 0
        , minLength = 0
        , maxLength = 0
        , xAxisType = AgeYears
        }
    , yAxis =
        { yAxisIntervals = 1
        , innerLinesNumber = 34
        , spaceType = SpaceAround
        , decimalPointsForText = 0
        }
    , neg2ToNeg3Color = AreaGreen
    , abovePos3Color = AreaRed
    }


heightForAgeLabels : Gender -> ChartAgeRange -> LabelConfig
heightForAgeLabels gender ageRange =
    let
        title =
            if ageRange == RangeBirthToTwoYears then
                Translate.LengthForAge gender

            else
                Translate.HeightForAge gender
    in
    { title = title
    , subtitle = Translate.ChartAgeRange ageRange
    , xAxis1 = Just Translate.Months
    , xAxis2 = Translate.AgeCompletedMonthsYears
    , yAxis = Translate.LengthCm
    }


weightForAgeLabels : Gender -> ChartAgeRange -> LabelConfig
weightForAgeLabels gender ageRange =
    { title = Translate.WeightForAge gender
    , subtitle = Translate.ChartAgeRange ageRange
    , xAxis1 = Just Translate.Months
    , xAxis2 = Translate.AgeCompletedMonthsYears
    , yAxis = Translate.WeightKg
    }


weightForHeightLabels : Gender -> ChartAgeRange -> LabelConfig
weightForHeightLabels gender ageRange =
    { title = Translate.WeightForLength gender
    , subtitle = Translate.ChartAgeRange ageRange
    , xAxis1 = Nothing
    , xAxis2 = Translate.LengthCm
    , yAxis = Translate.WeightKg
    }


headCircumferenceForAgeLabels : Gender -> ChartAgeRange -> LabelConfig
headCircumferenceForAgeLabels gender ageRange =
    let
        xAxis2 =
            if ageRange == RangeBirthToThirteenWeeks then
                Translate.AgeWeeks

            else
                Translate.AgeCompletedMonthsYears
    in
    { title = Translate.HeadCircumferenceForAge gender
    , subtitle = Translate.ChartAgeRange ageRange
    , xAxis1 = Nothing
    , xAxis2 = xAxis2
    , yAxis = Translate.HeadCircumferenceCm
    }


plotData : PlotConfig x y -> List { x : Float, y : Float } -> List String
plotData config data =
    plotPoints config data
        |> plotDataPoints


plotDataPoints : List ( Float, Float ) -> List String
plotDataPoints =
    List.map
        (\( x, y ) ->
            Round.round 1 x ++ "," ++ Round.round 1 y
        )


plotPoints : PlotConfig x y -> List { x : Float, y : Float } -> List ( Float, Float )
plotPoints config =
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
    List.map
        (\{ x, y } ->
            ( plotX x, plotY y )
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

        neg3points =
            getPoints -3

        neg2points =
            getPoints -2

        pos3points =
            getPoints 3

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
                                [ class <| "area below-neg-three " ++ areaColorToClass AreaRed
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
                                [ class <| "area neg-two-to-neg-three " ++ areaColorToClass config.neg2ToNeg3Color
                                , pointList
                                ]
                                []
                       )
                    |> Just

            else
                Nothing

        -- Points for a polygon from neg2 to pos3
        fillBetweenNegTwoAndPosThree =
            if config.paintLevels then
                neg2points
                    |> List.append (List.reverse pos3points)
                    |> plotData config
                    |> String.join " "
                    |> points
                    |> (\pointList ->
                            polygon
                                [ class <| "area neg-two-to-pos-three " ++ areaColorToClass AreaGreen
                                , pointList
                                ]
                                []
                       )
                    |> Just

            else
                Nothing

        -- Points for a polygon from pos3 to the top of the chart
        fillAbovePositiveThree =
            if config.paintLevels then
                [ { x = config.input.maxX
                  , y = config.input.maxY
                  }
                , { x = config.input.minX
                  , y = config.input.maxY
                  }
                ]
                    |> List.append pos3points
                    |> plotData config
                    |> String.join " "
                    |> points
                    |> (\pointList ->
                            polygon
                                [ class <| "area above-pos-three " ++ areaColorToClass config.abovePos3Color
                                , pointList
                                ]
                                []
                       )
                    |> Just

            else
                Nothing
    in
    [ fillBelowNegativeThree
    , fillBetweenNegTwoAndNegThree
    , fillBetweenNegTwoAndPosThree
    , fillAbovePositiveThree
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
    , Just <| makeLine pos3points "three-line-new"
    ]
        |> Maybe.Extra.values
        |> g []


plotChildData : PlotConfig x y -> List ( x, y ) -> List (Svg any)
plotChildData config data =
    let
        dataPoints =
            data
                |> List.map
                    (\( x, y ) ->
                        { x = config.toFloatX x
                        , y = config.toFloatY y
                        }
                    )
                |> List.sortBy .x
                |> plotPoints config

        pointsList =
            plotDataPoints dataPoints
                |> String.join " "
                |> points
    in
    polyline
        [ class "child-data"
        , pointsList
        ]
        []
        :: drawPoints "#06B9FF" dataPoints


viewHeightForAgeBoys : Language -> Model -> List ( Days, Centimetres ) -> Html any
viewHeightForAgeBoys language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (heightForAgeLabels Male RangeBirthToTwoYears)
        , yAxisLinesAndText heightForAgeConfig
        , xAxisLinesAndText heightForAgeConfig
        , zScoreLabelsHeightForAgeBoys
        , model.lengthHeightForAge
            |> RemoteData.map (.male >> .byDay >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData heightForAgeConfig
        ]
            ++ plotChildData heightForAgeConfig data


viewHeightForAgeBoys0To5 : Language -> Model -> List ( Days, Centimetres ) -> Html any
viewHeightForAgeBoys0To5 language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (heightForAgeLabels Male RangeBirthToFiveYears)
        , yAxisLinesAndText heightForAgeConfig0To5
        , xAxisLinesAndText heightForAgeConfig0To5
        , zScoreLabelsHeightForAgeBoys0To5
        , model.lengthHeightForAge
            |> RemoteData.map (.male >> .byDay >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData heightForAgeConfig0To5
        ]
            ++ plotChildData heightForAgeConfig0To5 data


viewHeightForAgeBoys5To19 : Language -> Model -> List ( Months, Centimetres ) -> Html any
viewHeightForAgeBoys5To19 language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (heightForAgeLabels Male RangeFiveToNineteenYears)
        , yAxisLinesAndText heightForAgeConfig5To19
        , xAxisLinesAndText heightForAgeConfig5To19
        , zScoreLabelsHeightForAgeBoys5To19
        , model.lengthHeightForAge
            |> RemoteData.map (.male >> .byMonth >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData heightForAgeConfig5To19
        ]
            ++ plotChildData heightForAgeConfig5To19 data


viewHeightForAgeGirls : Language -> Model -> List ( Days, Centimetres ) -> Html any
viewHeightForAgeGirls language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (heightForAgeLabels Female RangeBirthToTwoYears)
        , yAxisLinesAndText heightForAgeConfig
        , xAxisLinesAndText heightForAgeConfig
        , zScoreLabelsHeightForAgeGirls
        , model.lengthHeightForAge
            |> RemoteData.map (.female >> .byDay >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData heightForAgeConfig
        ]
            ++ plotChildData heightForAgeConfig data


viewHeightForAgeGirls0To5 : Language -> Model -> List ( Days, Centimetres ) -> Html any
viewHeightForAgeGirls0To5 language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (heightForAgeLabels Female RangeBirthToFiveYears)
        , yAxisLinesAndText heightForAgeConfig0To5
        , xAxisLinesAndText heightForAgeConfig0To5
        , zScoreLabelsHeightForAgeGirls0To5
        , model.lengthHeightForAge
            |> RemoteData.map (.female >> .byDay >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData heightForAgeConfig0To5
        ]
            ++ plotChildData heightForAgeConfig0To5 data


viewHeightForAgeGirls5To19 : Language -> Model -> List ( Months, Centimetres ) -> Html any
viewHeightForAgeGirls5To19 language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (heightForAgeLabels Female RangeFiveToNineteenYears)
        , yAxisLinesAndText heightForAgeConfig5To19
        , xAxisLinesAndText heightForAgeConfig5To19
        , zScoreLabelsHeightForAgeGirls5To19
        , model.lengthHeightForAge
            |> RemoteData.map (.female >> .byMonth >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData heightForAgeConfig5To19
        ]
            ++ plotChildData heightForAgeConfig5To19 data


viewWeightForAgeBoys : Language -> Model -> List ( Days, Kilograms ) -> Html any
viewWeightForAgeBoys language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (weightForAgeLabels Male RangeBirthToTwoYears)
        , yAxisLinesAndText weightForAgeConfig
        , xAxisLinesAndText weightForAgeConfig
        , zScoreLabelsWeightForAgeBoys
        , model.weightForAge
            |> RemoteData.map (.male >> .byDay >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData weightForAgeConfig
        ]
            ++ plotChildData weightForAgeConfig data


viewWeightForAgeBoys0To5 : Language -> Model -> List ( Days, Kilograms ) -> Html any
viewWeightForAgeBoys0To5 language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (weightForAgeLabels Male RangeBirthToFiveYears)
        , yAxisLinesAndText weightForAge0To5Config
        , xAxisLinesAndText weightForAge0To5Config
        , zScoreLabelsWeightForAge0To5Boys
        , model.weightForAge
            |> RemoteData.map (.male >> .byDay >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData weightForAge0To5Config
        ]
            ++ plotChildData weightForAge0To5Config data


viewWeightForAgeBoys5To10 : Language -> Model -> List ( Months, Kilograms ) -> Html any
viewWeightForAgeBoys5To10 language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (weightForAgeLabels Male RangeFiveToTenYears)
        , yAxisLinesAndText weightForAge5To10Config
        , xAxisLinesAndText weightForAge5To10Config
        , zScoreLabelsWeightForAge5To10Boys
        , model.weightForAge
            |> RemoteData.map (.male >> .byMonth >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData weightForAge5To10Config
        ]
            ++ plotChildData weightForAge5To10Config data


viewWeightForAgeGirls : Language -> Model -> List ( Days, Kilograms ) -> Html any
viewWeightForAgeGirls language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (weightForAgeLabels Female RangeBirthToTwoYears)
        , yAxisLinesAndText weightForAgeConfig
        , xAxisLinesAndText weightForAgeConfig
        , zScoreLabelsWeightForAgeGirls
        , model.weightForAge
            |> RemoteData.map (.female >> .byDay >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData weightForAgeConfig
        ]
            ++ plotChildData weightForAgeConfig data


viewWeightForAgeGirls0To5 : Language -> Model -> List ( Days, Kilograms ) -> Html any
viewWeightForAgeGirls0To5 language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (weightForAgeLabels Female RangeBirthToFiveYears)
        , yAxisLinesAndText weightForAge0To5Config
        , xAxisLinesAndText weightForAge0To5Config
        , zScoreLabelsWeightForAge0To5Girls
        , model.weightForAge
            |> RemoteData.map (.female >> .byDay >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData weightForAge0To5Config
        ]
            ++ plotChildData weightForAge0To5Config data


viewWeightForAgeGirls5To10 : Language -> Model -> List ( Months, Kilograms ) -> Html any
viewWeightForAgeGirls5To10 language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (weightForAgeLabels Female RangeFiveToTenYears)
        , yAxisLinesAndText weightForAge5To10Config
        , xAxisLinesAndText weightForAge5To10Config
        , zScoreLabelsWeightForAge5To10Girls
        , model.weightForAge
            |> RemoteData.map (.female >> .byMonth >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData weightForAge5To10Config
        ]
            ++ plotChildData weightForAge5To10Config data


viewWeightForHeightBoys : Language -> Model -> List ( Length, Kilograms ) -> Html any
viewWeightForHeightBoys language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (weightForHeightLabels Male RangeBirthToTwoYears)
        , yAxisLinesAndText weightForHeightConfig
        , xAxisLinesAndText weightForHeightConfig
        , zScoreLabelsWeightForHeightBoys
        , model.weightForLength
            |> RemoteData.map (.male >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData weightForHeightConfig
        ]
            ++ plotChildData weightForHeightConfig data


viewWeightForHeight0To5Boys : Language -> Model -> List ( Height, Kilograms ) -> Html any
viewWeightForHeight0To5Boys language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (weightForHeightLabels Male RangeBirthToFiveYears)
        , yAxisLinesAndText weightForHeight0To5Config
        , xAxisLinesAndText weightForHeight0To5Config
        , zScoreLabelsWeightForHeight0To5Boys
        , model.weightForHeight
            |> RemoteData.map (.male >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData weightForHeight0To5Config
        ]
            ++ plotChildData weightForHeight0To5Config data


viewWeightForHeightGirls : Language -> Model -> List ( Length, Kilograms ) -> Html any
viewWeightForHeightGirls language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (weightForHeightLabels Female RangeBirthToTwoYears)
        , yAxisLinesAndText weightForHeightConfig
        , xAxisLinesAndText weightForHeightConfig
        , zScoreLabelsWeightForHeightGirls
        , model.weightForLength
            |> RemoteData.map (.female >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData weightForHeightConfig
        ]
            ++ plotChildData weightForHeightConfig data


viewWeightForHeight0To5Girls : Language -> Model -> List ( Height, Kilograms ) -> Html any
viewWeightForHeight0To5Girls language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (weightForHeightLabels Female RangeBirthToFiveYears)
        , yAxisLinesAndText weightForHeight0To5Config
        , xAxisLinesAndText weightForHeight0To5Config
        , zScoreLabelsWeightForHeight0To5Girls
        , model.weightForHeight
            |> RemoteData.map (.female >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData weightForHeight0To5Config
        ]
            ++ plotChildData weightForHeight0To5Config data


viewHeadCircumferenceForAge0To13WeeksBoys : Language -> Model -> List ( Days, Centimetres ) -> Html any
viewHeadCircumferenceForAge0To13WeeksBoys language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (headCircumferenceForAgeLabels Male RangeBirthToThirteenWeeks)
        , yAxisLinesAndText headCircumferenceForAge0To13WeeksConfig
        , xAxisLinesAndText headCircumferenceForAge0To13WeeksConfig
        , zScoreLabelsHeadCircumferenceForAge0To13WeeksBoys
        , model.headCircumferenceForAge
            |> RemoteData.map (.male >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData headCircumferenceForAge0To13WeeksConfig
        ]
            ++ plotChildData headCircumferenceForAge0To13WeeksConfig data


viewHeadCircumferenceForAge0To2Boys : Language -> Model -> List ( Days, Centimetres ) -> Html any
viewHeadCircumferenceForAge0To2Boys language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (headCircumferenceForAgeLabels Male RangeBirthToTwoYears)
        , yAxisLinesAndText headCircumferenceForAge0To2Config
        , xAxisLinesAndText headCircumferenceForAge0To2Config
        , zScoreLabelsHeadCircumferenceForAge0To2Boys
        , model.headCircumferenceForAge
            |> RemoteData.map (.male >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData headCircumferenceForAge0To2Config
        ]
            ++ plotChildData headCircumferenceForAge0To2Config data


viewHeadCircumferenceForAge0To5Boys : Language -> Model -> List ( Days, Centimetres ) -> Html any
viewHeadCircumferenceForAge0To5Boys language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (headCircumferenceForAgeLabels Male RangeBirthToFiveYears)
        , yAxisLinesAndText headCircumferenceForAge0To5Config
        , xAxisLinesAndText headCircumferenceForAge0To5Config
        , zScoreLabelsHeadCircumferenceForAge0To5Boys
        , model.headCircumferenceForAge
            |> RemoteData.map (.male >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData headCircumferenceForAge0To5Config
        ]
            ++ plotChildData headCircumferenceForAge0To5Config data


viewHeadCircumferenceForAge0To13WeeksGirls : Language -> Model -> List ( Days, Centimetres ) -> Html any
viewHeadCircumferenceForAge0To13WeeksGirls language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (headCircumferenceForAgeLabels Female RangeBirthToThirteenWeeks)
        , yAxisLinesAndText headCircumferenceForAge0To13WeeksConfig
        , xAxisLinesAndText headCircumferenceForAge0To13WeeksConfig
        , zScoreLabelsHeadCircumferenceForAge0To13WeeksGirls
        , model.headCircumferenceForAge
            |> RemoteData.map (.female >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData headCircumferenceForAge0To13WeeksConfig
        ]
            ++ plotChildData headCircumferenceForAge0To13WeeksConfig data


viewHeadCircumferenceForAge0To2Girls : Language -> Model -> List ( Days, Centimetres ) -> Html any
viewHeadCircumferenceForAge0To2Girls language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (headCircumferenceForAgeLabels Female RangeBirthToTwoYears)
        , yAxisLinesAndText headCircumferenceForAge0To2Config
        , xAxisLinesAndText headCircumferenceForAge0To2Config
        , zScoreLabelsHeadCircumferenceForAge0To2Girls
        , model.headCircumferenceForAge
            |> RemoteData.map (.female >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData headCircumferenceForAge0To2Config
        ]
            ++ plotChildData headCircumferenceForAge0To2Config data


viewHeadCircumferenceForAge0To5Girls : Language -> Model -> List ( Days, Centimetres ) -> Html any
viewHeadCircumferenceForAge0To5Girls language model data =
    svg chartFrameAttributes <|
        [ frame "z-score-gray"
        , labels language (headCircumferenceForAgeLabels Female RangeBirthToFiveYears)
        , yAxisLinesAndText headCircumferenceForAge0To5Config
        , xAxisLinesAndText headCircumferenceForAge0To5Config
        , zScoreLabelsHeadCircumferenceForAge0To5Girls
        , model.headCircumferenceForAge
            |> RemoteData.map (.female >> AllDict.toList)
            |> RemoteData.withDefault []
            |> plotReferenceData headCircumferenceForAge0To5Config
        ]
            ++ plotChildData headCircumferenceForAge0To5Config data


chartFrameAttributes : List (Attribute any)
chartFrameAttributes =
    [ class "z-score"
    , x "0"
    , y "0"
    , viewBox "0 0 841.9 560"
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


zScoreLabelsWeightForHeight0To5Boys : Svg any
zScoreLabelsWeightForHeight0To5Boys =
    g []
        [ text_ [ transform "matrix(1 0 0 1 722.457 147.1022)", class "z-score-semibold st23" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 722.457 184.2829)", class "two-line z-score-semibold st23" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 722.457 214.9333)", class "one-line z-score-semibold st23" ] [ text "1" ]
        , text_ [ transform "matrix(1 0 0 1 722.457 242.8552)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        , text_ [ transform "matrix(1 0 0 1 720.498 268.0893)", class "one-line z-score-semibold st23" ] [ text "-1" ]
        , text_ [ transform "matrix(1 0 0 1 720.498 289.6576)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 720.498 308.9217)", class "z-score-semibold st23" ] [ text "-3" ]
        ]


zScoreLabelsWeightForHeight0To5Girls : Svg any
zScoreLabelsWeightForHeight0To5Girls =
    g []
        [ text_ [ transform "matrix(1 0 0 1 722.457 133.1022)", class "z-score-semibold st23" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 722.457 173.2829)", class "two-line z-score-semibold st23" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 722.457 207.9333)", class "one-line z-score-semibold st23" ] [ text "1" ]
        , text_ [ transform "matrix(1 0 0 1 722.457 238.5552)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        , text_ [ transform "matrix(1 0 0 1 720.498 263.3893)", class "one-line z-score-semibold st23" ] [ text "-1" ]
        , text_ [ transform "matrix(1 0 0 1 720.498 287.0576)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 720.498 307.5217)", class "z-score-semibold st23" ] [ text "-3" ]
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


zScoreLabelsHeightForAgeBoys0To5 : Svg any
zScoreLabelsHeightForAgeBoys0To5 =
    g
        []
        [ text_ [ transform "matrix(1 0 0 1 722.0057 131.1564)", class "z-score-semibold st23" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 722.0057 150.738)", class "two-line z-score-semibold st23" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 722.0057 196.241)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        , text_ [ transform "matrix(1 0 0 1 720.9237 240.7482)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 720.9237 263.3353)", class "z-score-semibold st23" ] [ text "-3" ]
        ]


zScoreLabelsHeightForAgeBoys5To19 : Svg any
zScoreLabelsHeightForAgeBoys5To19 =
    g
        []
        [ text_ [ transform "matrix(1 0 0 1 722.0057 130.1564)", class "z-score-semibold st23" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 722.0448 155.438)", class "two-line z-score-semibold st23" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 722.0448 180.738)", class "one-line z-score-semibold st23" ] [ text "1" ]
        , text_ [ transform "matrix(1 0 0 1 722.4686 205.841)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        , text_ [ transform "matrix(1 0 0 1 720.9237 230.6482)", class "one-line z-score-semibold st23" ] [ text "-1" ]
        , text_ [ transform "matrix(1 0 0 1 720.9237 256.2482)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 720.7001 282.6353)", class "z-score-semibold st23" ] [ text "-3" ]
        ]


zScoreLabelsHeightForAgeGirls0To5 : Svg any
zScoreLabelsHeightForAgeGirls0To5 =
    g
        []
        [ text_ [ transform "matrix(1 0 0 1 722.0057 132.1564)", class "z-score-semibold st23" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 722.0057 153.538)", class "two-line z-score-semibold st23" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 722.0057 198.241)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        , text_ [ transform "matrix(1 0 0 1 720.9237 244.2482)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 720.9237 268.1353)", class "z-score-semibold st23" ] [ text "-3" ]
        ]


zScoreLabelsHeightForAgeGirls5To19 : Svg any
zScoreLabelsHeightForAgeGirls5To19 =
    g
        []
        [ text_ [ transform "matrix(1 0 0 1 722.0057 185.1564)", class "z-score-semibold st23" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 722.0448 207.738)", class "two-line z-score-semibold st23" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 722.0448 229.738)", class "one-line z-score-semibold st23" ] [ text "1" ]
        , text_ [ transform "matrix(1 0 0 1 722.4686 253.241)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        , text_ [ transform "matrix(1 0 0 1 720.9237 275.8482)", class "one-line z-score-semibold st23" ] [ text "-1" ]
        , text_ [ transform "matrix(1 0 0 1 720.9237 298.8482)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 720.7001 322.0353)", class "z-score-semibold st23" ] [ text "-3" ]
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


zScoreLabelsWeightForAge0To5Boys : Svg any
zScoreLabelsWeightForAge0To5Boys =
    g []
        [ text_ [ transform "matrix(1 0 0 1 722.1398 153.3553)", class "z-score-semibold st23" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 722.1398 204.0234)", class "two-line z-score-semibold st23" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 722.1398 284.3636)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        , text_ [ transform "matrix(1 0 0 1 719.923 343.5098)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 719.923 366.3469)", class "z-score-semibold st23" ] [ text "-3" ]
        ]


zScoreLabelsWeightForAge5To10Boys : Svg any
zScoreLabelsWeightForAge5To10Boys =
    g []
        [ text_ [ transform "matrix(1 0 0 1 723.707 150.6711)", class "z-score-semibold st23" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 723.4619 238.8845)", class "two-line z-score-semibold st23" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 723.4619 299.8845)", class "one-line z-score-semibold st23" ] [ text "1" ]
        , text_ [ transform "matrix(1 0 0 1 723.498 346.1838)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        , text_ [ transform "matrix(1 0 0 1 720.9238 380.1916)", class "one-line z-score-semibold st23" ] [ text "-1" ]
        , text_ [ transform "matrix(1 0 0 1 720.9238 407.1916)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 720.7002 428.9709)", class "z-score-semibold st23" ] [ text "-3" ]
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
        [ text_ [ transform "matrix(1 0 0 1 720.9238 329.1916)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 720.7002 351.9709)", class "z-score-semibold st23" ] [ text "-3" ]
        , text_ [ transform "matrix(1 0 0 1 723.707 141.6711)", class "z-score-semibold st23" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 723.4619 190.8845)", class "two-line z-score-semibold st23" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 723.498 272.1838)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        ]


zScoreLabelsWeightForAge0To5Girls : Svg any
zScoreLabelsWeightForAge0To5Girls =
    g
        []
        [ text_ [ transform "matrix(1 0 0 1 723.707 130.6711)", class "z-score-semibold st23" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 723.707 194.8845)", class "two-line z-score-semibold st23" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 723.707 285.1838)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        , text_ [ transform "matrix(1 0 0 1 720.707 348.1916)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 720.707 370.9709)", class "z-score-semibold st23" ] [ text "-3" ]
        ]


zScoreLabelsWeightForAge5To10Girls : Svg any
zScoreLabelsWeightForAge5To10Girls =
    g
        []
        [ text_ [ transform "matrix(1 0 0 1 723.707 131.6711)", class "z-score-semibold st23" ] [ text "3" ]
        , text_ [ transform "matrix(1 0 0 1 723.4619 224.8845)", class "two-line z-score-semibold st23" ] [ text "2" ]
        , text_ [ transform "matrix(1 0 0 1 723.4619 290.8845)", class "one-line z-score-semibold st23" ] [ text "1" ]
        , text_ [ transform "matrix(1 0 0 1 723.498 340.1838)", class "zero-line z-score-semibold st23" ] [ text "0" ]
        , text_ [ transform "matrix(1 0 0 1 720.9238 378.1916)", class "one-line z-score-semibold st23" ] [ text "-1" ]
        , text_ [ transform "matrix(1 0 0 1 720.9238 406.1916)", class "two-line z-score-semibold st23" ] [ text "-2" ]
        , text_ [ transform "matrix(1 0 0 1 720.7002 429.9709)", class "z-score-semibold st23" ] [ text "-3" ]
        ]


zScoreLabelsHeadCircumferenceForAge0To13WeeksBoys : Svg any
zScoreLabelsHeadCircumferenceForAge0To13WeeksBoys =
    ZScoreValues 317.7 294.8 272 248.1 226.2 203.4 180.5
        |> zScoreValuesLabel


zScoreLabelsHeadCircumferenceForAge0To2Boys : Svg any
zScoreLabelsHeadCircumferenceForAge0To2Boys =
    ZScoreValues 263.1 245.5 228 210.4 192.9 175.3 157.8
        |> zScoreValuesLabel


zScoreLabelsHeadCircumferenceForAge0To5Boys : Svg any
zScoreLabelsHeadCircumferenceForAge0To5Boys =
    ZScoreValues 275.3 258.8 242.3 225.8 209.3 192.8 176.2
        |> zScoreValuesLabel


zScoreLabelsHeadCircumferenceForAge0To13WeeksGirls : Svg any
zScoreLabelsHeadCircumferenceForAge0To13WeeksGirls =
    ZScoreValues 340 316 292 268 244 220 196
        |> zScoreValuesLabel


zScoreLabelsHeadCircumferenceForAge0To2Girls : Svg any
zScoreLabelsHeadCircumferenceForAge0To2Girls =
    ZScoreValues 278.2 260.2 242.2 224.2 206.9 188.2 170.2
        |> zScoreValuesLabel


zScoreLabelsHeadCircumferenceForAge0To5Girls : Svg any
zScoreLabelsHeadCircumferenceForAge0To5Girls =
    ZScoreValues 282 266.2 250.5 234.8 219.1 203.3 187.6
        |> zScoreValuesLabel


type alias ZScoreValues =
    { neg3 : Float
    , neg2 : Float
    , neg1 : Float
    , zero : Float
    , pos1 : Float
    , pos2 : Float
    , pos3 : Float
    }


zScoreValuesLabel : ZScoreValues -> Svg any
zScoreValuesLabel values =
    let
        viewText xPosition yPosition zscore extraClass =
            let
                class_ =
                    if String.isEmpty extraClass then
                        "z-score-semibold st23"

                    else
                        extraClass ++ " z-score-semibold st23"
            in
            text_
                [ transform <| "matrix(1 0 0 1 " ++ String.fromFloat yPosition ++ " " ++ String.fromFloat xPosition ++ ")"
                , class class_
                ]
                [ text <| String.fromInt zscore ]
    in
    g []
        [ viewText values.pos3 723.5 3 ""
        , viewText values.pos2 723.5 2 "two-line"
        , viewText values.pos1 723.5 1 "one-line"
        , viewText values.zero 723.5 0 "zero-line"
        , viewText values.neg1 721.5 -1 "one-line"
        , viewText values.neg2 721.5 -2 "two-line"
        , viewText values.neg3 721.5 -3 ""
        ]


xAxisLinesAndText : PlotConfig x y -> Svg any
xAxisLinesAndText config =
    let
        ( xAxisList, breakdownLines, breakdownText ) =
            case config.xAxis.xAxisType of
                AgeWeeks ->
                    ( List.range config.xAxis.minAgeUnit config.xAxis.maxAgeUnit, True, False )

                AgeYears ->
                    ( List.range config.xAxis.minAgeUnit config.xAxis.maxAgeUnit, True, True )

                Height ->
                    ( List.range config.xAxis.minLength config.xAxis.maxLength
                        -- We want the list to contain only the successive numbers of 5.
                        |> List.filter
                            (\length ->
                                remainderBy 5 length == 0
                            )
                    , False
                    , False
                    )

        listCount =
            List.length xAxisList

        spaceBetweenLines =
            config.xAxis.width / toFloat listCount

        spaceBetweenInnerLines =
            if breakdownLines then
                -- We add one here because we want to give space before the next unit.
                spaceBetweenLines / toFloat (List.length config.xAxis.ageUnitBreakdown + 1)

            else
                spaceBetweenLines / toFloat (config.xAxis.innerLinesNumber + 1)

        -- Here we can define the lines as we want.
        lines =
            List.indexedMap
                (\i unit ->
                    let
                        index =
                            toFloat i

                        linesMargin =
                            if index == 0 then
                                config.output.minX

                            else
                                config.output.minX + (spaceBetweenLines * index)

                        lineTextPosition =
                            if unit < 10 then
                                (linesMargin - 2)
                                    |> Round.round 4

                            else
                                (linesMargin - 5)
                                    |> Round.round 4

                        linePosition =
                            linesMargin
                                |> Round.round 4

                        innerLinesAndText =
                            if breakdownLines then
                                List.indexedMap
                                    (\ii unitBreakdown ->
                                        if unit < config.xAxis.maxAgeUnit then
                                            let
                                                innerIndex =
                                                    toFloat ii

                                                innerMargin =
                                                    linesMargin + (spaceBetweenInnerLines * (innerIndex + 1))

                                                innerLinePosition =
                                                    Round.round 4 innerMargin

                                                breakdownText_ =
                                                    if breakdownText then
                                                        let
                                                            innerTextPosition =
                                                                if unitBreakdown < 10 then
                                                                    (innerMargin - 2)
                                                                        |> Round.round 4

                                                                else
                                                                    (innerMargin - 5)
                                                                        |> Round.round 4
                                                        in
                                                        [ text_ [ transform <| "matrix(1 0 0 1 " ++ innerTextPosition ++ " 516.5436)", class "z-score-white z-score-semibold st16" ] [ text <| fromInt unitBreakdown ]
                                                        ]

                                                    else
                                                        []
                                            in
                                            line [ class "unit-breakdown-line", x1 innerLinePosition, y1 "506.5", x2 innerLinePosition, y2 "119.5" ] [] :: breakdownText_

                                        else
                                            []
                                    )
                                    config.xAxis.ageUnitBreakdown
                                    |> List.concat

                            else if config.xAxis.innerLinesNumber > 0 then
                                List.range 1 config.xAxis.innerLinesNumber
                                    |> List.concatMap
                                        (\innerLine ->
                                            if unit < config.xAxis.maxLength then
                                                let
                                                    innerIndex =
                                                        toFloat innerLine

                                                    innerLinePosition =
                                                        linesMargin
                                                            + (spaceBetweenInnerLines * innerIndex)
                                                            |> Round.round 4
                                                in
                                                [ line [ class "unit-breakdown-line", x1 innerLinePosition, y1 "506.5", x2 innerLinePosition, y2 "119.5" ] [] ]

                                            else
                                                []
                                        )

                            else
                                []
                    in
                    [ line [ class "unit-line", x1 linePosition, y1 "514.5", x2 linePosition, y2 "119.5" ] []
                    , text_
                        [ transform <| "matrix(1 0 0 1 " ++ lineTextPosition ++ " 525.9767)"
                        , class "z-score-white z-score-semibold st20"
                        ]
                        [ text <| fromInt unit ]
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
                        (yInput == config.input.minY) || (yInput == config.input.maxY) || (remainderBy config.yAxis.yAxisIntervals (round yInput) == 0)
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
                                |> Round.round 2

                        linePosition =
                            linesMargin
                                |> Round.round 4

                        innerLines =
                            if lineText /= config.input.maxY then
                                List.range 1 config.yAxis.innerLinesNumber
                                    |> List.map
                                        (\innerLine ->
                                            let
                                                innerIndex =
                                                    toFloat innerLine

                                                innerLinePosition =
                                                    linesMargin
                                                        - (spaceBetweenInnerLines * innerIndex)
                                                        |> Round.round 4
                                            in
                                            line [ class "st19", x1 "110.8", y1 innerLinePosition, x2 "715.4", y2 innerLinePosition ] []
                                        )

                            else
                                []

                        leftTextVerticalPosition =
                            if config.yAxis.decimalPointsForText > 0 then
                                "88.4252"

                            else
                                "95.4252"

                        ( beginningText, endText ) =
                            ( text_ [ transform <| "matrix(1 0 0 1 " ++ leftTextVerticalPosition ++ " " ++ lineTextPosition ++ ")", class "z-score-white z-score-semibold st16" ] [ text <| Round.round config.yAxis.decimalPointsForText lineText ]
                            , text_ [ transform <| "matrix(1 0 0 1 745.0155 " ++ lineTextPosition ++ ")", class "z-score-white z-score-semibold st16" ] [ text <| Round.round config.yAxis.decimalPointsForText lineText ]
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
            , class "unit-breakdown-line"
            , width "626.8"
            , height "386.8"
            ]
            []
        , text_
            [ transform "matrix(1 0 0 1 60 72.5)"
            , class "gender"
            ]
            [ text <| translate language (ChartPhrase config.title) ++ " | " ++ translate language (ChartPhrase config.subtitle) ]
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


frame : String -> Svg any
frame color =
    g
        []
        [ rect
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
