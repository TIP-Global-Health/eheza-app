module Pages.Dashboard.GraphUtils exposing
    ( barChartHeight
    , barChartWidth
    , column
    , familyPlanningSignToColor
    , familyPlanningSignsColors
    , feverCauseToColor
    , feverCausesColors
    , gridXScale
    , gridYScale
    , padding
    , pieChartHeight
    , pieChartWidth
    , radius
    , xAxis
    , xGridLine
    , xScale
    , yAxis
    , yGridLine
    )

import AssocList as Dict exposing (Dict)
import Axis
import Backend.Dashboard.Model exposing (Nutrition)
import Backend.Measurement.Model exposing (FamilyPlanningSign(..))
import Color exposing (Color)
import Pages.Dashboard.Model exposing (FeverCause(..), allFeverCauses)
import Scale exposing (BandScale, ContinuousScale, defaultBandConfig)
import Time exposing (Month)
import TypedSvg exposing (g, line, rect)
import TypedSvg.Attributes as Explicit
import TypedSvg.Attributes.InPx exposing (height, rx, ry, strokeWidth, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg, attribute)
import TypedSvg.Types exposing (percent)


column : BandScale Month -> Float -> ( Month, Nutrition ) -> Svg msg
column scale yScaleMax ( date, nutrition ) =
    g []
        [ g [ Explicit.class [ "column moderate" ] ]
            [ rect
                [ x <| Scale.convert scale date + (Scale.bandwidth scale / 4)
                , y <| Scale.convert (yScale yScaleMax) (toFloat nutrition.moderateNutrition)
                , rx 2
                , ry 2
                , height <| barChartHeight - Scale.convert (yScale yScaleMax) (toFloat nutrition.moderateNutrition) - 2 * padding
                ]
                []
            ]
        , g [ Explicit.class [ "column severe" ] ]
            [ rect
                [ x <| Scale.convert scale date + (Scale.bandwidth scale / 2)
                , y <| Scale.convert (yScale yScaleMax) (toFloat nutrition.severeNutrition)
                , rx 2
                , ry 2
                , height <| barChartHeight - Scale.convert (yScale yScaleMax) (toFloat nutrition.severeNutrition) - 2 * padding
                ]
                []
            ]
        ]


xGridLine : Int -> Float -> Svg msg
xGridLine index tick =
    line
        [ y1 30
        , Explicit.y2 (percent 95)
        , x1 (Scale.convert gridXScale tick)
        , x2 (Scale.convert gridXScale tick)
        , strokeWidth (Basics.max (toFloat (modBy 2 index)) 0.5)
        , attribute "stroke-dasharray" "5,5"
        ]
        []


yGridLine : Int -> Float -> Svg msg
yGridLine index tick =
    line
        [ x1 30
        , Explicit.x2 (percent 100)
        , y1 (Scale.convert gridYScale tick)
        , y2 (Scale.convert gridYScale tick)
        , strokeWidth (Basics.max (toFloat (modBy 2 index)) 0.5)
        , attribute "stroke-dasharray" "5,5"
        ]
        []


padding : Float
padding =
    30


gridXScale : ContinuousScale Float
gridXScale =
    Scale.linear ( padding, barChartWidth - padding ) ( 0, 2 )


gridYScale : ContinuousScale Float
gridYScale =
    Scale.linear ( barChartHeight - padding, padding ) ( 0, 1 )


xScale : List ( Month, Nutrition ) -> BandScale Month
xScale data =
    List.map Tuple.first data
        |> Scale.band { defaultBandConfig | paddingInner = 0.1, paddingOuter = 0.2 } ( 0, barChartWidth - 2 * padding )


yScale : Float -> ContinuousScale Float
yScale yScaleMax =
    Scale.linear ( barChartHeight - 2 * padding, 0 ) ( 0, yScaleMax )


xAxis : List ( Month, Nutrition ) -> Svg msg
xAxis data =
    Axis.bottom [] (Scale.toRenderable Debug.toString (xScale data))


yAxis : Float -> Svg msg
yAxis yScaleMax =
    Axis.left [ Axis.tickCount 5 ] <| yScale yScaleMax


barChartWidth : Float
barChartWidth =
    940


barChartHeight : Float
barChartHeight =
    400


pieChartWidth : Float
pieChartWidth =
    1000


pieChartHeight : Float
pieChartHeight =
    1000


familyPlanningSignToColor : FamilyPlanningSign -> Color
familyPlanningSignToColor sign =
    case sign of
        AutoObservation ->
            Color.rgb (6 / 255) (185 / 255) (255 / 255)

        Condoms ->
            Color.rgb (255 / 255) (209 / 255) (102 / 255)

        CycleBeads ->
            Color.rgb (247 / 255) (147 / 255) (30 / 255)

        CycleCounting ->
            Color.rgb (165 / 255) (131 / 255) (255 / 255)

        Hysterectomy ->
            Color.rgb (247 / 255) (170 / 255) (198 / 255)

        Implants ->
            Color.rgb (2 / 255) (52 / 255) (54 / 255)

        Injectables ->
            Color.rgb (3 / 255) (181 / 255) (170 / 255)

        NoFamilyPlanning ->
            Color.rgb (204 / 255) (204 / 255) (204 / 255)

        IUD ->
            Color.rgb (2 / 255) (110 / 255) (164 / 255)

        LactationAmenorrhea ->
            Color.rgb (255 / 255) (182 / 255) (13 / 255)

        OralContraceptives ->
            Color.rgb (210 / 255) (193 / 255) (255 / 255)

        Spermicide ->
            Color.rgb (55 / 255) 0 (196 / 255)

        TubalLigatures ->
            Color.rgb (239 / 255) (86 / 255) (142 / 255)

        Vasectomy ->
            Color.rgb (3 / 255) (121 / 255) (113 / 255)


familyPlanningSignsColors : Dict FamilyPlanningSign Color
familyPlanningSignsColors =
    [ AutoObservation
    , Condoms
    , CycleBeads
    , CycleCounting
    , Hysterectomy
    , Implants
    , Injectables
    , NoFamilyPlanning
    , IUD
    , LactationAmenorrhea
    , OralContraceptives
    , Spermicide
    , TubalLigatures
    , Vasectomy
    ]
        |> List.map (\sign -> ( sign, familyPlanningSignToColor sign ))
        |> Dict.fromList


feverCauseToColor : FeverCause -> Color
feverCauseToColor cause =
    case cause of
        FeverCauseCovid19 ->
            Color.rgb (27 / 255) (207 / 255) (193 / 255)

        FeverCauseMalaria ->
            Color.rgb (253 / 255) (240 / 255) (124 / 255)

        FeverCauseRespiratory ->
            Color.rgb (240 / 255) (111 / 255) (107 / 255)

        FeverCauseGI ->
            Color.rgb (238 / 255) (87 / 255) (142 / 255)

        FeverCauseUnknown ->
            Color.rgb (157 / 255) (121 / 255) (188 / 255)


feverCausesColors : Dict FeverCause Color
feverCausesColors =
    allFeverCauses
        |> List.map (\cause -> ( cause, feverCauseToColor cause ))
        |> Dict.fromList


radius : Float
radius =
    min (pieChartWidth / 2) pieChartHeight / 2 - 10
