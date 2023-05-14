module Pages.Scoreboard.View exposing (view)

import App.Types exposing (Language)
import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (fromEntityId, toEntityId)
import Backend.Model exposing (ModelBackend)
import Backend.Scoreboard.Model exposing (ScoreboardData, SelectedEntity(..))
import Date
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate, diffMonths)
import Html exposing (..)
import Html.Attributes exposing (..)
import Icons
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Scoreboard.Model exposing (..)
import Pages.Scoreboard.Utils exposing (..)
import Pages.Utils exposing (viewYearSelector)
import Time exposing (Month(..))
import Translate exposing (TranslationId, translate)
import Utils.GeoLocation exposing (GeoLocationId, filterGeoLocationDictByParent, geoInfo, geoLocationDictToOptions)


view : Language -> NominalDate -> ModelBackend -> Model -> Html Msg
view language currentDate modelBackend model =
    case modelBackend.scoreboardData of
        Just (Ok data) ->
            viewScoreboardData language currentDate data model

        Just (Err err) ->
            text <| Debug.toString err

        Nothing ->
            emptyNode


viewScoreboardData : Language -> NominalDate -> ScoreboardData -> Model -> Html Msg
viewScoreboardData language currentDate data model =
    let
        topBar =
            div [ class "top-bar" ]
                [ div [ class "new-selection" ]
                    [ a [ href "/admin/reports/aggregated-ncda" ]
                        [ button []
                            [ text <| translate language Translate.NewSelection ]
                        ]
                    ]
                , viewYearSelector language currentDate model.yearSelectorGap ChaneYearGap
                , div [ class "values-percents" ] []
                ]

        monthsGap =
            generateMonthsGap currentDate model.yearSelectorGap
    in
    div [ class "page-content" ]
        [ topBar
        , viewAggregatedChildScoreboardPane language data
        , viewDemographicsPane language currentDate model.yearSelectorGap monthsGap data
        , viewAcuteMalnutritionPane language currentDate model.yearSelectorGap data
        , viewStuntingPane language currentDate model.yearSelectorGap monthsGap data
        , viewANCNewbornPane language currentDate model.yearSelectorGap data
        , viewUniversalInterventionPane language currentDate model.yearSelectorGap data
        , viewNutritionBehaviorPane language currentDate model.yearSelectorGap data
        , viewTargetedInterventionsPane language currentDate model.yearSelectorGap data
        , viewInfrastructureEnvironmentWashPane language currentDate model.yearSelectorGap data
        ]


viewAggregatedChildScoreboardPane : Language -> ScoreboardData -> Html any
viewAggregatedChildScoreboardPane language data =
    div [ class "pane" ]
        [ viewPaneHeading language Translate.AggregatedChildScoreboard
        , div [ class "pane-content" ]
            [ div []
                [ span [ class "selected-entity" ] [ text <| (translate language <| Translate.SelectedEntity data.entityType) ++ ":" ]
                , span [] [ text data.entityName ]
                ]
            ]
        ]


viewDemographicsPane : Language -> NominalDate -> Int -> Dict Int Int -> ScoreboardData -> Html any
viewDemographicsPane language currentDate yearSelectorGap monthsGap data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDADemographicsItemLabel item) itemValues
                )
                [ ChildrenUnder2, NewbornsThisMonth, LowBirthWeigh ]
                values

        valuesByRow =
            List.foldl
                (\record accum ->
                    let
                        ageInMonths =
                            diffMonths record.birthDate currentDate
                    in
                    List.indexedMap
                        (\index accumValue ->
                            Dict.get index monthsGap
                                |> Maybe.map
                                    (\gapInMoths ->
                                        let
                                            gap =
                                                ageInMonths - gapInMoths

                                            row1 =
                                                if gap >= 0 && gap < 24 then
                                                    accumValue.row1 + 1

                                                else
                                                    accumValue.row1

                                            row2 =
                                                if gap == 0 then
                                                    accumValue.row2 + 1

                                                else
                                                    accumValue.row2

                                            row3 =
                                                if gap == 0 && record.lowBirthWeight == Just True then
                                                    accumValue.row3 + 1

                                                else
                                                    accumValue.row3
                                        in
                                        { row1 = row1
                                        , row2 = row2
                                        , row3 = row3
                                        }
                                    )
                                |> Maybe.withDefault accumValue
                        )
                        accum
                )
                emptyValues
                data.records

        emptyValues =
            List.repeat 12 { row1 = 0, row2 = 0, row3 = 0 }

        values =
            [ List.map .row1 valuesByRow
            , List.map .row2 valuesByRow
            , List.map .row3 valuesByRow
            ]
    in
    div [ class "pane cyan" ]
        [ viewPaneHeading language Translate.Demographics
        , div [ class "pane-content" ] <|
            viewTableHeader language
                :: rows
        ]


viewAcuteMalnutritionPane : Language -> NominalDate -> Int -> ScoreboardData -> Html any
viewAcuteMalnutritionPane language currentDate yearSelectorGap data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDAAcuteMalnutritionItemLabel item) itemValues
                )
                [ SevereAcuteMalnutrition, ModerateAcuteMalnutrition, GoodNutrition ]
                values

        values =
            []
    in
    div [ class "pane orange" ]
        [ viewPaneHeading language Translate.AcuteMalnutrition
        , div [ class "pane-content" ] <|
            viewTableHeader language
                :: rows
        ]


viewStuntingPane : Language -> NominalDate -> Int -> Dict Int Int -> ScoreboardData -> Html any
viewStuntingPane language currentDate yearSelectorGap monthsGap data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDAStuntingItemLabel item) itemValues
                )
                [ SevereStunting, ModerateStunting, NoStunting ]
                values

        valuesByRow =
            List.foldl
                (\record accum ->
                    let
                        severeAsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.stuntingSevere

                        moderateAsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.stuntingModerate

                        normalAsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.stuntingNormal
                    in
                    List.indexedMap
                        (\index accumValue ->
                            Dict.get index monthsGap
                                |> Maybe.map
                                    (\gapInMoths ->
                                        let
                                            row1 =
                                                if List.member gapInMoths severeAsAgeInMonths then
                                                    accumValue.row1 + 1

                                                else
                                                    accumValue.row1

                                            row2 =
                                                if List.member gapInMoths moderateAsAgeInMonths then
                                                    accumValue.row2 + 1

                                                else
                                                    accumValue.row2

                                            row3 =
                                                if List.member gapInMoths normalAsAgeInMonths then
                                                    accumValue.row3 + 1

                                                else
                                                    accumValue.row3
                                        in
                                        { row1 = row1
                                        , row2 = row2
                                        , row3 = row3
                                        }
                                    )
                                |> Maybe.withDefault accumValue
                        )
                        accum
                )
                emptyValues
                data.records

        emptyValues =
            List.repeat 12 { row1 = 0, row2 = 0, row3 = 0 }

        values =
            [ List.map .row1 valuesByRow
            , List.map .row2 valuesByRow
            , List.map .row3 valuesByRow
            ]
    in
    div [ class "pane velvet" ]
        [ viewPaneHeading language Translate.Stunting
        , div [ class "pane-content" ] <|
            viewTableHeader language
                :: rows
        ]


viewANCNewbornPane : Language -> NominalDate -> Int -> ScoreboardData -> Html any
viewANCNewbornPane language currentDate yearSelectorGap data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDAANCNewbornItemLabel item) itemValues
                )
                [ RegularCheckups, IronDuringPregnancy ]
                values

        values =
            case data.entityType of
                EntityVillage ->
                    [ [ 10, 16, 13, 12, 18, 11, 14, 19, 17, 20, 15, 12 ]
                    , [ 10, 16, 13, 12, 18, 11, 14, 19, 17, 20, 15, 12 ]
                    ]

                EntityCell ->
                    [ [ 105, 138, 115, 131, 122, 126, 131, 146, 133, 147, 128, 105 ]
                    , [ 105, 138, 115, 131, 122, 126, 131, 146, 133, 147, 128, 105 ]
                    ]

                EntitySector ->
                    [ [ 259, 240, 212, 230, 265, 227, 211, 258, 215, 231, 274, 241 ]
                    , [ 259, 240, 212, 230, 265, 227, 211, 258, 215, 231, 274, 241 ]
                    ]

                EntityDistrict ->
                    [ [ 583, 557, 643, 619, 612, 632, 592, 640, 608, 562, 620, 569 ]
                    , [ 583, 557, 643, 619, 612, 632, 592, 640, 608, 562, 620, 569 ]
                    ]
    in
    div [ class "pane cyan" ]
        [ viewPaneHeading language Translate.ANCNewborn
        , div [ class "pane-content" ] <|
            viewTableHeader language
                :: rows
        ]


viewUniversalInterventionPane : Language -> NominalDate -> Int -> ScoreboardData -> Html any
viewUniversalInterventionPane language currentDate yearSelectorGap data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDAUniversalInterventionItemLabel item) itemValues
                )
                [ Immunization, VitaminA, OngeraMNP, OngeraMNP, ECDServices ]
                values

        values =
            case data.entityType of
                EntityVillage ->
                    [ [ 13, 14, 11, 17, 9, 8, 14, 16, 12, 10, 15, 12 ]
                    , [ 6, 10, 17, 13, 14, 12, 9, 8, 11, 15, 16, 12 ]
                    , [ 14, 12, 16, 17, 10, 9, 13, 7, 8, 11, 19, 11 ]
                    , [ 7, 12, 15, 10, 16, 8, 15, 17, 13, 14, 19, 11 ]
                    , [ 12, 15, 8, 11, 11, 9, 17, 16, 15, 12, 19, 11 ]
                    ]

                EntityCell ->
                    [ [ 89, 98, 76, 81, 105, 92, 113, 127, 140, 115, 92, 104 ]
                    , [ 102, 85, 121, 96, 133, 107, 127, 104, 141, 110, 88, 129 ]
                    , [ 129, 118, 144, 96, 142, 139, 112, 131, 147, 137, 135, 123 ]
                    , [ 136, 142, 98, 131, 143, 101, 131, 119, 144, 99, 109, 126 ]
                    , [ 117, 121, 124, 133, 104, 141, 109, 128, 101, 137, 122, 99 ]
                    ]

                EntitySector ->
                    [ [ 261, 217, 205, 238, 205, 281, 276, 220, 250, 299, 283, 252 ]
                    , [ 222, 206, 223, 196, 279, 261, 257, 216, 249, 233, 269, 248 ]
                    , [ 241, 267, 278, 217, 211, 251, 272, 229, 240, 221, 208, 220 ]
                    , [ 211, 248, 230, 235, 222, 240, 216, 212, 227, 262, 255, 225 ]
                    , [ 209, 224, 215, 247, 273, 263, 258, 214, 249, 236, 275, 249 ]
                    ]

                EntityDistrict ->
                    [ [ 597, 567, 620, 564, 485, 545, 663, 536, 498, 603, 665, 496 ]
                    , [ 547, 599, 581, 474, 505, 489, 655, 593, 605, 539, 629, 508 ]
                    , [ 632, 506, 551, 580, 647, 562, 508, 475, 659, 502, 642, 657 ]
                    , [ 608, 572, 519, 648, 655, 599, 586, 547, 596, 522, 618, 484 ]
                    , [ 567, 514, 601, 658, 557, 528, 505, 506, 540, 554, 529, 510 ]
                    ]
    in
    div [ class "pane orange" ]
        [ viewPaneHeading language Translate.UniversalIntervention
        , div [ class "pane-content" ] <|
            viewTableHeader language
                :: rows
        ]


viewNutritionBehaviorPane : Language -> NominalDate -> Int -> ScoreboardData -> Html any
viewNutritionBehaviorPane language currentDate yearSelectorGap data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDANutritionBehaviorItemLabel item) itemValues
                )
                [ BreastfedSixMonths, AppropriateComplementaryFeeding, DiverseDiet, MealsADay ]
                values

        values =
            case data.entityType of
                EntityVillage ->
                    [ [ 7, 17, 15, 19, 8, 13, 14, 11, 12, 11, 15, 12 ]
                    , [ 8, 14, 12, 16, 18, 9, 10, 7, 13, 14, 12, 11 ]
                    , [ 17, 18, 15, 13, 10, 7, 16, 9, 11, 17, 16, 11 ]
                    , [ 16, 10, 11, 18, 14, 13, 12, 17, 9, 16, 15, 12 ]
                    ]

                EntityCell ->
                    [ [ 104, 106, 120, 120, 102, 137, 102, 139, 98, 121, 139, 126 ]
                    , [ 120, 131, 107, 125, 135, 114, 103, 141, 127, 135, 118, 111 ]
                    , [ 133, 116, 119, 111, 123, 144, 111, 136, 128, 115, 101, 123 ]
                    , [ 126, 108, 147, 125, 121, 115, 121, 116, 112, 118, 121, 111 ]
                    ]

                EntitySector ->
                    [ [ 230, 207, 243, 232, 206, 252, 267, 211, 243, 235, 247, 230 ]
                    , [ 240, 232, 221, 276, 261, 211, 250, 209, 287, 262, 237, 248 ]
                    , [ 274, 290, 246, 230, 238, 231, 282, 237, 225, 279, 275, 230 ]
                    , [ 280, 227, 236, 252, 210, 230, 232, 233, 226, 254, 249, 245 ]
                    ]

                EntityDistrict ->
                    [ [ 567, 514, 601, 658, 557, 528, 505, 506, 540, 554, 529, 510 ]
                    , [ 536, 567, 633, 530, 622, 583, 571, 549, 484, 497, 566, 502 ]
                    , [ 507, 496, 609, 606, 575, 522, 548, 472, 645, 482, 483, 623 ]
                    , [ 610, 497, 528, 582, 569, 505, 477, 567, 657, 519, 544, 568 ]
                    ]
    in
    div [ class "pane velvet" ]
        [ viewPaneHeading language Translate.NutritionBehavior
        , div [ class "pane-content" ] <|
            viewTableHeader language
                :: rows
        ]


viewTargetedInterventionsPane : Language -> NominalDate -> Int -> ScoreboardData -> Html any
viewTargetedInterventionsPane language currentDate yearSelectorGap data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDATargetedInterventionsItemLabel item) itemValues
                )
                [ FBFGiven
                , TreatmentForAcuteMalnutrition
                , TreatmentForDiarrhea
                , SupportChildWithDisability
                , ConditionalCashTransfer
                , ConditionalFoodItems
                ]
                values

        values =
            case data.entityType of
                EntityVillage ->
                    [ [ 9, 15, 18, 7, 13, 11, 16, 9, 18, 8, 12, 10 ]
                    , [ 13, 12, 7, 9, 8, 14, 17, 17, 10, 16, 10, 17 ]
                    , [ 12, 15, 10, 18, 9, 16, 8, 11, 12, 17, 18, 14 ]
                    , [ 3, 8, 2, 0, 7, 6, 1, 5, 9, 4, 2, 3 ]
                    , [ 17, 12, 8, 16, 11, 10, 9, 18, 15, 7, 12, 13 ]
                    , [ 14, 10, 18, 11, 16, 12, 13, 7, 18, 12, 9, 15 ]
                    ]

                EntityCell ->
                    [ [ 117, 135, 107, 104, 146, 97, 120, 138, 128, 99, 133, 128 ]
                    , [ 139, 130, 131, 123, 103, 128, 123, 129, 145, 117, 99, 142 ]
                    , [ 140, 96, 134, 121, 105, 98, 105, 139, 139, 138, 98, 131 ]
                    , [ 98, 98, 122, 100, 173, 173, 173, 98, 100, 100, 122, 122 ]
                    , [ 142, 100, 129, 117, 141, 118, 120, 120, 123, 133, 98, 137 ]
                    , [ 110, 91, 146, 124, 133, 149, 114, 89, 107, 144, 147, 118 ]
                    ]

                EntitySector ->
                    [ [ 266, 288, 280, 238, 281, 275, 276, 259, 253, 246, 254, 259 ]
                    , [ 203, 257, 234, 245, 245, 256, 124, 145, 124, 145, 239, 240 ]
                    , [ 240, 229, 250, 240, 270, 216, 258, 247, 212, 250, 229, 209 ]
                    , [ 203, 203, 239, 220, 256, 256, 256, 203, 220, 220, 239, 239 ]
                    , [ 254, 261, 237, 238, 258, 249, 275, 275, 216, 239, 241, 231 ]
                    , [ 234, 227, 255, 265, 228, 208, 234, 206, 236, 238, 231, 252 ]
                    ]

                EntityDistrict ->
                    [ [ 582, 618, 604, 533, 550, 601, 648, 486, 503, 565, 491, 634 ]
                    , [ 491, 455, 640, 678, 524, 491, 545, 640, 563, 640, 455, 491 ]
                    , [ 497, 555, 484, 545, 518, 491, 537, 652, 633, 614, 616, 554 ]
                    , [ 530, 530, 491, 455, 640, 640, 640, 530, 455, 455, 491, 491 ]
                    , [ 620, 624, 578, 528, 530, 588, 583, 609, 625, 503, 651, 638 ]
                    , [ 673, 635, 695, 604, 552, 618, 651, 673, 624, 586, 555, 668 ]
                    ]
    in
    div [ class "pane cyan" ]
        [ viewPaneHeading language Translate.TargetedInterventions
        , div [ class "pane-content" ] <|
            viewTableHeader language
                :: rows
        ]


viewInfrastructureEnvironmentWashPane : Language -> NominalDate -> Int -> ScoreboardData -> Html any
viewInfrastructureEnvironmentWashPane language currentDate yearSelectorGap data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDAInfrastructureEnvironmentWashItemLabel item) itemValues
                )
                [ HasToilets, HasCleanWater, HasHandwashingFacility, HasKitchenGarden, InsecticideTreatedBedNets ]
                values

        values =
            case data.entityType of
                EntityVillage ->
                    [ [ 9, 14, 16, 12, 10, 8, 17, 11, 11, 16, 13, 15 ]
                    , [ 13, 9, 13, 16, 12, 8, 17, 10, 10, 12, 14, 11 ]
                    , [ 10, 9, 8, 16, 17, 11, 14, 18, 12, 15, 15, 11 ]
                    , [ 16, 12, 11, 7, 13, 8, 16, 19, 15, 14, 11, 18 ]
                    , [ 13, 8, 10, 9, 18, 11, 7, 17, 12, 10, 14, 17 ]
                    ]

                EntityCell ->
                    [ [ 118, 138, 106, 117, 123, 98, 138, 103, 125, 125, 108, 110 ]
                    , [ 122, 92, 146, 114, 125, 128, 138, 109, 91, 118, 115, 109 ]
                    , [ 127, 126, 130, 103, 143, 117, 121, 108, 108, 111, 136, 135 ]
                    , [ 104, 129, 132, 100, 99, 137, 132, 110, 127, 123, 131, 119 ]
                    , [ 116, 90, 102, 92, 115, 134, 118, 137, 92, 130, 121, 122 ]
                    ]

                EntitySector ->
                    [ [ 252, 244, 239, 247, 234, 259, 217, 259, 215, 250, 222, 264 ]
                    , [ 257, 261, 209, 263, 225, 213, 226, 236, 220, 259, 240, 243 ]
                    , [ 262, 209, 234, 237, 236, 237, 215, 267, 237, 228, 230, 256 ]
                    , [ 252, 249, 214, 226, 284, 291, 202, 279, 238, 215, 285, 271 ]
                    , [ 211, 262, 224, 244, 275, 237, 220, 246, 282, 265, 241, 241 ]
                    ]

                EntityDistrict ->
                    [ [ 631, 583, 667, 626, 621, 567, 652, 611, 506, 555, 665, 636 ]
                    , [ 537, 523, 588, 628, 617, 502, 562, 640, 504, 568, 522, 534 ]
                    , [ 625, 623, 556, 504, 664, 655, 661, 531, 637, 558, 638, 582 ]
                    , [ 657, 624, 577, 659, 643, 490, 532, 545, 601, 680, 506, 651 ]
                    , [ 530, 605, 652, 621, 650, 522, 559, 606, 548, 523, 656, 492 ]
                    ]
    in
    div [ class "pane orange" ]
        [ viewPaneHeading language Translate.InfrastructureEnvironmentWash
        , div [ class "pane-content" ] <|
            viewTableHeader language
                :: rows
        ]


viewPaneHeading : Language -> TranslationId -> Html any
viewPaneHeading language label =
    div [ class <| "pane-heading" ]
        [ text <| translate language label ]


generateMonthsGap : NominalDate -> Int -> Dict Int Int
generateMonthsGap currentDate yearSelectorGap =
    let
        currentMonthNumber =
            Date.monthNumber currentDate
    in
    List.range 1 12
        |> List.map (\monthNumber -> (-1 * 12 * yearSelectorGap) + (-1 * (monthNumber - currentMonthNumber)))
        |> List.indexedMap Tuple.pair
        |> Dict.fromList


viewTableHeader : Language -> Html any
viewTableHeader language =
    let
        statusCell =
            div [ class "cell activity" ] [ text <| translate language Translate.Status ]

        monthCells =
            List.map
                (\month ->
                    div [ class "cell" ] [ text <| translate language <| Translate.Month month ]
                )
                [ Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec ]
    in
    div [ class "table-header" ] <|
        statusCell
            :: monthCells


viewTableRow : Language -> NominalDate -> Int -> TranslationId -> List Int -> Html any
viewTableRow language currentDate yearSelectorGap itemTransId values =
    let
        activityCell =
            div [ class "cell activity" ] [ text <| translate language itemTransId ]

        valueCells =
            formatValues currentDate yearSelectorGap values
                |> List.map
                    (\value ->
                        div [ class "cell value" ]
                            [ text value ]
                    )
    in
    div [ class "table-row" ] <|
        activityCell
            :: valueCells


formatValues : NominalDate -> Int -> List Int -> List String
formatValues currentDate yearSelectorGap =
    let
        currentMonthNumber =
            Date.monthNumber currentDate
    in
    List.indexedMap
        (\index value ->
            if yearSelectorGap == 0 then
                if index < currentMonthNumber then
                    String.fromInt value

                else
                    ""

            else
                String.fromInt value
        )
