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
        , viewAcuteMalnutritionPane language currentDate model.yearSelectorGap monthsGap data
        , viewStuntingPane language currentDate model.yearSelectorGap monthsGap data
        , viewANCNewbornPane language currentDate model.yearSelectorGap monthsGap data
        , viewUniversalInterventionPane language currentDate model.yearSelectorGap data
        , viewNutritionBehaviorPane language currentDate model.yearSelectorGap monthsGap data
        , viewTargetedInterventionsPane language currentDate model.yearSelectorGap data
        , viewInfrastructureEnvironmentWashPane language currentDate model.yearSelectorGap monthsGap data
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
                                    (\gapInMonths ->
                                        let
                                            gap =
                                                ageInMonths - gapInMonths

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


viewAcuteMalnutritionPane : Language -> NominalDate -> Int -> Dict Int Int -> ScoreboardData -> Html any
viewAcuteMalnutritionPane language currentDate yearSelectorGap monthsGap data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDAAcuteMalnutritionItemLabel item) itemValues
                )
                [ SevereAcuteMalnutrition, ModerateAcuteMalnutrition, GoodNutrition ]
                values

        valuesByRow =
            List.foldl
                (\record accum ->
                    let
                        stuntingSevereAsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.nutrition.stunting.severe

                        stuntingModerateAsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.nutrition.stunting.moderate

                        stuntingNormalAsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.nutrition.stunting.normal

                        underweightSevereAsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.nutrition.underweight.severe

                        underweightModerateAsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.nutrition.underweight.moderate

                        underweightNormalAsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.nutrition.underweight.normal

                        wastingSevereAsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.nutrition.wasting.severe

                        wastingModerateAsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.nutrition.wasting.moderate

                        wastingNormalAsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.nutrition.wasting.normal

                        muacSevereAsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.nutrition.muac.severe

                        muacModerateAsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.nutrition.muac.moderate

                        muacNormalAsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.nutrition.muac.normal
                    in
                    List.indexedMap
                        (\index accumValue ->
                            Dict.get index monthsGap
                                |> Maybe.map
                                    (\gapInMonths ->
                                        let
                                            ( row1, row2, row3 ) =
                                                if
                                                    List.member gapInMonths stuntingSevereAsAgeInMonths
                                                        || List.member gapInMonths underweightSevereAsAgeInMonths
                                                        || List.member gapInMonths wastingSevereAsAgeInMonths
                                                        || List.member gapInMonths muacModerateAsAgeInMonths
                                                then
                                                    ( accumValue.row1 + 1, accumValue.row2, accumValue.row3 )

                                                else if
                                                    List.member gapInMonths stuntingModerateAsAgeInMonths
                                                        || List.member gapInMonths underweightModerateAsAgeInMonths
                                                        || List.member gapInMonths wastingModerateAsAgeInMonths
                                                        || List.member gapInMonths muacModerateAsAgeInMonths
                                                then
                                                    ( accumValue.row1, accumValue.row2 + 1, accumValue.row3 )

                                                else if
                                                    List.member gapInMonths stuntingNormalAsAgeInMonths
                                                        || List.member gapInMonths underweightNormalAsAgeInMonths
                                                        || List.member gapInMonths wastingNormalAsAgeInMonths
                                                        || List.member gapInMonths muacNormalAsAgeInMonths
                                                then
                                                    ( accumValue.row1, accumValue.row2, accumValue.row3 + 1 )

                                                else
                                                    ( accumValue.row1, accumValue.row2, accumValue.row3 )
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
                            List.map (\date -> diffMonths date currentDate) record.nutrition.stunting.severe

                        moderateAsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.nutrition.stunting.moderate

                        normalAsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.nutrition.stunting.normal
                    in
                    List.indexedMap
                        (\index accumValue ->
                            Dict.get index monthsGap
                                |> Maybe.map
                                    (\gapInMonths ->
                                        let
                                            row1 =
                                                if List.member gapInMonths severeAsAgeInMonths then
                                                    accumValue.row1 + 1

                                                else
                                                    accumValue.row1

                                            row2 =
                                                if List.member gapInMonths moderateAsAgeInMonths then
                                                    accumValue.row2 + 1

                                                else
                                                    accumValue.row2

                                            row3 =
                                                if List.member gapInMonths normalAsAgeInMonths then
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


viewANCNewbornPane : Language -> NominalDate -> Int -> Dict Int Int -> ScoreboardData -> Html any
viewANCNewbornPane language currentDate yearSelectorGap monthsGap data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDAANCNewbornItemLabel item) itemValues
                )
                [ RegularCheckups, IronDuringPregnancy ]
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
                                    (\gapInMonths ->
                                        let
                                            gap =
                                                gapInMonths - ageInMonths

                                            row1 =
                                                if record.ncda.postpartumCheckups && gap > 0 && gap < 10 then
                                                    accumValue.row1 + 1

                                                else
                                                    accumValue.row1

                                            row2 =
                                                if record.ncda.ironDuringPregnancy && gap > 0 && gap < 10 then
                                                    accumValue.row2 + 1

                                                else
                                                    accumValue.row2
                                        in
                                        { row1 = row1
                                        , row2 = row2
                                        }
                                    )
                                |> Maybe.withDefault accumValue
                        )
                        accum
                )
                emptyValues
                data.records

        emptyValues =
            List.repeat 12 { row1 = 0, row2 = 0 }

        values =
            [ List.map .row1 valuesByRow
            , List.map .row2 valuesByRow
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
                [ Immunization, VitaminA, Deworming, OngeraMNP, ECDServices ]
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


viewNutritionBehaviorPane : Language -> NominalDate -> Int -> Dict Int Int -> ScoreboardData -> Html any
viewNutritionBehaviorPane language currentDate yearSelectorGap monthsGap data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDANutritionBehaviorItemLabel item) itemValues
                )
                [ BreastfedSixMonths, AppropriateComplementaryFeeding, DiverseDiet, MealsADay ]
                values

        valuesByRow =
            List.foldl
                (\record accum ->
                    let
                        row1AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.nutritionBehavior.row1

                        row2AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.nutritionBehavior.row2

                        row3AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.nutritionBehavior.row3

                        row4AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.nutritionBehavior.row4
                    in
                    List.indexedMap
                        (\index accumValue ->
                            Dict.get index monthsGap
                                |> Maybe.map
                                    (\gapInMonths ->
                                        let
                                            row1 =
                                                if List.member gapInMonths row1AsAgeInMonths then
                                                    accumValue.row1 + 1

                                                else
                                                    accumValue.row1

                                            row2 =
                                                if List.member gapInMonths row2AsAgeInMonths then
                                                    accumValue.row2 + 1

                                                else
                                                    accumValue.row2

                                            row3 =
                                                if List.member gapInMonths row3AsAgeInMonths then
                                                    accumValue.row3 + 1

                                                else
                                                    accumValue.row3

                                            row4 =
                                                if List.member gapInMonths row4AsAgeInMonths then
                                                    accumValue.row4 + 1

                                                else
                                                    accumValue.row4
                                        in
                                        { row1 = row1
                                        , row2 = row2
                                        , row3 = row3
                                        , row4 = row4
                                        }
                                    )
                                |> Maybe.withDefault accumValue
                        )
                        accum
                )
                emptyValues
                data.records

        emptyValues =
            List.repeat 12 { row1 = 0, row2 = 0, row3 = 0, row4 = 0 }

        values =
            [ List.map .row1 valuesByRow
            , List.map .row2 valuesByRow
            , List.map .row3 valuesByRow
            , List.map .row4 valuesByRow
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


viewInfrastructureEnvironmentWashPane : Language -> NominalDate -> Int -> Dict Int Int -> ScoreboardData -> Html any
viewInfrastructureEnvironmentWashPane language currentDate yearSelectorGap monthsGap data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDAInfrastructureEnvironmentWashItemLabel item) itemValues
                )
                [ HasToilets, HasCleanWater, HasHandwashingFacility, InsecticideTreatedBedNets, HasKitchenGarden ]
                values

        valuesByRow =
            List.foldl
                (\record accum ->
                    let
                        ageInMonths =
                            diffMonths record.birthDate currentDate

                        row1AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.infrastructureEnvironmentWash.row1

                        row2AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.infrastructureEnvironmentWash.row2

                        row3AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.infrastructureEnvironmentWash.row3

                        row5AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.infrastructureEnvironmentWash.row5
                    in
                    List.indexedMap
                        (\index accumValue ->
                            Dict.get index monthsGap
                                |> Maybe.map
                                    (\gapInMonths ->
                                        let
                                            row1 =
                                                if List.member gapInMonths row1AsAgeInMonths then
                                                    accumValue.row1 + 1

                                                else
                                                    accumValue.row1

                                            row2 =
                                                if List.member gapInMonths row2AsAgeInMonths then
                                                    accumValue.row2 + 1

                                                else
                                                    accumValue.row2

                                            row3 =
                                                if List.member gapInMonths row3AsAgeInMonths then
                                                    accumValue.row3 + 1

                                                else
                                                    accumValue.row3

                                            row4 =
                                                let
                                                    gap =
                                                        ageInMonths - gapInMonths
                                                in
                                                if record.ncda.infrastructureEnvironmentWash.row4 && gap >= 0 && gap < 24 then
                                                    accumValue.row4 + 1

                                                else
                                                    accumValue.row4

                                            row5 =
                                                if List.member gapInMonths row5AsAgeInMonths then
                                                    accumValue.row5 + 1

                                                else
                                                    accumValue.row5
                                        in
                                        { row1 = row1
                                        , row2 = row2
                                        , row3 = row3
                                        , row4 = row4
                                        , row5 = row5
                                        }
                                    )
                                |> Maybe.withDefault accumValue
                        )
                        accum
                )
                emptyValues
                data.records

        emptyValues =
            List.repeat 12 { row1 = 0, row2 = 0, row3 = 0, row4 = 0, row5 = 0 }

        values =
            [ List.map .row1 valuesByRow
            , List.map .row2 valuesByRow
            , List.map .row3 valuesByRow
            , List.map .row4 valuesByRow
            , List.map .row5 valuesByRow
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
