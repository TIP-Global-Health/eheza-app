module Pages.Scoreboard.View exposing (view)

import App.Types exposing (Language)
import AssocList as Dict exposing (Dict)
import Date
import Gizra.Html exposing (emptyNode, showIf)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Icons
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Scoreboard.Model exposing (..)
import Pages.Scoreboard.Utils exposing (..)
import Pages.Utils exposing (emptySelectOption, viewActionButton, viewLabel, viewYearSelector)
import Restful.Endpoint exposing (fromEntityId, toEntityId)
import Time exposing (Month(..))
import Translate exposing (TranslationId, translate)
import Utils.GeoLocation exposing (GeoLocationId, filterGeoLocationDictByParent, geoInfo, geoLocationDictToOptions)


view : Language -> NominalDate -> Model -> Html Msg
view language currentDate model =
    case model.displayMode of
        DisplayViewSelection ->
            viewDisplayViewSelection language model

        DisplayResultTable value ->
            viewDisplayResultTable language currentDate value model


viewDisplayViewSelection : Language -> Model -> Html Msg
viewDisplayViewSelection language model =
    let
        provinceInput =
            let
                options =
                    geoLocationDictToOptions geoInfo.provinces
            in
            viewSelectListInput language
                model.form.province
                options
                (SetGeoLocation
                    (\value form ->
                        { form
                            | province =
                                String.toInt value |> Maybe.map toEntityId
                        }
                    )
                )
                Translate.Province
                (isJust model.form.district)

        districtInput =
            Maybe.map
                (\parentId ->
                    let
                        options =
                            filterGeoLocationDictByParent (fromEntityId parentId) geoInfo.districts
                                |> geoLocationDictToOptions
                    in
                    viewSelectListInput language
                        model.form.district
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | district = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        Translate.District
                        (isJust model.form.sector)
                )
                model.form.province
                |> Maybe.withDefault emptyNode

        sectorInput =
            Maybe.map
                (\parentId ->
                    let
                        options =
                            filterGeoLocationDictByParent (fromEntityId parentId) geoInfo.sectors
                                |> geoLocationDictToOptions
                    in
                    viewSelectListInput language
                        model.form.sector
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | sector = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        Translate.Sector
                        (isJust model.form.cell)
                )
                model.form.district
                |> Maybe.withDefault emptyNode

        cellInput =
            Maybe.map
                (\parentId ->
                    let
                        options =
                            filterGeoLocationDictByParent (fromEntityId parentId) geoInfo.cells
                                |> geoLocationDictToOptions
                    in
                    viewSelectListInput language
                        model.form.cell
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | cell = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        Translate.Cell
                        (isJust model.form.village)
                )
                model.form.sector
                |> Maybe.withDefault emptyNode

        villageInput =
            Maybe.map
                (\parentId ->
                    let
                        options =
                            filterGeoLocationDictByParent (fromEntityId parentId) geoInfo.villages
                                |> geoLocationDictToOptions
                    in
                    viewSelectListInput language
                        model.form.village
                        options
                        (SetGeoLocation
                            (\value form ->
                                { form
                                    | village = String.toInt value |> Maybe.map toEntityId
                                }
                            )
                        )
                        Translate.Village
                        False
                )
                model.form.cell
                |> Maybe.withDefault emptyNode
    in
    div [ class "page-content" ] <|
        [ div [ class "header" ] [ text "Please select desired view mode:" ]
        , div [ class "inputs" ]
            [ provinceInput
            , districtInput
            , sectorInput
            , cellInput
            , villageInput
            ]
        , viewActionButton language Translate.GenerateReport True GenerateReport
            |> showIf (isJust model.form.province && isJust model.form.district)
        ]


viewSelectListInput :
    Language
    -> Maybe GeoLocationId
    -> List ( String, String )
    -> (String -> Msg)
    -> TranslationId
    -> Bool
    -> Html Msg
viewSelectListInput language currentValue options setMsg labelTransId disabled =
    let
        selectOptions =
            emptyOption
                :: List.map
                    (\option_ ->
                        let
                            isSelected =
                                Tuple.first option_
                                    |> String.toInt
                                    |> Maybe.map
                                        (\id ->
                                            currentValue == (Just <| toEntityId id)
                                        )
                                    |> Maybe.withDefault False
                        in
                        option
                            [ value <| Tuple.first option_
                            , selected isSelected
                            ]
                            [ text <| Tuple.second option_ ]
                    )
                    options

        emptyOption =
            emptySelectOption (currentValue == Nothing)
    in
    div
        [ classList
            [ ( "select-input-wrapper", True )
            , ( "disabled", disabled )
            ]
        ]
        [ viewLabel language labelTransId
        , select
            [ onInput setMsg
            , class "select-input"
            ]
            selectOptions
        ]


viewDisplayResultTable : Language -> NominalDate -> ViewSelectionValue -> Model -> Html Msg
viewDisplayResultTable language currentDate value model =
    let
        ( entityId, entityType ) =
            case value.village of
                Just id ->
                    ( id, EntityVillage )

                Nothing ->
                    case value.cell of
                        Just id ->
                            ( id, EntityCell )

                        Nothing ->
                            case value.sector of
                                Just id ->
                                    ( id, EntitySector )

                                Nothing ->
                                    ( value.district, EntityDistrict )

        topBar =
            div [ class "top-bar" ]
                [ div [ class "new-selection" ]
                    [ button [ onClick ResetSelection ]
                        [ text <| translate language Translate.NewSelection ]
                    ]
                , viewYearSelector language currentDate model.yearSelectorGap ChaneYearGap
                , div [ class "values-percents" ] []
                ]
    in
    div [ class "page-content" ]
        [ topBar
        , viewAggregatedChildScoreboardPane language ( entityId, entityType )
        , viewDemographicsPane language currentDate model.yearSelectorGap entityType
        , viewAcuteMalnutritionPane language currentDate model.yearSelectorGap entityType
        , viewStuntingPane language currentDate model.yearSelectorGap entityType
        , viewANCNewbornPane language currentDate model.yearSelectorGap entityType
        , viewUniversalInterventionPane language currentDate model.yearSelectorGap entityType
        , viewNutritionBehaviorPane language currentDate model.yearSelectorGap entityType
        , viewTargetedInterventionsPane language currentDate model.yearSelectorGap entityType
        , viewInfrastructureEnvironmentWashPane language currentDate model.yearSelectorGap entityType
        ]


viewAggregatedChildScoreboardPane :
    Language
    -> ( GeoLocationId, SelectedEntity )
    -> Html any
viewAggregatedChildScoreboardPane language ( entityId, entityType ) =
    let
        entityName =
            case entityType of
                EntityDistrict ->
                    resolveEnityName entityId geoInfo.districts

                EntitySector ->
                    resolveEnityName entityId geoInfo.sectors

                EntityCell ->
                    resolveEnityName entityId geoInfo.cells

                EntityVillage ->
                    resolveEnityName entityId geoInfo.villages

        resolveEnityName id dict =
            Dict.get id dict
                |> Maybe.map .name
                |> Maybe.withDefault ""
    in
    div [ class "pane" ]
        [ viewPaneHeading language Translate.AggregatedChildScoreboard
        , div [ class "pane-content" ]
            [ div []
                [ span [ class "selected-entity" ] [ text <| (translate language <| Translate.SelectedEntity entityType) ++ ":" ]
                , span [] [ text entityName ]
                ]
            ]
        ]


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


viewDemographicsPane : Language -> NominalDate -> Int -> SelectedEntity -> Html any
viewDemographicsPane language currentDate yearSelectorGap entityType =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDADemographicsItemLabel item) itemValues
                )
                [ ChildrenUnder2, NewbornsThisMonth, LowBirthWeigh ]
                values

        values =
            case entityType of
                EntityVillage ->
                    [ [ 12, 12, 14, 13, 15, 15, 15, 12, 13, 13, 14, 14 ]
                    , [ 11, 11, 17, 15, 16, 16, 16, 11, 15, 15, 17, 17 ]
                    , [ 5, 8, 6, 7, 1, 4, 3, 5, 8, 3, 1, 6 ]
                    ]

                EntityCell ->
                    [ [ 98, 98, 122, 100, 173, 173, 173, 98, 100, 100, 122, 122 ]
                    , [ 97, 97, 126, 106, 176, 176, 176, 97, 102, 102, 132, 132 ]
                    , [ 25, 34, 32, 21, 23, 34, 45, 13, 34, 56, 12, 34 ]
                    ]

                EntitySector ->
                    [ [ 203, 203, 239, 220, 256, 256, 256, 203, 220, 220, 239, 239 ]
                    , [ 205, 205, 238, 227, 266, 266, 266, 205, 227, 227, 238, 238 ]
                    , [ 145, 146, 124, 145, 124, 145, 123, 145, 134, 135, 123, 234 ]
                    ]

                EntityDistrict ->
                    [ [ 530, 530, 491, 455, 640, 640, 640, 530, 455, 455, 491, 491 ]
                    , [ 531, 531, 516, 455, 640, 640, 640, 531, 455, 455, 516, 516 ]
                    , [ 345, 345, 356, 455, 214, 256, 289, 278, 267, 256, 256, 245 ]
                    ]
    in
    div [ class "pane cyan" ]
        [ viewPaneHeading language Translate.Demographics
        , div [ class "pane-content" ] <|
            viewTableHeader language
                :: rows
        ]


viewAcuteMalnutritionPane : Language -> NominalDate -> Int -> SelectedEntity -> Html any
viewAcuteMalnutritionPane language currentDate yearSelectorGap entityType =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDAAcuteMalnutritionItemLabel item) itemValues
                )
                [ SevereAcuteMalnutrition, ModerateAcuteMalnutrition, GoodNutrition ]
                values

        values =
            case entityType of
                EntityVillage ->
                    [ [ 11, 17, 19, 15, 15, 7, 8, 12, 11, 17, 11, 12 ]
                    , [ 3, 8, 2, 0, 7, 6, 1, 5, 9, 4, 2, 3 ]
                    , [ 9, 6, 2, 8, 12, 1, 25, 3, 24, 5, 7, 11 ]
                    ]

                EntityCell ->
                    [ [ 98, 129, 100, 123, 112, 145, 173, 98, 145, 134, 135, 122 ]
                    , [ 98, 98, 122, 100, 173, 173, 173, 98, 100, 100, 122, 122 ]
                    , [ 35, 72, 98, 41, 84, 63, 52, 77, 96, 88, 55, 47 ]
                    ]

                EntitySector ->
                    [ [ 203, 257, 234, 245, 245, 256, 124, 145, 124, 145, 239, 240 ]
                    , [ 203, 203, 239, 220, 256, 256, 256, 203, 220, 220, 239, 239 ]
                    , [ 213, 243, 239, 221, 246, 236, 266, 223, 229, 221, 229, 234 ]
                    ]

                EntityDistrict ->
                    [ [ 491, 455, 640, 678, 524, 491, 545, 640, 563, 640, 455, 491 ]
                    , [ 530, 530, 491, 455, 640, 640, 640, 530, 455, 455, 491, 491 ]
                    , [ 223, 569, 854, 732, 988, 622, 901, 775, 666, 444, 888, 998 ]
                    ]
    in
    div [ class "pane orange" ]
        [ viewPaneHeading language Translate.AcuteMalnutrition
        , div [ class "pane-content" ] <|
            viewTableHeader language
                :: rows
        ]


viewStuntingPane : Language -> NominalDate -> Int -> SelectedEntity -> Html any
viewStuntingPane language currentDate yearSelectorGap entityType =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDAStuntingItemLabel item) itemValues
                )
                [ SevereStunting, ModerateStunting, NoStunting ]
                values

        values =
            case entityType of
                EntityVillage ->
                    [ [ 23, 21, 17, 14, 9, 12, 18, 21, 16, 13, 19, 22 ]
                    , [ 8, 14, 7, 18, 13, 17, 12, 15, 19, 16, 11, 10 ]
                    , [ 19, 23, 18, 13, 15, 21, 14, 17, 22, 16, 11, 20 ]
                    ]

                EntityCell ->
                    [ [ 153, 129, 102, 124, 148, 115, 149, 178, 162, 148, 161, 138 ]
                    , [ 102, 125, 136, 129, 149, 131, 125, 117, 144, 146, 137, 108 ]
                    , [ 116, 123, 151, 135, 112, 141, 152, 126, 123, 135, 146, 148 ]
                    ]

                EntitySector ->
                    [ [ 270, 245, 214, 231, 265, 238, 249, 218, 221, 267, 236, 260 ]
                    , [ 246, 269, 240, 232, 258, 215, 207, 236, 274, 252, 214, 233 ]
                    , [ 238, 245, 214, 260, 219, 231, 241, 237, 218, 238, 255, 261 ]
                    ]

                EntityDistrict ->
                    [ [ 605, 596, 562, 640, 621, 546, 661, 592, 635, 539, 587, 612 ]
                    , [ 595, 581, 562, 605, 656, 576, 593, 635, 625, 655, 620, 575 ]
                    , [ 604, 642, 553, 655, 577, 622, 600, 571, 598, 621, 542, 596 ]
                    ]
    in
    div [ class "pane velvet" ]
        [ viewPaneHeading language Translate.Stunting
        , div [ class "pane-content" ] <|
            viewTableHeader language
                :: rows
        ]


viewANCNewbornPane : Language -> NominalDate -> Int -> SelectedEntity -> Html any
viewANCNewbornPane language currentDate yearSelectorGap entityType =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDAANCNewbornItemLabel item) itemValues
                )
                [ RegularCheckups, IronDuringPregnancy ]
                values

        values =
            case entityType of
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


viewUniversalInterventionPane : Language -> NominalDate -> Int -> SelectedEntity -> Html any
viewUniversalInterventionPane language currentDate yearSelectorGap entityType =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDAUniversalInterventionItemLabel item) itemValues
                )
                [ Immunization, VitaminA, OngeraMNP, OngeraMNP, ECDServices ]
                values

        values =
            case entityType of
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


viewNutritionBehaviorPane : Language -> NominalDate -> Int -> SelectedEntity -> Html any
viewNutritionBehaviorPane language currentDate yearSelectorGap entityType =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDANutritionBehaviorItemLabel item) itemValues
                )
                [ BreastfedSixMonths, AppropriateComplementaryFeeding, DiverseDiet, MealsADay ]
                values

        values =
            case entityType of
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


viewTargetedInterventionsPane : Language -> NominalDate -> Int -> SelectedEntity -> Html any
viewTargetedInterventionsPane language currentDate yearSelectorGap entityType =
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
            case entityType of
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


viewInfrastructureEnvironmentWashPane : Language -> NominalDate -> Int -> SelectedEntity -> Html any
viewInfrastructureEnvironmentWashPane language currentDate yearSelectorGap entityType =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDAInfrastructureEnvironmentWashItemLabel item) itemValues
                )
                [ HasToilets, HasCleanWater, HasHandwashingFacility, HasKitchenGarden, InsecticideTreatedBedNets ]
                values

        values =
            case entityType of
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
