module Pages.Scoreboard.View exposing (view)

import App.Types exposing (Language, Site)
import AssocList exposing (Dict)
import Backend.Model exposing (ModelBackend)
import Backend.Scoreboard.Model exposing (ScoreboardData)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.Components.Utils exposing (isSyncComplete, viewSyncingPlaceholder)
import Pages.Scoreboard.Model exposing (Model, Msg(..), NCDAANCNewbornItem(..), NCDAAcuteMalnutritionItem(..), NCDADemographicsItem(..), NCDAInfrastructureEnvironmentWashItem(..), NCDANutritionBehaviorItem(..), NCDAStuntingItem(..), NCDATargetedInterventionsItem(..), NCDAUniversalInterventionItem(..), ViewMode(..))
import Pages.Scoreboard.Utils exposing (countChildrenUnder2, datesForMonth, foldRecordsByMonth, formatValues, generateMonthsGap, isOnTrackForImmunisation, isPregnancyMonth, monthsSinceMonthOf, registeredBy, valuesByViewMode, viewPercentage)
import Pages.Utils exposing (viewBackendData, viewYearSelector)
import Time exposing (Month(..))
import Translate exposing (TranslationId, translate)


view : Language -> NominalDate -> ModelBackend -> Model -> Html Msg
view language currentDate modelBackend model =
    viewBackendData modelBackend.scoreboardData
        (\data -> viewScoreboardData language currentDate data model)


viewScoreboardData : Language -> NominalDate -> ScoreboardData -> Model -> Html Msg
viewScoreboardData language currentDate data model =
    let
        topBar =
            let
                viewModeToggle =
                    if isSyncComplete data.remainingForDownload then
                        div [ class "values-percents" ]
                            [ div
                                [ classList
                                    [ ( "item", True )
                                    , ( "selected", model.viewMode == ModePercentages )
                                    ]
                                , onClick <| SetViewMode ModePercentages
                                ]
                                [ text "%" ]
                            , div
                                [ classList
                                    [ ( "item", True )
                                    , ( "selected", model.viewMode == ModeValues )
                                    ]
                                , onClick <| SetViewMode ModeValues
                                ]
                                [ text "#" ]
                            ]

                    else
                        emptyNode
            in
            div [ class "top-bar" ]
                [ div [ class "new-selection" ]
                    [ a [ href "/admin/reports/aggregated-ncda" ]
                        [ button []
                            [ text <| translate language Translate.NewSelection ]
                        ]
                    ]
                , viewYearSelector currentDate model.yearSelectorGap ChaneYearGap
                , viewModeToggle
                ]

        panes =
            if isSyncComplete data.remainingForDownload then
                let
                    monthsGap =
                        generateMonthsGap currentDate model.yearSelectorGap

                    childrenUnder2 =
                        countChildrenUnder2 currentDate monthsGap data.records
                in
                [ viewAggregatedChildScoreboardPane language data
                , viewDemographicsPane language currentDate model.yearSelectorGap monthsGap childrenUnder2 model.viewMode data
                , viewAcuteMalnutritionPane language currentDate model.yearSelectorGap monthsGap childrenUnder2 model.viewMode data
                , viewStuntingPane language currentDate model.yearSelectorGap monthsGap childrenUnder2 model.viewMode data
                , viewANCNewbornPane language currentDate model.yearSelectorGap monthsGap childrenUnder2 model.viewMode data
                , viewUniversalInterventionPane language currentDate data.site model.yearSelectorGap monthsGap childrenUnder2 model.viewMode data
                , viewNutritionBehaviorPane language currentDate model.yearSelectorGap monthsGap childrenUnder2 model.viewMode data
                , viewTargetedInterventionsPane language currentDate model.yearSelectorGap monthsGap childrenUnder2 model.viewMode data
                , viewInfrastructureEnvironmentWashPane language currentDate model.yearSelectorGap monthsGap childrenUnder2 model.viewMode data
                ]

            else
                [ viewSyncingPlaceholder language (List.length data.records) data.remainingForDownload ]
    in
    div [ class "page-content" ]
        (topBar :: panes)


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


viewDemographicsPane : Language -> NominalDate -> Int -> Dict Int Int -> List Int -> ViewMode -> ScoreboardData -> Html any
viewDemographicsPane language currentDate yearSelectorGap monthsGap childrenUnder2 viewMode data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    viewTableRow language currentDate yearSelectorGap (Translate.NCDADemographicsItemLabel item) itemValues
                )
                [ ChildrenUnder2, NewbornsThisMonth, LowBirthWeigh ]
                [ List.map String.fromInt childrenUnder2, newbornsForView, lowBirthWeightForView ]

        valuesByRow =
            foldRecordsByMonth currentDate
                monthsGap
                (\targetDateForMonth record accumValue ->
                    let
                        ageInMonths =
                            monthsSinceMonthOf record.birthDate targetDateForMonth

                        existedDuringExaminationMonth =
                            registeredBy targetDateForMonth record

                        row2 =
                            if existedDuringExaminationMonth && ageInMonths == 0 then
                                accumValue.row2 + 1

                            else
                                accumValue.row2

                        row3 =
                            if
                                existedDuringExaminationMonth
                                    && (ageInMonths == 0)
                                    && (record.lowBirthWeight == Just True)
                            then
                                accumValue.row3 + 1

                            else
                                accumValue.row3
                    in
                    { row2 = row2
                    , row3 = row3
                    }
                )
                emptyValue
                data.records

        emptyValue =
            { row2 = 0, row3 = 0 }

        newborns =
            List.map .row2 valuesByRow

        newbornsForView =
            case viewMode of
                ModePercentages ->
                    List.map2 viewPercentage newborns childrenUnder2

                ModeValues ->
                    List.map String.fromInt newborns

        lowBirthWeight =
            List.map .row3 valuesByRow

        lowBirthWeightForView =
            case viewMode of
                ModePercentages ->
                    List.map2 viewPercentage lowBirthWeight newborns

                ModeValues ->
                    List.map String.fromInt lowBirthWeight
    in
    div [ class "pane cyan" ]
        [ viewPaneHeading language Translate.Demographics
        , div [ class "pane-content" ] <|
            viewTableHeader language
                :: rows
        ]


viewAcuteMalnutritionPane : Language -> NominalDate -> Int -> Dict Int Int -> List Int -> ViewMode -> ScoreboardData -> Html any
viewAcuteMalnutritionPane language currentDate yearSelectorGap monthsGap childrenUnder2 viewMode data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    valuesByViewMode viewMode childrenUnder2 itemValues
                        |> viewTableRow language currentDate yearSelectorGap (Translate.NCDAAcuteMalnutritionItemLabel item)
                )
                [ SevereAcuteMalnutrition, ModerateAcuteMalnutrition, GoodNutrition ]
                values

        valuesByRow =
            foldRecordsByMonth currentDate
                monthsGap
                (\targetDateForMonth record accumValue ->
                    let
                        existedDuringExaminationMonth =
                            registeredBy targetDateForMonth record

                        muacSevereAsAgeInMonths =
                            datesForMonth targetDateForMonth record.nutrition.muac.severe

                        ( row1, row2, row3 ) =
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty muacSevereAsAgeInMonths)
                            then
                                ( accumValue.row1 + 1, accumValue.row2, accumValue.row3 )

                            else
                                let
                                    muacModerateAsAgeInMonths =
                                        datesForMonth targetDateForMonth record.nutrition.muac.moderate
                                in
                                if
                                    existedDuringExaminationMonth
                                        && (not <| List.isEmpty muacModerateAsAgeInMonths)
                                then
                                    ( accumValue.row1, accumValue.row2 + 1, accumValue.row3 )

                                else
                                    let
                                        muacNormalAsAgeInMonths =
                                            datesForMonth targetDateForMonth record.nutrition.muac.normal
                                    in
                                    if
                                        existedDuringExaminationMonth
                                            && (not <| List.isEmpty muacNormalAsAgeInMonths)
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
                emptyValue
                data.records

        emptyValue =
            { row1 = 0, row2 = 0, row3 = 0 }

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


viewStuntingPane : Language -> NominalDate -> Int -> Dict Int Int -> List Int -> ViewMode -> ScoreboardData -> Html any
viewStuntingPane language currentDate yearSelectorGap monthsGap childrenUnder2 viewMode data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    valuesByViewMode viewMode childrenUnder2 itemValues
                        |> viewTableRow language currentDate yearSelectorGap (Translate.NCDAStuntingItemLabel item)
                )
                [ SevereStunting, ModerateStunting, NoStunting ]
                values

        valuesByRow =
            foldRecordsByMonth currentDate
                monthsGap
                (\targetDateForMonth record accumValue ->
                    let
                        existedDuringExaminationMonth =
                            registeredBy targetDateForMonth record

                        severeAsAgeInMonths =
                            datesForMonth targetDateForMonth record.nutrition.stunting.severe

                        moderateAsAgeInMonths =
                            datesForMonth targetDateForMonth record.nutrition.stunting.moderate

                        normalAsAgeInMonths =
                            datesForMonth targetDateForMonth record.nutrition.stunting.normal

                        row1 =
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty severeAsAgeInMonths)
                            then
                                accumValue.row1 + 1

                            else
                                accumValue.row1

                        row2 =
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty moderateAsAgeInMonths)
                            then
                                accumValue.row2 + 1

                            else
                                accumValue.row2

                        row3 =
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty normalAsAgeInMonths)
                            then
                                accumValue.row3 + 1

                            else
                                accumValue.row3
                    in
                    { row1 = row1
                    , row2 = row2
                    , row3 = row3
                    }
                )
                emptyValue
                data.records

        emptyValue =
            { row1 = 0, row2 = 0, row3 = 0 }

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


viewANCNewbornPane : Language -> NominalDate -> Int -> Dict Int Int -> List Int -> ViewMode -> ScoreboardData -> Html any
viewANCNewbornPane language currentDate yearSelectorGap monthsGap childrenUnder2 viewMode data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    valuesByViewMode viewMode childrenUnder2 itemValues
                        |> viewTableRow language currentDate yearSelectorGap (Translate.NCDAANCNewbornItemLabel item)
                )
                [ RegularCheckups, IronDuringPregnancy ]
                values

        valuesByRow =
            foldRecordsByMonth currentDate
                monthsGap
                (\targetDateForMonth record accumValue ->
                    let
                        row1AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.ancNewborn.row1

                        row1 =
                            --  We do not a condition to check if child existed during
                            -- examination month because we're examining pregnancy months
                            -- and it's likely that child did not existy on the system.
                            if not <| List.isEmpty row1AsAgeInMonths then
                                accumValue.row1 + 1

                            else
                                accumValue.row1

                        row2 =
                            --  We do not a condition to check if child existed during
                            -- examination month because we're examining pregnancy months
                            -- and it's likely that child did not existy on the system.
                            if record.ncda.ancNewborn.row2 && isPregnancyMonth record.eddDate targetDateForMonth then
                                accumValue.row2 + 1

                            else
                                accumValue.row2
                    in
                    { row1 = row1
                    , row2 = row2
                    }
                )
                emptyValue
                data.records

        emptyValue =
            { row1 = 0, row2 = 0 }

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


viewUniversalInterventionPane : Language -> NominalDate -> Site -> Int -> Dict Int Int -> List Int -> ViewMode -> ScoreboardData -> Html any
viewUniversalInterventionPane language currentDate site yearSelectorGap monthsGap childrenUnder2 viewMode data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    valuesByViewMode viewMode childrenUnder2 itemValues
                        |> viewTableRow language currentDate yearSelectorGap (Translate.NCDAUniversalInterventionItemLabel item)
                )
                [ Immunization, VitaminA, Deworming, OngeraMNP, ECDServices ]
                values

        valuesByRow =
            foldRecordsByMonth currentDate
                monthsGap
                (\targetDateForMonth record accumValue ->
                    let
                        existedDuringExaminationMonth =
                            registeredBy targetDateForMonth record

                        row2AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.universalIntervention.row2

                        row3AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.universalIntervention.row3

                        row4AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.universalIntervention.row4

                        row5AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.universalIntervention.row5

                        row1 =
                            if existedDuringExaminationMonth && isOnTrackForImmunisation site targetDateForMonth record then
                                accumValue.row1 + 1

                            else
                                accumValue.row1

                        row2 =
                            -- Value is taken from NCDA questionnaire, that is given monthly, until child
                            -- reaches age of 2 years.
                            -- NCDA data is also for childern that up until 2 years old, so
                            -- no need to check child age for given month.
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row2AsAgeInMonths)
                            then
                                accumValue.row2 + 1

                            else
                                accumValue.row2

                        row3 =
                            -- Value is taken from NCDA questionnaire, that is given monthly, until child
                            -- reaches age of 2 years.
                            -- NCDA data is also for childern that up until 2 years old, so
                            -- no need to check child age for given month.
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row3AsAgeInMonths)
                            then
                                accumValue.row3 + 1

                            else
                                accumValue.row3

                        row4 =
                            -- Value is taken from NCDA questionnaire, that is given monthly, until child
                            -- reaches age of 2 years.
                            -- NCDA data is also for childern that up until 2 years old, so
                            -- no need to check child age for given month.
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row4AsAgeInMonths)
                            then
                                accumValue.row4 + 1

                            else
                                accumValue.row4

                        row5 =
                            -- Value is taken from NCDA questionnaire, that is given monthly, until child
                            -- reaches age of 2 years.
                            -- NCDA data is also for childern that up until 2 years old, so
                            -- no need to check child age for given month.
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row5AsAgeInMonths)
                            then
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
                emptyValue
                data.records

        emptyValue =
            { row1 = 0, row2 = 0, row3 = 0, row4 = 0, row5 = 0 }

        values =
            [ List.map .row1 valuesByRow
            , List.map .row2 valuesByRow
            , List.map .row3 valuesByRow
            , List.map .row4 valuesByRow
            , List.map .row5 valuesByRow
            ]
    in
    div [ class "pane orange" ]
        [ viewPaneHeading language Translate.UniversalIntervention
        , div [ class "pane-content" ] <|
            viewTableHeader language
                :: rows
        ]


viewNutritionBehaviorPane : Language -> NominalDate -> Int -> Dict Int Int -> List Int -> ViewMode -> ScoreboardData -> Html any
viewNutritionBehaviorPane language currentDate yearSelectorGap monthsGap childrenUnder2 viewMode data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    valuesByViewMode viewMode childrenUnder2 itemValues
                        |> viewTableRow language currentDate yearSelectorGap (Translate.NCDANutritionBehaviorItemLabel item)
                )
                [ BreastfedSixMonths, AppropriateComplementaryFeeding, DiverseDiet, MealsADay ]
                values

        valuesByRow =
            foldRecordsByMonth currentDate
                monthsGap
                (\targetDateForMonth record accumValue ->
                    let
                        existedDuringExaminationMonth =
                            registeredBy targetDateForMonth record

                        ageInMonths =
                            monthsSinceMonthOf record.birthDate targetDateForMonth

                        row2AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.nutritionBehavior.row2

                        row3AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.nutritionBehavior.row3

                        row4AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.nutritionBehavior.row4

                        row1 =
                            if
                                existedDuringExaminationMonth
                                    && (ageInMonths >= 0)
                                    && (ageInMonths < 6)
                                    && record.ncda.nutritionBehavior.row1
                            then
                                accumValue.row1 + 1

                            else
                                accumValue.row1

                        row2 =
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row2AsAgeInMonths)
                            then
                                accumValue.row2 + 1

                            else
                                accumValue.row2

                        row3 =
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row3AsAgeInMonths)
                            then
                                accumValue.row3 + 1

                            else
                                accumValue.row3

                        row4 =
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row4AsAgeInMonths)
                            then
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
                emptyValue
                data.records

        emptyValue =
            { row1 = 0, row2 = 0, row3 = 0, row4 = 0 }

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


viewTargetedInterventionsPane : Language -> NominalDate -> Int -> Dict Int Int -> List Int -> ViewMode -> ScoreboardData -> Html any
viewTargetedInterventionsPane language currentDate yearSelectorGap monthsGap childrenUnder2 viewMode data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    valuesByViewMode viewMode childrenUnder2 itemValues
                        |> viewTableRow language currentDate yearSelectorGap (Translate.NCDATargetedInterventionsItemLabel item)
                )
                [ FBFGiven
                , TreatmentForAcuteMalnutrition
                , TreatmentForDiarrhea
                , SupportChildWithDisability
                , ConditionalCashTransfer
                , ConditionalFoodItems
                ]
                values

        valuesByRow =
            foldRecordsByMonth currentDate
                monthsGap
                (\targetDateForMonth record accumValue ->
                    let
                        existedDuringExaminationMonth =
                            registeredBy targetDateForMonth record

                        ageInMonths =
                            monthsSinceMonthOf record.birthDate targetDateForMonth

                        row1AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.targetedInterventions.row1

                        row2AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.targetedInterventions.row2

                        row3AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.targetedInterventions.row3

                        row4AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.targetedInterventions.row4

                        row5AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.targetedInterventions.row5

                        row6AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.targetedInterventions.row6

                        row1 =
                            -- FBFs are distrubuted for children at FBF groups, where
                            -- children age is up until 2 years old.
                            -- NCDA data is also for childern that up until 2 years old, so
                            -- no need to check child age for given month.
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row1AsAgeInMonths)
                            then
                                accumValue.row1 + 1

                            else
                                accumValue.row1

                        row2 =
                            -- Manutrition treatment can be given to children older that 2 years, therefore,
                            -- we must verify that at given month, child age is between 0 and 24 months.
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row2AsAgeInMonths)
                                    && (ageInMonths >= 0)
                                    && (ageInMonths < 24)
                            then
                                accumValue.row2 + 1

                            else
                                accumValue.row2

                        row3 =
                            -- Diarrhea treatment can be given to children older that 2 years, therefore,
                            -- we must verify that at given month, child age is between 0 and 24 months.
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row3AsAgeInMonths)
                                    && (ageInMonths >= 0)
                                    && (ageInMonths < 24)
                            then
                                accumValue.row3 + 1

                            else
                                accumValue.row3

                        row4 =
                            -- Value is taken from NCDA questionnaire, that is given monthly, until child
                            -- reaches age of 2 years.
                            -- NCDA data is also for childern that up until 2 years old, so
                            -- no need to check child age for given month.
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row4AsAgeInMonths)
                            then
                                accumValue.row4 + 1

                            else
                                accumValue.row4

                        row5 =
                            -- Value is taken from NCDA questionnaire, that is given monthly, until child
                            -- reaches age of 2 years.
                            -- NCDA data is also for childern that up until 2 years old, so
                            -- no need to check child age for given month.
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row5AsAgeInMonths)
                            then
                                accumValue.row5 + 1

                            else
                                accumValue.row5

                        row6 =
                            -- Value is taken from NCDA questionnaire, that is given monthly, until child
                            -- reaches age of 2 years.
                            -- NCDA data is also for childern that up until 2 years old, so
                            -- no need to check child age for given month.
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row6AsAgeInMonths)
                            then
                                accumValue.row6 + 1

                            else
                                accumValue.row6
                    in
                    { row1 = row1
                    , row2 = row2
                    , row3 = row3
                    , row4 = row4
                    , row5 = row5
                    , row6 = row6
                    }
                )
                emptyValue
                data.records

        emptyValue =
            { row1 = 0, row2 = 0, row3 = 0, row4 = 0, row5 = 0, row6 = 0 }

        values =
            [ List.map .row1 valuesByRow
            , List.map .row2 valuesByRow
            , List.map .row3 valuesByRow
            , List.map .row4 valuesByRow
            , List.map .row5 valuesByRow
            , List.map .row6 valuesByRow
            ]
    in
    div [ class "pane cyan" ]
        [ viewPaneHeading language Translate.TargetedInterventions
        , div [ class "pane-content" ] <|
            viewTableHeader language
                :: rows
        ]


viewInfrastructureEnvironmentWashPane : Language -> NominalDate -> Int -> Dict Int Int -> List Int -> ViewMode -> ScoreboardData -> Html any
viewInfrastructureEnvironmentWashPane language currentDate yearSelectorGap monthsGap childrenUnder2 viewMode data =
    let
        rows =
            List.map2
                (\item itemValues ->
                    valuesByViewMode viewMode childrenUnder2 itemValues
                        |> viewTableRow language currentDate yearSelectorGap (Translate.NCDAInfrastructureEnvironmentWashItemLabel item)
                )
                -- In the order the rows are stored in, so that the label of a
                -- row and the sign it counts cannot come apart. See
                -- hedley_ncda.module, which fills them.
                [ HasCleanWater, HasToilets, HasHandwashingFacility, InsecticideTreatedBedNets, HasKitchenGarden ]
                values

        valuesByRow =
            foldRecordsByMonth currentDate
                monthsGap
                (\targetDateForMonth record accumValue ->
                    let
                        existedDuringExaminationMonth =
                            registeredBy targetDateForMonth record

                        row1AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.infrastructureEnvironmentWash.row1

                        row2AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.infrastructureEnvironmentWash.row2

                        row3AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.infrastructureEnvironmentWash.row3

                        row4AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.infrastructureEnvironmentWash.row4

                        row5AsAgeInMonths =
                            datesForMonth targetDateForMonth record.ncda.infrastructureEnvironmentWash.row5

                        row1 =
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row1AsAgeInMonths)
                            then
                                accumValue.row1 + 1

                            else
                                accumValue.row1

                        row2 =
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row2AsAgeInMonths)
                            then
                                accumValue.row2 + 1

                            else
                                accumValue.row2

                        row3 =
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row3AsAgeInMonths)
                            then
                                accumValue.row3 + 1

                            else
                                accumValue.row3

                        row4 =
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row4AsAgeInMonths)
                            then
                                accumValue.row4 + 1

                            else
                                accumValue.row4

                        row5 =
                            if
                                existedDuringExaminationMonth
                                    && (not <| List.isEmpty row5AsAgeInMonths)
                            then
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
                emptyValue
                data.records

        emptyValue =
            { row1 = 0, row2 = 0, row3 = 0, row4 = 0, row5 = 0 }

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
    div [ class "pane-heading" ]
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


viewTableRow : Language -> NominalDate -> Int -> TranslationId -> List String -> Html any
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
