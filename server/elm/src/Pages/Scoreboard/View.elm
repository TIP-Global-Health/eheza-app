module Pages.Scoreboard.View exposing (view)

import App.Types exposing (Language)
import AssocList as Dict exposing (Dict)
import Backend.Model exposing (ModelBackend)
import Backend.Scoreboard.Model exposing (ScoreboardData)
import Date
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, diffMonths)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.Scoreboard.Model exposing (..)
import Pages.Scoreboard.Utils exposing (..)
import Pages.Utils exposing (viewYearSelector)
import Time exposing (Month(..))
import Translate exposing (TranslationId, translate)


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
                , div [ class "values-percents" ]
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
                ]

        monthsGap =
            generateMonthsGap currentDate model.yearSelectorGap

        childrenUnder2 =
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
                                        in
                                        if gap >= 0 && gap < 24 then
                                            accumValue + 1

                                        else
                                            accumValue
                                    )
                                |> Maybe.withDefault accumValue
                        )
                        accum
                )
                (List.repeat 12 0)
                data.records
    in
    div [ class "page-content" ]
        [ topBar
        , viewAggregatedChildScoreboardPane language data
        , viewDemographicsPane language currentDate model.yearSelectorGap monthsGap childrenUnder2 model.viewMode data
        , viewAcuteMalnutritionPane language currentDate model.yearSelectorGap monthsGap childrenUnder2 model.viewMode data
        , viewStuntingPane language currentDate model.yearSelectorGap monthsGap childrenUnder2 model.viewMode data
        , viewANCNewbornPane language currentDate model.yearSelectorGap monthsGap childrenUnder2 model.viewMode data
        , viewUniversalInterventionPane language currentDate model.yearSelectorGap monthsGap childrenUnder2 model.viewMode data
        , viewNutritionBehaviorPane language currentDate model.yearSelectorGap monthsGap childrenUnder2 model.viewMode data
        , viewTargetedInterventionsPane language currentDate model.yearSelectorGap monthsGap childrenUnder2 model.viewMode data
        , viewInfrastructureEnvironmentWashPane language currentDate model.yearSelectorGap monthsGap childrenUnder2 model.viewMode data
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
                                        { row2 = row2
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
            List.repeat 12 { row2 = 0, row3 = 0 }

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
                                                        || List.member gapInMonths muacSevereAsAgeInMonths
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
            List.foldl
                (\record accum ->
                    let
                        ageInMonths =
                            -- Using EDD date to properly resolve the month of
                            -- prgnancy (as child may have been borm premature).
                            diffMonths record.eddDate currentDate

                        row1AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.ancNewborn.row1
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
                                                if List.member gapInMonths row1AsAgeInMonths then
                                                    accumValue.row1 + 1

                                                else
                                                    accumValue.row1

                                            row2 =
                                                if record.ncda.ancNewborn.row2 && gap > 0 && gap < 10 then
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


viewUniversalInterventionPane : Language -> NominalDate -> Int -> Dict Int Int -> List Int -> ViewMode -> ScoreboardData -> Html any
viewUniversalInterventionPane language currentDate yearSelectorGap monthsGap childrenUnder2 viewMode data =
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
            List.foldl
                (\record accum ->
                    let
                        ageInMonths =
                            diffMonths record.birthDate currentDate

                        row2AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.universalIntervention.row2

                        row3AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.universalIntervention.row3

                        row4AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.universalIntervention.row4

                        row5AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.universalIntervention.row5
                    in
                    List.indexedMap
                        (\index accumValue ->
                            Dict.get index monthsGap
                                |> Maybe.map
                                    (\gapInMonths ->
                                        let
                                            ageInMonthsForIndexCell =
                                                ageInMonths - gapInMonths

                                            row1 =
                                                if (ageInMonthsForIndexCell < 0) || (ageInMonthsForIndexCell >= 24) then
                                                    accumValue.row1

                                                else
                                                    let
                                                        referenceDate =
                                                            -- We use it to determine if child was
                                                            -- behind on any of vaccines at that month.
                                                            resolveLastDayForMonthX ageInMonthsForIndexCell record.birthDate

                                                        -- Filter out vaccinations that were performed
                                                        -- after the reference date.
                                                        vaccinationProgressOnReferrenceDate =
                                                            Dict.map
                                                                (\_ dosesDict ->
                                                                    Dict.filter
                                                                        (\_ administeredDate ->
                                                                            Date.compare administeredDate referenceDate == LT
                                                                        )
                                                                        dosesDict
                                                                )
                                                                record.ncda.universalIntervention.row1

                                                        futureVaccinations =
                                                            generateFutureVaccinationsData record.birthDate vaccinationProgressOnReferrenceDate

                                                        closestDateForVaccination =
                                                            List.filterMap (Tuple.second >> Maybe.map Tuple.second) futureVaccinations
                                                                |> List.sortWith Date.compare
                                                                |> List.head
                                                    in
                                                    Maybe.map
                                                        (\closestDate ->
                                                            if Date.compare closestDate referenceDate == GT then
                                                                -- Closest date when vaccine is required is after end of
                                                                -- referenced month, which means that we're on track.
                                                                accumValue.row1 + 1

                                                            else
                                                                -- Otherwise, we're off track.
                                                                accumValue.row1
                                                        )
                                                        closestDateForVaccination
                                                        |> Maybe.withDefault
                                                            -- This indicates that there're no future vaccinations to be
                                                            -- done, and therefore, we're on track at referenced month.
                                                            (accumValue.row1 + 1)

                                            row2 =
                                                -- Value is taken from NCDA questionnaire, that is given monthly, until child
                                                -- reaches age of 2 years.
                                                -- NCDA data is also for childern that up until 2 years old, so
                                                -- no need to check child age for given month.
                                                if List.member gapInMonths row2AsAgeInMonths then
                                                    accumValue.row2 + 1

                                                else
                                                    accumValue.row2

                                            row3 =
                                                -- Value is taken from NCDA questionnaire, that is given monthly, until child
                                                -- reaches age of 2 years.
                                                -- NCDA data is also for childern that up until 2 years old, so
                                                -- no need to check child age for given month.
                                                if List.member gapInMonths row3AsAgeInMonths then
                                                    accumValue.row3 + 1

                                                else
                                                    accumValue.row3

                                            row4 =
                                                -- Value is taken from NCDA questionnaire, that is given monthly, until child
                                                -- reaches age of 2 years.
                                                -- NCDA data is also for childern that up until 2 years old, so
                                                -- no need to check child age for given month.
                                                if List.member gapInMonths row4AsAgeInMonths then
                                                    accumValue.row4 + 1

                                                else
                                                    accumValue.row4

                                            row5 =
                                                -- Value is taken from NCDA questionnaire, that is given monthly, until child
                                                -- reaches age of 2 years.
                                                -- NCDA data is also for childern that up until 2 years old, so
                                                -- no need to check child age for given month.
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

        -- Resolves the date for last day of month X after child birth date.
        -- For example, for X = 0, this is
        -- the last day, before child turns 1 month old.
        resolveLastDayForMonthX monthX childBirthDate =
            -- Get to first day of the birth months.
            Date.floor Date.Month childBirthDate
                |> -- Add required number of months.
                   Date.add Date.Months (monthX + 1)
                |> -- Substract one day
                   Date.add Date.Days -1

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
            List.foldl
                (\record accum ->
                    let
                        ageInMonths =
                            diffMonths record.birthDate currentDate

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
                                            gap =
                                                ageInMonths - gapInMonths

                                            row1 =
                                                if gap >= 0 && gap < 6 && record.ncda.nutritionBehavior.row1 then
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
            List.foldl
                (\record accum ->
                    let
                        ageInMonths =
                            diffMonths record.birthDate currentDate

                        row1AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.targetedInterventions.row1

                        row2AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.targetedInterventions.row2

                        row3AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.targetedInterventions.row3

                        row4AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.targetedInterventions.row4

                        row5AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.targetedInterventions.row5

                        row6AsAgeInMonths =
                            List.map (\date -> diffMonths date currentDate) record.ncda.targetedInterventions.row6
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
                                                -- FBFs are distrubuted for children at FBF groups, where
                                                -- children age is up until 2 years old.
                                                -- NCDA data is also for childern that up until 2 years old, so
                                                -- no need to check child age for given month.
                                                if List.member gapInMonths row1AsAgeInMonths then
                                                    accumValue.row1 + 1

                                                else
                                                    accumValue.row1

                                            row2 =
                                                -- Manutrition treatment can be given to children older that 2 years, therefore,
                                                -- we must verify that at given month, child age is between 0 and 24 months.
                                                if List.member gapInMonths row2AsAgeInMonths && gap >= 0 && gap < 24 then
                                                    accumValue.row2 + 1

                                                else
                                                    accumValue.row2

                                            row3 =
                                                -- Diarrhea treatment can be given to children older that 2 years, therefore,
                                                -- we must verify that at given month, child age is between 0 and 24 months.
                                                if List.member gapInMonths row3AsAgeInMonths && gap >= 0 && gap < 24 then
                                                    accumValue.row3 + 1

                                                else
                                                    accumValue.row3

                                            row4 =
                                                -- Value is taken from NCDA questionnaire, that is given monthly, until child
                                                -- reaches age of 2 years.
                                                -- NCDA data is also for childern that up until 2 years old, so
                                                -- no need to check child age for given month.
                                                if List.member gapInMonths row4AsAgeInMonths then
                                                    accumValue.row4 + 1

                                                else
                                                    accumValue.row4

                                            row5 =
                                                -- Value is taken from NCDA questionnaire, that is given monthly, until child
                                                -- reaches age of 2 years.
                                                -- NCDA data is also for childern that up until 2 years old, so
                                                -- no need to check child age for given month.
                                                if List.member gapInMonths row5AsAgeInMonths then
                                                    accumValue.row5 + 1

                                                else
                                                    accumValue.row5

                                            row6 =
                                                -- Value is taken from NCDA questionnaire, that is given monthly, until child
                                                -- reaches age of 2 years.
                                                -- NCDA data is also for childern that up until 2 years old, so
                                                -- no need to check child age for given month.
                                                if List.member gapInMonths row6AsAgeInMonths then
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
                                |> Maybe.withDefault accumValue
                        )
                        accum
                )
                emptyValues
                data.records

        emptyValues =
            List.repeat 12 { row1 = 0, row2 = 0, row3 = 0, row4 = 0, row5 = 0, row6 = 0 }

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


formatValues : NominalDate -> Int -> List String -> List String
formatValues currentDate yearSelectorGap =
    let
        currentMonthNumber =
            Date.monthNumber currentDate
    in
    List.indexedMap
        (\index value ->
            if yearSelectorGap == 0 then
                if index < currentMonthNumber then
                    value

                else
                    ""

            else
                value
        )
