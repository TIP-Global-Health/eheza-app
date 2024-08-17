module Pages.Completion.View exposing (view)

import App.Types exposing (Language, Site)
import AssocList as Dict exposing (Dict)
import Backend.Completion.Model exposing (CompletionData, EncounterData, NutritionActivity(..), SelectedEntity(..))
import Backend.Model exposing (ModelBackend)
import Date exposing (Interval(..), Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, customFormatDDMMYYYY, formatDDMMYYYY, sortByDateDesc)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List.Extra
import Maybe.Extra exposing (isJust, isNothing)
import Pages.Completion.Model exposing (..)
import Pages.Completion.Utils exposing (reportTypeToString)
import Pages.Components.View exposing (viewNutritionMetricsResultsTable)
import Pages.Model exposing (MetricsResultsTableData)
import Pages.Utils exposing (launchDate, viewSelectListInput, wrapSelectListInput)
import RemoteData exposing (RemoteData(..))
import Round
import Time exposing (Month(..))
import Translate exposing (TranslationId, translate)
import Utils.Html exposing (viewModal)


view : Language -> NominalDate -> String -> ModelBackend -> Model -> Html Msg
view language currentDate themePath modelBackend model =
    case modelBackend.completionData of
        Just (Ok data) ->
            viewCompletionData language currentDate themePath data model

        Just (Err err) ->
            text <| Debug.toString err

        Nothing ->
            emptyNode


viewCompletionData : Language -> NominalDate -> String -> CompletionData -> Model -> Html Msg
viewCompletionData language currentDate themePath data model =
    let
        topBar =
            let
                scopeLabel =
                    case data.entityType of
                        EntityGlobal ->
                            translate language Translate.Global

                        EntityHealthCenter ->
                            data.entityName
            in
            div [ class "top-bar" ]
                [ div [ class "new-selection" ]
                    [ a [ href "/admin/reports/completion" ]
                        [ button []
                            [ text <| translate language Translate.NewScope ]
                        ]
                    ]
                , div [ class "scope" ]
                    [ text <| translate language Translate.Scope ++ ": " ++ scopeLabel ]
                ]

        dateInputs =
            Maybe.map
                (\reportType ->
                    let
                        startDateInput =
                            let
                                dateSelectorConfig =
                                    { select = SetStartDate
                                    , close = SetStartDateSelectorState Nothing
                                    , dateFrom = launchDate
                                    , dateTo = currentDate
                                    , dateDefault = Just launchDate
                                    }

                                dateForView =
                                    Maybe.map formatDDMMYYYY model.startDate
                                        |> Maybe.withDefault ""
                            in
                            div
                                [ class "form-input date"
                                , onClick <| SetStartDateSelectorState (Just dateSelectorConfig)
                                ]
                                [ text dateForView ]
                                |> wrapSelectListInput language Translate.SelectStartDate False

                        limitDateInput =
                            if
                                -- Reports requires setting start date before
                                -- limit date can be shown.
                                isNothing model.startDate
                            then
                                emptyNode

                            else
                                let
                                    dateFrom =
                                        Maybe.withDefault launchDate model.startDate

                                    dateSelectorConfig =
                                        { select = SetLimitDate
                                        , close = SetLimitDateSelectorState Nothing
                                        , dateFrom = dateFrom
                                        , dateTo = currentDate
                                        , dateDefault = Just currentDate
                                        }

                                    limitDateForView =
                                        Maybe.map formatDDMMYYYY model.limitDate
                                            |> Maybe.withDefault ""
                                in
                                div
                                    [ class "form-input date"
                                    , onClick <| SetLimitDateSelectorState (Just dateSelectorConfig)
                                    ]
                                    [ text limitDateForView ]
                                    |> wrapSelectListInput language Translate.SelectLimitDate False
                    in
                    [ startDateInput, limitDateInput ]
                )
                model.reportType
                |> Maybe.withDefault []

        content =
            if
                isJust model.startDateSelectorPopupState
                    || isJust model.limitDateSelectorPopupState
            then
                -- Date selector is open, so no need to calcualte
                -- intermediate results.
                emptyNode

            else
                Maybe.map3
                    (\reportType startDate limitDate ->
                        case reportType of
                            ReportNutritionIndividual ->
                                viewNutritionIndividualReport language startDate limitDate data.nutritionIndividualData
                    )
                    model.reportType
                    model.startDate
                    model.limitDate
                    |> Maybe.withDefault emptyNode
    in
    div [ class "page-content completion" ]
        [ topBar
        , div [ class "inputs" ] <|
            [ viewSelectListInput language
                model.reportType
                [ ReportNutritionIndividual ]
                reportTypeToString
                SetReportType
                Translate.CompletionReportType
                "select-input"
                |> wrapSelectListInput language Translate.ReportTypeLabel False
            ]
                ++ dateInputs
                ++ [ content ]
        , viewModal <| viewCalendarPopup language model.startDateSelectorPopupState model.startDate
        , viewModal <| viewCalendarPopup language model.limitDateSelectorPopupState model.limitDate
        ]


viewNutritionIndividualReport : Language -> NominalDate -> NominalDate -> List (EncounterData NutritionActivity) -> Html Msg
viewNutritionIndividualReport language startDate limitDate reportData =
    let
        filteredData =
            List.filter
                (\encounter ->
                    (not <| Date.compare encounter.startDate startDate == LT)
                        && (not <| Date.compare encounter.startDate limitDate == GT)
                )
                reportData
                |> generateNutritionReportData language
    in
    div [ class "report nutrition-individual" ] <|
        viewNutritionMetricsResultsTable filteredData


generateNutritionReportData :
    Language
    -> List (EncounterData NutritionActivity)
    -> MetricsResultsTableData
generateNutritionReportData language records =
    let
        count resolveFunc activity =
            List.filter (resolveFunc >> List.member activity) records
                |> List.length

        calcualtePercentage nominator total =
            if total == 0 then
                "0"

            else
                Round.round 3 ((toFloat nominator / toFloat total) * 100) ++ "%"
    in
    { heading = translate language Translate.NutritionIndividual
    , captions =
        [ translate language Translate.Activity
        , translate language Translate.Expected
        , translate language Translate.Completed
        , "%"
        ]
    , rows =
        List.map
            (\activity ->
                let
                    expected =
                        count .expectedActivities activity

                    completed =
                        count .completedActivities activity
                in
                [ translate language <| Translate.NutritionActivity activity
                , String.fromInt expected
                , String.fromInt completed
                , calcualtePercentage completed expected
                ]
            )
            allNutritionActivities
    }
