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
import Pages.Model exposing (MetricsResultsTableData)
import Pages.Utils exposing (viewStandardCells, viewStandardRow)
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

        reportData =
            generateNutritionReportData language data.nutritionIndividualData

        captionsRow =
            viewStandardCells reportData.captions
                |> div [ class "row captions" ]
    in
    div [ class "page-content completion" ]
        [ topBar
        , div [ class "inputs" ]
            [ div [ class "report" ]
                [ div [ class "table" ] <|
                    captionsRow
                        :: List.map viewStandardRow reportData.rows
                ]
            ]
        ]


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
