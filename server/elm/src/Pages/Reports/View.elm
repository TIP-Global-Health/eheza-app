module Pages.Reports.View exposing (view)

import App.Types exposing (Language, Site)
import AssocList as Dict exposing (Dict)
import Backend.Model exposing (ModelBackend)
import Backend.Reports.Model exposing (Gender(..), ReportsData)
import Date exposing (Interval(..), Unit(..))
import DateSelector.SelectorPopup exposing (viewCalendarPopup)
import Gizra.Html exposing (emptyNode)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Pages.Reports.Model exposing (..)
import Pages.Reports.Utils exposing (..)
import Pages.Utils exposing (wrapSelectListInput)
import Translate exposing (TranslationId, translate)
import Utils.Html exposing (viewModal)


view : Language -> NominalDate -> ModelBackend -> Model -> Html Msg
view language currentDate modelBackend model =
    case modelBackend.reportsData of
        Just (Ok data) ->
            viewReportsData language currentDate data model

        Just (Err err) ->
            text <| Debug.toString err

        Nothing ->
            emptyNode


viewReportsData : Language -> NominalDate -> ReportsData -> Model -> Html Msg
viewReportsData language currentDate data model =
    let
        topBar =
            div [ class "top-bar" ]
                [ div [ class "new-selection" ]
                    [ a [ href "/admin/reports/aggregated-reports" ]
                        [ button []
                            [ text <| translate language Translate.NewSelection ]
                        ]
                    ]
                ]

        dateSelectorConfig =
            { select = SetLimitDate
            , close = SetLimitDateSelectorState Nothing
            , dateFrom = Date.add Years -6 currentDate
            , dateTo = Date.add Days -1 currentDate
            , dateDefault = Nothing
            }

        limitDateForView =
            Maybe.map formatDDMMYYYY model.limitDate
                |> Maybe.withDefault ""

        results =
            Maybe.map
                (\limitDate ->
                    let
                        ( males, females ) =
                            List.filter
                                (\record ->
                                    Date.compare record.created limitDate == LT
                                )
                                data.records
                                |> List.partition (.gender >> (==) Male)

                        totalMales =
                            "Males: " ++ String.fromInt (List.length males)

                        totalFemales =
                            "Females: " ++ String.fromInt (List.length females)

                        total =
                            "Total: " ++ String.fromInt (List.length <| males ++ females)
                    in
                    div [ class "label" ] [ text <| totalMales ++ ", " ++ totalFemales ++ ", " ++ total ]
                        |> wrapSelectListInput language Translate.Result False
                )
                model.limitDate
                |> Maybe.withDefault emptyNode
    in
    div [ class "page-content" ]
        [ topBar
        , div [ class "inputs" ]
            [ div
                [ class "form-input date"
                , onClick <| SetLimitDateSelectorState (Just dateSelectorConfig)
                ]
                [ text limitDateForView ]
                |> wrapSelectListInput language Translate.SelectLimitDate False
            , results
            ]
        , viewModal <| viewCalendarPopup language model.dateSelectorPopupState model.limitDate
        ]
