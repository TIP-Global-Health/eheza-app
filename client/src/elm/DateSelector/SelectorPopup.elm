module DateSelector.SelectorPopup exposing (view, viewCalendarPopup)

import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)
import DateSelector.Selector
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Translate exposing (Language, translate)


view : Language -> (Date -> msg) -> msg -> Date -> Date -> Maybe Date -> Html msg
view language toSelect toClose minimum maximum selected =
    div [ class "date-selector-popup" ]
        [ DateSelector.Selector.viewPopup language minimum maximum selected |> Html.map toSelect
        , div
            [ class "ui button"
            , onClick toClose
            ]
            [ text <| translate language Translate.Save ]
        ]


viewCalendarPopup : Language -> Maybe (DateSelectorConfig msg) -> Maybe Date -> Maybe (Html msg)
viewCalendarPopup language popupState selected =
    Maybe.map
        (\config ->
            div [ class "ui active modal calendar-popup" ]
                [ view language
                    config.select
                    config.close
                    config.dateFrom
                    config.dateTo
                    selected
                ]
        )
        popupState
