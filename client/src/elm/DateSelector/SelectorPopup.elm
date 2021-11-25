module DateSelector.SelectorPopup exposing (view)

import Date exposing (Date)
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
            [ text <| translate language Translate.Close ]
        ]
