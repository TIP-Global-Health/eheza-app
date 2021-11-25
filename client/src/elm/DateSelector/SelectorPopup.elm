module DateSelector.SelectorPopup exposing (view)

import Date exposing (Date)
import DateSelector.Selector
import Html exposing (Html)
import Translate exposing (Language)


view : Language -> (Date -> msg) -> Date -> Date -> Maybe Date -> Html msg
view language toSelect minimum maximum selected =
    DateSelector.Selector.viewPopup language minimum maximum selected |> Html.map toSelect
