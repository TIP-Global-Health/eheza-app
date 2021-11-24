module DateSelector.SelectorPopup exposing (view)

import Date exposing (Date)
import DateSelector.Selector
import Html exposing (Html)


view : (Date -> msg) -> Date -> Date -> Maybe Date -> Html msg
view toSelect minimum maximum selected =
    DateSelector.Selector.viewPopup minimum maximum selected |> Html.map toSelect
