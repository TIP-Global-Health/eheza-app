module DateSelector.Model exposing (..)

import Date exposing (Date)


type alias DateSelectorConfig msg =
    { select : Date -> msg
    , close : msg
    , dateFrom : Date
    , dateTo : Date
    , dateDefault : Maybe Date
    }
