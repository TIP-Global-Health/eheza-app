module Pages.Reports.Model exposing (..)

import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)


type alias Model =
    { limitDate : Maybe Date
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


emptyModel : Model
emptyModel =
    { limitDate = Nothing
    , dateSelectorPopupState = Nothing
    }


type Msg
    = SetLimitDate Date
    | SetLimitDateSelectorState (Maybe (DateSelectorConfig Msg))
