module Pages.Reports.Model exposing (..)

import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)


type alias Model =
    { reportType : Maybe ReportType
    , limitDate : Maybe Date
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


emptyModel : Model
emptyModel =
    { reportType = Nothing
    , limitDate = Nothing
    , dateSelectorPopupState = Nothing
    }


type ReportType
    = ReportDemographics
    | ReportNutrition


type Msg
    = SetReportType String
    | SetLimitDate Date
    | SetLimitDateSelectorState (Maybe (DateSelectorConfig Msg))
