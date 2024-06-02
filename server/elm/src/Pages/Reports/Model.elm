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


type alias NutritionMetrics =
    { stuntingNormal : Int
    , stuntingModerate : Int
    , stuntingSevere : Int
    , wastingNormal : Int
    , wastingModerate : Int
    , wastingSevere : Int
    , underweightNormal : Int
    , underweightModerate : Int
    , underweightSevere : Int
    }


emptyNutritionMetrics : NutritionMetrics
emptyNutritionMetrics =
    { stuntingNormal = 0
    , stuntingModerate = 0
    , stuntingSevere = 0
    , wastingNormal = 0
    , wastingModerate = 0
    , wastingSevere = 0
    , underweightNormal = 0
    , underweightModerate = 0
    , underweightSevere = 0
    }


type alias NutritionIncidence =
    { stuntingModerate : Float
    , stuntingSevere : Float
    , wastingModerate : Float
    , wastingSevere : Float
    , underweightModerate : Float
    , underweightSevere : Float
    }


type Msg
    = SetReportType String
    | SetLimitDate Date
    | SetLimitDateSelectorState (Maybe (DateSelectorConfig Msg))
