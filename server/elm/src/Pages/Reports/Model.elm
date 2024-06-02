module Pages.Reports.Model exposing (..)

import Backend.Reports.Model exposing (PersonId)
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
    { stuntingNormal : List PersonId
    , stuntingModerate : List PersonId
    , stuntingSevere : List PersonId
    , wastingNormal : List PersonId
    , wastingModerate : List PersonId
    , wastingSevere : List PersonId
    , underweightNormal : List PersonId
    , underweightModerate : List PersonId
    , underweightSevere : List PersonId
    }


emptyNutritionMetrics : NutritionMetrics
emptyNutritionMetrics =
    { stuntingNormal = []
    , stuntingModerate = []
    , stuntingSevere = []
    , wastingNormal = []
    , wastingModerate = []
    , wastingSevere = []
    , underweightNormal = []
    , underweightModerate = []
    , underweightSevere = []
    }


type alias NutritionPrevalence =
    { stuntingModerate : Float
    , stuntingSevere : Float
    , wastingModerate : Float
    , wastingSevere : Float
    , underweightModerate : Float
    , underweightSevere : Float
    }


emptyNutritionPrevalence : NutritionPrevalence
emptyNutritionPrevalence =
    { stuntingModerate = 0
    , stuntingSevere = 0
    , wastingModerate = 0
    , wastingSevere = 0
    , underweightModerate = 0
    , underweightSevere = 0
    }


type Msg
    = SetReportType String
    | SetLimitDate Date
    | SetLimitDateSelectorState (Maybe (DateSelectorConfig Msg))
