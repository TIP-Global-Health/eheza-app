module Pages.Reports.Model exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Reports.Model exposing (PatientData, PersonId)
import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)
import RemoteData exposing (RemoteData(..))


type alias Model =
    { reportType : Maybe ReportType
    , limitDate : Maybe Date
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    , nutritionReportData : RemoteData String NutritionReportData
    }


emptyModel : Model
emptyModel =
    { reportType = Nothing
    , limitDate = Nothing
    , dateSelectorPopupState = Nothing
    , nutritionReportData = NotAsked
    }


type ReportType
    = ReportDemographics
    | ReportNutrition
    | ReportPrenatal


type alias NutritionReportData =
    { impacted : List PersonId
    , encountersByMonth : Dict ( Int, Int ) NutritionMetrics
    }


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


type alias NutritionMetricsResults =
    { stuntingModerate : Float
    , stuntingSevere : Float
    , wastingModerate : Float
    , wastingSevere : Float
    , underweightModerate : Float
    , underweightSevere : Float
    }


emptyNutritionMetricsResults : NutritionMetricsResults
emptyNutritionMetricsResults =
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
    | NutritionReportDataCalculationCompleted (Result String NutritionReportData)
