module Pages.Reports.Model exposing (Model, Msg(..), NutritionMetrics, NutritionMetricsResults, NutritionReportData, PregnancyTrimester(..), PrenatalContactType(..), ReportType(..), emptyModel, emptyNutritionMetrics)

import AssocList exposing (Dict)
import Backend.Reports.Model exposing (PersonId)
import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)
import RemoteData exposing (RemoteData(..))


type alias Model =
    { reportType : Maybe ReportType
    , startDate : Maybe Date
    , startDateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    , limitDate : Maybe Date
    , limitDateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    , nutritionReportData : RemoteData String NutritionReportData
    }


emptyModel : Model
emptyModel =
    { reportType = Nothing
    , startDate = Nothing
    , startDateSelectorPopupState = Nothing
    , limitDate = Nothing
    , limitDateSelectorPopupState = Nothing
    , nutritionReportData = NotAsked
    }


type ReportType
    = ReportAcuteIllness
    | ReportDemographics
    | ReportNutrition
    | ReportPrenatal
    | ReportPrenatalContacts
    | ReportPrenatalDiagnoses


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
    , acuteMalnutritionNormal : List PersonId
    , acuteMalnutritionMam : List PersonId
    , acuteMalnutritionSam : List PersonId
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
    , acuteMalnutritionNormal = []
    , acuteMalnutritionMam = []
    , acuteMalnutritionSam = []
    }


type alias NutritionMetricsResults =
    { stuntingModerate : Float
    , stuntingSevere : Float
    , wastingModerate : Float
    , wastingSevere : Float
    , underweightModerate : Float
    , underweightSevere : Float
    , acuteMalnutritionMam : Float
    , acuteMalnutritionSam : Float
    }


type PregnancyTrimester
    = FirstTrimester
    | SecondTrimester
    | ThirdTrimester


type PrenatalContactType
    = PrenatalContact1
    | PrenatalContact2
    | PrenatalContact3
    | PrenatalContact4
    | PrenatalContact5
    | PrenatalContact6
    | PrenatalContact7
    | PrenatalContact8


type Msg
    = SetReportType String
    | SetStartDate Date
    | SetStartDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SetLimitDate Date
    | SetLimitDateSelectorState (Maybe (DateSelectorConfig Msg))
    | NutritionReportDataCalculationCompleted (Result String NutritionReportData)
    | DownloadCSV String String
