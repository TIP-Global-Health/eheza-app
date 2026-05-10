module Pages.Reports.Model exposing (FbfDistributionCategory(..), Model, Msg(..), NutritionMetrics, NutritionMetricsResults, NutritionReportData, PregnancyTrimester(..), PrenatalContactType(..), ReportType(..), allFbfDistributionCategories, emptyModel, emptyNutritionMetrics)

import AssocList exposing (Dict)
import Backend.Components.Model exposing (PersonId)
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
    | ReportFBFDistribution
    | ReportNutrition
    | ReportPeripartum
    | ReportPostnatalCare
    | ReportPrenatal
    | ReportPrenatalContacts
    | ReportPrenatalDiagnoses


{-| Categories shown as rows in the FBF Distribution report. Only the FBF
group with `field_group_type = 'fbf'` contributes child distributions
(Achi-clinic child\_fbf is intentionally out of scope).
-}
type FbfDistributionCategory
    = FbfDistributionAhezaChild
    | FbfDistributionAhezaMother
    | FbfDistributionFbfChild
    | FbfDistributionFbfChildAchi
    | FbfDistributionFbfMother


allFbfDistributionCategories : List FbfDistributionCategory
allFbfDistributionCategories =
    [ FbfDistributionFbfChild
    , FbfDistributionFbfMother
    , FbfDistributionFbfChildAchi
    , FbfDistributionAhezaChild
    , FbfDistributionAhezaMother
    ]


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
