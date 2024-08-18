module Pages.Completion.Model exposing (..)

import Backend.Completion.Model exposing (NutritionChildActivity(..), TakenBy)
import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)


type alias Model =
    { reportType : Maybe ReportType
    , takenBy : Maybe TakenBy
    , startDate : Maybe Date
    , startDateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    , limitDate : Maybe Date
    , limitDateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


emptyModel : Model
emptyModel =
    { reportType = Nothing
    , takenBy = Nothing
    , startDate = Nothing
    , startDateSelectorPopupState = Nothing
    , limitDate = Nothing
    , limitDateSelectorPopupState = Nothing
    }


type ReportType
    = ReportNutritionIndividual


type Msg
    = NoOp
    | SetReportType String
    | SetTakenBy String
    | SetStartDate Date
    | SetStartDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SetLimitDate Date
    | SetLimitDateSelectorState (Maybe (DateSelectorConfig Msg))


allNutritionActivities : List NutritionChildActivity
allNutritionActivities =
    [ NutritionHeight
    , NutritionNutrition
    , NutritionPhoto
    , NutritionWeight
    , NutritionMUAC
    , NutritionContributingFactors
    , NutritionFollowUp
    , NutritionHealthEducation
    , NutritionSendToHC
    , NutritionNCDA
    ]
