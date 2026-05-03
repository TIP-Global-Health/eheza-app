module Pages.Completion.Model exposing (Model, Msg(..), ReportType(..), emptyModel)

import Backend.Completion.Model exposing (TakenBy)
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
    = ReportAcuteIllness
    | ReportChildScoreboard
    | ReportHIV
    | ReportHomeVisit
    | ReportNCD
    | ReportNewbornExam
    | ReportNutritionGroup
    | ReportNutritionIndividual
    | ReportPrenatal
    | ReportTuberculosis
    | ReportWellChild


type Msg
    = SetReportType String
    | SetTakenBy String
    | SetStartDate Date
    | SetStartDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SetLimitDate Date
    | SetLimitDateSelectorState (Maybe (DateSelectorConfig Msg))
