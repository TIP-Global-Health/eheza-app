module Pages.MessagingCenter.Model exposing (..)

import AssocList exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Gender)
import Backend.Nurse.Model exposing (Nurse, ResilienceRole)
import Backend.Person.Model exposing (EducationLevel, MaritalStatus, Ubudehe)
import Backend.ResilienceSurvey.Model exposing (ResilienceSurveyQuestionOption)
import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { -- filter : Maybe CaseManagementFilter
      -- , dialogState : Maybe FollowUpEncounterDataType
      kickOffForm : KickOffForm
    , monthlySurveyForm : MonthlySurveyForm
    }


emptyModel : Model
emptyModel =
    { --  filter = Nothing
      -- , dialogState = Nothing
      kickOffForm = emptyKickOffForm
    , monthlySurveyForm = emptyMonthlySurveyForm
    }


type alias KickOffForm =
    { role : Maybe ResilienceRole
    , birthDate : Maybe NominalDate
    , gender : Maybe Gender
    , educationLevel : Maybe EducationLevel
    , ubudehe : Maybe Ubudehe
    , maritalStatus : Maybe MaritalStatus
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    }


emptyKickOffForm : KickOffForm
emptyKickOffForm =
    { role = Nothing
    , birthDate = Nothing
    , gender = Nothing
    , educationLevel = Nothing
    , ubudehe = Nothing
    , maritalStatus = Nothing
    , dateSelectorPopupState = Nothing
    }


type alias MonthlySurveyForm =
    { answer1 : Maybe ResilienceSurveyQuestionOption
    , answer2 : Maybe ResilienceSurveyQuestionOption
    , answer3 : Maybe ResilienceSurveyQuestionOption
    , answer4 : Maybe ResilienceSurveyQuestionOption
    }


emptyMonthlySurveyForm : MonthlySurveyForm
emptyMonthlySurveyForm =
    { answer1 = Nothing
    , answer2 = Nothing
    , answer3 = Nothing
    , answer4 = Nothing
    }


type Msg
    = SetActivePage Page
    | SetRole String
    | SetBirthDate Date
    | SetBirthDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SetGender String
    | SetEducationLevel String
    | SetUbudehe String
    | SetMaritalStatus String
    | SaveKickOffSurvey NurseId Nurse
    | SetMonthlySurveyAnswer (ResilienceSurveyQuestionOption -> MonthlySurveyForm -> MonthlySurveyForm) ResilienceSurveyQuestionOption
