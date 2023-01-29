module Pages.MessagingCenter.Model exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Gender)
import Backend.Nurse.Model exposing (Nurse, ResilienceRole)
import Backend.Person.Model exposing (EducationLevel, MaritalStatus, Ubudehe)
import Backend.ResilienceMessage.Model exposing (ResilienceMessage)
import Backend.ResilienceSurvey.Model
    exposing
        ( ResilienceSurvey
        , ResilienceSurveyQuestion
        , ResilienceSurveyQuestionOption
        )
import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { activeTab : MessagingTab
    , tabScrollPosition : Int
    , expandedMessages : EverySet ResilienceMessageId
    , messageOptionsDialogState : Maybe ( ResilienceMessageId, ResilienceMessage )
    , kickOffForm : KickOffForm
    , monthlySurveyForm : MonthlySurveyForm
    }


emptyModel : Model
emptyModel =
    { activeTab = TabUnread
    , tabScrollPosition = 0
    , expandedMessages = EverySet.empty
    , messageOptionsDialogState = Nothing
    , kickOffForm = emptyKickOffForm
    , monthlySurveyForm = emptyMonthlySurveyForm
    }


type MessagingTab
    = TabUnread
    | TabFavorites
    | TabGrowth
    | TabConnecting
    | TabSelfcare
    | TabStress
    | TabMindfullnes


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
    Dict ResilienceSurveyQuestion ResilienceSurveyQuestionOption


emptyMonthlySurveyForm : MonthlySurveyForm
emptyMonthlySurveyForm =
    Dict.empty


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
    | SetMonthlySurveyAnswer ResilienceSurveyQuestion ResilienceSurveyQuestionOption
    | SaveMonthlySurvey NurseId
    | SetActiveTab MessagingTab
    | ScrollTab Int
    | ResilienceMessageClicked NurseId ResilienceMessageId ResilienceMessage Bool
    | SetMessageOptionsDialogState (Maybe ( ResilienceMessageId, ResilienceMessage ))
