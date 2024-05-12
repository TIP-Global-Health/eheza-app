module Pages.MessagingCenter.Model exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Gender)
import Backend.Nurse.Model exposing (Nurse, ResilienceRole)
import Backend.Person.Model exposing (EducationLevel, MaritalStatus, Ubudehe)
import Backend.ResilienceMessage.Model exposing (ResilienceMessage)
import Backend.ResilienceSurvey.Model
    exposing
        ( ResilienceSurveyQuestion
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
    , expandedMessages : EverySet ResilienceMessageNEWId
    , messageOptionsDialogState : Maybe MessageOptionsDialogState
    , kickOffForm : KickOffForm
    , monthlySurveyForm : MonthlySurveyForm
    , surveyScoreDialogState : Maybe SurveyScoreDialogState
    }


emptyModel : Model
emptyModel =
    { activeTab = TabUnread
    , tabScrollPosition = 0
    , expandedMessages = EverySet.empty
    , messageOptionsDialogState = Nothing
    , kickOffForm = emptyKickOffForm
    , monthlySurveyForm = emptyMonthlySurveyForm
    , surveyScoreDialogState = Nothing
    }


type MessagingTab
    = TabUnread
    | TabFavorites
    | TabGrowth
    | TabConnecting
    | TabSelfcare
    | TabStress
    | TabMindfullnes


type MessageOptionsDialogState
    = MessageOptionsStateMain ( ResilienceMessageNEWId, ResilienceMessage )
    | MessageOptionsStateReminder ( ResilienceMessageNEWId, ResilienceMessage )


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


type SurveyScoreDialogState
    = MonthlySurveyScore Int


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
    | SetSurveyScoreDialogState (Maybe SurveyScoreDialogState)
    | SetActiveTab MessagingTab
    | ScrollTab Int
    | ResilienceMessageClicked NurseId ResilienceMessageNEWId ResilienceMessage Bool
    | SetMessageOptionsDialogState (Maybe MessageOptionsDialogState)
    | ToggleMessageRead NurseId ResilienceMessageNEWId ResilienceMessage Bool
    | ToggleMessageFavorite NurseId ResilienceMessageNEWId ResilienceMessage
    | ScheduleMessageReminder Int NurseId ResilienceMessageNEWId ResilienceMessage
