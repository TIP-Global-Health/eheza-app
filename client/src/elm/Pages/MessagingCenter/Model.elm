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
        , ResilienceSurveyType
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
    , messageOptionsDialogState : Maybe MessageOptionsDialogState
    , kickOffForm : KickOffForm
    , surveyForm : SurveyForm
    , surveyScoreDialogState : Maybe SurveyScoreDialogState
    , consentForm : ConsentForm
    , hasGivenConsent : Bool
    }


type ReasonForNotConsenting
    = ManyOtherCommitments
    | NoDedicatedTimeForTheProgram
    | ProgramNotAddressingMyStressors
    | DontWantToBeSeenAsStruggling
    | TriedSimilarProgramBefore
    | NotInterestedInProgram


type alias ConsentForm =
    { agreesToParticipate : Maybe Bool
    , reasonsToNotConsent : Maybe ReasonForNotConsenting
    }


emptyConsentForm : ConsentForm
emptyConsentForm =
    { agreesToParticipate = Nothing
    , reasonsToNotConsent = Nothing
    }


emptyModel : Model
emptyModel =
    { activeTab = TabUnread
    , tabScrollPosition = 0
    , expandedMessages = EverySet.empty
    , messageOptionsDialogState = Nothing
    , kickOffForm = emptyKickOffForm
    , surveyForm = emptySurveyForm
    , surveyScoreDialogState = Nothing
    , consentForm = emptyConsentForm
    , hasGivenConsent = False
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
    = MessageOptionsStateMain ( ResilienceMessageId, ResilienceMessage )
    | MessageOptionsStateReminder ( ResilienceMessageId, ResilienceMessage )


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


type alias SurveyForm =
    Dict ResilienceSurveyQuestion ResilienceSurveyQuestionOption


emptySurveyForm : SurveyForm
emptySurveyForm =
    Dict.empty


type SurveyScoreDialogState
    = QuarterlySurveyScore Int
    | AdoptionSurveyScore (List Int)


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
    | SetSurveyAnswer ResilienceSurveyQuestion ResilienceSurveyQuestionOption
    | SaveSurvey ResilienceSurveyType NurseId
    | SetSurveyScoreDialogState (Maybe SurveyScoreDialogState)
    | SetActiveTab MessagingTab
    | ScrollTab Int
    | ResilienceMessageClicked ResilienceMessageId NurseId Nurse Bool
    | SetMessageOptionsDialogState (Maybe MessageOptionsDialogState)
    | ToggleMessageRead ResilienceMessageId NurseId Nurse Bool
    | ToggleMessageFavorite ResilienceMessageId NurseId Nurse
    | ScheduleMessageReminder Int ResilienceMessageId NurseId Nurse
    | SetConsentAgree Bool
    | SaveConsent NurseId Nurse
