module Pages.MessagingCenter.Model exposing (..)

import AssocList exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (Gender)
import Backend.Nurse.Model exposing (ResilienceRole)
import Backend.Person.Model exposing (EducationLevel, MaritalStatus, Ubudehe)
import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Page exposing (Page)


type alias Model =
    { -- filter : Maybe CaseManagementFilter
      -- , dialogState : Maybe FollowUpEncounterDataType
      kickOffForm : KickOffForm
    }


emptyModel : Model
emptyModel =
    { --  filter = Nothing
      -- , dialogState = Nothing
      kickOffForm = emptyKickOffForm
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


type Msg
    = SetActivePage Page
    | SetRole String
    | SetBirthDate Date
    | SetBirthDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SetGender String
    | SetEducationLevel String
    | SetUbudehe String
    | SetMaritalStatus String
