module App.Model exposing (emptyModel, FileId(..), Flags, Msg(..), Model, Theme(..), ThemeConfig)

import App.PageType exposing (Page(..))
import Clinic.Model exposing (ClinicId, Clinic)
import Config.Model
import Date exposing (Date)
import Drupal.Restful exposing (EntityDictList)
import Gizra.NominalDate exposing (NominalDate)
import Pages.Login.Model exposing (emptyModel, Model)
import RemoteData exposing (RemoteData(..), WebData)
import ParticipantManager.Model exposing (emptyModel, Model)
import Session.Model exposing (SessionId, Session)
import Time exposing (Time)
import Translate exposing (Language(..))
import User.Model exposing (..)


type Msg
    = FetchClinics
    | FetchSessionsOpenOn NominalDate
    | HandleFetchedClinics (WebData (EntityDictList ClinicId Clinic))
    | HandleFetchedSessions NominalDate (WebData (EntityDictList SessionId Session))
    | HandleOfflineEvent (Result String Bool)
    | Logout
    | MsgParticipantManager ParticipantManager.Model.Msg
    | PageLogin Pages.Login.Model.Msg
    | RedirectByActivePage
    | SetActivePage Page
    | SetCurrentDate Date
    | ThemeSwitch Theme
    | Tick Time


type alias Model =
    { accessToken : String
    , activePage : Page
    , config : RemoteData String Config.Model.Model
    , clinics : WebData (EntityDictList ClinicId Clinic)
    , currentDate : Date
    , dropzoneFile : Maybe FileId
    , language : Language
    , offline : Bool
    , pageLogin : Pages.Login.Model.Model
    , pageParticipant : ParticipantManager.Model.Model

    -- This tracks which sessions are currently available for data-entry,
    -- given the scheduled date range for each session. We remember which
    -- date we asked about, so that if the date changes (i.e. it becomes
    -- tomorrow, due to the passage of time), we can know that we ought to
    -- ask again.
    , openSessions : WebData ( NominalDate, EntityDictList SessionId Session )
    , theme : Theme
    , user : WebData User
    }


type FileId
    = FileId Int


type alias Flags =
    { accessToken : String
    , hostname : String
    }


type Theme
    = Dark
    | Light


type alias ThemeConfig =
    { from : String
    , to : String
    }


emptyModel : Model
emptyModel =
    { accessToken = ""
    , activePage = Login
    , clinics = NotAsked
    , config = NotAsked
    , currentDate = Date.fromTime 0
    , dropzoneFile = Nothing
    , language = English
    , offline = False
    , openSessions = NotAsked
    , pageLogin = Pages.Login.Model.emptyModel
    , pageParticipant = ParticipantManager.Model.emptyModel
    , theme = Light
    , user = NotAsked
    }
