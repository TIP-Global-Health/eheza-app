module App.Model exposing (emptyModel, FileId(..), Flags, Msg(..), Model, Theme(..), ThemeConfig)

import App.PageType exposing (Page(..))
import Config.Model
import Date exposing (Date)
import Pages.Login.Model exposing (emptyModel, Model)
import RemoteData exposing (RemoteData(..), WebData)
import ParticipantManager.Model exposing (emptyModel, Model)
import Time exposing (Time)
import Translate exposing (Language(..))
import User.Model exposing (..)


type Msg
    = HandleOfflineEvent (Result String Bool)
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
    , currentDate : Date
    , dropzoneFile : Maybe FileId
    , language : Language
    , offline : Bool
    , pageLogin : Pages.Login.Model.Model
    , pageParticipant : ParticipantManager.Model.Model
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
    , config = NotAsked
    , currentDate = Date.fromTime 0
    , dropzoneFile = Nothing
    , language = English
    , offline = False
    , pageLogin = Pages.Login.Model.emptyModel
    , pageParticipant = ParticipantManager.Model.emptyModel
    , theme = Light
    , user = NotAsked
    }
