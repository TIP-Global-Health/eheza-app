module App.Model exposing (emptyModel, FileId(..), Flags, Msg(..), Model)

import App.PageType exposing (Page(..))
import Config.Model
import Date exposing (Date)
import Pages.Login.Model exposing (emptyModel, Model)
import RemoteData exposing (RemoteData(..), WebData)
import PatientManager.Model exposing (emptyModel, Model)
import Time exposing (Time)
import Translate exposing (Language(..))
import User.Model exposing (..)


type Msg
    = HandleDropzoneNewFile (Maybe Int)
    | HandleOfflineEvent (Result String Bool)
    | Logout
    | MsgPatientManager PatientManager.Model.Msg
    | PageLogin Pages.Login.Model.Msg
    | SetActivePage Page
    | SetCurrentDate Date
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
    , pagePatient : PatientManager.Model.Model
    , user : WebData User
    }


type FileId
    = FileId Int


type alias Flags =
    { accessToken : String
    , hostname : String
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
    , pagePatient = PatientManager.Model.emptyModel
    , user = NotAsked
    }
