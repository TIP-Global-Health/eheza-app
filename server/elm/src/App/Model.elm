module App.Model exposing
    ( Flags
    , Model
    , Msg(..)
    , PagesReturn
    , emptyModel
    )

import App.Types exposing (Language(..), Page(..))
import Backend.Model
import Error.Model exposing (Error)
import Json.Decode exposing (Value)
import Pages.Scoreboard.Model
import Time


type alias PagesReturn subModel subMsg =
    { model : subModel
    , cmd : Cmd subMsg
    , error : Maybe Error
    , appMsgs : List Msg
    }


type Msg
    = MsgBackend Backend.Model.Msg
    | MsgScoreboardPage Pages.Scoreboard.Model.Msg
    | NoOp
    | SetActivePage Page
    | SetCurrentDate Time.Posix


type alias Flags =
    { appData : Value
    }


type alias Model =
    { backend : Backend.Model.ModelBackend
    , errors : List Error
    , language : Language
    , activePage : Page
    , currentDate : Time.Posix
    , scoreboardPage : Pages.Scoreboard.Model.Model
    }


emptyModel : Model
emptyModel =
    { backend = Backend.Model.emptyModelBackend
    , errors = []
    , language = English
    , activePage = Scoreboard
    , currentDate = Time.millisToPosix 0
    , scoreboardPage = Pages.Scoreboard.Model.emptyModel
    }
