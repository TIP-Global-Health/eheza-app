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
import Pages.Menu.Model
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
    | MsgMenuPage Pages.Menu.Model.Msg
    | MsgScoreboardPage Pages.Scoreboard.Model.Msg
    | SetCurrentTime Time.Posix


type alias Flags =
    { -- Type of application.
      page : String
    , appData : Value
    }


type alias Model =
    { backend : Backend.Model.ModelBackend
    , errors : List Error
    , language : Language
    , activePage : Page
    , currentTime : Time.Posix
    , menuPage : Pages.Menu.Model.Model
    , scoreboardPage : Pages.Scoreboard.Model.Model
    }


emptyModel : Model
emptyModel =
    { backend = Backend.Model.emptyModelBackend
    , errors = []
    , language = English
    , activePage = NotFound
    , currentTime = Time.millisToPosix 0
    , menuPage = Pages.Menu.Model.emptyModel
    , scoreboardPage = Pages.Scoreboard.Model.emptyModel
    }
