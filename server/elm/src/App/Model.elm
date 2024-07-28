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
import Pages.Reports.Model
import Pages.ReportsMenu.Model
import Pages.Scoreboard.Model
import Pages.ScoreboardMenu.Model
import Time


type alias PagesReturn subModel subMsg =
    { model : subModel
    , cmd : Cmd subMsg
    , error : Maybe Error
    , appMsgs : List Msg
    }


type Msg
    = MsgBackend Backend.Model.Msg
    | MsgScoreboardMenuPage Pages.ScoreboardMenu.Model.Msg
    | MsgScoreboardPage Pages.Scoreboard.Model.Msg
    | MsgReportsMenuPage Pages.ReportsMenu.Model.Msg
    | MsgReportsPage Pages.Reports.Model.Msg
    | SetCurrentTime Time.Posix


type alias Flags =
    { -- Type of application.
      page : String
    , appData : Value
    , themePath : String
    }


type alias Model =
    { backend : Backend.Model.ModelBackend
    , errors : List Error
    , language : Language
    , activePage : Page
    , themePath : String
    , currentTime : Time.Posix
    , scoreboardMenuPage : Pages.ScoreboardMenu.Model.Model
    , scoreboardPage : Pages.Scoreboard.Model.Model
    , reportsMenuPage : Pages.ReportsMenu.Model.Model
    , reportsPage : Pages.Reports.Model.Model
    }


emptyModel : Model
emptyModel =
    { backend = Backend.Model.emptyModelBackend
    , errors = []
    , language = English
    , activePage = NotFound
    , themePath = ""
    , currentTime = Time.millisToPosix 0
    , scoreboardMenuPage = Pages.ScoreboardMenu.Model.emptyModel
    , scoreboardPage = Pages.Scoreboard.Model.emptyModel
    , reportsMenuPage = Pages.ReportsMenu.Model.emptyModel
    , reportsPage = Pages.Reports.Model.emptyModel
    }
