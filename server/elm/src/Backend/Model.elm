module Backend.Model exposing (..)

{-| This model basically represents things we have locally which also belong
on the backend. So, conceptually it is a kind of a local cache of some of the
things on the backend.
-}

import Backend.Reports.Model exposing (ReportsData)
import Backend.ReportsMenu.Model
import Backend.Scoreboard.Model exposing (ScoreboardData)
import Backend.ScoreboardMenu.Model
import Json.Decode


type alias ModelBackend =
    { scoreboardMenuData : Maybe (Result Json.Decode.Error Backend.ScoreboardMenu.Model.MenuData)
    , scoreboardData : Maybe (Result Json.Decode.Error ScoreboardData)
    , reportsMenuData : Maybe (Result Json.Decode.Error Backend.ReportsMenu.Model.MenuData)
    , reportsData : Maybe (Result Json.Decode.Error ReportsData)
    }


emptyModelBackend : ModelBackend
emptyModelBackend =
    { scoreboardMenuData = Nothing
    , scoreboardData = Nothing
    , reportsMenuData = Nothing
    , reportsData = Nothing
    }


{-| These are all the messages related to getting things from the backend and
putting things back into the backend.
-}
type Msg
    = MsgScoreboardMenu Backend.ScoreboardMenu.Model.Msg
    | MsgScoreboard Backend.Scoreboard.Model.Msg
    | MsgReports Backend.Reports.Model.Msg
    | MsgReportsMenu Backend.ReportsMenu.Model.Msg
