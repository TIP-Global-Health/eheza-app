module Backend.Model exposing (..)

{-| This model basically represents things we have locally which also belong
on the backend. So, conceptually it is a kind of a local cache of some of the
things on the backend.
-}

import Backend.Menu.Model exposing (MenuData)
import Backend.Scoreboard.Model exposing (ScoreboardData)
import Json.Decode


type alias ModelBackend =
    { menuData : Maybe (Result Json.Decode.Error MenuData)
    , scoreboardData : Maybe (Result Json.Decode.Error ScoreboardData)
    }


emptyModelBackend : ModelBackend
emptyModelBackend =
    { menuData = Nothing
    , scoreboardData = Nothing
    }


{-| These are all the messages related to getting things from the backend and
putting things back into the backend.
-}
type Msg
    = MsgMenu Backend.Menu.Model.Msg
    | MsgScoreboard Backend.Scoreboard.Model.Msg
