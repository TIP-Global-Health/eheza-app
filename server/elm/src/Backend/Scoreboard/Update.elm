module Backend.Scoreboard.Update exposing (update)

import Backend.Model exposing (ModelBackend)
import Backend.Scoreboard.Decoder exposing (decodeScoreboardData)
import Backend.Scoreboard.Model exposing (Msg(..))
import Backend.Types exposing (BackendReturn)
import Error.Utils exposing (noError)
import Gizra.NominalDate exposing (NominalDate)
import Json.Decode exposing (decodeValue)


update : NominalDate -> Msg -> ModelBackend -> BackendReturn Msg
update currentDate msg model =
    case msg of
        SetData value ->
            let
                modelUpdated =
                    { model | scoreboardData = Just <| decodeValue (decodeScoreboardData currentDate) value }
            in
            BackendReturn
                modelUpdated
                Cmd.none
                noError
                []
