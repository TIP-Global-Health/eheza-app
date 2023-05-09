module Backend.Scoreboard.Update exposing (update)

import Backend.Model exposing (ModelBackend)
import Backend.Scoreboard.Decoder exposing (decodeScoreboardData)
import Backend.Scoreboard.Model exposing (Msg(..))
import Backend.Types exposing (BackendReturn)
import Error.Utils exposing (noError)
import Json.Decode exposing (decodeValue)


update : Msg -> ModelBackend -> BackendReturn Msg
update msg model =
    case msg of
        SetData value ->
            let
                _ =
                    Debug.log "value" value

                modelUpdated =
                    { model | scoreboardData = Just <| decodeValue decodeScoreboardData value }
            in
            BackendReturn
                modelUpdated
                Cmd.none
                noError
                []
