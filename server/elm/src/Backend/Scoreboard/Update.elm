module Backend.Scoreboard.Update exposing (update)

import AssocList as Dict exposing (Dict)
import Backend.Model exposing (ModelBackend)
import Backend.Scoreboard.Decoder exposing (decodeScoreboardData)
import Backend.Scoreboard.Model exposing (Msg(..))
import Backend.Types exposing (BackendReturn)
import Error.Utils exposing (noError)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Json.Decode exposing (decodeValue)
import Result


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
