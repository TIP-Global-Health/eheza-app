module Backend.ScoreboardMenu.Update exposing (update)

import Backend.Model exposing (ModelBackend)
import Backend.ScoreboardMenu.Decoder exposing (decodeMenuData)
import Backend.ScoreboardMenu.Model exposing (Msg(..))
import Backend.Types exposing (BackendReturn)
import Error.Utils exposing (noError)
import Gizra.NominalDate exposing (NominalDate)
import Json.Decode exposing (decodeValue)


update : Msg -> ModelBackend -> BackendReturn Msg
update msg model =
    case msg of
        SetData value ->
            let
                modelUpdated =
                    { model | scoreboardMenuData = Just <| decodeValue decodeMenuData value }
            in
            BackendReturn
                modelUpdated
                Cmd.none
                noError
                []
