module Backend.Dashboard.Update exposing (update)

import Backend.Components.Decoder exposing (decodeMenuData)
import Backend.Dashboard.Model exposing (Msg(..))
import Backend.Model exposing (ModelBackend)
import Backend.Types exposing (BackendReturn)
import Error.Utils exposing (noError)
import Json.Decode exposing (decodeValue)


update : Msg -> ModelBackend -> BackendReturn Msg
update msg model =
    case msg of
        SetData value ->
            let
                modelUpdated =
                    { model | dashboardData = Just <| decodeValue decodeMenuData value }
            in
            BackendReturn
                modelUpdated
                Cmd.none
                noError
                []
