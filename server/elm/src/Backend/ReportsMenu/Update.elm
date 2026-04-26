module Backend.ReportsMenu.Update exposing (update)

import Backend.Model exposing (ModelBackend)
import Backend.ReportsMenu.Decoder exposing (decodeMenuData)
import Backend.ReportsMenu.Model exposing (Msg(..))
import Backend.Types exposing (BackendReturn)
import Error.Utils exposing (noError)
import Json.Decode exposing (decodeValue)


update : Msg -> ModelBackend -> BackendReturn Msg
update msg model =
    case msg of
        SetData value ->
            let
                modelUpdated =
                    { model | reportsMenuData = Just <| decodeValue decodeMenuData value }
            in
            BackendReturn
                modelUpdated
                Cmd.none
                noError
                []
