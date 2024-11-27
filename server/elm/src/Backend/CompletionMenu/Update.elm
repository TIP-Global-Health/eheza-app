module Backend.CompletionMenu.Update exposing (update)

import Backend.CompletionMenu.Decoder exposing (decodeMenuData)
import Backend.CompletionMenu.Model exposing (Msg(..))
import Backend.Model exposing (ModelBackend)
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
                    { model | completionMenuData = Just <| decodeValue decodeMenuData value }
            in
            BackendReturn
                modelUpdated
                Cmd.none
                noError
                []
