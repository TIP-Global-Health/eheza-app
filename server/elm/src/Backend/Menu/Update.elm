module Backend.Menu.Update exposing (update)

import Backend.Menu.Decoder exposing (decodeMenuData)
import Backend.Menu.Model exposing (Msg(..))
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
                    { model | menuData = Just <| decodeValue decodeMenuData value }
            in
            BackendReturn
                modelUpdated
                Cmd.none
                noError
                []
