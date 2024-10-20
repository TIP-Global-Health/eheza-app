module Backend.Completion.Update exposing (update)

import Backend.Completion.Decoder exposing (decodeCompletionData)
import Backend.Completion.Model exposing (Msg(..))
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
                    { model | completionData = Just <| decodeValue decodeCompletionData value }
            in
            BackendReturn
                modelUpdated
                Cmd.none
                noError
                []
