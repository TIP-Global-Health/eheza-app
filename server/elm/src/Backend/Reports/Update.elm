module Backend.Reports.Update exposing (update)

import Backend.Model exposing (ModelBackend)
import Backend.Reports.Decoder exposing (decodeReportsData)
import Backend.Reports.Model exposing (Msg(..))
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
                    { model | reportsData = Just <| decodeValue decodeReportsData value }
            in
            BackendReturn
                modelUpdated
                Cmd.none
                noError
                []
