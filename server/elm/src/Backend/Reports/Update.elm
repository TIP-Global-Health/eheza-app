module Backend.Reports.Update exposing (update)

import Backend.Model exposing (ModelBackend)
import Backend.Reports.Decoder exposing (decodeReportsData)
import Backend.Reports.Model exposing (Msg(..))
import Backend.Types exposing (BackendReturn)
import Error.Utils exposing (noError)
import Gizra.NominalDate exposing (NominalDate)
import HttpBuilder exposing (..)
import Json.Decode exposing (decodeValue)
import Json.Encode
import RemoteData


update : NominalDate -> String -> Msg -> ModelBackend -> BackendReturn Msg
update currentDate backendUrl msg model =
    case msg of
        NoOp ->
            BackendReturn
                model
                Cmd.none
                noError
                []

        SetData value ->
            let
                _ =
                    Debug.log "backendUrl" backendUrl

                modelUpdated =
                    { model | reportsData = Just <| decodeValue decodeReportsData value }

                cmd =
                    let
                        params =
                            [ ( "app_type", Json.Encode.string "reports" )
                            , ( "base_revision", Json.Encode.string "0" )
                            ]
                    in
                    HttpBuilder.post (backendUrl ++ "/api/reports-data")
                        |> withJsonBody (Json.Encode.object params)
                        |> HttpBuilder.send (always NoOp)
            in
            BackendReturn
                modelUpdated
                cmd
                noError
                []
