module Backend.Components.Sync exposing
    ( Config
    , handleSendRequest
    , handleSetData
    , handleSyncResponse
    )

{-| Shared sync loop for the paginated /api/reports-data endpoint.

Reports, Scoreboard and Completion all hit the same endpoint with the same
cursor-based protocol; the only differences are the `app_type` literal, the
data/response decoders, and how a response merges into the page's data
record. This module captures that protocol once; each caller supplies a
`Config` and forwards its three `Msg` constructors to `handleSetData`,
`handleSendRequest`, `handleSyncResponse`.

-}

import Backend.Components.Encoder exposing (encodeReportParams)
import Backend.Components.Model exposing (ReportParams)
import Backend.Model exposing (ModelBackend)
import Backend.Types exposing (BackendReturn)
import Error.Utils exposing (noError)
import HttpBuilder exposing (withExpectJson, withHeader, withJsonBody)
import Json.Decode exposing (Decoder, Value, decodeValue)
import Json.Encode exposing (object, string)
import RemoteData exposing (WebData)


type alias Config data syncResponse msg =
    { appType : String
    , backendUrl : String
    , csrfToken : String
    , dataDecoder : Decoder data
    , syncResponseDecoder : Decoder syncResponse
    , getData : ModelBackend -> Maybe (Result Json.Decode.Error data)
    , setData : Maybe (Result Json.Decode.Error data) -> ModelBackend -> ModelBackend
    , getParams : data -> ReportParams
    , mergeResponse : syncResponse -> data -> data
    , getRemaining : syncResponse -> Int
    , getLastIdSynced : syncResponse -> Int
    , wrapHandleResponse : WebData syncResponse -> msg
    }


handleSetData : Config data sr msg -> Value -> ModelBackend -> BackendReturn msg
handleSetData config value model =
    config.setData (Just <| decodeValue config.dataDecoder value) model
        |> handleSendRequest config 0


handleSendRequest : Config data sr msg -> Int -> ModelBackend -> BackendReturn msg
handleSendRequest config fromPersonId model =
    let
        geoParams =
            config.getData model
                |> Maybe.andThen Result.toMaybe
                |> Maybe.map (config.getParams >> encodeReportParams)
                |> Maybe.withDefault []

        params =
            [ ( "app_type", string config.appType )
            , ( "base_revision", string (String.fromInt fromPersonId) )
            ]
                ++ geoParams

        cmd =
            HttpBuilder.post (config.backendUrl ++ "/api/reports-data")
                |> withHeader "X-CSRF-Token" config.csrfToken
                |> withJsonBody (object params)
                |> withExpectJson config.syncResponseDecoder
                |> HttpBuilder.send (RemoteData.fromResult >> config.wrapHandleResponse)
    in
    BackendReturn model cmd noError []


handleSyncResponse : Config data sr msg -> WebData sr -> ModelBackend -> BackendReturn msg
handleSyncResponse config data model =
    RemoteData.toMaybe data
        |> Maybe.map
            (\response ->
                let
                    modelUpdated =
                        config.getData model
                            |> Maybe.andThen Result.toMaybe
                            |> Maybe.map
                                (\d ->
                                    config.setData (Just (Ok (config.mergeResponse response d))) model
                                )
                            |> Maybe.withDefault model
                in
                if config.getRemaining response == 0 then
                    BackendReturn modelUpdated Cmd.none noError []

                else
                    handleSendRequest config (config.getLastIdSynced response) modelUpdated
            )
        |> Maybe.withDefault (BackendReturn model Cmd.none noError [])
