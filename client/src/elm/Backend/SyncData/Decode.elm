module Backend.SyncData.Decoder exposing (decodeSyncData)

import Backend.SyncData.Model exposing (..)
import Date exposing (Date, fromTime)
import Gizra.Json exposing (decodeFloat, decodeInt)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


decodeSyncData : Decoder SyncData
decodeSyncData =
    map2 SyncData (maybe decodeSyncStatus) decodeSyncAttempt


decodeSyncStatus : Decoder SyncStatus
decodeSyncStatus =
    succeed SyncStatus
        |> required "last_contact" (map fromTime decodeFloat)
        |> required "last_timestamp" decodeInt
        |> required "remaining" decodeInt


decodeSyncAttempt : Decoder SyncAttempt
decodeSyncAttempt =
    field "tag" string
        |> andThen
            (\s ->
                case s of
                    "NotAsked" ->
                        succeed NotAsked

                    "Success" ->
                        succeed Success

                    "DatabaseError" ->
                        succeed DatabaseError
                            |> required "message" string
                            |> decodeFailure

                    "NetworkError" ->
                        succeed NetworkError
                            |> required "message" string
                            |> required "willRetry" bool
                            |> decodeFailure

                    "NoCredentials" ->
                        succeed NoCredentials
                            |> decodeFailure

                    "BadResponse" ->
                        succeed BadResponse
                            |> required "status" decodeInt
                            |> required "statusText" string
                            |> decodeFailure

                    "BadJson" ->
                        succeed BadJson
                            |> decodeFailure

                    "Loading" ->
                        succeed Loading
                            |> custom decodeTimestamp
                            |> required "revision" decodeInt

                    _ ->
                        fail <|
                            s
                                ++ " is not a recognized SyncAttempt tag"
            )


decodeFailure : Decoder SyncError -> Decoder SyncAttempt
decodeFailure =
    map2 Failure decodeTimestamp


decodeTimestamp : Decoder Date
decodeTimestamp =
    map fromTime (field "timestamp" decodeFloat)
