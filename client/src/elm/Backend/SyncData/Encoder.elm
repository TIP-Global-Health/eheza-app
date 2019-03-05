module Backend.SyncData.Encoder exposing (encodeSyncData)

import Backend.SyncData.Model exposing (..)
import Date exposing (Date, toTime)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)


encodeSyncData : SyncData -> Value
encodeSyncData data =
    object
        [ ( "download", maybe encodeDownloadStatus data.downloadStatus )
        , ( "upload", maybe encodeUploadStatus data.uploadStatus )
        , ( "attempt", encodeSyncAttempt data.attempt )
        ]


encodeDownloadStatus : DownloadStatus -> Value
encodeDownloadStatus data =
    object
        [ ( "last_contact", float (toTime data.lastSuccessfulContact) )
        , ( "last_timestamp", int data.lastTimestamp )
        , ( "remaining", int data.remaining )
        ]


encodeUploadStatus : UploadStatus -> Value
encodeUploadStatus data =
    object
        [ ( "first_timestamp", maybe int data.firstTimestamp )
        , ( "remaining", int data.remaining )
        ]


encodeSyncAttempt : SyncAttempt -> Value
encodeSyncAttempt data =
    object <|
        case data of
            NotAsked ->
                [ ( "tag", string "NotAsked" )
                ]

            Downloading date revision ->
                [ ( "tag", string "Loading" )
                , ( "timestamp", float (toTime date) )
                , ( "revision", int revision )
                ]

            Uploading date ->
                [ ( "tag", string "Uploading" )
                , ( "timestamp", float (toTime date) )
                ]

            Success ->
                [ ( "tag", string "Success" )
                ]

            Failure date err ->
                ( "timestamp", float (toTime date) )
                    :: (case err of
                            DatabaseError message ->
                                [ ( "tag", string "DatabaseError" )
                                , ( "message", string message )
                                ]

                            NetworkError message ->
                                [ ( "tag", string "NetworkError" )
                                , ( "message", string message )
                                ]

                            NoCredentials ->
                                [ ( "tag", string "NoCredentials" )
                                ]

                            BadResponse code text ->
                                [ ( "tag", string "BadResponse" )
                                , ( "status", int code )
                                , ( "statusText", string text )
                                ]

                            BadJson ->
                                [ ( "tag", string "BadJson" )
                                ]
                       )
