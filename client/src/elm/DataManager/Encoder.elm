module DataManager.Encoder exposing (encodeSyncData)

import DataManager.Model exposing (..)
import Json.Encode exposing (..)
import Json.Encode.Extra exposing (maybe)
import Time


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
        [ ( "last_contact", int (Time.posixToMillis data.lastSuccessfulContact) )
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

            Downloading time revision ->
                [ ( "tag", string "Loading" )
                , ( "timestamp", int (Time.posixToMillis time) )
                , ( "revision", int revision )
                ]

            Uploading time ->
                [ ( "tag", string "Uploading" )
                , ( "timestamp", int (Time.posixToMillis time) )
                ]

            Success ->
                [ ( "tag", string "Success" )
                ]

            Failure time err ->
                ( "timestamp", int (Time.posixToMillis time) )
                    :: (case err of
                            DatabaseError message ->
                                [ ( "tag", string "DatabaseError" )
                                , ( "message", string message )
                                ]

                            NetworkError message ->
                                [ ( "tag", string "NetworkError" )
                                , ( "message", string message )
                                ]

                            ImageNotFound url ->
                                [ ( "tag", string "ImageNotFound" )
                                , ( "url", string url )
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
