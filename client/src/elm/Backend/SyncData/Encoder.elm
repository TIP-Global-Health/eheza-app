module Backend.SyncData.Encoder exposing (encodeSyncData)

import Backend.SyncData.Model exposing (..)
import Date exposing (Date, toTime)
import Json.Encode exposing (..)


encodeSyncData : SyncData -> Value
encodeSyncData data =
    data.status
        |> Maybe.map encodeSyncStatusFields
        |> Maybe.withDefault []
        |> (::) ( "status", encodeSyncAttempt data.attempt )
        |> object


encodeSyncStatusFields : SyncStatus -> List ( String, Value )
encodeSyncStatusFields data =
    [ ( "last_contact", float (toTime data.lastContact) )
    , ( "last_timestamp", int data.lastTimestamp )
    , ( "remaining", int data.remaining )
    ]


encodeSyncAttempt : SyncAttempt -> Value
encodeSyncAttempt data =
    object <|
        case data of
            NotAsked ->
                [ ( "tag", string "NotAsked" )
                ]

            Loading date revision ->
                [ ( "tag", string "Loading" )
                , ( "timestamp", float (toTime date) )
                , ( "revision", int revision )
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
