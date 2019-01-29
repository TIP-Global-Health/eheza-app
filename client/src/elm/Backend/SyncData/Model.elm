module Backend.SyncData.Model exposing (SyncAttempt(..), SyncData, SyncError(..), SyncStatus, emptySyncData)

import Date exposing (Date)


type alias SyncData =
    { status : Maybe SyncStatus
    , attempt : SyncAttempt
    }


emptySyncData : SyncData
emptySyncData =
    { status = Nothing
    , attempt = NotAsked
    }


type alias SyncStatus =
    -- The last time we successfully contacted the backend
    { lastContact : Date

    -- The timestamp of the last revision on the backend
    , lastTimestamp : Int

    -- How many revisions have we not downloaded yet?
    , remaining : Int
    }


type SyncAttempt
    = NotAsked
    | Loading Date Int -- in progress, from base revision
    | Failure Date SyncError
    | Success


type SyncError
    = DatabaseError String
    | NetworkError String
    | NoCredentials
    | BadResponse Int String
    | BadJson
