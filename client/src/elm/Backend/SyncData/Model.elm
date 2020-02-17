module Backend.SyncData.Model exposing (DownloadStatus, SyncAttempt(..), SyncData, SyncError(..), UploadStatus, emptySyncData)

import Time


type alias SyncData =
    { downloadStatus : Maybe DownloadStatus
    , uploadStatus : Maybe UploadStatus
    , attempt : SyncAttempt
    }


emptySyncData : SyncData
emptySyncData =
    { downloadStatus = Nothing
    , uploadStatus = Nothing
    , attempt = NotAsked
    }


type alias DownloadStatus =
    -- The last time we successfully contacted the backend
    { lastSuccessfulContact : Time.Posix

    -- The timestamp of the last revision on the backend
    , lastTimestamp : Int

    -- How many revisions have we not downloaded yet?
    , remaining : Int
    }


type alias UploadStatus =
    -- Timestamp of the first revision we haven't uploaded
    -- (if there is such a revision).
    { firstTimestamp : Maybe Int

    -- How many revisions remain to be uploaded?
    , remaining : Int
    }


type SyncAttempt
    = NotAsked
    | Downloading Time.Posix Int -- in progress, from base revision
    | Uploading Time.Posix
    | Failure Time.Posix SyncError
    | Success


type SyncError
    = DatabaseError String
    | NetworkError String
    | NoCredentials
    | BadResponse Int String
    | BadJson
    | ImageNotFound String
