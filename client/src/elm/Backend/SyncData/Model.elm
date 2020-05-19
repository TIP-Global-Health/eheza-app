module Backend.SyncData.Model exposing
    ( BackendGeneralEntity(..)
    , DownloadStatus
    , Model
    , Msg(..)
    , SyncAttempt(..)
    , SyncData
    , SyncError(..)
    , UploadStatus
    , emptySyncData
    )

import Backend.Person.Model exposing (Person)
import RemoteData exposing (WebData)
import Time



{- The "general" entities are ones that currently don't belong to a specific
   authority (e.g. Health center). For example, a person is a "general" entity,
   but a child's measurements is per authority.
-}


type BackendGeneralEntity
    = BackendGeneralEntityPerson Person
      -- Don't fail on unknown types.
    | BackendGeneralEntityUnknown


type alias LastFetchedRevisionId =
    Int


type alias Model =
    { backendGeneralEntities : WebData (List BackendGeneralEntity)

    -- @todo: Get this value from flags.
    , backendGeneralLastFetchedRevisionId : Int
    }


emptyModel : Model
emptyModel =
    { backendGeneralEntities = RemoteData.NotAsked
    , backendGeneralLastFetchedRevisionId = 0
    }


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


type Msg
    = BackendGeneralFetch LastFetchedRevisionId
    | BackendGeneralFetchHandle LastFetchedRevisionId (WebData (List BackendGeneralEntity))
    | SetbackendGeneralLastFetchedRevisionId LastFetchedRevisionId
