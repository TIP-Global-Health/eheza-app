module DataManager.Model exposing
    ( BackendGeneralEntity(..)
    , DownloadStatus
    , DownloadSyncResponse
    , Model
    , Msg(..)
    , RevisionIdPerAuthority
    , SyncAttempt(..)
    , SyncData
    , SyncError(..)
    , SyncStatus(..)
    , UploadStatus
    , emptyModel
    , emptySyncData
    )

import Backend.Measurement.Model exposing (Measurement, Weight)
import Backend.Person.Model exposing (Person)
import Backend.PmtctParticipant.Model exposing (PmtctParticipant)
import List.Zipper exposing (Zipper)
import RemoteData exposing (WebData)
import Time


{-| The "general" entities are ones that currently don't belong to a specific
authority (e.g. Health center). For example, a person is a "general" entity,
but a child's measurements is per authority.
-}
type
    BackendGeneralEntity
    -- UUID is not part of the entities, so we'd keep it along with the entity
    -- itself. We keep the UUID is regular string to keep decoder code easier to
    -- manage.
    = BackendGeneralEntityPerson String Int Person
    | BackendGeneralPmtctParticipant String Int PmtctParticipant
      -- Don't fail on unknown types. We'd like to keep the type name along with
      -- the `vid`. The reason we keep the vid, is that we fetched some content
      -- which we don't recognize, but we want to keep fetching later content.
    | BackendGeneralEntityUnknown String Int


{-| The "Authority" entities are ones that belong to a specific
authority (e.g. Health center). For example, a child's measurements is per
authority.
-}
type BackendAuthorityEntity
    = BackendAuthorityEntityMeasurementWeight String Int Weight
      -- Don't fail on unknown types. We'd like to keep the type name along with
      -- the `vid`. The reason we keep the vid, is that we fetched some content
      -- which we don't recognize, but we want to keep fetching later content.
    | BackendAuthorityEntityUnknown String Int


type alias LastFetchedRevisionIdGeneral =
    Int


type alias RevisionIdPerAuthority =
    { uuid : String
    , revisionId : Int
    }


type alias RevisionIdPerAuthorityZipper =
    Maybe (Zipper RevisionIdPerAuthority)


type alias Model =
    { syncStatus : SyncStatus
    , lastFetchedRevisionIdGeneral : LastFetchedRevisionIdGeneral

    -- We may have multiple authorities, and each one has its own revision ID to
    -- fetch from.
    , revisionIdPerAuthorityZipper : RevisionIdPerAuthorityZipper
    , lastTryBackendGeneralDownloadTime : Time.Posix
    , syncData : SyncData
    }


emptyModel : LastFetchedRevisionIdGeneral -> RevisionIdPerAuthorityZipper -> Model
emptyModel lastFetchedRevisionIdGeneral revisionIdPerAuthorityZipper =
    { syncStatus = SyncDownloadGeneral RemoteData.NotAsked
    , lastFetchedRevisionIdGeneral = lastFetchedRevisionIdGeneral
    , revisionIdPerAuthorityZipper = revisionIdPerAuthorityZipper
    , lastTryBackendGeneralDownloadTime = Time.millisToPosix 0
    , syncData = emptySyncData
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


{-| Hold the info we're going to decode from a GET call to /api/sync.
-}
type alias DownloadSyncResponse =
    { backendGeneralEntities : List BackendGeneralEntity
    , lastTimestampOfLastRevision : Time.Posix
    , revisionCount : Int
    }


{-| The Sync (download or upload), by its order.
-}
type SyncStatus
    = SyncIdle
    | SyncUpload
    | SyncDownloadGeneral (WebData DownloadSyncResponse)
    | SyncDownloadAuthority RevisionIdPerAuthorityZipper (WebData DownloadSyncResponse)


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
    = BackendAuthorityFetch
    | BackendAuthorityFetchHandle (WebData DownloadSyncResponse)
    | BackendGeneralFetch
    | BackendGeneralFetchHandle (WebData DownloadSyncResponse)
    | SetLastFetchedRevisionIdGeneral Int
