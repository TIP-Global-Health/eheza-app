module DataManager.Model exposing
    ( BackendAuthorityEntity(..)
    , BackendGeneralEntity(..)
    , DownloadPhotos(..)
    , DownloadPhotosBatchRec
    , DownloadStatus
    , DownloadSyncResponse
    , FetchFromIndexDbQueryType(..)
    , IndexDbQueryDeferredPhotoResultRecord
    , IndexDbQueryTypeResult(..)
    , Model
    , Msg(..)
    , RevisionIdPerAuthority
    , RevisionIdPerAuthorityZipper
    , SyncAttempt(..)
    , SyncData
    , SyncError(..)
    , SyncStatus(..)
    , UploadStatus
    , emptyDownloadPhotosBatchRec
    , emptyModel
    , emptySyncData
    )

import AssocList exposing (Dict)
import Backend.Entities exposing (HealthCenterId)
import Backend.HealthCenter.Model exposing (HealthCenter)
import Backend.Measurement.Model exposing (Measurement, Photo, Weight)
import Backend.Person.Model exposing (Person)
import Backend.PmtctParticipant.Model exposing (PmtctParticipant)
import Json.Decode exposing (Value)
import List.Zipper exposing (Zipper)
import RemoteData exposing (RemoteData, WebData)
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
    = BackendGeneralHealthCenter String Int HealthCenter
    | BackendGeneralPerson String Int Person
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
    = -- BackendAuthorityMeasurementWeight String Int Weight
      BackendAuthorityPhoto String Int Photo
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

    -- Determine how we're going to download photos.
    , downloadPhotos : DownloadPhotos

    -- If `DownloadPhotosBatch` is selected as download mechanism, indicate what's
    -- the batch size.
    , downloadPhotosBatchSize : Int

    -- Determine is Sync status should be rotated automatically, or manually for debug
    -- purposes.
    , syncStatusRotateAutomatic : Bool

    -- @todo: Remove
    , syncData : SyncData
    }


emptyModel : LastFetchedRevisionIdGeneral -> RevisionIdPerAuthorityZipper -> Int -> Model
emptyModel lastFetchedRevisionIdGeneral revisionIdPerAuthorityZipper batchSize =
    { syncStatus = SyncDownloadGeneral RemoteData.NotAsked

    -- syncStatus = SyncDownloadPhotos (DownloadPhotosAll RemoteData.NotAsked)
    , lastFetchedRevisionIdGeneral = lastFetchedRevisionIdGeneral
    , revisionIdPerAuthorityZipper = revisionIdPerAuthorityZipper
    , lastTryBackendGeneralDownloadTime = Time.millisToPosix 0
    , syncData = emptySyncData
    , downloadPhotos = DownloadPhotosBatch (emptyDownloadPhotosBatchRec batchSize)
    , downloadPhotosBatchSize = batchSize
    , syncStatusRotateAutomatic = True

    --, syncStatusRotateAutomatic = False
    }


{-| Hold the info we're going to decode from a GET call to /api/sync.

We can have the `a` replaced with BackendGeneralEntity or BackendAuthorityEntity

-}
type alias DownloadSyncResponse a =
    { entities : List a
    , lastTimestampOfLastRevision : Time.Posix
    , revisionCount : Int
    }


{-| Determine how photos are going to be downloaded.
-}
type DownloadPhotos
    = -- Don't download any photos at all.
      DownloadPhotosNone
      -- Download up to a number of photos, and then skip to the next Sync status,
      -- which is `SyncIdle`. This is used to grab photos, but without blocking
      -- completely the rest of the syncing of data.
      -- So the first Int, is the default batch size, and the second is used as
      -- a counter.
    | DownloadPhotosBatch DownloadPhotosBatchRec
      -- Download all photos.
    | DownloadPhotosAll DownloadPhotosAllRec


type alias DownloadPhotosBatchRec =
    { batchSize : Int
    , batchCounter : Int
    , indexDbRemoteData : DeferredPhotoIndexDbRemoteData
    , backendRemoteData : WebData ()
    }


emptyDownloadPhotosBatchRec : Int -> DownloadPhotosBatchRec
emptyDownloadPhotosBatchRec batchSize =
    { batchSize = batchSize
    , batchCounter = batchSize
    , indexDbRemoteData = RemoteData.NotAsked
    , backendRemoteData = RemoteData.NotAsked
    }


type alias DownloadPhotosAllRec =
    { indexDbRemoteData : DeferredPhotoIndexDbRemoteData
    , backendRemoteData : WebData ()
    }


{-| RemoteData to indicate fetching deferred photos info from IndexDB.
-}
type alias DeferredPhotoIndexDbRemoteData =
    RemoteData String (Maybe IndexDbQueryDeferredPhotoResultRecord)


{-| The Sync (download or upload), by its order.
-}
type SyncStatus
    = SyncIdle
    | SyncUpload
    | SyncDownloadGeneral (WebData (DownloadSyncResponse BackendGeneralEntity))
    | SyncDownloadAuthority (WebData (DownloadSyncResponse BackendAuthorityEntity))
    | SyncDownloadPhotos DownloadPhotos


{-| Indicate what content, or query we'd like to get from IndexDB.
-}
type FetchFromIndexDbQueryType
    = -- Get a single deferred photo.
      IndexDbQueryDeferredPhoto
    | IndexDbQueryHealthCenters
      -- When we successfully download a photo, we remove it from the `deferredPhotos` table.
      -- We just need the UUID.
    | IndexDbQueryRemoveDeferredPhotoAttempts String
      -- Update the number of attempts, a deferred photos was un-successfully downloaded.
      -- We don't count cases where we were offline.
    | IndexDbQueryUpdateDeferredPhotoAttempts IndexDbQueryDeferredPhotoResultRecord


type IndexDbQueryTypeResult
    = IndexDbQueryHealthCentersResult (Dict HealthCenterId HealthCenter)
      -- A single deferred photo, if exists.
    | IndexDbQueryDeferredPhotoResult (Maybe IndexDbQueryDeferredPhotoResultRecord)


type alias IndexDbQueryDeferredPhotoResultRecord =
    { uuid : String
    , photo : String

    -- The number of attempts we've tried to get the image.
    , attempts : Int
    }


type Msg
    = BackendAuthorityFetch
    | BackendAuthorityFetchHandle (Zipper RevisionIdPerAuthority) (WebData (DownloadSyncResponse BackendAuthorityEntity))
      -- This is the main entry point for the Sync loop. This will dispatch a call
      -- according to the `syncStatus`.
    | BackendFetchMain
    | BackendGeneralFetch
    | BackendGeneralFetchHandle (WebData (DownloadSyncResponse BackendGeneralEntity))
      -- Fetch a deferred photo from the server.
    | BackendDeferredPhotoFetch IndexDbQueryDeferredPhotoResultRecord
    | BackendDeferredPhotoFetchHandle IndexDbQueryDeferredPhotoResultRecord (WebData ())
    | FetchFromIndexDb FetchFromIndexDbQueryType
    | FetchFromIndexDbHandle Value
    | FetchFromIndexDbDeferredPhoto
    | SetLastFetchedRevisionIdAuthority (Zipper RevisionIdPerAuthority) Int
    | SetLastFetchedRevisionIdGeneral Int
    | SetSyncStatusRotateAutomatic Bool



-- @todo: Remove.


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
