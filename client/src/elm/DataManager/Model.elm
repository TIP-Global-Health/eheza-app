module DataManager.Model exposing
    ( BackendAuthorityEntity(..)
    , BackendGeneralEntity(..)
    , DownloadPhotos(..)
    , DownloadPhotosBatchRec
    , DownloadSyncResponse
    , Flags
    , IndexDbQueryDeferredPhotoResultRecord
    , IndexDbQueryType(..)
    , IndexDbQueryTypeResult(..)
    , IndexDbQueryUploadGeneralResultRecord
    , IndexDbQueryUploadPhotoResultRecord
    , Model
    , Msg(..)
    , RevisionIdPerAuthority
    , RevisionIdPerAuthorityZipper
    , SyncSpeed
    , SyncStatus(..)
    , UploadMethod(..)
    , emptyDownloadPhotosBatchRec
    , emptyModel
    , emptyRevisionIdPerAuthority
    , emptyUploadRec
    )

import AssocList exposing (Dict)
import Backend.Entities exposing (HealthCenterId)
import Backend.HealthCenter.Model exposing (CatchmentArea, HealthCenter)
import Backend.Measurement.Model exposing (Attendance, Measurement, Photo, Weight)
import Backend.Nurse.Model exposing (Nurse)
import Backend.Person.Model exposing (Person)
import Backend.PmtctParticipant.Model exposing (PmtctParticipant)
import Backend.Relationship.Model exposing (Relationship)
import Editable exposing (Editable)
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
    -- The `String` is the UUID which is not part of the entities, so we'd keep
    -- it along with the entity itself. We keep the UUID is regular string to
    -- keep decoder code easier to manage.
    -- When downloading, the `Int` is the vid of the node.
    -- When uploading, the `Int` the the `localId` from IndexDB.
    = BackendGeneralCatchmentArea String Int CatchmentArea
    | BackendGeneralHealthCenter String Int HealthCenter
    | BackendGeneralNurse String Int Nurse
    | BackendGeneralPerson String Int Person
    | BackendGeneralPmtctParticipant String Int PmtctParticipant
    | BackendGeneralRelationship String Int Relationship
      -- Don't fail on unknown types. We'd like to keep the type name along with
      -- the `vid`. The reason we keep the vid, is that we fetched some content
      -- which we don't recognize, but we want to keep fetching later content.
    | BackendGeneralEntityUnknown String Int


{-| The "Authority" entities are ones that belong to a specific
authority (e.g. Health center). For example, a child's measurements is per
authority.
-}
type BackendAuthorityEntity
    = BackendAuthorityAttendance String Int Attendance
    | BackendAuthorityPhoto String Int Photo
    | BackendAuthorityWeight String Int Weight
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


emptyRevisionIdPerAuthority : String -> RevisionIdPerAuthority
emptyRevisionIdPerAuthority uuid =
    { uuid = uuid
    , revisionId = 0
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
    , syncCycle : Bool

    -- Time in seconds while idle or while syncing.
    -- In production, a good value would be:
    -- `idle` - 50; which is the minimum we will allow.
    -- `sync` - 10000. The means that sync will sit idle for 10 seconds.
    , syncSpeed : Editable SyncSpeed
    }


emptyModel : Flags -> Model
emptyModel flags =
    { syncStatus = SyncUploadPhotoGeneral RemoteData.NotAsked
    , lastFetchedRevisionIdGeneral = flags.lastFetchedRevisionIdGeneral
    , revisionIdPerAuthorityZipper = flags.revisionIdPerAuthorityZipper
    , lastTryBackendGeneralDownloadTime = Time.millisToPosix 0
    , downloadPhotos = DownloadPhotosBatch (emptyDownloadPhotosBatchRec flags.batchSize)
    , downloadPhotosBatchSize = flags.batchSize
    , syncCycle = True
    , syncSpeed = Editable.ReadOnly flags.syncSpeed
    }


{-| The information we get initially from App.Model via flags.
-}
type alias Flags =
    { lastFetchedRevisionIdGeneral : LastFetchedRevisionIdGeneral
    , revisionIdPerAuthorityZipper : RevisionIdPerAuthorityZipper
    , batchSize : Int
    , syncSpeed : SyncSpeed
    }


type alias SyncSpeed =
    { idle : Int
    , cycle : Int
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


{-| Hold info related to uploading entities.
-}
type alias UploadRec =
    { indexDbRemoteData : IndexDbUploadRemoteData
    , backendRemoteData : WebData ()
    }


emptyUploadRec : UploadRec
emptyUploadRec =
    { indexDbRemoteData = RemoteData.NotAsked
    , backendRemoteData = RemoteData.NotAsked
    }


type alias DownloadPhotosBatchRec =
    { batchSize : Int
    , batchCounter : Int
    , indexDbRemoteData : IndexDbDeferredPhotoRemoteData
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
    { indexDbRemoteData : IndexDbDeferredPhotoRemoteData
    , backendRemoteData : WebData ()
    }


{-| RemoteData to indicate fetching deferred photos info from IndexDB.
-}
type alias IndexDbDeferredPhotoRemoteData =
    RemoteData () (Maybe IndexDbQueryDeferredPhotoResultRecord)


{-| RemoteData to indicate fetching entities for upload info from IndexDB.
-}
type alias IndexDbUploadRemoteData =
    RemoteData () (Maybe IndexDbQueryUploadGeneralResultRecord)


{-| The Sync (download or upload), by its order.
-}
type SyncStatus
    = SyncIdle
    | SyncUploadPhotoGeneral (RemoteData () (Maybe IndexDbQueryUploadPhotoResultRecord))
    | SyncUploadGeneral UploadRec
    | SyncDownloadGeneral (WebData (DownloadSyncResponse BackendGeneralEntity))
    | SyncDownloadAuthority (WebData (DownloadSyncResponse BackendAuthorityEntity))
    | SyncDownloadPhotos DownloadPhotos


{-| Indicate what content, or query we'd like to get from IndexDB.
-}
type IndexDbQueryType
    = -- Get a single photo pending uploading
      IndexDbQueryUploadPhotoGeneral
    | IndexDbQueryUploadGeneral
      -- Get a single deferred photo.
    | IndexDbQueryDeferredPhoto
      -- When we successfully download a photo, we remove it from the `deferredPhotos` table.
      -- We just need the UUID.
    | IndexDbQueryRemoveDeferredPhotoAttempts String
      -- Update the number of attempts, a deferred photos was un-successfully downloaded.
      -- We don't count cases where we were offline.
    | IndexDbQueryUpdateDeferredPhotoAttempts IndexDbQueryDeferredPhotoResultRecord


type IndexDbQueryTypeResult
    = -- A single photo for upload, if exists.
      IndexDbQueryUploadPhotoGeneralResult (Maybe IndexDbQueryUploadPhotoResultRecord)
    | IndexDbQueryUploadGeneralResult (Maybe IndexDbQueryUploadGeneralResultRecord)
      -- A single deferred photo, if exists.
    | IndexDbQueryDeferredPhotoResult (Maybe IndexDbQueryDeferredPhotoResultRecord)


{-| The info we get from query to `generalPhotoUploadChanges`.
-}
type alias IndexDbQueryUploadPhotoResultRecord =
    { uuid : String
    , photo : String
    , localId : Int

    -- If photo was uploaded to Drupal, get the file ID.
    , fileId : Maybe Int
    }


{-| Indicate if we should create (POST) or update (PATCH) and entity.
-}
type UploadMethod
    = UploadMethodCreate
    | UploadMethodUpdate


type alias IndexDbQueryUploadGeneralResultRecord =
    { entities : List ( BackendGeneralEntity, UploadMethod )

    -- Instead of list, it would be handier to get a Dict, keyed by the `localId`
    -- so when we would like to switch the photo URL with Drupal's file ID, we could
    -- get that info quicker.
    , uploadPhotos : Dict Int IndexDbQueryUploadPhotoResultRecord
    }


{-| The info we get from query to `deferredPhotos`.
-}
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
    | BackendDeferredPhotoFetch (Maybe IndexDbQueryDeferredPhotoResultRecord)
    | BackendDeferredPhotoFetchHandle IndexDbQueryDeferredPhotoResultRecord (WebData ())
      -- Unlike other `Backend...` msgs, we have no HTTP activity from Elm. That is,
      -- uploading the photos happens in JS, since we have to deal with file blobs
      -- which would be harder in Elm, given we have elm/http@1.0.
      -- This is the reason it doesn't get as arguments the result of the IndexDB.
    | BackendPhotoUploadGeneral
    | BackendUploadGeneral (Maybe IndexDbQueryUploadGeneralResultRecord)
    | BackendUploadGeneralHandle IndexDbQueryUploadGeneralResultRecord (WebData ())
    | QueryIndexDb IndexDbQueryType
    | QueryIndexDbHandle Value
    | FetchFromIndexDbDeferredPhoto
    | FetchFromIndexDbUploadGeneral
    | RevisionIdAuthorityAdd HealthCenterId
    | RevisionIdAuthorityRemove HealthCenterId
    | SetLastFetchedRevisionIdAuthority (Zipper RevisionIdPerAuthority) Int
    | SetLastFetchedRevisionIdGeneral Int
    | SetSyncStatusRotateAutomatic Bool
      -- UI settings
    | ResetSettings
    | SaveSettings
    | SetSyncSpeedIdle String
    | SetSyncSpeedCycle String
