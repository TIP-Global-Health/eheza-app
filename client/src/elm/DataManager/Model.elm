module DataManager.Model exposing
    ( BackendAuthorityEntity(..)
    , BackendEntity
    , BackendEntityIdentifier
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
    , SyncCycle(..)
    , SyncSpeed
    , SyncStatus(..)
    , UploadMethod(..)
    , UploadPhotoError(..)
    , emptyDownloadPhotosBatchRec
    , emptyModel
    , emptyRevisionIdPerAuthority
    , emptyUploadRec
    )

import AssocList exposing (Dict)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Counseling.Model exposing (CounselingSchedule, CounselingTopic)
import Backend.Entities exposing (HealthCenterId)
import Backend.HealthCenter.Model exposing (CatchmentArea, HealthCenter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (Attendance, BreastExam, ChildNutrition, CorePhysicalExam, CounselingSession, DangerSigns, FamilyPlanning, Fbf, Height, Lactation, LastMenstrualPeriod, Measurement, MedicalHistory, Medication, Muac, NutritionHeight, NutritionMuac, NutritionNutrition, NutritionPhoto, NutritionWeight, ObstetricHistory, ObstetricHistoryStep2, ObstetricalExam, ParticipantConsent, Photo, Weight)
import Backend.Nurse.Model exposing (Nurse)
import Backend.NutritionEncounter.Model exposing (NutritionEncounter)
import Backend.ParticipantConsent.Model exposing (ParticipantForm)
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
type BackendGeneralEntity
    = BackendGeneralCatchmentArea (BackendEntity CatchmentArea)
    | BackendGeneralCounselingSchedule (BackendEntity CounselingSchedule)
    | BackendGeneralCounselingTopic (BackendEntity CounselingTopic)
    | BackendGeneralHealthCenter (BackendEntity HealthCenter)
    | BackendGeneralNurse (BackendEntity Nurse)
    | BackendGeneralParticipantForm (BackendEntity ParticipantForm)
    | BackendGeneralPerson (BackendEntity Person)
    | BackendGeneralPmtctParticipant (BackendEntity PmtctParticipant)
    | BackendGeneralRelationship (BackendEntity Relationship)
      -- Don't fail on unknown types. We'd like to keep the type name along with
      -- the `vid`. The reason we keep the vid, is that we fetched some content
      -- which we don't recognize, but we want to keep fetching later content.
      -- @todo: Remove after we decode all entity types.
    | BackendGeneralEntityUnknown String Int


{-| The "Authority" entities are ones that belong to a specific
authority (e.g. Health center). For example, a child's measurements is per
authority.
-}
type BackendAuthorityEntity
    = BackendAuthorityAttendance (BackendEntity Attendance)
    | BackendAuthorityBreastExam (BackendEntity BreastExam)
    | BackendAuthorityChildFbf (BackendEntity Fbf)
    | BackendAuthorityClinic (BackendEntity Clinic)
    | BackendAuthorityCounselingSession (BackendEntity CounselingSession)
    | BackendAuthorityCorePhysicalExam (BackendEntity CorePhysicalExam)
    | BackendAuthorityDangerSigns (BackendEntity DangerSigns)
    | BackendAuthorityFamilyPlanning (BackendEntity FamilyPlanning)
    | BackendAuthorityHeight (BackendEntity Height)
    | BackendAuthorityIndividualParticipant (BackendEntity IndividualEncounterParticipant)
    | BackendAuthorityLactation (BackendEntity Lactation)
    | BackendAuthorityLastMenstrualPeriod (BackendEntity LastMenstrualPeriod)
    | BackendAuthorityMedicalHistory (BackendEntity MedicalHistory)
    | BackendAuthorityMedication (BackendEntity Medication)
    | BackendAuthorityMotherFbf (BackendEntity Fbf)
    | BackendAuthorityMuac (BackendEntity Muac)
    | BackendAuthorityNutrition (BackendEntity ChildNutrition)
    | BackendAuthorityNutritionEncounter (BackendEntity NutritionEncounter)
    | BackendAuthorityNutritionHeight (BackendEntity NutritionHeight)
    | BackendAuthorityNutritionMuac (BackendEntity NutritionMuac)
    | BackendAuthorityNutritionNutrition (BackendEntity NutritionNutrition)
    | BackendAuthorityNutritionPhoto (BackendEntity NutritionPhoto)
    | BackendAuthorityNutritionWeight (BackendEntity NutritionWeight)
    | BackendAuthorityObstetricHistory (BackendEntity ObstetricHistory)
    | BackendAuthorityObstetricHistoryStep2 (BackendEntity ObstetricHistoryStep2)
    | BackendAuthorityObstetricalExam (BackendEntity ObstetricalExam)
    | BackendAuthorityParticipantConsent (BackendEntity ParticipantConsent)
    | BackendAuthorityPhoto (BackendEntity Photo)
    | BackendAuthorityWeight (BackendEntity Weight)
      -- Don't fail on unknown types. We'd like to keep the type name along with
      -- the `vid`. The reason we keep the vid, is that we fetched some content
      -- which we don't recognize, but we want to keep fetching later content.
      -- @todo: Remove after we decode all entity types.
    | BackendAuthorityEntityUnknown String Int


{-| Wrapper for a Backend entity (both General and Authority).
-}
type alias BackendEntity a =
    { -- The `String` is the UUID which is not part of the entities, so we'd keep
      -- it along with the entity itself. We keep the UUID is regular string to
      -- keep decoder code easier to manage.
      uuid : String

    -- When downloading, the `Int` is the vid of the node.
    -- When uploading, the `Int` the the `localId` from IndexDB.
    , revision : Int
    , entity : a
    }


{-| Get info about an entity. `revision` would be the Drupal revision
in case of download, or the `localId` in case of upload.
-}
type alias BackendEntityIdentifier =
    { uuid : String, revision : Int, type_ : String }


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
    , syncCycle : SyncCycle

    -- Time in seconds while idle or while syncing.
    -- In production, a good value would be:
    -- `idle` - 50; which is the minimum we will allow.
    -- `sync` - 10000. The means that sync will sit idle for 10 seconds.
    , syncSpeed : Editable SyncSpeed
    }


emptyModel : Flags -> Model
emptyModel flags =
    { syncStatus = SyncIdle
    , lastFetchedRevisionIdGeneral = flags.lastFetchedRevisionIdGeneral
    , revisionIdPerAuthorityZipper = flags.revisionIdPerAuthorityZipper
    , lastTryBackendGeneralDownloadTime = Time.millisToPosix 0
    , downloadPhotos = DownloadPhotosBatch (emptyDownloadPhotosBatchRec flags.batchSize)
    , downloadPhotosBatchSize = flags.batchSize
    , syncCycle = SyncCycleOn
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

    -- If we're offline, we don't want to hammer the system with HTTP requests
    -- that will fail, so we have a longer pause.
    , offline : Int
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
    | SyncUploadPhotoGeneral (RemoteData UploadPhotoError (Maybe IndexDbQueryUploadPhotoResultRecord))
    | SyncUploadGeneral UploadRec
    | SyncDownloadGeneral (WebData (DownloadSyncResponse BackendGeneralEntity))
    | SyncDownloadAuthority (WebData (DownloadSyncResponse BackendAuthorityEntity))
    | SyncDownloadPhotos DownloadPhotos


type SyncCycle
    = -- Work normally.
      SyncCycleOn
      -- Keep calling sync, but never switch to the next Sync status. For example,
      -- if we're currently downloading from General, by selecting this, we'd
      -- keep trying to download from General, without switching to downloading
      -- from Authority.
    | SyncCycleStayOnCurrentSyncStatus
      -- Completely pause sync.
    | SyncCyclePause


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
      IndexDbQueryUploadPhotoGeneralResult (RemoteData UploadPhotoError (Maybe IndexDbQueryUploadPhotoResultRecord))
    | IndexDbQueryUploadGeneralResult (Maybe IndexDbQueryUploadGeneralResultRecord)
      -- A single deferred photo, if exists.
    | IndexDbQueryDeferredPhotoResult (Maybe IndexDbQueryDeferredPhotoResultRecord)


type UploadPhotoError
    = PhotoNotFoundOnCacheStorage
    | FetchError String
    | BadJson String


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
    | BackendUploadPhotoGeneralHandle (RemoteData UploadPhotoError (Maybe IndexDbQueryUploadPhotoResultRecord))
    | QueryIndexDb IndexDbQueryType
    | QueryIndexDbHandle Value
    | FetchFromIndexDbDeferredPhoto
    | FetchFromIndexDbUploadGeneral
    | RevisionIdAuthorityAdd HealthCenterId
    | RevisionIdAuthorityRemove HealthCenterId
    | SetLastFetchedRevisionIdAuthority (Zipper RevisionIdPerAuthority) Int
    | SetLastFetchedRevisionIdGeneral Int
      -- UI settings
    | ResetSettings
    | SaveSettings
    | SetSyncCycle SyncCycle
    | SetSyncSpeedIdle String
    | SetSyncSpeedCycle String
    | SetSyncSpeedOffline String
