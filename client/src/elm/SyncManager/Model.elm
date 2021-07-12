module SyncManager.Model exposing (..)

import AssocList exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounter)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Counseling.Model exposing (CounselingSchedule, CounselingTopic)
import Backend.Dashboard.Model exposing (DashboardStatsRaw)
import Backend.Entities exposing (HealthCenterId)
import Backend.HealthCenter.Model exposing (CatchmentArea, HealthCenter)
import Backend.HomeVisitEncounter.Model exposing (HomeVisitEncounter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant)
import Backend.Measurement.Model exposing (..)
import Backend.Nurse.Model exposing (Nurse)
import Backend.NutritionEncounter.Model exposing (NutritionEncounter)
import Backend.ParticipantConsent.Model exposing (ParticipantForm)
import Backend.Person.Model exposing (Person)
import Backend.PmtctParticipant.Model exposing (PmtctParticipant)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
import Backend.Relationship.Model exposing (Relationship)
import Backend.Session.Model exposing (Session)
import Backend.Village.Model exposing (Village)
import Backend.WellChildEncounter.Model exposing (WellChildEncounter)
import Debouncer.Basic as Debouncer exposing (Debouncer, debounce, toDebouncer)
import Editable exposing (Editable)
import Gizra.NominalDate exposing (NominalDate, formatDDMMYYYY)
import Json.Decode exposing (Value)
import List.Zipper exposing (Zipper)
import RemoteData exposing (RemoteData(..), WebData)
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
    | BackendGeneralVillage (BackendEntity Village)


{-| The "Authority" entities are ones that belong to a specific
authority (e.g. Health center). For example, a child's measurements is per
authority.
-}
type BackendAuthorityEntity
    = BackendAuthorityAcuteFindings (BackendEntity AcuteFindings)
    | BackendAuthorityAcuteIllnessDangerSigns (BackendEntity AcuteIllnessDangerSigns)
    | BackendAuthorityAcuteIllnessEncounter (BackendEntity AcuteIllnessEncounter)
    | BackendAuthorityAcuteIllnessFollowUp (BackendEntity AcuteIllnessFollowUp)
    | BackendAuthorityAcuteIllnessMuac (BackendEntity AcuteIllnessMuac)
    | BackendAuthorityAcuteIllnessNutrition (BackendEntity AcuteIllnessNutrition)
    | BackendAuthorityAcuteIllnessVitals (BackendEntity AcuteIllnessVitals)
    | BackendAuthorityAppointmentConfirmation (BackendEntity PrenatalAppointmentConfirmation)
    | BackendAuthorityAttendance (BackendEntity Attendance)
    | BackendAuthorityBreastExam (BackendEntity BreastExam)
    | BackendAuthorityBirthPlan (BackendEntity BirthPlan)
    | BackendAuthorityChildFbf (BackendEntity Fbf)
    | BackendAuthorityCall114 (BackendEntity Call114)
    | BackendAuthorityClinic (BackendEntity Clinic)
    | BackendAuthorityContributingFactors (BackendEntity ContributingFactors)
    | BackendAuthorityCorePhysicalExam (BackendEntity CorePhysicalExam)
    | BackendAuthorityCounselingSession (BackendEntity CounselingSession)
    | BackendAuthorityDangerSigns (BackendEntity DangerSigns)
    | BackendAuthorityDashboardStats (BackendEntity DashboardStatsRaw)
    | BackendAuthorityExposure (BackendEntity Exposure)
    | BackendAuthorityFamilyPlanning (BackendEntity FamilyPlanning)
    | BackendAuthorityFollowUp (BackendEntity FollowUp)
    | BackendAuthorityGroupHealthEducation (BackendEntity GroupHealthEducation)
    | BackendAuthorityGroupSendToHC (BackendEntity GroupSendToHC)
    | BackendAuthorityHealthEducation (BackendEntity HealthEducation)
    | BackendAuthorityHCContact (BackendEntity HCContact)
    | BackendAuthorityHeight (BackendEntity Height)
    | BackendAuthorityHomeVisitEncounter (BackendEntity HomeVisitEncounter)
    | BackendAuthorityIndividualParticipant (BackendEntity IndividualEncounterParticipant)
    | BackendAuthorityIsolation (BackendEntity Isolation)
    | BackendAuthorityLactation (BackendEntity Lactation)
    | BackendAuthorityLastMenstrualPeriod (BackendEntity LastMenstrualPeriod)
    | BackendAuthorityMalariaTesting (BackendEntity MalariaTesting)
    | BackendAuthorityMedicalHistory (BackendEntity MedicalHistory)
    | BackendAuthorityMedication (BackendEntity Medication)
    | BackendAuthorityMedicationDistribution (BackendEntity MedicationDistribution)
    | BackendAuthorityMotherFbf (BackendEntity Fbf)
    | BackendAuthorityMuac (BackendEntity Muac)
    | BackendAuthorityNutrition (BackendEntity ChildNutrition)
    | BackendAuthorityNutritionCaring (BackendEntity NutritionCaring)
    | BackendAuthorityNutritionContributingFactors (BackendEntity NutritionContributingFactors)
    | BackendAuthorityNutritionEncounter (BackendEntity NutritionEncounter)
    | BackendAuthorityNutritionFeeding (BackendEntity NutritionFeeding)
    | BackendAuthorityNutritionFollowUp (BackendEntity NutritionFollowUp)
    | BackendAuthorityNutritionFoodSecurity (BackendEntity NutritionFoodSecurity)
    | BackendAuthorityNutritionHealthEducation (BackendEntity NutritionHealthEducation)
    | BackendAuthorityNutritionHeight (BackendEntity NutritionHeight)
    | BackendAuthorityNutritionHygiene (BackendEntity NutritionHygiene)
    | BackendAuthorityNutritionMuac (BackendEntity NutritionMuac)
    | BackendAuthorityNutritionNutrition (BackendEntity NutritionNutrition)
    | BackendAuthorityNutritionPhoto (BackendEntity NutritionPhoto)
    | BackendAuthorityNutritionSendToHC (BackendEntity NutritionSendToHC)
    | BackendAuthorityNutritionWeight (BackendEntity NutritionWeight)
    | BackendAuthorityObstetricHistory (BackendEntity ObstetricHistory)
    | BackendAuthorityObstetricHistoryStep2 (BackendEntity ObstetricHistoryStep2)
    | BackendAuthorityObstetricalExam (BackendEntity ObstetricalExam)
    | BackendAuthorityParticipantConsent (BackendEntity ParticipantConsent)
    | BackendAuthorityPerson (BackendEntity Person)
    | BackendAuthorityPhoto (BackendEntity Photo)
    | BackendAuthorityPmtctParticipant (BackendEntity PmtctParticipant)
    | BackendAuthorityPregnancyTesting (BackendEntity PregnancyTest)
    | BackendAuthorityPrenatalHealthEducation (BackendEntity PrenatalHealthEducation)
    | BackendAuthorityPrenatalFollowUp (BackendEntity PrenatalFollowUp)
    | BackendAuthorityPrenatalSendToHC (BackendEntity PrenatalSendToHC)
    | BackendAuthorityPrenatalPhoto (BackendEntity PrenatalPhoto)
    | BackendAuthorityPrenatalFamilyPlanning (BackendEntity PrenatalFamilyPlanning)
    | BackendAuthorityPrenatalNutrition (BackendEntity PrenatalNutrition)
    | BackendAuthorityPrenatalEncounter (BackendEntity PrenatalEncounter)
    | BackendAuthorityRelationship (BackendEntity Relationship)
    | BackendAuthorityResource (BackendEntity Resource)
    | BackendAuthoritySendToHC (BackendEntity SendToHC)
    | BackendAuthoritySession (BackendEntity Session)
    | BackendAuthoritySocialHistory (BackendEntity SocialHistory)
    | BackendAuthoritySymptomsGeneral (BackendEntity SymptomsGeneral)
    | BackendAuthoritySymptomsGI (BackendEntity SymptomsGI)
    | BackendAuthoritySymptomsRespiratory (BackendEntity SymptomsRespiratory)
    | BackendAuthorityTravelHistory (BackendEntity TravelHistory)
    | BackendAuthorityTreatmentOngoing (BackendEntity TreatmentOngoing)
    | BackendAuthorityTreatmentReview (BackendEntity TreatmentReview)
    | BackendAuthorityVitals (BackendEntity Vitals)
    | BackendAuthorityWeight (BackendEntity Weight)
    | BackendAuthorityWellChildContributingFactors (BackendEntity WellChildContributingFactors)
    | BackendAuthorityWellChildECD (BackendEntity WellChildECD)
    | BackendAuthorityWellChildEncounter (BackendEntity WellChildEncounter)
    | BackendAuthorityWellChildFollowUp (BackendEntity WellChildFollowUp)
    | BackendAuthorityWellChildHeadCircumference (BackendEntity WellChildHeadCircumference)
    | BackendAuthorityWellChildHealthEducation (BackendEntity WellChildHealthEducation)
    | BackendAuthorityWellChildHeight (BackendEntity WellChildHeight)
    | BackendAuthorityWellChildMuac (BackendEntity WellChildMuac)
    | BackendAuthorityWellChildNutrition (BackendEntity WellChildNutrition)
    | BackendAuthorityWellChildPhoto (BackendEntity WellChildPhoto)
    | BackendAuthorityWellChildSendToHC (BackendEntity WellChildSendToHC)
    | BackendAuthorityWellChildSymptomsReview (BackendEntity WellChildSymptomsReview)
    | BackendAuthorityWellChildVitals (BackendEntity WellChildVitals)
    | BackendAuthorityWellChildWeight (BackendEntity WellChildWeight)


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


type alias SyncInfoGeneral =
    { lastFetchedRevisionId : Int
    , lastSuccesfulContact : Int
    , remainingToUpload : Int
    , remainingToDownload : Int
    , deviceName : String
    , status : SyncInfoStatus
    }


type alias SyncInfoGeneralForPort =
    { lastFetchedRevisionId : Int
    , lastSuccesfulContact : Int
    , remainingToUpload : Int
    , remainingToDownload : Int
    , deviceName : String
    , status : String
    }


type alias SyncInfoAuthority =
    { uuid : String
    , lastFetchedRevisionId : Int
    , lastSuccesfulContact : Int
    , remainingToUpload : Int
    , remainingToDownload : Int
    , statsCacheHash : String
    , status : SyncInfoStatus
    }


type alias SyncInfoAuthorityForPort =
    { uuid : String
    , lastFetchedRevisionId : Int
    , lastSuccesfulContact : Int
    , remainingToUpload : Int
    , remainingToDownload : Int
    , statsCacheHash : String
    , status : String
    }


type SyncInfoStatus
    = Downloading
    | Error
    | NotAvailable
    | Success
    | Uploading


emptySyncInfoAuthority : String -> SyncInfoAuthority
emptySyncInfoAuthority uuid =
    { uuid = uuid
    , lastFetchedRevisionId = 0
    , lastSuccesfulContact = 0
    , remainingToUpload = 0
    , remainingToDownload = 0
    , statsCacheHash = ""
    , status = NotAvailable
    }


type alias SyncInfoAuthorityZipper =
    Maybe (Zipper SyncInfoAuthority)


type alias Model =
    { debouncer : Debouncer Msg Msg
    , syncStatus : SyncStatus
    , downloadPhotosStatus : DownloadPhotosStatus
    , syncInfoGeneral : SyncInfoGeneral
    , syncInfoAuthorities : SyncInfoAuthorityZipper

    -- We store download responses until we get an acknowledgement
    -- from IndexedDB for the save operation.
    -- Only then we know that DB was updated and we can
    -- update the Model as well.
    , downloadAuthorityResponse : WebData (DownloadSyncResponse BackendAuthorityEntity)
    , downloadGeneralResponse : WebData (DownloadSyncResponse BackendGeneralEntity)

    -- Used to determine if download request has timed out.
    , downloadRequestTime : Time.Posix

    -- Determine how we're going to download photos.
    , downloadPhotosMode : DownloadPhotosMode

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
    { debouncer = debounce 15000 |> toDebouncer
    , syncStatus = SyncIdle
    , downloadPhotosStatus = DownloadPhotosIdle
    , syncInfoGeneral = flags.syncInfoGeneral
    , syncInfoAuthorities = flags.syncInfoAuthorities
    , downloadAuthorityResponse = NotAsked
    , downloadGeneralResponse = NotAsked
    , downloadRequestTime = Time.millisToPosix 0
    , downloadPhotosMode = DownloadPhotosAll emptyDownloadPhotosAllRec
    , downloadPhotosBatchSize = flags.batchSize
    , syncCycle = SyncCycleOn
    , syncSpeed = Editable.ReadOnly flags.syncSpeed
    }


{-| The information we get initially from App.Model via flags.
-}
type alias Flags =
    { syncInfoGeneral : SyncInfoGeneral
    , syncInfoAuthorities : SyncInfoAuthorityZipper
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
    , deviceName : String
    }


{-| Determine how photos are going to be downloaded.
-}
type DownloadPhotosMode
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


{-| Hold info related to uploading General entities.
-}
type alias UploadRec a =
    { indexDbRemoteData : IndexDbUploadRemoteData a
    , backendRemoteData : WebData ()
    }


emptyUploadRec : UploadRec a
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


emptyDownloadPhotosAllRec : DownloadPhotosAllRec
emptyDownloadPhotosAllRec =
    { indexDbRemoteData = RemoteData.NotAsked
    , backendRemoteData = RemoteData.NotAsked
    }


{-| RemoteData to indicate fetching deferred photos info from IndexDB.
-}
type alias IndexDbDeferredPhotoRemoteData =
    IndexDbUploadRemoteData IndexDbQueryDeferredPhotoResultRecord


{-| RemoteData to indicate fetching entities for upload info from IndexDB.
-}
type alias IndexDbUploadRemoteData a =
    RemoteData () (Maybe a)


{-| The Sync (download or upload), by its order.
-}
type SyncStatus
    = SyncIdle
    | SyncUploadGeneral (UploadRec IndexDbQueryUploadGeneralResultRecord)
      -- Int is used for a counter, to track file upload errors.
      -- If counter exceeds threshold, sync will be stopped.
    | SyncUploadPhotoAuthority Int (RemoteData UploadPhotoError (Maybe IndexDbQueryUploadPhotoResultRecord))
    | SyncUploadAuthority (UploadRec IndexDbQueryUploadAuthorityResultRecord)
    | SyncDownloadGeneral (WebData (DownloadSyncResponse BackendGeneralEntity))
    | SyncDownloadAuthority (WebData (DownloadSyncResponse BackendAuthorityEntity))
    | SyncDownloadAuthorityDashboardStats (WebData (DownloadSyncResponse BackendAuthorityEntity))
    | SyncReportIncident SyncIncidentType


type DownloadPhotosStatus
    = DownloadPhotosIdle
    | DownloadPhotosInProcess DownloadPhotosMode


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
    = -- Get a single photo pending uploading.
      IndexDbQueryUploadPhotoAuthority
    | IndexDbQueryUploadGeneral
      -- Query one authority at a time, to make sure
      -- content is being uploaded in correct order,
      -- and we present correct 'remianing for upload'
      -- on sync screen.
    | IndexDbQueryUploadAuthority String
      -- Get a single deferred photo.
    | IndexDbQueryDeferredPhoto
      -- When we successfully download a photo, we remove it from the `deferredPhotos` table.
      -- We just need the UUID.
    | IndexDbQueryRemoveDeferredPhoto String
      -- Update the number of attempts, a deferred photos was un-successfully downloaded.
      -- We don't count cases where we were offline.
    | IndexDbQueryUpdateDeferredPhotoAttempts IndexDbQueryDeferredPhotoResultRecord
    | IndexDbQueryRemoveUploadPhotos (List Int)
      -- Reports the number of entries at shardChanges table.
    | IndexDbQueryGetTotalEntriesToUpload


type IndexDbQueryTypeResult
    = -- A single photo for upload, if exists.
      IndexDbQueryUploadPhotoAuthorityResult (RemoteData UploadPhotoError (Maybe IndexDbQueryUploadPhotoResultRecord))
    | IndexDbQueryUploadAuthorityResult (Maybe IndexDbQueryUploadAuthorityResultRecord)
    | IndexDbQueryUploadGeneralResult (Maybe IndexDbQueryUploadGeneralResultRecord)
      -- A single deferred photo, if exists.
    | IndexDbQueryDeferredPhotoResult (Maybe IndexDbQueryDeferredPhotoResultRecord)
    | IndexDbQueryGetTotalEntriesToUploadResult Int


type UploadPhotoError
    = BadJson String
    | NetworkError String
    | UploadError String


type alias IndexDbSaveResult =
    { table : IndexDbSaveResultTable
    , status : IndexDbSaveStatus
    , timestamp : String
    }


type IndexDbSaveResultTable
    = IndexDbSaveResultTableAutority
    | IndexDbSaveResultTableAuthorityStats
    | IndexDbSaveResultTableDeferredPhotos
    | IndexDbSaveResultTableGeneral


type IndexDbSaveStatus
    = IndexDbSaveFailure
    | IndexDbSaveSuccess


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
    , remaining : Int
    }


type alias IndexDbQueryUploadAuthorityResultRecord =
    { entities : List ( BackendAuthorityEntity, UploadMethod )
    , remaining : Int

    -- Instead of list, it is be handier to get a Dict, keyed by the `localId`
    -- so when we would like to switch the photo URL with Drupal's file ID, we
    -- could get that info quicker.
    , uploadPhotos : Dict Int IndexDbQueryUploadPhotoResultRecord
    }


{-| The info we get from query to `deferredPhotos`.
-}
type alias IndexDbQueryDeferredPhotoResultRecord =
    { uuid : String
    , photo : String

    -- The number of attempts we've tried to get the image.
    , attempts : Int

    -- The number of photos remaining at deferredPhotos table.
    , remaining : Int
    }


{-| For slow devices, download request 'fetch' phase
takes less than 12 seconds, and the 'save' phase,
less than 3. Timeout is double the sum of the 2.
-}
downloadRequestTimeout : Int
downloadRequestTimeout =
    (12000 + 3000) * 2


type SyncIncidentType
    = FileUploadIncident IncidentContnentIdentifier
    | ContentUploadIncident IncidentContnentIdentifier


type alias IncidentContnentIdentifier =
    -- For file upload incident, identifier is the cache url of file.
    -- For content upload, it's the UUID of uploaded entity.
    String


type Msg
    = MsgDebouncer (Debouncer.Msg Msg)
    | NoOp
    | SchedulePageRefresh
    | SchedulePhotosDownload
    | RefreshPage
    | BackendAuthorityFetch
    | BackendAuthorityFetchHandle (Zipper SyncInfoAuthority) (WebData (DownloadSyncResponse BackendAuthorityEntity))
    | BackendAuthorityFetchedDataSavedHandle String
    | BackendAuthorityDashboardStatsFetch
    | BackendAuthorityDashboardStatsFetchHandle (Zipper SyncInfoAuthority) (WebData (DownloadSyncResponse BackendAuthorityEntity))
      -- This is the main entry point for the Sync loop. This will dispatch a call
      -- according to the `syncStatus`.
    | BackendFetchMain
    | BackendFetchPhotos
    | BackendGeneralFetch
    | BackendGeneralFetchHandle (WebData (DownloadSyncResponse BackendGeneralEntity))
    | BackendGeneralFetchedDataSavedHandle String
      -- Fetch a deferred photo from the server.
    | BackendDeferredPhotoFetch (Maybe IndexDbQueryDeferredPhotoResultRecord)
    | BackendDeferredPhotoFetchHandle IndexDbQueryDeferredPhotoResultRecord (WebData ())
      -- Unlike other `Backend...` msgs, we have no HTTP activity from Elm. That is,
      -- uploading the photos happens in JS, since we have to deal with file blobs
      -- which would be harder in Elm, given we have elm/http@1.0.
      -- This is the reason it doesn't get as arguments the result of the IndexDB.
    | BackendPhotoUploadAuthority
    | BackendUploadAuthority (Maybe IndexDbQueryUploadAuthorityResultRecord)
    | BackendUploadAuthorityHandle IndexDbQueryUploadAuthorityResultRecord (WebData ())
    | BackendUploadGeneral (Maybe IndexDbQueryUploadGeneralResultRecord)
    | BackendUploadGeneralHandle IndexDbQueryUploadGeneralResultRecord (WebData ())
    | BackendUploadPhotoAuthorityHandle (RemoteData UploadPhotoError (Maybe IndexDbQueryUploadPhotoResultRecord))
    | BackendReportState Int
    | BackendReportSyncIncident SyncIncidentType
    | QueryIndexDb IndexDbQueryType
    | QueryIndexDbHandle Value
    | SavedAtIndexDbHandle Value
    | FetchFromIndexDbDeferredPhoto
    | FetchFromIndexDbUploadGeneral
    | FetchFromIndexDbUploadAuthority
    | RevisionIdAuthorityAdd HealthCenterId
    | RevisionIdAuthorityRemove HealthCenterId
    | SetLastFetchedRevisionIdAuthority (Zipper SyncInfoAuthority) Int
    | SetLastFetchedRevisionIdGeneral Int
      -- UI settings
    | ResetSettings
    | SaveSettings
    | SetSyncCycle SyncCycle
    | SetSyncSpeedIdle String
    | SetSyncSpeedCycle String
    | SetSyncSpeedOffline String
    | TrySyncing
    | TryDownloadingPhotos
