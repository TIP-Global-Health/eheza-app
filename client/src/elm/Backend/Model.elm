module Backend.Model exposing (CachedSessionError(..), ModelBackend, ModelCached, ModelIndexedDb, MsgBackend(..), MsgCached(..), MsgIndexedDb(..), Revision(..), TrainingSessionAction(..), TrainingSessionRequest, emptyModelBackend, emptyModelCached, emptyModelIndexedDb)

{-| The `Backend` hierarchy is for code that represents entities from the
backend. It is reponsible for fetching them, saving them, etc.

  - There shouldn't be any UI code here (except possibly some UI that
    is specifically related to fetching and saving -- we'll see).

  - There shouldn't be data here that purely relates to the local state of the
    app. If it isn't persisted to the backend, that state can go elsewhere.

The nice thing about this is that we can segregate local state (like whether
a dialog box is open etc.) from the state that persists to the backend.
That way, we can more easily have a single source of truth for the
backend data -- we're not tempted to duplicate it in various places
in the UI.

-}

import Backend.Child.Model exposing (Child)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Counseling.Model exposing (CounselingSchedule, CounselingTopic)
import Backend.Entities exposing (..)
import Backend.HealthCenter.Model exposing (CatchmentArea, HealthCenter)
import Backend.Measurement.Model exposing (ChildNutrition, FamilyPlanning, Height, MeasurementEdits, Muac, ParticipantConsent, Photo, Weight)
import Backend.Mother.Model exposing (Mother)
import Backend.Nurse.Model exposing (Nurse)
import Backend.ParticipantConsent.Model exposing (ParticipantForm)
import Backend.Session.Model exposing (EditableSession, MsgEditableSession, OfflineSession, Session)
import Backend.SyncData.Model exposing (SyncData)
import CacheStorage.Model
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import Gizra.NominalDate exposing (NominalDate)
import Http exposing (Error)
import Json.Encode exposing (Value)
import RemoteData exposing (RemoteData(..), WebData)


{-| This model basically represents things we have locally which also belong
on the backend. So, conceptually it is a kind of a local cache of some of the
things on the backend.
-}
type alias ModelBackend =
    -- This tracks future sessions ... that is, sessions which are either
    -- available now, or will be in the future. We remember which
    -- date we asked about, so that if the date changes (i.e. it becomes
    -- tomorrow, due to the passage of time), we can know that we ought to
    -- ask again.
    --
    -- We fetch all the future sessions at once, if we need them at all.
    -- The data type is probably small enough that this is fine ... we can
    -- fetch them in smaller batches if necessary (by clinicId, probably).
    --
    -- TODO: Restful.Endpoint should eventually have a `QueryResult` type which
    -- remembers the params we supplied and a WebData for the result ...
    -- since one would really always want to remember what query the results
    -- represent. (And, eventually, one would want to remember the `count`
    -- and which pages you have etc.).
    { futureSessions : WebData ( NominalDate, EveryDictList SessionId Session )

    -- This is a flag which tracks our progress in downloading an
    -- offlineSession from the backend. We don't actually **store** the data
    -- here, because we want to use it from the cache, and only consider it
    -- **really** available if we can get if from the cache. However, it's
    -- still handy to have a flag that tells us whether a request is in
    -- progress or not. (In fact, we need to know, for the UI).
    --
    -- We do remember which sessionID we downloaded, since that helps a bit
    -- to match things up in the UI.
    , offlineSessionRequest : WebData SessionId

    -- Another flag, tracking our progress in uploading edits to the backend.
    -- Again, we track which session we uploaded.
    , uploadEditsRequest : WebData SessionId

    -- Tracks a request to create a new session.
    , postSessionRequest : WebData ( SessionId, Session )

    -- Tracks a request to handle training session actions. Note that the
    -- backend currently doesn't supply a key, so we don't track one here.
    -- (That might change if the backend actually queued these requests, rather
    -- than processing them immediately).
    , postTrainingSessionRequest : WebData TrainingSessionRequest
    }


emptyModelBackend : ModelBackend
emptyModelBackend =
    { futureSessions = NotAsked
    , offlineSessionRequest = NotAsked
    , uploadEditsRequest = NotAsked
    , postSessionRequest = NotAsked
    , postTrainingSessionRequest = NotAsked
    }


{-| These are all the messages related to getting things from the backend and
putting things back into the backend.
-}
type MsgBackend
    = FetchFutureSessions NominalDate
    | FetchOfflineSessionFromBackend SessionId
    | HandleFetchedOfflineSessionFromBackend (Result Error ( SessionId, OfflineSession ))
    | HandleFetchedSessions NominalDate (WebData (EveryDictList SessionId Session))
    | HandleRefetchedOfflineSession (Result Error ( SessionId, OfflineSession ))
    | HandleUploadedEdits SessionId (Result Error ())
    | HandleUploadPhotoResponse Photo (Result Error Int)
    | PostSession Session
    | PostTrainingSessionRequest TrainingSessionRequest
    | HandlePostedSession (WebData ( SessionId, Session ))
    | HandleTrainingSessionResponse (WebData TrainingSessionRequest)
    | RefetchOfflineSession SessionId
    | ResetErrors -- reset errors to `NotAsked` when certain requests succeed, so they will retry
    | ResetSessionRequests -- resets certain requests to `NotAsked` if successful or error ... upon navigation
    | ResetOfflineSessionRequest -- resets it to `NotAsked`
    | ResetUploadEditsRequest
    | UploadEdits SessionId MeasurementEdits
    | UploadPhoto Photo


{-| This tracks data we fetch from IndexedDB via the service worker. Gradually, we'll
move things here from ModelBackend and ModelCached.
-}
type alias ModelIndexedDb =
    { clinics : WebData (EveryDictList ClinicId Clinic)
    , healthCenters : WebData (EveryDictList HealthCenterId HealthCenter)
    , syncData : WebData (EveryDictList HealthCenterId SyncData)
    , sessionsByClinic : EveryDict ClinicId (WebData (EveryDictList SessionId Session))
    }


emptyModelIndexedDb : ModelIndexedDb
emptyModelIndexedDb =
    { clinics = NotAsked
    , healthCenters = NotAsked
    , syncData = NotAsked
    , sessionsByClinic = EveryDict.empty
    }


type MsgIndexedDb
    = FetchClinics
    | FetchHealthCenters
    | FetchSessionsByClinic ClinicId
    | FetchSyncData
    | HandleFetchedClinics (WebData (EveryDictList ClinicId Clinic))
    | HandleFetchedHealthCenters (WebData (EveryDictList HealthCenterId HealthCenter))
    | HandleFetchedSessionsByClinic ClinicId (WebData (EveryDictList SessionId Session))
    | HandleFetchedSyncData (WebData (EveryDictList HealthCenterId SyncData))
    | HandleRevisions (List Revision)
    | SaveSyncData HealthCenterId SyncData
    | DeleteSyncData HealthCenterId
    | IgnoreResponse


{-| Wrapper for all the revisions we can receive.
-}
type Revision
    = CatchmentAreaRevision CatchmentAreaId CatchmentArea
    | ChildRevision ChildId Child
    | ClinicRevision ClinicId Clinic
    | HealthCenterRevision HealthCenterId HealthCenter
    | MotherRevision MotherId Mother
    | SessionRevision SessionId Session
    | NurseRevision NurseId Nurse
    | FamilyPlanningRevision FamilyPlanningId FamilyPlanning
    | HeightRevision HeightId Height
    | MuacRevision MuacId Muac
    | ChildNutritionRevision ChildNutritionId ChildNutrition
    | PhotoRevision PhotoId Photo
    | WeightRevision WeightId Weight
    | ParticipantFormRevision ParticipantFormId ParticipantForm
    | CounselingScheduleRevision CounselingScheduleId CounselingSchedule
    | CounselingTopicRevision CounselingTopicId CounselingTopic
      -- This last one is temporary, as we gradually convert from IDs to UUIDs
    | NotYetImplemented


{-| This models things which we cache locally ... so, like `ModelBackend`, but
instead of saving them to the backend, we save them locally.
-}
type alias ModelCached =
    -- This tracks, if we have one, the EditableSession which we're currently
    -- doing data entry for.
    --
    -- The `WebData` wrapper represents whether we've tried to fetch it from
    -- our local cache (and any error that may have occurred). The inner
    -- `Maybe` represents whether it was actually found. That is, if we
    -- successfully query our local cache, and find it's not there, then the
    -- `WebData` layer is a `Success`, and the `Maybe` is a `Nothing`.
    --
    -- At least at first, we'll track our "mode" by whether we have an editable
    -- session in local storage. So:
    --
    -- * We'll automatically try to load an editable session from local storage
    --   when the app starts up.
    --
    -- * If we get one, we'll automatically show that in the UI, and prevent
    --   other things from showing.
    --
    -- * If we don't, then we'll show other things.
    --
    -- Note that this assumes that:
    --
    -- * We're only allowing a single offline session at a time to be stored
    --   locally.
    --
    -- * If we have one, we're definitely using it, not doing something else.
    --
    -- In fact, we'll also do slightly different things in the UI depending
    -- on whether our editable session has edits or not ... you won't be
    -- locked into edit mode until you've made an edit.
    { editableSession : RemoteData CachedSessionError (Maybe ( SessionId, EditableSession ))

    -- This uses the `CacheStorage` API, which ultimately will be nicer
    -- than using local storage ... so, eventually could transition
    -- editableSession into here as well.
    , cacheStorage : CacheStorage.Model.Model
    }


{-| The errors that can occur as we try to read the cached sesssion from
local storage.

  - FoundEditsButNoSession means that the decoders succeeded, and we found
    some edits, but there was no session.

  - DecodersFailed means that one or both of the decoders failed.

-}
type CachedSessionError
    = FoundEditsButNoSession { editsJson : String }
    | DecodersFailed
        { editsJson : String
        , editsError : Maybe String
        , offlineSessionJson : String
        , offlineSessionError : Maybe String
        }


emptyModelCached : ModelCached
emptyModelCached =
    { cacheStorage = CacheStorage.Model.emptyModel
    , editableSession = NotAsked
    }


{-| This represents a request sent to `/api/training_sessions`, which is an
endpoint that represents certain actions that can be taken with respect to
training sessions as a whole. So, "creating" a request there is like queueing
up an action for the backend to take.

As a simplification, the backend currently executes the action immediately, but
you might imagine it queuing it up, in which case we could have an ID field
here, to use in future requests. (For instance, DELETE might cancel the
request).

-}
type alias TrainingSessionRequest =
    { action : TrainingSessionAction
    }


{-| An action we can ask `/api/training_sessions` to perform.

  - CreateAll will create a new training session, for today, for every clinic
    that doesn't already have a training session starting today.

  - DeleteAll will delete all training sessions.

A training session is just like a regular session, except that you can delete
it with `DeleteAll` here. So, it facilitates having some "permanent" sessions
(for pre-existing data), and some sessions you create and delete as training
occurs.

-}
type TrainingSessionAction
    = CreateAll
    | DeleteAll


{-| These are all the messages related to getting things from the cache and
putting things back into the cache.

We parameterize by the sessionId for many of these. For now, we're only
really using one cache slot, so we have to be careful that we don't
blow away something we actually want to keep. (That is, we have to be
careful not to save over an EditableSession that has edits). We could
instead use multiple slots easily enough, I suppose.

-}
type MsgCached
    = -- Caches the whole editable session, including edits. We've only
      -- got one slot, for now, so you need to make sure you're not
      -- overwriting something that has edits. Or, perhaps we could
      -- check ... I suppose we know!
      CacheEditableSession
    | CacheEditableSessionResult Value
      -- Fetches the whole editable session from the cache.
    | FetchEditableSessionFromCache
    | HandleEditableSession ( String, String )
      -- Just cache the edits ... assumes we already have the offlineSession
      -- part cached, so we don't need to keep doing it. That is, we treat
      -- the offlineSession part as immutable, so we only have to keep
      -- saving the edits over and over.
      --
      -- For now, we save all the edits at once, so in that sense we save
      -- them over and over. If that ends up causing any trouble, we could
      -- do something more sophisticated, but it's probably not necessary.
    | CacheEdits
    | CacheEditsResult Value
      -- Calls back to MsgBackend to upload edits
    | ContinueUploadingEdits
      -- Deletes an editable session from the cache. You shouldn't call this
      -- if the session has edits that haven't been saved to the backend!
    | DeleteEditableSession
      -- Messages for cacheStorage
    | MsgCacheStorage CacheStorage.Model.Msg
      -- Some messages which we define elsewhere that the UI can send to
      -- modify an editable session.
    | MsgEditableSession MsgEditableSession
      -- Replace whatever we have with this
    | SetEditableSession SessionId EditableSession
      -- Replace just the OfflineSession, if the sessionId's match
    | SetOfflineSession SessionId OfflineSession
