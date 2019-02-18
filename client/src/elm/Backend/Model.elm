module Backend.Model exposing (ModelBackend, ModelIndexedDb, MsgBackend(..), MsgIndexedDb(..), Participants, Revision(..), TrainingSessionAction(..), TrainingSessionRequest, emptyModelBackend, emptyModelIndexedDb)

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
import Backend.Counseling.Model exposing (CounselingSchedule, CounselingTopic, EveryCounselingSchedule)
import Backend.Entities exposing (..)
import Backend.HealthCenter.Model exposing (CatchmentArea, HealthCenter)
import Backend.Measurement.Model exposing (ChildMeasurementList, ChildNutrition, CounselingSession, FamilyPlanning, Height, MeasurementEdits, MotherMeasurementList, Muac, ParticipantConsent, Photo, Weight)
import Backend.Mother.Model exposing (Mother)
import Backend.Nurse.Model exposing (Nurse)
import Backend.ParticipantConsent.Model exposing (ParticipantForm)
import Backend.Session.Model exposing (EditableSession, OfflineSession, Session)
import Backend.SyncData.Model exposing (SyncData)
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
    -- Tracks a request to create a new session.
    { postSessionRequest : WebData ( SessionId, Session )

    -- Tracks a request to handle training session actions. Note that the
    -- backend currently doesn't supply a key, so we don't track one here.
    -- (That might change if the backend actually queued these requests, rather
    -- than processing them immediately).
    , postTrainingSessionRequest : WebData TrainingSessionRequest
    }


emptyModelBackend : ModelBackend
emptyModelBackend =
    { postSessionRequest = NotAsked
    , postTrainingSessionRequest = NotAsked
    }


{-| These are all the messages related to getting things from the backend and
putting things back into the backend.
-}
type MsgBackend
    = PostSession Session
    | PostTrainingSessionRequest TrainingSessionRequest
    | HandlePostedSession (WebData ( SessionId, Session ))
    | HandleTrainingSessionResponse (WebData TrainingSessionRequest)
    | ResetSessionRequests


type alias Participants =
    { children : EveryDictList ChildId Child
    , mothers : EveryDictList MotherId Mother
    }


{-| This tracks data we fetch from IndexedDB via the service worker. Gradually, we'll
move things here from ModelBackend and ModelCached.
-}
type alias ModelIndexedDb =
    -- For several kinds of data, we keep all the basic data in memory. For
    -- instance, all basic data for clinics, health centers, etc. We might not
    -- actually need all the clinics at once, but there should be a reasonable
    -- number.
    { clinics : WebData (EveryDictList ClinicId Clinic)
    , everyCounselingSchedule : WebData EveryCounselingSchedule
    , healthCenters : WebData (EveryDictList HealthCenterId HealthCenter)
    , participantForms : WebData (EveryDictList ParticipantFormId ParticipantForm)
    , syncData : WebData (EveryDictList HealthCenterId SyncData)

    -- For basic session data, we organize it in several ways in memory. One is
    -- by clinic, which we use when you navigate to a clinic page. The other
    -- tracks the sessions we expect a child to have attended. This is
    -- necessary for the progress report, because we organize that by session
    -- date, rather than measurement date, and we want to show blanks for
    -- missed sessions.  Because a child could change clinics, it's easier to
    -- ask the service worker to figure out the expected sessions rather than
    -- deriving it from data we already have in memory.
    , expectedSessions : EveryDict ChildId (WebData (EveryDictList SessionId Session))
    , sessionsByClinic : EveryDict ClinicId (WebData (EveryDictList SessionId Session))
    , sessions : EveryDict SessionId (WebData Session)

    -- We provide a mechanism for loading the children and mothers expected
    -- at a particular session.
    , expectedParticipants : EveryDict SessionId (WebData Participants)

    -- Measurement data for children and mothers. From this, we can construct
    -- the things we need for an `EditableSession` or for use on the progress
    -- report.
    , childMeasurements : EveryDict ChildId (WebData ChildMeasurementList)
    , motherMeasurements : EveryDict MotherId (WebData MotherMeasurementList)
    }


emptyModelIndexedDb : ModelIndexedDb
emptyModelIndexedDb =
    { childMeasurements = EveryDict.empty
    , clinics = NotAsked
    , everyCounselingSchedule = NotAsked
    , expectedParticipants = EveryDict.empty
    , expectedSessions = EveryDict.empty
    , healthCenters = NotAsked
    , motherMeasurements = EveryDict.empty
    , participantForms = NotAsked
    , sessions = EveryDict.empty
    , sessionsByClinic = EveryDict.empty
    , syncData = NotAsked
    }


type MsgIndexedDb
    = -- Messages which fetch various kinds of data
      FetchChildMeasurements ChildId
    | FetchClinics
    | FetchEveryCounselingSchedule
    | FetchExpectedParticipants SessionId
    | FetchExpectedSessions ChildId
    | FetchHealthCenters
    | FetchMotherMeasurements MotherId
    | FetchParticipantForms
    | FetchSession SessionId
    | FetchSessionsByClinic ClinicId
    | FetchSyncData
      -- Messages which handle responses to data
    | HandleFetchedChildMeasurements ChildId (WebData ChildMeasurementList)
    | HandleFetchedEveryCounselingSchedule (WebData EveryCounselingSchedule)
    | HandleFetchedMotherMeasurements MotherId (WebData MotherMeasurementList)
    | HandleFetchedClinics (WebData (EveryDictList ClinicId Clinic))
    | HandleFetchedExpectedParticipants SessionId (WebData Participants)
    | HandleFetchedExpectedSessions ChildId (WebData (EveryDictList SessionId Session))
    | HandleFetchedHealthCenters (WebData (EveryDictList HealthCenterId HealthCenter))
    | HandleFetchedParticipantForms (WebData (EveryDictList ParticipantFormId ParticipantForm))
    | HandleFetchedSession SessionId (WebData Session)
    | HandleFetchedSessionsByClinic ClinicId (WebData (EveryDictList SessionId Session))
    | HandleFetchedSyncData (WebData (EveryDictList HealthCenterId SyncData))
      -- Process some revisions we've received from the backend. In some cases,
      -- we can update our in-memory structures appropriately. In other cases, we
      -- can set them to `NotAsked` and let the "fetch" mechanism re-fetch them.
    | HandleRevisions (List Revision)
      -- Updating SyncData
    | SaveSyncData HealthCenterId SyncData
    | DeleteSyncData HealthCenterId
      -- Handling edits to session data
    | MsgSession SessionId Backend.Session.Model.Msg
      -- Temporary!
    | IgnoreResponse


{-| Wrapper for all the revisions we can receive.
-}
type Revision
    = CatchmentAreaRevision CatchmentAreaId CatchmentArea
    | ChildNutritionRevision ChildNutritionId ChildNutrition
    | ChildRevision ChildId Child
    | ClinicRevision ClinicId Clinic
    | CounselingScheduleRevision CounselingScheduleId CounselingSchedule
    | CounselingSessionRevision CounselingSessionId CounselingSession
    | CounselingTopicRevision CounselingTopicId CounselingTopic
    | FamilyPlanningRevision FamilyPlanningId FamilyPlanning
    | HealthCenterRevision HealthCenterId HealthCenter
    | HeightRevision HeightId Height
    | MotherRevision MotherId Mother
    | MuacRevision MuacId Muac
    | NurseRevision NurseId Nurse
    | ParticipantConsentRevision ParticipantConsentId ParticipantConsent
    | ParticipantFormRevision ParticipantFormId ParticipantForm
    | PhotoRevision PhotoId Photo
    | SessionRevision SessionId Session
    | WeightRevision WeightId Weight
      -- This last one is temporary, as we gradually convert from IDs to UUIDs
    | NotYetImplemented


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
