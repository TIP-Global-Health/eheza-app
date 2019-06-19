module Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..), Revision(..), emptyModelIndexedDb)

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

import Backend.Clinic.Model exposing (Clinic)
import Backend.Counseling.Model exposing (CounselingSchedule, CounselingTopic, EveryCounselingSchedule)
import Backend.Entities exposing (..)
import Backend.HealthCenter.Model exposing (CatchmentArea, HealthCenter)
import Backend.Measurement.Model exposing (Attendance, ChildMeasurementList, ChildNutrition, CounselingSession, FamilyPlanning, Height, MotherMeasurementList, Muac, ParticipantConsent, Photo, Weight)
import Backend.Nurse.Model exposing (Nurse)
import Backend.ParticipantConsent.Model exposing (ParticipantForm)
import Backend.Person.Model exposing (Person)
import Backend.PmtctParticipant.Model exposing (PmtctParticipant)
import Backend.Relationship.Model exposing (MyRelationship, Relationship)
import Backend.Session.Model exposing (EditableSession, ExpectedParticipants, OfflineSession, Session)
import Backend.SyncData.Model exposing (SyncData)
import Dict exposing (Dict)
import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import RemoteData exposing (RemoteData(..), WebData)


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

    -- Data and requests relating to sync data
    , syncData : WebData (EveryDictList HealthCenterId SyncData)
    , saveSyncDataRequests : EveryDict HealthCenterId (WebData ())
    , deleteSyncDataRequests : EveryDict HealthCenterId (WebData ())

    -- For basic session data, we organize it in several ways in memory. One is
    -- by clinic, which we use when you navigate to a clinic page. The other
    -- tracks the sessions we expect a child to have attended. This is
    -- necessary for the progress report, because we organize that by session
    -- date, rather than measurement date, and we want to show blanks for
    -- missed sessions.  Because a child could change clinics, it's easier to
    -- ask the service worker to figure out the expected sessions rather than
    -- deriving it from data we already have in memory.
    , expectedSessions : EveryDict PersonId (WebData (EveryDictList SessionId Session))
    , sessionsByClinic : EveryDict ClinicId (WebData (EveryDictList SessionId Session))
    , sessions : EveryDict SessionId (WebData Session)

    -- Then, we also have a "synthetic" type `EditableSession`, which organizes
    -- the session data in a way that is congenial for our views. Ideally, we would
    -- redesign the views to use more "basic" data, but there is a fair bit of logic
    -- involved, so it require substantial work. For now, at least we remember the
    -- organized data here, and recalculate it when necessary.
    , editableSessions : EveryDict SessionId (WebData EditableSession)

    -- Tracks requests in progress to update sessions
    , sessionRequests : EveryDict SessionId Backend.Session.Model.Model

    -- We provide a mechanism for loading the children and mothers expected
    -- at a particular session.
    , expectedParticipants : EveryDict SessionId (WebData ExpectedParticipants)

    -- Measurement data for children and mothers. From this, we can construct
    -- the things we need for an `EditableSession` or for use on the progress
    -- report.
    , childMeasurements : EveryDict PersonId (WebData ChildMeasurementList)
    , motherMeasurements : EveryDict PersonId (WebData MotherMeasurementList)

    -- Tracks searchs for participants by name. The key is the phrase we are
    -- searching for.
    , personSearches : Dict String (WebData (EveryDictList PersonId Person))

    -- A simple cache of people.
    , people : EveryDict PersonId (WebData Person)

    -- From the point of view of the specified person, all of their relationships.
    , relationshipsByPerson : EveryDict PersonId (WebData (EveryDictList RelationshipId MyRelationship))

    -- Track what PMTCT groups a participant is in. (Inside a session, we can use
    -- `expectedParticipants`, but for registration etc. this is useful.
    , participantsByPerson : EveryDict PersonId (WebData (EveryDict PmtctParticipantId PmtctParticipant))

    -- Track requests to mutate data
    , postPerson : WebData PersonId
    , postPmtctParticipant : EveryDict PersonId (WebData ( PmtctParticipantId, PmtctParticipant ))
    , postRelationship : EveryDict PersonId (WebData MyRelationship)
    }


emptyModelIndexedDb : ModelIndexedDb
emptyModelIndexedDb =
    { childMeasurements = EveryDict.empty
    , clinics = NotAsked
    , deleteSyncDataRequests = EveryDict.empty
    , editableSessions = EveryDict.empty
    , everyCounselingSchedule = NotAsked
    , expectedParticipants = EveryDict.empty
    , expectedSessions = EveryDict.empty
    , healthCenters = NotAsked
    , motherMeasurements = EveryDict.empty
    , participantForms = NotAsked
    , participantsByPerson = EveryDict.empty
    , people = EveryDict.empty
    , personSearches = Dict.empty
    , postPerson = NotAsked
    , postPmtctParticipant = EveryDict.empty
    , postRelationship = EveryDict.empty
    , relationshipsByPerson = EveryDict.empty
    , saveSyncDataRequests = EveryDict.empty
    , sessionRequests = EveryDict.empty
    , sessions = EveryDict.empty
    , sessionsByClinic = EveryDict.empty
    , syncData = NotAsked
    }


type MsgIndexedDb
    = -- Messages which fetch various kinds of data.
      FetchChildMeasurements PersonId
    | FetchClinics
      -- For `FetchEditableSession`, you'll also need to send the messages
      -- you get from `Backend.Session.Fetch.fetchEditableSession`
    | FetchEditableSession SessionId
    | FetchEveryCounselingSchedule
    | FetchExpectedParticipants SessionId
    | FetchExpectedSessions PersonId
    | FetchHealthCenters
    | FetchMotherMeasurements PersonId
    | FetchParticipantForms
    | FetchParticipantsForPerson PersonId
    | FetchPeopleByName String
    | FetchPerson PersonId
    | FetchRelationshipsForPerson PersonId
    | FetchSession SessionId
    | FetchSessionsByClinic ClinicId
    | FetchSyncData
      -- Messages which handle responses to data
    | HandleFetchedChildMeasurements PersonId (WebData ChildMeasurementList)
    | HandleFetchedEveryCounselingSchedule (WebData EveryCounselingSchedule)
    | HandleFetchedMotherMeasurements PersonId (WebData MotherMeasurementList)
    | HandleFetchedClinics (WebData (EveryDictList ClinicId Clinic))
    | HandleFetchedExpectedParticipants SessionId (WebData ExpectedParticipants)
    | HandleFetchedExpectedSessions PersonId (WebData (EveryDictList SessionId Session))
    | HandleFetchedHealthCenters (WebData (EveryDictList HealthCenterId HealthCenter))
    | HandleFetchedParticipantForms (WebData (EveryDictList ParticipantFormId ParticipantForm))
    | HandleFetchedParticipantsForPerson PersonId (WebData (EveryDict PmtctParticipantId PmtctParticipant))
    | HandleFetchedPeopleByName String (WebData (EveryDictList PersonId Person))
    | HandleFetchedPerson PersonId (WebData Person)
    | HandleFetchedRelationshipsForPerson PersonId (WebData (EveryDictList RelationshipId MyRelationship))
    | HandleFetchedSession SessionId (WebData Session)
    | HandleFetchedSessionsByClinic ClinicId (WebData (EveryDictList SessionId Session))
    | HandleFetchedSyncData (WebData (EveryDictList HealthCenterId SyncData))
      -- Messages which mutate data
    | PostPerson (Maybe PersonId) Person -- The first person is a person we ought to offer setting a relationship to.
    | PostRelationship PersonId MyRelationship (Maybe ClinicId)
    | PostPmtctParticipant PmtctParticipant
      -- Messages which handle responses to mutating data
    | HandlePostedPerson (Maybe PersonId) (WebData PersonId)
    | HandlePostedRelationship PersonId (WebData MyRelationship)
    | HandlePostedPmtctParticipant PersonId (WebData ( PmtctParticipantId, PmtctParticipant ))
      -- Process some revisions we've received from the backend. In some cases,
      -- we can update our in-memory structures appropriately. In other cases, we
      -- can set them to `NotAsked` and let the "fetch" mechanism re-fetch them.
    | HandleRevisions (List Revision)
      -- Updating SyncData
    | SaveSyncData HealthCenterId SyncData
    | DeleteSyncData HealthCenterId
    | HandleSavedSyncData HealthCenterId (WebData ())
    | HandleDeletedSyncData HealthCenterId (WebData ())
      -- Handling edits to session data
    | MsgSession SessionId Backend.Session.Model.Msg


{-| Wrapper for all the revisions we can receive.
-}
type Revision
    = AttendanceRevision AttendanceId Attendance
    | CatchmentAreaRevision CatchmentAreaId CatchmentArea
    | ChildNutritionRevision ChildNutritionId ChildNutrition
    | ClinicRevision ClinicId Clinic
    | CounselingScheduleRevision CounselingScheduleId CounselingSchedule
    | CounselingSessionRevision CounselingSessionId CounselingSession
    | CounselingTopicRevision CounselingTopicId CounselingTopic
    | FamilyPlanningRevision FamilyPlanningId FamilyPlanning
    | HealthCenterRevision HealthCenterId HealthCenter
    | HeightRevision HeightId Height
    | MuacRevision MuacId Muac
    | NurseRevision NurseId Nurse
    | ParticipantConsentRevision ParticipantConsentId ParticipantConsent
    | ParticipantFormRevision ParticipantFormId ParticipantForm
    | PersonRevision PersonId Person
    | PhotoRevision PhotoId Photo
    | PmtctParticipantRevision PmtctParticipantId PmtctParticipant
    | RelationshipRevision RelationshipId Relationship
    | SessionRevision SessionId Session
    | WeightRevision WeightId Weight
