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

import AssocList as Dict exposing (Dict)
import Backend.Clinic.Model exposing (Clinic, ClinicType)
import Backend.Counseling.Model exposing (CounselingSchedule, CounselingTopic, EveryCounselingSchedule)
import Backend.Entities exposing (..)
import Backend.HealthCenter.Model exposing (CatchmentArea, HealthCenter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))
import Backend.Measurement.Model exposing (..)
import Backend.Nurse.Model exposing (Nurse)
import Backend.NutritionEncounter.Model exposing (NutritionEncounter)
import Backend.ParticipantConsent.Model exposing (ParticipantForm)
import Backend.Person.Model exposing (Person, RegistrationInitiator)
import Backend.PmtctParticipant.Model exposing (PmtctParticipant)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)
import Backend.Relationship.Model exposing (MyRelationship, Relationship)
import Backend.Session.Model exposing (EditableSession, ExpectedParticipants, OfflineSession, Session)
import Backend.SyncData.Model exposing (SyncData)
import Backend.Village.Model exposing (Village)
import RemoteData exposing (RemoteData(..), WebData)


{-| This tracks data we fetch from IndexedDB via the service worker. Gradually, we'll
move things here from ModelBackend and ModelCached.
-}
type alias ModelIndexedDb =
    -- For several kinds of data, we keep all the basic data in memory. For
    -- instance, all basic data for clinics, health centers, etc. We might not
    -- actually need all the clinics at once, but there should be a reasonable
    -- number.
    { clinics : WebData (Dict ClinicId Clinic)
    , everyCounselingSchedule : WebData EveryCounselingSchedule
    , healthCenters : WebData (Dict HealthCenterId HealthCenter)
    , villages : WebData (Dict VillageId Village)
    , participantForms : WebData (Dict ParticipantFormId ParticipantForm)

    -- Data and requests relating to sync data
    , syncData : WebData (Dict HealthCenterId SyncData)
    , saveSyncDataRequests : Dict HealthCenterId (WebData ())
    , deleteSyncDataRequests : Dict HealthCenterId (WebData ())

    -- For basic session data, we organize it in several ways in memory. One is
    -- by clinic, which we use when you navigate to a clinic page. The other
    -- tracks the sessions we expect a child to have attended. This is
    -- necessary for the progress report, because we organize that by session
    -- date, rather than measurement date, and we want to show blanks for
    -- missed sessions.  Because a child could change clinics, it's easier to
    -- ask the service worker to figure out the expected sessions rather than
    -- deriving it from data we already have in memory.
    , expectedSessions : Dict PersonId (WebData (Dict SessionId Session))
    , sessionsByClinic : Dict ClinicId (WebData (Dict SessionId Session))
    , sessions : Dict SessionId (WebData Session)

    -- Then, we also have a "synthetic" type `EditableSession`, which organizes
    -- the session data in a way that is congenial for our views. Ideally, we would
    -- redesign the views to use more "basic" data, but there is a fair bit of logic
    -- involved, so it require substantial work. For now, at least we remember the
    -- organized data here, and recalculate it when necessary.
    , editableSessions : Dict SessionId (WebData EditableSession)

    -- Tracks requests in progress to update sessions, prenatal sessions or prenatal encounters.
    , sessionRequests : Dict SessionId Backend.Session.Model.Model
    , prenatalEncounterRequests : Dict PrenatalEncounterId Backend.PrenatalEncounter.Model.Model
    , nutritionEncounterRequests : Dict NutritionEncounterId Backend.NutritionEncounter.Model.Model
    , individualSessionRequests : Dict IndividualEncounterParticipantId Backend.IndividualEncounterParticipant.Model.Model

    -- We provide a mechanism for loading the children and mothers expected
    -- at a particular session.
    , expectedParticipants : Dict SessionId (WebData ExpectedParticipants)

    -- Measurement data for children and mothers. From this, we can construct
    -- the things we need for an `EditableSession` or for use on the progress
    -- report.
    , childMeasurements : Dict PersonId (WebData ChildMeasurementList)
    , motherMeasurements : Dict PersonId (WebData MotherMeasurementList)

    -- Tracks searchs for participants by name. The key is the phrase we are
    -- searching for.
    , personSearches : Dict String (WebData (Dict PersonId Person))

    -- A simple cache of several things.
    , people : Dict PersonId (WebData Person)
    , prenatalEncounters : Dict PrenatalEncounterId (WebData PrenatalEncounter)
    , nutritionEncounters : Dict NutritionEncounterId (WebData NutritionEncounter)
    , individualParticipants : Dict IndividualEncounterParticipantId (WebData IndividualEncounterParticipant)

    -- Cache things organized in certain ways.
    , individualParticipantsByPerson : Dict PersonId (WebData (Dict IndividualEncounterParticipantId IndividualEncounterParticipant))
    , prenatalEncountersByParticipant : Dict IndividualEncounterParticipantId (WebData (Dict PrenatalEncounterId PrenatalEncounter))
    , nutritionEncountersByParticipant : Dict IndividualEncounterParticipantId (WebData (Dict NutritionEncounterId NutritionEncounter))
    , prenatalMeasurements : Dict PrenatalEncounterId (WebData PrenatalMeasurements)
    , nutritionMeasurements : Dict NutritionEncounterId (WebData NutritionMeasurements)

    -- From the point of view of the specified person, all of their relationships.
    , relationshipsByPerson : Dict PersonId (WebData (Dict RelationshipId MyRelationship))

    -- Track what PMTCT groups a participant is in. (Inside a session, we can use
    -- `expectedParticipants`, but for registration etc. this is useful.
    , participantsByPerson : Dict PersonId (WebData (Dict PmtctParticipantId PmtctParticipant))

    -- Track requests to mutate data
    , postPerson : WebData PersonId
    , postPmtctParticipant : Dict PersonId (WebData ( PmtctParticipantId, PmtctParticipant ))
    , postRelationship : Dict PersonId (WebData MyRelationship)
    , postSession : WebData SessionId
    , postIndividualSession : Dict PersonId (WebData ( IndividualEncounterParticipantId, IndividualEncounterParticipant ))
    , postPrenatalEncounter : Dict IndividualEncounterParticipantId (WebData ( PrenatalEncounterId, PrenatalEncounter ))
    , postNutritionEncounter : Dict IndividualEncounterParticipantId (WebData ( NutritionEncounterId, NutritionEncounter ))
    }


emptyModelIndexedDb : ModelIndexedDb
emptyModelIndexedDb =
    { childMeasurements = Dict.empty
    , clinics = NotAsked
    , deleteSyncDataRequests = Dict.empty
    , editableSessions = Dict.empty
    , everyCounselingSchedule = NotAsked
    , expectedParticipants = Dict.empty
    , expectedSessions = Dict.empty
    , healthCenters = NotAsked
    , villages = NotAsked
    , motherMeasurements = Dict.empty
    , nutritionEncounters = Dict.empty
    , nutritionEncountersByParticipant = Dict.empty
    , nutritionMeasurements = Dict.empty
    , participantForms = NotAsked
    , participantsByPerson = Dict.empty
    , people = Dict.empty
    , personSearches = Dict.empty
    , postNutritionEncounter = Dict.empty
    , postPerson = NotAsked
    , postPmtctParticipant = Dict.empty
    , postIndividualSession = Dict.empty
    , postPrenatalEncounter = Dict.empty
    , postRelationship = Dict.empty
    , postSession = NotAsked
    , prenatalEncounters = Dict.empty
    , prenatalEncounterRequests = Dict.empty
    , nutritionEncounterRequests = Dict.empty
    , individualSessionRequests = Dict.empty
    , individualParticipants = Dict.empty
    , individualParticipantsByPerson = Dict.empty
    , prenatalEncountersByParticipant = Dict.empty
    , prenatalMeasurements = Dict.empty
    , relationshipsByPerson = Dict.empty
    , saveSyncDataRequests = Dict.empty
    , sessionRequests = Dict.empty
    , sessions = Dict.empty
    , sessionsByClinic = Dict.empty
    , syncData = NotAsked
    }


type MsgIndexedDb
    = -- Messages which fetch various kinds of data.
      FetchChildMeasurements PersonId
    | FetchChildrenMeasurements (List PersonId)
    | FetchClinics
      -- For `FetchEditableSession`, you'll also need to send the messages
      -- you get from `Backend.Session.Fetch.fetchEditableSession`
    | FetchEditableSession SessionId (List MsgIndexedDb)
    | FetchEditableSessionCheckedIn SessionId
    | FetchEditableSessionMeasurements SessionId
    | FetchEditableSessionSummaryByActivity SessionId
    | FetchEditableSessionSummaryByParticipant SessionId
    | FetchEveryCounselingSchedule
    | FetchExpectedParticipants SessionId
    | FetchExpectedSessions PersonId
    | FetchHealthCenters
    | FetchIndividualEncounterParticipantsForPerson PersonId
    | FetchMotherMeasurements PersonId
    | FetchMothersMeasurements (List PersonId)
    | FetchNutritionEncounter NutritionEncounterId
    | FetchNutritionEncountersForParticipant IndividualEncounterParticipantId
    | FetchNutritionMeasurements NutritionEncounterId
    | FetchParticipantForms
    | FetchParticipantsForPerson PersonId
    | FetchPeopleByName String
    | FetchPeople (List PersonId)
    | FetchPerson PersonId
    | FetchPrenatalEncounter PrenatalEncounterId
    | FetchPrenatalEncountersForParticipant IndividualEncounterParticipantId
    | FetchPrenatalMeasurements PrenatalEncounterId
    | FetchIndividualEncounterParticipant IndividualEncounterParticipantId
    | FetchRelationshipsForPerson PersonId
    | FetchSession SessionId
    | FetchSessionsByClinic ClinicId
    | FetchSyncData
    | FetchVillages
      -- Messages which handle responses to data
    | HandleFetchedChildMeasurements PersonId (WebData ChildMeasurementList)
    | HandleFetchedChildrenMeasurements (WebData (Dict PersonId ChildMeasurementList))
    | HandleFetchedClinics (WebData (Dict ClinicId Clinic))
    | HandleFetchedEveryCounselingSchedule (WebData EveryCounselingSchedule)
    | HandleFetchedExpectedParticipants SessionId (WebData ExpectedParticipants)
    | HandleFetchedExpectedSessions PersonId (WebData (Dict SessionId Session))
    | HandleFetchedHealthCenters (WebData (Dict HealthCenterId HealthCenter))
    | HandleFetchedIndividualEncounterParticipantsForPerson PersonId (WebData (Dict IndividualEncounterParticipantId IndividualEncounterParticipant))
    | HandleFetchedMotherMeasurements PersonId (WebData MotherMeasurementList)
    | HandleFetchedMothersMeasurements (WebData (Dict PersonId MotherMeasurementList))
    | HandleFetchedNutritionEncounter NutritionEncounterId (WebData NutritionEncounter)
    | HandleFetchedNutritionEncountersForParticipant IndividualEncounterParticipantId (WebData (Dict NutritionEncounterId NutritionEncounter))
    | HandleFetchedNutritionMeasurements NutritionEncounterId (WebData NutritionMeasurements)
    | HandleFetchedParticipantForms (WebData (Dict ParticipantFormId ParticipantForm))
    | HandleFetchedParticipantsForPerson PersonId (WebData (Dict PmtctParticipantId PmtctParticipant))
    | HandleFetchedPeopleByName String (WebData (Dict PersonId Person))
    | HandleFetchedPerson PersonId (WebData Person)
    | HandleFetchPeople (WebData (Dict PersonId Person))
    | HandleFetchedPrenatalEncounter PrenatalEncounterId (WebData PrenatalEncounter)
    | HandleFetchedPrenatalEncountersForParticipant IndividualEncounterParticipantId (WebData (Dict PrenatalEncounterId PrenatalEncounter))
    | HandleFetchedPrenatalMeasurements PrenatalEncounterId (WebData PrenatalMeasurements)
    | HandleFetchedIndividualEncounterParticipant IndividualEncounterParticipantId (WebData IndividualEncounterParticipant)
    | HandleFetchedRelationshipsForPerson PersonId (WebData (Dict RelationshipId MyRelationship))
    | HandleFetchedSession SessionId (WebData Session)
    | HandleFetchedSessionsByClinic ClinicId (WebData (Dict SessionId Session))
    | HandleFetchedSyncData (WebData (Dict HealthCenterId SyncData))
    | HandleFetchedVillages (WebData (Dict VillageId Village))
      -- Messages which mutate data
    | PostPerson (Maybe PersonId) RegistrationInitiator Person -- The first person is a person we ought to offer setting a relationship to.
    | PatchPerson PersonId Person
    | PostRelationship PersonId MyRelationship (Maybe ClinicId)
    | PostPmtctParticipant PmtctParticipant
    | PostSession Session
    | PostIndividualSession IndividualEncounterParticipant
    | PostPrenatalEncounter PrenatalEncounter
    | PostNutritionEncounter NutritionEncounter
      -- Messages which handle responses to mutating data
    | HandlePostedPerson (Maybe PersonId) RegistrationInitiator (WebData PersonId)
    | HandlePatchedPerson PersonId (WebData Person)
    | HandlePostedRelationship PersonId (WebData MyRelationship)
    | HandlePostedPmtctParticipant PersonId (WebData ( PmtctParticipantId, PmtctParticipant ))
    | HandlePostedSession ClinicType (WebData SessionId)
    | HandlePostedIndividualSession PersonId IndividualEncounterType (WebData ( IndividualEncounterParticipantId, IndividualEncounterParticipant ))
    | HandlePostedPrenatalEncounter IndividualEncounterParticipantId (WebData ( PrenatalEncounterId, PrenatalEncounter ))
    | HandlePostedNutritionEncounter IndividualEncounterParticipantId (WebData ( NutritionEncounterId, NutritionEncounter ))
      -- Process some revisions we've received from the backend. In some cases,
      -- we can update our in-memory structures appropriately. In other cases, we
      -- can set them to `NotAsked` and let the "fetch" mechanism re-fetch them.
    | HandleRevisions (List Revision)
      -- Updating SyncData
    | SaveSyncData HealthCenterId SyncData
    | DeleteSyncData HealthCenterId
    | HandleSavedSyncData HealthCenterId (WebData ())
    | HandleDeletedSyncData HealthCenterId (WebData ())
      -- Handling edits to session data or encounter data
    | MsgSession SessionId Backend.Session.Model.Msg
    | MsgPrenatalEncounter PrenatalEncounterId Backend.PrenatalEncounter.Model.Msg
    | MsgNutritionEncounter NutritionEncounterId Backend.NutritionEncounter.Model.Msg
    | MsgIndividualSession IndividualEncounterParticipantId Backend.IndividualEncounterParticipant.Model.Msg


{-| Wrapper for all the revisions we can receive.
-}
type Revision
    = AttendanceRevision AttendanceId Attendance
    | BreastExamRevision BreastExamId BreastExam
    | CatchmentAreaRevision CatchmentAreaId CatchmentArea
    | ChildFbfRevision ChildFbfId Fbf
    | ChildNutritionRevision ChildNutritionId ChildNutrition
    | ClinicRevision ClinicId Clinic
    | CorePhysicalExamRevision CorePhysicalExamId CorePhysicalExam
    | CounselingScheduleRevision CounselingScheduleId CounselingSchedule
    | CounselingSessionRevision CounselingSessionId CounselingSession
    | CounselingTopicRevision CounselingTopicId CounselingTopic
    | DangerSignsRevision DangerSignsId DangerSigns
    | FamilyPlanningRevision FamilyPlanningId FamilyPlanning
    | LactationRevision LactationId Lactation
    | HealthCenterRevision HealthCenterId HealthCenter
    | HeightRevision HeightId Height
    | LastMenstrualPeriodRevision LastMenstrualPeriodId LastMenstrualPeriod
    | MedicalHistoryRevision MedicalHistoryId MedicalHistory
    | MedicationRevision MedicationId Medication
    | MotherFbfRevision MotherFbfId Fbf
    | MuacRevision MuacId Muac
    | NurseRevision NurseId Nurse
    | NutritionEncounterRevision NutritionEncounterId NutritionEncounter
    | NutritionHeightRevision NutritionHeightId NutritionHeight
    | NutritionMuacRevision NutritionMuacId NutritionMuac
    | NutritionNutritionRevision NutritionNutritionId NutritionNutrition
    | NutritionPhotoRevision NutritionPhotoId NutritionPhoto
    | NutritionWeightRevision NutritionWeightId NutritionWeight
    | ObstetricalExamRevision ObstetricalExamId ObstetricalExam
    | ObstetricHistoryRevision ObstetricHistoryId ObstetricHistory
    | ObstetricHistoryStep2Revision ObstetricHistoryStep2Id ObstetricHistoryStep2
    | ParticipantConsentRevision ParticipantConsentId ParticipantConsent
    | ParticipantFormRevision ParticipantFormId ParticipantForm
    | PersonRevision PersonId Person
    | PhotoRevision PhotoId Photo
    | PmtctParticipantRevision PmtctParticipantId PmtctParticipant
    | PrenatalFamilyPlanningRevision PrenatalFamilyPlanningId PrenatalFamilyPlanning
    | PrenatalNutritionRevision PrenatalNutritionId PrenatalNutrition
    | IndividualEncounterParticipantRevision IndividualEncounterParticipantId IndividualEncounterParticipant
    | PrenatalEncounterRevision PrenatalEncounterId PrenatalEncounter
    | PrenatalPhotoRevision PrenatalPhotoId PrenatalPhoto
    | RelationshipRevision RelationshipId Relationship
    | ResourceRevision ResourceId Resource
    | SessionRevision SessionId Session
    | SocialHistoryRevision SocialHistoryId SocialHistory
    | VillageRevision VillageId Village
    | VitalsRevision VitalsId Vitals
    | WeightRevision WeightId Weight
