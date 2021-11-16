module Backend.Model exposing (ComputedDashboard, ModelIndexedDb, MsgIndexedDb(..), Revision(..), emptyModelIndexedDb)

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
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounter)
import Backend.Clinic.Model exposing (Clinic, ClinicType)
import Backend.Counseling.Model exposing (CounselingSchedule, CounselingTopic, EveryCounselingSchedule)
import Backend.Dashboard.Model exposing (DashboardStatsRaw)
import Backend.Entities exposing (..)
import Backend.HealthCenter.Model exposing (CatchmentArea, HealthCenter)
import Backend.HomeVisitEncounter.Model exposing (HomeVisitEncounter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..), IndividualParticipantExtraData)
import Backend.Measurement.Model exposing (..)
import Backend.Nurse.Model exposing (Nurse)
import Backend.NutritionEncounter.Model exposing (NutritionEncounter)
import Backend.ParticipantConsent.Model exposing (ParticipantForm)
import Backend.Person.Model exposing (Initiator, Person)
import Backend.PmtctParticipant.Model exposing (PmtctParticipant)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterPostCreateDestination)
import Backend.Relationship.Model exposing (MyRelationship, Relationship)
import Backend.Session.Model exposing (EditableSession, ExpectedParticipants, OfflineSession, Session)
import Backend.Village.Model exposing (Village)
import Backend.WellChildEncounter.Model exposing (WellChildEncounter)
import Pages.Dashboard.Model
import RemoteData exposing (RemoteData(..), WebData)
import Time


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
    , acuteIllnessEncounterRequests : Dict AcuteIllnessEncounterId Backend.AcuteIllnessEncounter.Model.Model
    , individualSessionRequests : Dict IndividualEncounterParticipantId Backend.IndividualEncounterParticipant.Model.Model
    , homeVisitEncounterRequests : Dict HomeVisitEncounterId Backend.HomeVisitEncounter.Model.Model
    , wellChildEncounterRequests : Dict WellChildEncounterId Backend.WellChildEncounter.Model.Model

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
    , acuteIllnessEncounters : Dict AcuteIllnessEncounterId (WebData AcuteIllnessEncounter)
    , homeVisitEncounters : Dict HomeVisitEncounterId (WebData HomeVisitEncounter)
    , wellChildEncounters : Dict WellChildEncounterId (WebData WellChildEncounter)
    , individualParticipants : Dict IndividualEncounterParticipantId (WebData IndividualEncounterParticipant)
    , traceContacts : Dict AcuteIllnessTraceContactId (WebData AcuteIllnessTraceContact)

    -- Cache things organized in certain ways.
    , individualParticipantsByPerson : Dict PersonId (WebData (Dict IndividualEncounterParticipantId IndividualEncounterParticipant))
    , prenatalEncountersByParticipant : Dict IndividualEncounterParticipantId (WebData (Dict PrenatalEncounterId PrenatalEncounter))
    , nutritionEncountersByParticipant : Dict IndividualEncounterParticipantId (WebData (Dict NutritionEncounterId NutritionEncounter))
    , acuteIllnessEncountersByParticipant : Dict IndividualEncounterParticipantId (WebData (Dict AcuteIllnessEncounterId AcuteIllnessEncounter))
    , homeVisitEncountersByParticipant : Dict IndividualEncounterParticipantId (WebData (Dict HomeVisitEncounterId HomeVisitEncounter))
    , wellChildEncountersByParticipant : Dict IndividualEncounterParticipantId (WebData (Dict WellChildEncounterId WellChildEncounter))
    , prenatalMeasurements : Dict PrenatalEncounterId (WebData PrenatalMeasurements)
    , nutritionMeasurements : Dict NutritionEncounterId (WebData NutritionMeasurements)
    , acuteIllnessMeasurements : Dict AcuteIllnessEncounterId (WebData AcuteIllnessMeasurements)
    , followUpMeasurements : Dict HealthCenterId (WebData FollowUpMeasurements)
    , homeVisitMeasurements : Dict HomeVisitEncounterId (WebData HomeVisitMeasurements)
    , wellChildMeasurements : Dict WellChildEncounterId (WebData WellChildMeasurements)

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
    , postAcuteIllnessEncounter : Dict IndividualEncounterParticipantId (WebData ( AcuteIllnessEncounterId, AcuteIllnessEncounter ))
    , postHomeVisitEncounter : Dict IndividualEncounterParticipantId (WebData ( HomeVisitEncounterId, HomeVisitEncounter ))
    , postWellChildEncounter : Dict IndividualEncounterParticipantId (WebData ( WellChildEncounterId, WellChildEncounter ))

    -- Dashboard Statistics.
    , computedDashboards : Dict HealthCenterId ComputedDashboard
    , computedDashboardLastFetched : Time.Posix
    }


emptyModelIndexedDb : ModelIndexedDb
emptyModelIndexedDb =
    { clinics = NotAsked
    , editableSessions = Dict.empty
    , everyCounselingSchedule = NotAsked
    , expectedParticipants = Dict.empty
    , expectedSessions = Dict.empty
    , healthCenters = NotAsked
    , villages = NotAsked
    , motherMeasurements = Dict.empty
    , childMeasurements = Dict.empty
    , prenatalEncounters = Dict.empty
    , prenatalEncountersByParticipant = Dict.empty
    , prenatalMeasurements = Dict.empty
    , nutritionEncounters = Dict.empty
    , nutritionEncountersByParticipant = Dict.empty
    , nutritionMeasurements = Dict.empty
    , acuteIllnessEncounters = Dict.empty
    , acuteIllnessEncountersByParticipant = Dict.empty
    , acuteIllnessMeasurements = Dict.empty
    , homeVisitEncounters = Dict.empty
    , homeVisitEncountersByParticipant = Dict.empty
    , homeVisitMeasurements = Dict.empty
    , wellChildEncounters = Dict.empty
    , wellChildEncountersByParticipant = Dict.empty
    , wellChildMeasurements = Dict.empty
    , participantForms = NotAsked
    , participantsByPerson = Dict.empty
    , people = Dict.empty
    , traceContacts = Dict.empty
    , personSearches = Dict.empty
    , postPerson = NotAsked
    , postPmtctParticipant = Dict.empty
    , postIndividualSession = Dict.empty
    , postPrenatalEncounter = Dict.empty
    , postNutritionEncounter = Dict.empty
    , postHomeVisitEncounter = Dict.empty
    , postWellChildEncounter = Dict.empty
    , postAcuteIllnessEncounter = Dict.empty
    , postRelationship = Dict.empty
    , postSession = NotAsked
    , prenatalEncounterRequests = Dict.empty
    , nutritionEncounterRequests = Dict.empty
    , acuteIllnessEncounterRequests = Dict.empty
    , homeVisitEncounterRequests = Dict.empty
    , wellChildEncounterRequests = Dict.empty
    , individualSessionRequests = Dict.empty
    , individualParticipants = Dict.empty
    , individualParticipantsByPerson = Dict.empty
    , relationshipsByPerson = Dict.empty
    , sessionRequests = Dict.empty
    , sessions = Dict.empty
    , sessionsByClinic = Dict.empty
    , followUpMeasurements = Dict.empty
    , computedDashboards = Dict.empty
    , computedDashboardLastFetched = Time.millisToPosix 0
    }


{-| This is the data type we store for single health center statistics.
It consist of raw statistics that we get from the backend, and a dictionary
of assembeled data permutations we may need.
We need this to avoid making heavy calculations (repeated every few seconds),
that are need to present the dashboards page.
We have different options for displaying data on page:

  - Overall Statistics
  - Statistics for single group type (FBF, PMTCT, ...)
  - Statistics for selected village.

To support this, each permutation of assembeled data is combination of
selected program type and village (optional). This combinations serves
as key of assembled data permutations dictionary).

-}
type alias ComputedDashboard =
    { statsRaw : Backend.Dashboard.Model.DashboardStatsRaw
    , assembledPermutations : Dict ( Pages.Dashboard.Model.FilterProgramType, Maybe VillageId ) Backend.Dashboard.Model.AssembledData
    }


type MsgIndexedDb
    = -- Messages which fetch various kinds of data.
      FetchAcuteIllnessEncounter AcuteIllnessEncounterId
    | FetchAcuteIllnessEncountersForParticipant IndividualEncounterParticipantId
    | FetchAcuteIllnessMeasurements AcuteIllnessEncounterId
    | FetchChildMeasurements PersonId
    | FetchChildrenMeasurements (List PersonId)
    | FetchClinics
    | FetchComputedDashboard HealthCenterId
      -- Request to generate assembled daya needed to display Dashboards
      -- page for selected program type and village (optional).
    | FetchComputedDashboardAssembledPermutation HealthCenterId Pages.Dashboard.Model.FilterProgramType (Maybe VillageId)
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
    | FetchFollowUpMeasurements HealthCenterId
    | FetchFollowUpParticipants (List PersonId)
    | FetchHealthCenters
    | FetchIndividualEncounterParticipantsForPerson PersonId
    | FetchMotherMeasurements PersonId
    | FetchMothersMeasurements (List PersonId)
    | FetchNutritionEncounter NutritionEncounterId
    | FetchNutritionEncountersForParticipant IndividualEncounterParticipantId
    | FetchNutritionMeasurements NutritionEncounterId
    | FetchHomeVisitEncounter HomeVisitEncounterId
    | FetchHomeVisitEncountersForParticipant IndividualEncounterParticipantId
    | FetchHomeVisitMeasurements HomeVisitEncounterId
    | FetchWellChildEncounter WellChildEncounterId
    | FetchWellChildEncountersForParticipant IndividualEncounterParticipantId
    | FetchWellChildMeasurements WellChildEncounterId
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
    | FetchVillages
    | FetchTraceContact AcuteIllnessTraceContactId
      -- Messages which handle responses to data
    | HandleFetchedAcuteIllnessEncounter AcuteIllnessEncounterId (WebData AcuteIllnessEncounter)
    | HandleFetchedAcuteIllnessEncountersForParticipant IndividualEncounterParticipantId (WebData (Dict AcuteIllnessEncounterId AcuteIllnessEncounter))
    | HandleFetchedAcuteIllnessMeasurements AcuteIllnessEncounterId (WebData AcuteIllnessMeasurements)
    | HandleFetchedChildMeasurements PersonId (WebData ChildMeasurementList)
    | HandleFetchedComputedDashboard HealthCenterId (WebData (Dict HealthCenterId DashboardStatsRaw))
    | HandleFetchedChildrenMeasurements (WebData (Dict PersonId ChildMeasurementList))
    | HandleFetchedClinics (WebData (Dict ClinicId Clinic))
    | HandleFetchedEveryCounselingSchedule (WebData EveryCounselingSchedule)
    | HandleFetchedExpectedParticipants SessionId (WebData ExpectedParticipants)
    | HandleFetchedExpectedSessions PersonId (WebData (Dict SessionId Session))
    | HandleFetchedFollowUpMeasurements HealthCenterId (WebData FollowUpMeasurements)
    | HandleFetchFollowUpParticipants (WebData (Dict PersonId Person))
    | HandleFetchedHealthCenters (WebData (Dict HealthCenterId HealthCenter))
    | HandleFetchedIndividualEncounterParticipantsForPerson PersonId (WebData (Dict IndividualEncounterParticipantId IndividualEncounterParticipant))
    | HandleFetchedMotherMeasurements PersonId (WebData MotherMeasurementList)
    | HandleFetchedMothersMeasurements (WebData (Dict PersonId MotherMeasurementList))
    | HandleFetchedNutritionEncounter NutritionEncounterId (WebData NutritionEncounter)
    | HandleFetchedNutritionEncountersForParticipant IndividualEncounterParticipantId (WebData (Dict NutritionEncounterId NutritionEncounter))
    | HandleFetchedNutritionMeasurements NutritionEncounterId (WebData NutritionMeasurements)
    | HandleFetchedHomeVisitEncounter HomeVisitEncounterId (WebData HomeVisitEncounter)
    | HandleFetchedHomeVisitEncountersForParticipant IndividualEncounterParticipantId (WebData (Dict HomeVisitEncounterId HomeVisitEncounter))
    | HandleFetchedHomeVisitMeasurements HomeVisitEncounterId (WebData HomeVisitMeasurements)
    | HandleFetchedWellChildEncounter WellChildEncounterId (WebData WellChildEncounter)
    | HandleFetchedWellChildEncountersForParticipant IndividualEncounterParticipantId (WebData (Dict WellChildEncounterId WellChildEncounter))
    | HandleFetchedWellChildMeasurements WellChildEncounterId (WebData WellChildMeasurements)
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
    | HandleFetchedVillages (WebData (Dict VillageId Village))
    | HandleFetchedTraceContact AcuteIllnessTraceContactId (WebData AcuteIllnessTraceContact)
      -- Messages which mutate data
    | PostPerson (Maybe PersonId) Initiator Person -- The first person is a person we ought to offer setting a relationship to.
    | PatchPerson PersonId Person
    | PostRelationship PersonId MyRelationship (Maybe ClinicId) Initiator
    | PostPmtctParticipant Initiator PmtctParticipant
    | PostSession Session
    | PostIndividualSession IndividualParticipantExtraData IndividualEncounterParticipant
    | PostPrenatalEncounter PrenatalEncounterPostCreateDestination PrenatalEncounter
    | PostNutritionEncounter NutritionEncounter
    | PostAcuteIllnessEncounter AcuteIllnessEncounter
    | PostHomeVisitEncounter HomeVisitEncounter
    | PostWellChildEncounter WellChildEncounter
      -- Messages which handle responses to mutating data
    | HandlePostedPerson (Maybe PersonId) Initiator (WebData PersonId)
    | HandlePatchedPerson PersonId (WebData Person)
    | HandlePostedRelationship PersonId Initiator (WebData MyRelationship)
    | HandlePostedPmtctParticipant PersonId Initiator (WebData ( PmtctParticipantId, PmtctParticipant ))
    | HandlePostedSession ClinicType (WebData SessionId)
    | HandlePostedIndividualSession PersonId IndividualEncounterType IndividualParticipantExtraData (WebData ( IndividualEncounterParticipantId, IndividualEncounterParticipant ))
    | HandlePostedPrenatalEncounter IndividualEncounterParticipantId PrenatalEncounterPostCreateDestination (WebData ( PrenatalEncounterId, PrenatalEncounter ))
    | HandlePostedNutritionEncounter IndividualEncounterParticipantId (WebData ( NutritionEncounterId, NutritionEncounter ))
    | HandlePostedAcuteIllnessEncounter IndividualEncounterParticipantId (WebData ( AcuteIllnessEncounterId, AcuteIllnessEncounter ))
    | HandlePostedHomeVisitEncounter IndividualEncounterParticipantId (WebData ( HomeVisitEncounterId, HomeVisitEncounter ))
    | HandlePostedWellChildEncounter IndividualEncounterParticipantId (WebData ( WellChildEncounterId, WellChildEncounter ))
      -- Operations we may want to perform when logout is clicked.
    | HandleLogout
      -- Process some revisions we've received from the backend. In some cases,
      -- we can update our in-memory structures appropriately. In other cases, we
      -- can set them to `NotAsked` and let the "fetch" mechanism re-fetch them.
    | HandleRevisions (List Revision)
      -- Handling edits to session data or encounter data
    | MsgSession SessionId Backend.Session.Model.Msg
    | MsgPrenatalEncounter PrenatalEncounterId Backend.PrenatalEncounter.Model.Msg
    | MsgNutritionEncounter NutritionEncounterId Backend.NutritionEncounter.Model.Msg
    | MsgAcuteIllnessEncounter AcuteIllnessEncounterId Backend.AcuteIllnessEncounter.Model.Msg
    | MsgHomeVisitEncounter HomeVisitEncounterId Backend.HomeVisitEncounter.Model.Msg
    | MsgWellChildEncounter WellChildEncounterId Backend.WellChildEncounter.Model.Msg
    | MsgIndividualSession IndividualEncounterParticipantId Backend.IndividualEncounterParticipant.Model.Msg
    | ResetFailedToFetchAuthorities


{-| Wrapper for all the revisions we can receive.
-}
type Revision
    = AcuteFindingsRevision AcuteFindingsId AcuteFindings
    | AcuteIllnessContactsTracingRevision AcuteIllnessContactsTracingId AcuteIllnessContactsTracing
    | AcuteIllnessCoreExamRevision AcuteIllnessCoreExamId AcuteIllnessCoreExam
    | AcuteIllnessDangerSignsRevision AcuteIllnessDangerSignsId AcuteIllnessDangerSigns
    | AcuteIllnessFollowUpRevision AcuteIllnessFollowUpId AcuteIllnessFollowUp
    | AcuteIllnessMuacRevision AcuteIllnessMuacId AcuteIllnessMuac
    | AcuteIllnessNutritionRevision AcuteIllnessNutritionId AcuteIllnessNutrition
    | AcuteIllnessEncounterRevision AcuteIllnessEncounterId AcuteIllnessEncounter
    | AcuteIllnessTraceContactRevision AcuteIllnessTraceContactId AcuteIllnessTraceContact
    | AcuteIllnessVitalsRevision AcuteIllnessVitalsId AcuteIllnessVitals
    | AppointmentConfirmationRevision PrenatalAppointmentConfirmationId PrenatalAppointmentConfirmation
    | AttendanceRevision AttendanceId Attendance
    | BreastExamRevision BreastExamId BreastExam
    | BirthPlanRevision BirthPlanId BirthPlan
    | Call114Revision Call114Id Call114
    | CatchmentAreaRevision CatchmentAreaId CatchmentArea
    | ChildFbfRevision ChildFbfId Fbf
    | ChildNutritionRevision ChildNutritionId ChildNutrition
    | ClinicRevision ClinicId Clinic
    | ContributingFactorsRevision ContributingFactorsId ContributingFactors
    | CorePhysicalExamRevision CorePhysicalExamId CorePhysicalExam
    | CovidTestingRevision CovidTestingId CovidTesting
    | CounselingScheduleRevision CounselingScheduleId CounselingSchedule
    | CounselingSessionRevision CounselingSessionId CounselingSession
    | CounselingTopicRevision CounselingTopicId CounselingTopic
    | DangerSignsRevision DangerSignsId DangerSigns
    | DashboardStatsRevision HealthCenterId DashboardStatsRaw
    | ExposureRevision ExposureId Exposure
    | FamilyPlanningRevision FamilyPlanningId FamilyPlanning
    | FollowUpRevision FollowUpId FollowUp
    | GroupHealthEducationRevision GroupHealthEducationId GroupHealthEducation
    | GroupSendToHCRevision GroupSendToHCId GroupSendToHC
    | HCContactRevision HCContactId HCContact
    | HealthCenterRevision HealthCenterId HealthCenter
    | HealthEducationRevision HealthEducationId HealthEducation
    | HeightRevision HeightId Height
    | HomeVisitEncounterRevision HomeVisitEncounterId HomeVisitEncounter
    | IndividualEncounterParticipantRevision IndividualEncounterParticipantId IndividualEncounterParticipant
    | IsolationRevision IsolationId Isolation
    | LactationRevision LactationId Lactation
    | LastMenstrualPeriodRevision LastMenstrualPeriodId LastMenstrualPeriod
    | MalariaTestingRevision MalariaTestingId MalariaTesting
    | MedicalHistoryRevision MedicalHistoryId MedicalHistory
    | MedicationRevision MedicationId Medication
    | MedicationDistributionRevision MedicationDistributionId MedicationDistribution
    | MotherFbfRevision MotherFbfId Fbf
    | MuacRevision MuacId Muac
    | NurseRevision NurseId Nurse
    | NutritionCaringRevision NutritionCaringId NutritionCaring
    | NutritionContributingFactorsRevision NutritionContributingFactorsId NutritionContributingFactors
    | NutritionEncounterRevision NutritionEncounterId NutritionEncounter
    | NutritionFeedingRevision NutritionFeedingId NutritionFeeding
    | NutritionFollowUpRevision NutritionFollowUpId NutritionFollowUp
    | NutritionFoodSecurityRevision NutritionFoodSecurityId NutritionFoodSecurity
    | NutritionHealthEducationRevision NutritionHealthEducationId NutritionHealthEducation
    | NutritionHeightRevision NutritionHeightId NutritionHeight
    | NutritionHygieneRevision NutritionHygieneId NutritionHygiene
    | NutritionMuacRevision NutritionMuacId NutritionMuac
    | NutritionNutritionRevision NutritionNutritionId NutritionNutrition
    | NutritionPhotoRevision NutritionPhotoId NutritionPhoto
    | NutritionSendToHCRevision NutritionSendToHCId NutritionSendToHC
    | NutritionWeightRevision NutritionWeightId NutritionWeight
    | ObstetricalExamRevision ObstetricalExamId ObstetricalExam
    | ObstetricHistoryRevision ObstetricHistoryId ObstetricHistory
    | ObstetricHistoryStep2Revision ObstetricHistoryStep2Id ObstetricHistoryStep2
    | ParticipantConsentRevision ParticipantConsentId ParticipantConsent
    | ParticipantFormRevision ParticipantFormId ParticipantForm
    | PersonRevision PersonId Person
    | PhotoRevision PhotoId Photo
    | PmtctParticipantRevision PmtctParticipantId PmtctParticipant
    | PregnancyTestingRevision PregnancyTestId PregnancyTest
    | PrenatalFamilyPlanningRevision PrenatalFamilyPlanningId PrenatalFamilyPlanning
    | PrenatalHealthEducationRevision PrenatalHealthEducationId PrenatalHealthEducation
    | PrenatalFollowUpRevision PrenatalFollowUpId PrenatalFollowUp
    | PrenatalSendToHCRevision PrenatalSendToHcId PrenatalSendToHC
    | PrenatalNutritionRevision PrenatalNutritionId PrenatalNutrition
    | PrenatalEncounterRevision PrenatalEncounterId PrenatalEncounter
    | PrenatalPhotoRevision PrenatalPhotoId PrenatalPhoto
    | RelationshipRevision RelationshipId Relationship
    | ResourceRevision ResourceId Resource
    | SendToHCRevision SendToHCId SendToHC
    | SessionRevision SessionId Session
    | SocialHistoryRevision SocialHistoryId SocialHistory
    | SymptomsGeneralRevision SymptomsGeneralId SymptomsGeneral
    | SymptomsGIRevision SymptomsGIId SymptomsGI
    | SymptomsRespiratoryRevision SymptomsRespiratoryId SymptomsRespiratory
    | TravelHistoryRevision TravelHistoryId TravelHistory
    | TreatmentOngoingRevision TreatmentOngoingId TreatmentOngoing
    | TreatmentReviewRevision TreatmentReviewId TreatmentReview
    | VillageRevision VillageId Village
    | VitalsRevision VitalsId Vitals
    | WeightRevision WeightId Weight
    | WellChildAlbendazoleRevision WellChildAlbendazoleId WellChildAlbendazole
    | WellChildBCGImmunisationRevision WellChildBCGImmunisationId WellChildBCGImmunisation
    | WellChildContributingFactorsRevision WellChildContributingFactorsId WellChildContributingFactors
    | WellChildDTPImmunisationRevision WellChildDTPImmunisationId WellChildDTPImmunisation
    | WellChildECDRevision WellChildECDId WellChildECD
    | WellChildEncounterRevision WellChildEncounterId WellChildEncounter
    | WellChildFollowUpRevision WellChildFollowUpId WellChildFollowUp
    | WellChildHeadCircumferenceRevision WellChildHeadCircumferenceId WellChildHeadCircumference
    | WellChildHealthEducationRevision WellChildHealthEducationId WellChildHealthEducation
    | WellChildHeightRevision WellChildHeightId WellChildHeight
    | WellChildHPVImmunisationRevision WellChildHPVImmunisationId WellChildHPVImmunisation
    | WellChildIPVImmunisationRevision WellChildIPVImmunisationId WellChildIPVImmunisation
    | WellChildMebendezoleRevision WellChildMebendezoleId WellChildMebendezole
    | WellChildMRImmunisationRevision WellChildMRImmunisationId WellChildMRImmunisation
    | WellChildMuacRevision WellChildMuacId WellChildMuac
    | WellChildNextVisitRevision WellChildNextVisitId WellChildNextVisit
    | WellChildNutritionRevision WellChildNutritionId WellChildNutrition
    | WellChildOPVImmunisationRevision WellChildOPVImmunisationId WellChildOPVImmunisation
    | WellChildPCV13ImmunisationRevision WellChildPCV13ImmunisationId WellChildPCV13Immunisation
    | WellChildPhotoRevision WellChildPhotoId WellChildPhoto
    | WellChildPregnancySummaryRevision WellChildPregnancySummaryId WellChildPregnancySummary
    | WellChildRotarixImmunisationRevision WellChildRotarixImmunisationId WellChildRotarixImmunisation
    | WellChildSendToHCRevision WellChildSendToHCId WellChildSendToHC
    | WellChildSymptomsReviewRevision WellChildSymptomsReviewId WellChildSymptomsReview
    | WellChildVitalsRevision WellChildVitalsId WellChildVitals
    | WellChildVitaminARevision WellChildVitaminAId WellChildVitaminA
    | WellChildWeightRevision WellChildWeightId WellChildWeight
