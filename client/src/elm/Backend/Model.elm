module Backend.Model exposing
    ( ComputedDashboard
    , ModelIndexedDb
    , MsgIndexedDb(..)
    , Revision(..)
    , emptyModelIndexedDb
    )

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
import Backend.ChildScoreboardEncounter.Model exposing (ChildScoreboardEncounter)
import Backend.Clinic.Model exposing (Clinic)
import Backend.Counseling.Model exposing (CounselingSchedule, CounselingTopic, EveryCounselingSchedule)
import Backend.Dashboard.Model exposing (DashboardStatsRaw)
import Backend.EducationSession.Model exposing (EducationSession)
import Backend.Entities exposing (..)
import Backend.HIVEncounter.Model exposing (HIVEncounter)
import Backend.HealthCenter.Model exposing (CatchmentArea, HealthCenter)
import Backend.HomeVisitEncounter.Model exposing (HomeVisitEncounter)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType, IndividualParticipantExtraData)
import Backend.Measurement.Model exposing (..)
import Backend.NCDEncounter.Model exposing (NCDEncounter)
import Backend.Nurse.Model exposing (Nurse)
import Backend.NutritionEncounter.Model exposing (NutritionEncounter)
import Backend.ParticipantConsent.Model exposing (ParticipantForm)
import Backend.Person.Model exposing (Initiator, PatchPersonInitator, Person)
import Backend.PmtctParticipant.Model exposing (PmtctParticipant)
import Backend.PrenatalEncounter.Model exposing (PrenatalEncounter, PrenatalEncounterPostCreateDestination)
import Backend.Relationship.Model exposing (MyRelationship, Relationship)
import Backend.ResilienceSurvey.Model exposing (ResilienceSurvey)
import Backend.Session.Model exposing (EditableSession, ExpectedParticipants, Session)
import Backend.StockUpdate.Model exposing (StockManagementData)
import Backend.TraceContact.Model
import Backend.TuberculosisEncounter.Model exposing (TuberculosisEncounter)
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
    , individualEncounterParticipantRequests : Dict IndividualEncounterParticipantId Backend.IndividualEncounterParticipant.Model.Model
    , homeVisitEncounterRequests : Dict HomeVisitEncounterId Backend.HomeVisitEncounter.Model.Model
    , wellChildEncounterRequests : Dict WellChildEncounterId Backend.WellChildEncounter.Model.Model
    , ncdEncounterRequests : Dict NCDEncounterId Backend.NCDEncounter.Model.Model
    , childScoreboardEncounterRequests : Dict ChildScoreboardEncounterId Backend.ChildScoreboardEncounter.Model.Model
    , tuberculosisEncounterRequests : Dict TuberculosisEncounterId Backend.TuberculosisEncounter.Model.Model
    , hivEncounterRequests : Dict HIVEncounterId Backend.HIVEncounter.Model.Model
    , traceContactRequests : Dict AcuteIllnessTraceContactId Backend.TraceContact.Model.Model
    , nurseRequests : Dict NurseId Backend.Nurse.Model.Model
    , resilienceSurveyRequests : Dict NurseId Backend.ResilienceSurvey.Model.Model
    , stockUpdateRequests : Dict NurseId Backend.StockUpdate.Model.Model
    , educationSessionRequests : Dict EducationSessionId Backend.EducationSession.Model.Model

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
    , personSearchesByName : Dict String (WebData (Dict PersonId Person))

    -- Tracks searches for participants by name. The key is the phrase we are
    -- searching for.
    , personSearchesByNationalId : Dict String (WebData (Dict PersonId Person))
    , peopleInVillage : Dict VillageId (WebData (Dict PersonId Person))

    -- A simple cache of several things.
    , people : Dict PersonId (WebData Person)
    , prenatalEncounters : Dict PrenatalEncounterId (WebData PrenatalEncounter)
    , nutritionEncounters : Dict NutritionEncounterId (WebData NutritionEncounter)
    , acuteIllnessEncounters : Dict AcuteIllnessEncounterId (WebData AcuteIllnessEncounter)
    , homeVisitEncounters : Dict HomeVisitEncounterId (WebData HomeVisitEncounter)
    , wellChildEncounters : Dict WellChildEncounterId (WebData WellChildEncounter)
    , ncdEncounters : Dict NCDEncounterId (WebData NCDEncounter)
    , childScoreboardEncounters : Dict ChildScoreboardEncounterId (WebData ChildScoreboardEncounter)
    , tuberculosisEncounters : Dict TuberculosisEncounterId (WebData TuberculosisEncounter)
    , hivEncounters : Dict HIVEncounterId (WebData HIVEncounter)
    , individualParticipants : Dict IndividualEncounterParticipantId (WebData IndividualEncounterParticipant)
    , traceContacts : Dict AcuteIllnessTraceContactId (WebData AcuteIllnessTraceContact)
    , educationSessions : Dict EducationSessionId (WebData EducationSession)

    -- Cache things organized in certain ways.
    , individualParticipantsByPerson : Dict PersonId (WebData (Dict IndividualEncounterParticipantId IndividualEncounterParticipant))
    , prenatalEncountersByParticipant : Dict IndividualEncounterParticipantId (WebData (Dict PrenatalEncounterId PrenatalEncounter))
    , nutritionEncountersByParticipant : Dict IndividualEncounterParticipantId (WebData (Dict NutritionEncounterId NutritionEncounter))
    , acuteIllnessEncountersByParticipant : Dict IndividualEncounterParticipantId (WebData (Dict AcuteIllnessEncounterId AcuteIllnessEncounter))
    , homeVisitEncountersByParticipant : Dict IndividualEncounterParticipantId (WebData (Dict HomeVisitEncounterId HomeVisitEncounter))
    , wellChildEncountersByParticipant : Dict IndividualEncounterParticipantId (WebData (Dict WellChildEncounterId WellChildEncounter))
    , ncdEncountersByParticipant : Dict IndividualEncounterParticipantId (WebData (Dict NCDEncounterId NCDEncounter))
    , childScoreboardEncountersByParticipant : Dict IndividualEncounterParticipantId (WebData (Dict ChildScoreboardEncounterId ChildScoreboardEncounter))
    , tuberculosisEncountersByParticipant : Dict IndividualEncounterParticipantId (WebData (Dict TuberculosisEncounterId TuberculosisEncounter))
    , hivEncountersByParticipant : Dict IndividualEncounterParticipantId (WebData (Dict HIVEncounterId HIVEncounter))
    , prenatalMeasurements : Dict PrenatalEncounterId (WebData PrenatalMeasurements)
    , nutritionMeasurements : Dict NutritionEncounterId (WebData NutritionMeasurements)
    , acuteIllnessMeasurements : Dict AcuteIllnessEncounterId (WebData AcuteIllnessMeasurements)
    , followUpMeasurements : Dict HealthCenterId (WebData FollowUpMeasurements)
    , homeVisitMeasurements : Dict HomeVisitEncounterId (WebData HomeVisitMeasurements)
    , wellChildMeasurements : Dict WellChildEncounterId (WebData WellChildMeasurements)
    , ncdMeasurements : Dict NCDEncounterId (WebData NCDMeasurements)
    , childScoreboardMeasurements : Dict ChildScoreboardEncounterId (WebData ChildScoreboardMeasurements)
    , tuberculosisMeasurements : Dict TuberculosisEncounterId (WebData TuberculosisMeasurements)
    , hivMeasurements : Dict HIVEncounterId (WebData HIVMeasurements)
    , stockManagementMeasurements : Dict HealthCenterId (WebData StockManagementMeasurements)
    , stockManagementData : Dict HealthCenterId (WebData StockManagementData)
    , pregnancyByNewborn : Dict PersonId (WebData (Maybe ( IndividualEncounterParticipantId, IndividualEncounterParticipant )))

    -- From the point of view of the specified person, all of their relationships.
    , relationshipsByPerson : Dict PersonId (WebData (Dict RelationshipId MyRelationship))

    -- Track what PMTCT groups a participant is in. (Inside a session, we can use
    -- `expectedParticipants`, but for registration etc. this is useful.
    , participantsByPerson : Dict PersonId (WebData (Dict PmtctParticipantId PmtctParticipant))

    -- Dashboard Statistics.
    , computedDashboards : Dict HealthCenterId ComputedDashboard
    , computedDashboardLastFetched : Time.Posix

    -- Resilience.
    , resilienceSurveysByNurse : Dict NurseId (WebData (Dict ResilienceSurveyId ResilienceSurvey))

    -- Stock Management.
    , stockUpdates : WebData (Dict StockUpdateId StockUpdate)

    -- Group Education.
    , educationSessionsByPerson : Dict PersonId (WebData (Dict EducationSessionId EducationSession))

    -- Track requests to mutate data.
    , postPerson : WebData PersonId
    , postRelationship : Dict PersonId (WebData MyRelationship)
    , postPmtctParticipant : Dict PersonId (WebData ( PmtctParticipantId, PmtctParticipant ))
    , postSession : WebData SessionId
    , postIndividualEncounterParticipant : Dict PersonId (WebData ( IndividualEncounterParticipantId, IndividualEncounterParticipant ))
    , postPrenatalEncounter : Dict IndividualEncounterParticipantId (WebData ( PrenatalEncounterId, PrenatalEncounter ))
    , postNutritionEncounter : Dict IndividualEncounterParticipantId (WebData ( NutritionEncounterId, NutritionEncounter ))
    , postAcuteIllnessEncounter : Dict IndividualEncounterParticipantId (WebData ( AcuteIllnessEncounterId, AcuteIllnessEncounter ))
    , postHomeVisitEncounter : Dict IndividualEncounterParticipantId (WebData ( HomeVisitEncounterId, HomeVisitEncounter ))
    , postWellChildEncounter : Dict IndividualEncounterParticipantId (WebData ( WellChildEncounterId, WellChildEncounter ))
    , postNCDEncounter : Dict IndividualEncounterParticipantId (WebData ( NCDEncounterId, NCDEncounter ))
    , postChildScoreboardEncounter : Dict IndividualEncounterParticipantId (WebData ( ChildScoreboardEncounterId, ChildScoreboardEncounter ))
    , postTuberculosisEncounter : Dict IndividualEncounterParticipantId (WebData ( TuberculosisEncounterId, TuberculosisEncounter ))
    , postHIVEncounter : Dict IndividualEncounterParticipantId (WebData ( HIVEncounterId, HIVEncounter ))
    , postEducationSession : WebData ( EducationSessionId, EducationSession )
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
    , ncdEncounters = Dict.empty
    , ncdEncountersByParticipant = Dict.empty
    , ncdMeasurements = Dict.empty
    , childScoreboardMeasurements = Dict.empty
    , tuberculosisMeasurements = Dict.empty
    , hivMeasurements = Dict.empty
    , stockManagementMeasurements = Dict.empty
    , stockManagementData = Dict.empty
    , pregnancyByNewborn = Dict.empty
    , childScoreboardEncounters = Dict.empty
    , childScoreboardEncountersByParticipant = Dict.empty
    , tuberculosisEncounters = Dict.empty
    , tuberculosisEncountersByParticipant = Dict.empty
    , hivEncounters = Dict.empty
    , hivEncountersByParticipant = Dict.empty
    , educationSessions = Dict.empty
    , participantForms = NotAsked
    , participantsByPerson = Dict.empty
    , people = Dict.empty
    , traceContacts = Dict.empty
    , personSearchesByName = Dict.empty
    , personSearchesByNationalId = Dict.empty
    , peopleInVillage = Dict.empty
    , prenatalEncounterRequests = Dict.empty
    , nutritionEncounterRequests = Dict.empty
    , acuteIllnessEncounterRequests = Dict.empty
    , homeVisitEncounterRequests = Dict.empty
    , wellChildEncounterRequests = Dict.empty
    , ncdEncounterRequests = Dict.empty
    , childScoreboardEncounterRequests = Dict.empty
    , tuberculosisEncounterRequests = Dict.empty
    , hivEncounterRequests = Dict.empty
    , educationSessionRequests = Dict.empty
    , traceContactRequests = Dict.empty
    , individualEncounterParticipantRequests = Dict.empty
    , nurseRequests = Dict.empty
    , resilienceSurveyRequests = Dict.empty
    , stockUpdateRequests = Dict.empty
    , individualParticipants = Dict.empty
    , individualParticipantsByPerson = Dict.empty
    , relationshipsByPerson = Dict.empty
    , sessionRequests = Dict.empty
    , sessions = Dict.empty
    , sessionsByClinic = Dict.empty
    , followUpMeasurements = Dict.empty
    , computedDashboards = Dict.empty
    , computedDashboardLastFetched = Time.millisToPosix 0
    , resilienceSurveysByNurse = Dict.empty
    , stockUpdates = NotAsked
    , educationSessionsByPerson = Dict.empty
    , postPerson = NotAsked
    , postRelationship = Dict.empty
    , postPmtctParticipant = Dict.empty
    , postSession = NotAsked
    , postIndividualEncounterParticipant = Dict.empty
    , postPrenatalEncounter = Dict.empty
    , postNutritionEncounter = Dict.empty
    , postHomeVisitEncounter = Dict.empty
    , postWellChildEncounter = Dict.empty
    , postAcuteIllnessEncounter = Dict.empty
    , postNCDEncounter = Dict.empty
    , postChildScoreboardEncounter = Dict.empty
    , postTuberculosisEncounter = Dict.empty
    , postHIVEncounter = Dict.empty
    , postEducationSession = NotAsked
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
    | FetchAcuteIllnessEncounters (List AcuteIllnessEncounterId)
    | FetchAcuteIllnessEncountersForParticipant IndividualEncounterParticipantId
    | FetchAcuteIllnessEncountersForParticipants (List IndividualEncounterParticipantId)
    | FetchAcuteIllnessMeasurements AcuteIllnessEncounterId
    | FetchChildMeasurements PersonId
    | FetchChildrenMeasurements (List PersonId)
    | FetchChildScoreboardEncounter ChildScoreboardEncounterId
    | FetchChildScoreboardEncountersForParticipant IndividualEncounterParticipantId
    | FetchChildScoreboardMeasurements ChildScoreboardEncounterId
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
    | FetchEducationSession EducationSessionId
    | FetchEducationSessionsForPerson PersonId
    | FetchEveryCounselingSchedule
    | FetchExpectedParticipants SessionId
    | FetchExpectedSessions PersonId
    | FetchFollowUpMeasurements HealthCenterId
    | FetchFollowUpParticipants (List PersonId)
    | FetchHealthCenters
    | FetchIndividualEncounterParticipant IndividualEncounterParticipantId
    | FetchIndividualEncounterParticipants (List IndividualEncounterParticipantId)
    | FetchIndividualEncounterParticipantsForPerson PersonId
    | FetchIndividualEncounterParticipantsForPeople (List PersonId)
    | FetchMotherMeasurements PersonId
    | FetchMothersMeasurements (List PersonId)
    | FetchNutritionEncounter NutritionEncounterId
    | FetchNutritionEncountersForParticipant IndividualEncounterParticipantId
    | FetchNutritionMeasurements NutritionEncounterId
    | FetchHomeVisitEncounter HomeVisitEncounterId
    | FetchHomeVisitEncountersForParticipant IndividualEncounterParticipantId
    | FetchHomeVisitEncountersForParticipants (List IndividualEncounterParticipantId)
    | FetchHomeVisitMeasurements HomeVisitEncounterId
    | FetchWellChildEncounter WellChildEncounterId
    | FetchWellChildEncountersForParticipant IndividualEncounterParticipantId
    | FetchWellChildEncountersForParticipants (List IndividualEncounterParticipantId)
    | FetchWellChildMeasurements WellChildEncounterId
    | FetchNCDEncounter NCDEncounterId
    | FetchNCDEncountersForParticipant IndividualEncounterParticipantId
    | FetchNCDEncountersForParticipants (List IndividualEncounterParticipantId)
    | FetchNCDMeasurements NCDEncounterId
    | FetchParticipantForms
    | FetchParticipantsForPerson PersonId
    | FetchPeople (List PersonId)
    | FetchPeopleByName String
    | FetchPeopleByNationalId String
    | FetchPeopleInVillage VillageId
    | FetchPerson PersonId
    | FetchPrenatalEncounter PrenatalEncounterId
    | FetchPrenatalEncounters (List PrenatalEncounterId)
    | FetchPrenatalEncountersForParticipant IndividualEncounterParticipantId
    | FetchPrenatalEncountersForParticipants (List IndividualEncounterParticipantId)
    | FetchPrenatalMeasurements PrenatalEncounterId
    | FetchRelationshipsForPerson PersonId
    | FetchResilienceSurveysForNurse NurseId
    | FetchSession SessionId
    | FetchSessionsByClinic ClinicId
    | FetchStockManagementMeasurements HealthCenterId
    | FetchStockManagementData HealthCenterId
    | FetchTuberculosisEncounter TuberculosisEncounterId
    | FetchTuberculosisEncounters (List TuberculosisEncounterId)
    | FetchTuberculosisEncountersForParticipant IndividualEncounterParticipantId
    | FetchTuberculosisEncountersForParticipants (List IndividualEncounterParticipantId)
    | FetchTuberculosisMeasurements TuberculosisEncounterId
    | FetchHIVEncounter HIVEncounterId
    | FetchHIVEncounters (List HIVEncounterId)
    | FetchHIVEncountersForParticipant IndividualEncounterParticipantId
    | FetchHIVEncountersForParticipants (List IndividualEncounterParticipantId)
    | FetchHIVMeasurements HIVEncounterId
    | MarkForRecalculationStockManagementData HealthCenterId
    | FetchVillages
    | FetchTraceContact AcuteIllnessTraceContactId
    | FetchPregnancyByNewborn PersonId
      -- Messages which handle responses to data
    | HandleFetchedAcuteIllnessEncounter AcuteIllnessEncounterId (WebData AcuteIllnessEncounter)
    | HandleFetchedAcuteIllnessEncounters (WebData (Dict AcuteIllnessEncounterId AcuteIllnessEncounter))
    | HandleFetchedAcuteIllnessEncountersForParticipant IndividualEncounterParticipantId (WebData (Dict AcuteIllnessEncounterId AcuteIllnessEncounter))
    | HandleFetchedAcuteIllnessEncountersForParticipants (WebData (Dict IndividualEncounterParticipantId (Dict AcuteIllnessEncounterId AcuteIllnessEncounter)))
    | HandleFetchedAcuteIllnessMeasurements AcuteIllnessEncounterId (WebData AcuteIllnessMeasurements)
    | HandleFetchedChildMeasurements PersonId (WebData ChildMeasurementList)
    | HandleFetchedComputedDashboard HealthCenterId (WebData (Dict HealthCenterId DashboardStatsRaw))
    | HandleFetchedChildrenMeasurements (WebData (Dict PersonId ChildMeasurementList))
    | HandleFetchedChildScoreboardEncounter ChildScoreboardEncounterId (WebData ChildScoreboardEncounter)
    | HandleFetchedChildScoreboardEncountersForParticipant IndividualEncounterParticipantId (WebData (Dict ChildScoreboardEncounterId ChildScoreboardEncounter))
    | HandleFetchedChildScoreboardMeasurements ChildScoreboardEncounterId (WebData ChildScoreboardMeasurements)
    | HandleFetchedClinics (WebData (Dict ClinicId Clinic))
    | HandleFetchedEducationSession EducationSessionId (WebData EducationSession)
    | HandleFetchedEducationSessionsForPerson PersonId (WebData (Dict EducationSessionId EducationSession))
    | HandleFetchedEveryCounselingSchedule (WebData EveryCounselingSchedule)
    | HandleFetchedExpectedParticipants SessionId (WebData ExpectedParticipants)
    | HandleFetchedExpectedSessions PersonId (WebData (Dict SessionId Session))
    | HandleFetchedFollowUpMeasurements HealthCenterId (WebData FollowUpMeasurements)
    | HandleFetchFollowUpParticipants (WebData (Dict PersonId Person))
    | HandleFetchedHealthCenters (WebData (Dict HealthCenterId HealthCenter))
    | HandleFetchedIndividualEncounterParticipant IndividualEncounterParticipantId (WebData IndividualEncounterParticipant)
    | HandleFetchedIndividualEncounterParticipants (WebData (Dict IndividualEncounterParticipantId IndividualEncounterParticipant))
    | HandleFetchedIndividualEncounterParticipantsForPerson PersonId (WebData (Dict IndividualEncounterParticipantId IndividualEncounterParticipant))
    | HandleFetchedIndividualEncounterParticipantsForPeople (WebData (Dict PersonId (Dict IndividualEncounterParticipantId IndividualEncounterParticipant)))
    | HandleFetchedMotherMeasurements PersonId (WebData MotherMeasurementList)
    | HandleFetchedMothersMeasurements (WebData (Dict PersonId MotherMeasurementList))
    | HandleFetchedNutritionEncounter NutritionEncounterId (WebData NutritionEncounter)
    | HandleFetchedNutritionEncountersForParticipant IndividualEncounterParticipantId (WebData (Dict NutritionEncounterId NutritionEncounter))
    | HandleFetchedNutritionMeasurements NutritionEncounterId (WebData NutritionMeasurements)
    | HandleFetchedHomeVisitEncounter HomeVisitEncounterId (WebData HomeVisitEncounter)
    | HandleFetchedHomeVisitEncountersForParticipant IndividualEncounterParticipantId (WebData (Dict HomeVisitEncounterId HomeVisitEncounter))
    | HandleFetchedHomeVisitEncountersForParticipants (WebData (Dict IndividualEncounterParticipantId (Dict HomeVisitEncounterId HomeVisitEncounter)))
    | HandleFetchedHomeVisitMeasurements HomeVisitEncounterId (WebData HomeVisitMeasurements)
    | HandleFetchedWellChildEncounter WellChildEncounterId (WebData WellChildEncounter)
    | HandleFetchedWellChildEncountersForParticipant IndividualEncounterParticipantId (WebData (Dict WellChildEncounterId WellChildEncounter))
    | HandleFetchedWellChildEncountersForParticipants (WebData (Dict IndividualEncounterParticipantId (Dict WellChildEncounterId WellChildEncounter)))
    | HandleFetchedWellChildMeasurements WellChildEncounterId (WebData WellChildMeasurements)
    | HandleFetchedNCDEncounter NCDEncounterId (WebData NCDEncounter)
    | HandleFetchedNCDEncountersForParticipant IndividualEncounterParticipantId (WebData (Dict NCDEncounterId NCDEncounter))
    | HandleFetchedNCDEncountersForParticipants (WebData (Dict IndividualEncounterParticipantId (Dict NCDEncounterId NCDEncounter)))
    | HandleFetchedNCDMeasurements NCDEncounterId (WebData NCDMeasurements)
    | HandleFetchedParticipantForms (WebData (Dict ParticipantFormId ParticipantForm))
    | HandleFetchedParticipantsForPerson PersonId (WebData (Dict PmtctParticipantId PmtctParticipant))
    | HandleFetchedPeople (WebData (Dict PersonId Person))
    | HandleFetchedPeopleByName String (WebData (Dict PersonId Person))
    | HandleFetchedPeopleByNationalId String (WebData (Dict PersonId Person))
    | HandleFetchedPeopleInVillage VillageId (WebData (Dict PersonId Person))
    | HandleFetchedPerson PersonId (WebData Person)
    | HandleFetchedPrenatalEncounter PrenatalEncounterId (WebData PrenatalEncounter)
    | HandleFetchedPrenatalEncounters (WebData (Dict PrenatalEncounterId PrenatalEncounter))
    | HandleFetchedPrenatalEncountersForParticipant IndividualEncounterParticipantId (WebData (Dict PrenatalEncounterId PrenatalEncounter))
    | HandleFetchedPrenatalEncountersForParticipants (WebData (Dict IndividualEncounterParticipantId (Dict PrenatalEncounterId PrenatalEncounter)))
    | HandleFetchedPrenatalMeasurements PrenatalEncounterId (WebData PrenatalMeasurements)
    | HandleFetchedRelationshipsForPerson PersonId (WebData (Dict RelationshipId MyRelationship))
    | HandleFetchedResilienceSurveysForNurse NurseId (WebData (Dict ResilienceSurveyId ResilienceSurvey))
    | HandleFetchedSession SessionId (WebData Session)
    | HandleFetchedSessionsByClinic ClinicId (WebData (Dict SessionId Session))
    | HandleFetchedStockManagementMeasurements HealthCenterId (WebData StockManagementMeasurements)
    | HandleFetchedTuberculosisEncounter TuberculosisEncounterId (WebData TuberculosisEncounter)
    | HandleFetchedTuberculosisEncounters (WebData (Dict TuberculosisEncounterId TuberculosisEncounter))
    | HandleFetchedTuberculosisEncountersForParticipant IndividualEncounterParticipantId (WebData (Dict TuberculosisEncounterId TuberculosisEncounter))
    | HandleFetchedTuberculosisEncountersForParticipants (WebData (Dict IndividualEncounterParticipantId (Dict TuberculosisEncounterId TuberculosisEncounter)))
    | HandleFetchedTuberculosisMeasurements TuberculosisEncounterId (WebData TuberculosisMeasurements)
    | HandleFetchedHIVEncounter HIVEncounterId (WebData HIVEncounter)
    | HandleFetchedHIVEncounters (WebData (Dict HIVEncounterId HIVEncounter))
    | HandleFetchedHIVEncountersForParticipant IndividualEncounterParticipantId (WebData (Dict HIVEncounterId HIVEncounter))
    | HandleFetchedHIVEncountersForParticipants (WebData (Dict IndividualEncounterParticipantId (Dict HIVEncounterId HIVEncounter)))
    | HandleFetchedHIVMeasurements HIVEncounterId (WebData HIVMeasurements)
    | HandleFetchedVillages (WebData (Dict VillageId Village))
    | HandleFetchedTraceContact AcuteIllnessTraceContactId (WebData AcuteIllnessTraceContact)
    | HandleFetchedPregnancyByNewborn PersonId (WebData (Maybe ( IndividualEncounterParticipantId, IndividualEncounterParticipant )))
      -- Messages which mutate data
    | PostPerson (Maybe PersonId) Initiator Person -- The first person is a person we ought to offer setting a relationship to.
    | PatchPerson PatchPersonInitator PersonId Person
    | PostRelationship PersonId MyRelationship (Maybe ClinicId) Initiator
    | PostPmtctParticipant Initiator PmtctParticipant
    | PostSession Session
    | PostIndividualEncounterParticipant IndividualParticipantExtraData IndividualEncounterParticipant
    | PostPrenatalEncounter PrenatalEncounterPostCreateDestination PrenatalEncounter
    | PostNutritionEncounter NutritionEncounter
    | PostAcuteIllnessEncounter AcuteIllnessEncounter
    | PostHomeVisitEncounter HomeVisitEncounter
    | PostWellChildEncounter WellChildEncounter
    | PostNCDEncounter NCDEncounter
    | PostChildScoreboardEncounter ChildScoreboardEncounter
    | PostTuberculosisEncounter TuberculosisEncounter
    | PostHIVEncounter HIVEncounter
    | PostEducationSession EducationSession
      -- Messages which handle responses to mutating data
    | HandlePostedPerson (Maybe PersonId) Initiator (WebData PersonId)
    | HandlePatchedPerson PatchPersonInitator PersonId (WebData Person)
    | HandlePostedRelationship PersonId Initiator (WebData MyRelationship)
    | HandlePostedPmtctParticipant PersonId Initiator (WebData ( PmtctParticipantId, PmtctParticipant ))
    | HandlePostedSession (WebData SessionId)
    | HandlePostedIndividualEncounterParticipant PersonId IndividualEncounterType IndividualParticipantExtraData (WebData ( IndividualEncounterParticipantId, IndividualEncounterParticipant ))
    | HandlePostedPrenatalEncounter IndividualEncounterParticipantId PrenatalEncounterPostCreateDestination (WebData ( PrenatalEncounterId, PrenatalEncounter ))
    | HandlePostedNutritionEncounter IndividualEncounterParticipantId (WebData ( NutritionEncounterId, NutritionEncounter ))
    | HandlePostedAcuteIllnessEncounter IndividualEncounterParticipantId (WebData ( AcuteIllnessEncounterId, AcuteIllnessEncounter ))
    | HandlePostedHomeVisitEncounter IndividualEncounterParticipantId (WebData ( HomeVisitEncounterId, HomeVisitEncounter ))
    | HandlePostedWellChildEncounter IndividualEncounterParticipantId (WebData ( WellChildEncounterId, WellChildEncounter ))
    | HandlePostedNCDEncounter IndividualEncounterParticipantId (WebData ( NCDEncounterId, NCDEncounter ))
    | HandlePostedChildScoreboardEncounter IndividualEncounterParticipantId (WebData ( ChildScoreboardEncounterId, ChildScoreboardEncounter ))
    | HandlePostedTuberculosisEncounter IndividualEncounterParticipantId (WebData ( TuberculosisEncounterId, TuberculosisEncounter ))
    | HandlePostedHIVEncounter IndividualEncounterParticipantId (WebData ( HIVEncounterId, HIVEncounter ))
    | HandlePostedEducationSession (WebData ( EducationSessionId, EducationSession ))
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
    | MsgNCDEncounter NCDEncounterId Backend.NCDEncounter.Model.Msg
    | MsgChildScoreboardEncounter ChildScoreboardEncounterId Backend.ChildScoreboardEncounter.Model.Msg
    | MsgTuberculosisEncounter TuberculosisEncounterId Backend.TuberculosisEncounter.Model.Msg
    | MsgHIVEncounter HIVEncounterId Backend.HIVEncounter.Model.Msg
    | MsgEducationSession EducationSessionId Backend.EducationSession.Model.Msg
    | MsgTraceContact AcuteIllnessTraceContactId Backend.TraceContact.Model.Msg
    | MsgIndividualEncounterParticipant IndividualEncounterParticipantId Backend.IndividualEncounterParticipant.Model.Msg
    | MsgNurse NurseId Backend.Nurse.Model.Msg
    | MsgResilienceSurvey NurseId Backend.ResilienceSurvey.Model.Msg
    | MsgStockUpdate NurseId Backend.StockUpdate.Model.Msg
    | ResetFailedToFetchAuthorities


{-| Wrapper for all the revisions we can receive.
-}
type Revision
    = AcuteFindingsRevision AcuteFindingsId AcuteFindings
    | AcuteIllnessContactsTracingRevision AcuteIllnessContactsTracingId AcuteIllnessContactsTracing
    | AcuteIllnessCoreExamRevision AcuteIllnessCoreExamId AcuteIllnessCoreExam
    | AcuteIllnessDangerSignsRevision AcuteIllnessDangerSignsId AcuteIllnessDangerSigns
    | AcuteIllnessEncounterRevision AcuteIllnessEncounterId AcuteIllnessEncounter
    | AcuteIllnessENTRevision AcuteIllnessENTId AcuteIllnessENT
    | AcuteIllnessEyesRevision AcuteIllnessEyesId AcuteIllnessEyes
    | AcuteIllnessFollowUpRevision AcuteIllnessFollowUpId AcuteIllnessFollowUp
    | AcuteIllnessGURevision AcuteIllnessGUId AcuteIllnessGU
    | AcuteIllnessMuacRevision AcuteIllnessMuacId AcuteIllnessMuac
    | AcuteIllnessNutritionRevision AcuteIllnessNutritionId AcuteIllnessNutrition
    | AcuteIllnessOralRevision AcuteIllnessOralId AcuteIllnessOral
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
    | ChildScoreboardEncounterRevision ChildScoreboardEncounterId ChildScoreboardEncounter
    | ChildScoreboardBCGImmunisationRevision ChildScoreboardBCGImmunisationId ChildScoreboardBCGImmunisation
    | ChildScoreboardDTPImmunisationRevision ChildScoreboardDTPImmunisationId ChildScoreboardDTPImmunisation
    | ChildScoreboardDTPStandaloneImmunisationRevision ChildScoreboardDTPStandaloneImmunisationId ChildScoreboardDTPStandaloneImmunisation
    | ChildScoreboardIPVImmunisationRevision ChildScoreboardIPVImmunisationId ChildScoreboardIPVImmunisation
    | ChildScoreboardMRImmunisationRevision ChildScoreboardMRImmunisationId ChildScoreboardMRImmunisation
    | ChildScoreboardNCDARevision ChildScoreboardNCDAId ChildScoreboardNCDA
    | ChildScoreboardOPVImmunisationRevision ChildScoreboardOPVImmunisationId ChildScoreboardOPVImmunisation
    | ChildScoreboardPCV13ImmunisationRevision ChildScoreboardPCV13ImmunisationId ChildScoreboardPCV13Immunisation
    | ChildScoreboardRotarixImmunisationRevision ChildScoreboardRotarixImmunisationId ChildScoreboardRotarixImmunisation
    | ClinicRevision ClinicId Clinic
    | ContributingFactorsRevision ContributingFactorsId ContributingFactors
    | CorePhysicalExamRevision CorePhysicalExamId CorePhysicalExam
    | CovidTestingRevision CovidTestingId CovidTesting
    | CounselingScheduleRevision CounselingScheduleId CounselingSchedule
    | CounselingSessionRevision CounselingSessionId CounselingSession
    | CounselingTopicRevision CounselingTopicId CounselingTopic
    | DangerSignsRevision DangerSignsId DangerSigns
    | DashboardStatsRevision HealthCenterId DashboardStatsRaw
    | EducationSessionRevision EducationSessionId EducationSession
    | ExposureRevision ExposureId Exposure
    | FamilyPlanningRevision FamilyPlanningId FamilyPlanning
    | FollowUpRevision FollowUpId FollowUp
    | GroupHealthEducationRevision GroupHealthEducationId GroupHealthEducation
    | GroupNCDARevision GroupNCDAId GroupNCDA
    | GroupSendToHCRevision GroupSendToHCId GroupSendToHC
    | HCContactRevision HCContactId HCContact
    | HealthCenterRevision HealthCenterId HealthCenter
    | HealthEducationRevision HealthEducationId HealthEducation
    | HeightRevision HeightId Height
    | HIVDiagnosticsRevision HIVDiagnosticsId HIVDiagnostics
    | HIVEncounterRevision HIVEncounterId HIVEncounter
    | HIVFollowUpRevision HIVFollowUpId HIVFollowUp
    | HIVHealthEducationRevision HIVHealthEducationId HIVHealthEducation
    | HIVMedicationRevision HIVMedicationId HIVMedication
    | HIVReferralRevision HIVReferralId HIVReferral
    | HIVSymptomReviewRevision HIVSymptomReviewId HIVSymptomReview
    | HIVTreatmentReviewRevision HIVTreatmentReviewId HIVTreatmentReview
    | HomeVisitEncounterRevision HomeVisitEncounterId HomeVisitEncounter
    | IndividualEncounterParticipantRevision IndividualEncounterParticipantId IndividualEncounterParticipant
    | IsolationRevision IsolationId Isolation
    | LactationRevision LactationId Lactation
    | LastMenstrualPeriodRevision LastMenstrualPeriodId LastMenstrualPeriod
    | MalariaTestingRevision MalariaTestingId MalariaTesting
    | MalariaPreventionRevision MalariaPreventionId MalariaPrevention
    | MedicalHistoryRevision MedicalHistoryId MedicalHistory
    | MedicationRevision MedicationId Medication
    | MedicationDistributionRevision MedicationDistributionId MedicationDistribution
    | MotherFbfRevision MotherFbfId Fbf
    | MuacRevision MuacId Muac
    | NCDCoMorbiditiesRevision NCDCoMorbiditiesId NCDCoMorbidities
    | NCDCoreExamRevision NCDCoreExamId NCDCoreExam
    | NCDCreatinineTestRevision NCDCreatinineTestId NCDCreatinineTest
    | NCDDangerSignsRevision NCDDangerSignsId NCDDangerSigns
    | NCDEncounterRevision NCDEncounterId NCDEncounter
    | NCDFamilyHistoryRevision NCDFamilyHistoryId NCDFamilyHistory
    | NCDFamilyPlanningRevision NCDFamilyPlanningId NCDFamilyPlanning
    | NCDHbA1cTestRevision NCDHbA1cTestId NCDHbA1cTest
    | NCDHealthEducationRevision NCDHealthEducationId NCDHealthEducation
    | NCDHIVTestRevision NCDHIVTestId NCDHIVTest
    | NCDLabsResultsRevision NCDLabsResultsId NCDLabsResults
    | NCDLipidPanelTestRevision NCDLipidPanelTestId NCDLipidPanelTest
    | NCDLiverFunctionTestRevision NCDLiverFunctionTestId NCDLiverFunctionTest
    | NCDMedicationDistributionRevision NCDMedicationDistributionId NCDMedicationDistribution
    | NCDMedicationHistoryRevision NCDMedicationHistoryId NCDMedicationHistory
    | NCDOutsideCareRevision NCDOutsideCareId NCDOutsideCare
    | NCDPregnancyTestRevision NCDPregnancyTestId NCDPregnancyTest
    | NCDRandomBloodSugarTestRevision NCDRandomBloodSugarTestId NCDRandomBloodSugarTest
    | NCDReferralRevision NCDReferralId NCDReferral
    | NCDSocialHistoryRevision NCDSocialHistoryId NCDSocialHistory
    | NCDSymptomReviewRevision NCDSymptomReviewId NCDSymptomReview
    | NCDUrineDipstickTestRevision NCDUrineDipstickTestId NCDUrineDipstickTest
    | NCDVitalsRevision NCDVitalsId NCDVitals
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
    | NutritionNCDARevision NutritionNCDAId NutritionNCDA
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
    | PregnancyTestRevision PregnancyTestId PregnancyTest
    | PrenatalAspirinRevision PrenatalAspirinId PrenatalAspirin
    | PrenatalBloodGpRsTestRevision PrenatalBloodGpRsTestId PrenatalBloodGpRsTest
    | PrenatalBreastfeedingRevision PrenatalBreastfeedingId PrenatalBreastfeeding
    | PrenatalCalciumRevision PrenatalCalciumId PrenatalCalcium
    | PrenatalEncounterRevision PrenatalEncounterId PrenatalEncounter
    | PrenatalFamilyPlanningRevision PrenatalFamilyPlanningId PrenatalFamilyPlanning
    | PrenatalFefolRevision PrenatalFefolId PrenatalFefol
    | PrenatalFolateRevision PrenatalFolateId PrenatalFolate
    | PrenatalFollowUpRevision PrenatalFollowUpId PrenatalFollowUp
    | PrenatalGUExamRevision PrenatalGUExamId PrenatalGUExam
    | PrenatalHealthEducationRevision PrenatalHealthEducationId PrenatalHealthEducation
    | PrenatalHemoglobinTestRevision PrenatalHemoglobinTestId PrenatalHemoglobinTest
    | PrenatalHepatitisBTestRevision PrenatalHepatitisBTestId PrenatalHepatitisBTest
    | PrenatalHIVTestRevision PrenatalHIVTestId PrenatalHIVTest
    | PrenatalHIVPCRTestRevision PrenatalHIVPCRTestId PrenatalHIVPCRTest
    | PrenatalIronRevision PrenatalIronId PrenatalIron
    | PrenatalLabsResultsRevision PrenatalLabsResultsId PrenatalLabsResults
    | PrenatalMalariaTestRevision PrenatalMalariaTestId PrenatalMalariaTest
    | PrenatalMebendazoleRevision PrenatalMebendazoleId PrenatalMebendazole
    | PrenatalMentalHealthRevision PrenatalMentalHealthId PrenatalMentalHealth
    | PrenatalMedicationDistributionRevision PrenatalMedicationDistributionId PrenatalMedicationDistribution
    | PrenatalMMSRevision PrenatalMMSId PrenatalMMS
    | PrenatalNutritionRevision PrenatalNutritionId PrenatalNutrition
    | PrenatalOutsideCareRevision PrenatalOutsideCareId PrenatalOutsideCare
    | PrenatalPartnerHIVTestRevision PrenatalPartnerHIVTestId PrenatalPartnerHIVTest
    | PrenatalPhotoRevision PrenatalPhotoId PrenatalPhoto
    | PrenatalRandomBloodSugarTestRevision PrenatalRandomBloodSugarTestId PrenatalRandomBloodSugarTest
    | PrenatalSendToHCRevision PrenatalSendToHCId PrenatalSendToHC
    | PrenatalSpecialityCareRevision PrenatalSpecialityCareId PrenatalSpecialityCare
    | PrenatalSymptomReviewRevision PrenatalSymptomReviewId PrenatalSymptomReview
    | PrenatalSyphilisTestRevision PrenatalSyphilisTestId PrenatalSyphilisTest
    | PrenatalTetanusImmunisationRevision PrenatalTetanusImmunisationId PrenatalTetanusImmunisation
    | PrenatalUrineDipstickTestRevision PrenatalUrineDipstickTestId PrenatalUrineDipstickTest
    | RelationshipRevision RelationshipId Relationship
    | ResilienceSurveyRevision ResilienceSurveyId ResilienceSurvey
    | SendToHCRevision SendToHCId SendToHC
    | SessionRevision SessionId Session
    | SocialHistoryRevision SocialHistoryId SocialHistory
    | StockUpdateRevision StockUpdateId StockUpdate
    | SymptomsGeneralRevision SymptomsGeneralId SymptomsGeneral
    | SymptomsGIRevision SymptomsGIId SymptomsGI
    | SymptomsRespiratoryRevision SymptomsRespiratoryId SymptomsRespiratory
    | TravelHistoryRevision TravelHistoryId TravelHistory
    | TreatmentOngoingRevision TreatmentOngoingId TreatmentOngoing
    | TreatmentReviewRevision TreatmentReviewId TreatmentReview
    | TuberculosisDiagnosticsRevision TuberculosisDiagnosticsId TuberculosisDiagnostics
    | TuberculosisDOTRevision TuberculosisDOTId TuberculosisDOT
    | TuberculosisEncounterRevision TuberculosisEncounterId TuberculosisEncounter
    | TuberculosisFollowUpRevision TuberculosisFollowUpId TuberculosisFollowUp
    | TuberculosisHealthEducationRevision TuberculosisHealthEducationId TuberculosisHealthEducation
    | TuberculosisMedicationRevision TuberculosisMedicationId TuberculosisMedication
    | TuberculosisReferralRevision TuberculosisReferralId TuberculosisReferral
    | TuberculosisSymptomReviewRevision TuberculosisSymptomReviewId TuberculosisSymptomReview
    | TuberculosisTreatmentReviewRevision TuberculosisTreatmentReviewId TuberculosisTreatmentReview
    | VillageRevision VillageId Village
    | VitalsRevision VitalsId Vitals
    | WeightRevision WeightId Weight
    | WellChildAlbendazoleRevision WellChildAlbendazoleId WellChildAlbendazole
    | WellChildBCGImmunisationRevision WellChildBCGImmunisationId WellChildBCGImmunisation
    | WellChildCaringRevision WellChildCaringId WellChildCaring
    | WellChildContributingFactorsRevision WellChildContributingFactorsId WellChildContributingFactors
    | WellChildDTPImmunisationRevision WellChildDTPImmunisationId WellChildDTPImmunisation
    | WellChildDTPStandaloneImmunisationRevision WellChildDTPStandaloneImmunisationId WellChildDTPStandaloneImmunisation
    | WellChildECDRevision WellChildECDId WellChildECD
    | WellChildEncounterRevision WellChildEncounterId WellChildEncounter
    | WellChildFeedingRevision WellChildFeedingId WellChildFeeding
    | WellChildFollowUpRevision WellChildFollowUpId WellChildFollowUp
    | WellChildFoodSecurityRevision WellChildFoodSecurityId WellChildFoodSecurity
    | WellChildHeadCircumferenceRevision WellChildHeadCircumferenceId WellChildHeadCircumference
    | WellChildHealthEducationRevision WellChildHealthEducationId WellChildHealthEducation
    | WellChildHeightRevision WellChildHeightId WellChildHeight
    | WellChildHygieneRevision WellChildHygieneId WellChildHygiene
    | WellChildHPVImmunisationRevision WellChildHPVImmunisationId WellChildHPVImmunisation
    | WellChildIPVImmunisationRevision WellChildIPVImmunisationId WellChildIPVImmunisation
    | WellChildMebendezoleRevision WellChildMebendezoleId WellChildMebendezole
    | WellChildMRImmunisationRevision WellChildMRImmunisationId WellChildMRImmunisation
    | WellChildMuacRevision WellChildMuacId WellChildMuac
    | WellChildNCDARevision WellChildNCDAId WellChildNCDA
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
