module Backend.Update exposing (updateIndexedDb)

import Activity.Model exposing (Activity(..), ChildActivity(..), SummaryByActivity, SummaryByParticipant)
import Activity.Utils exposing (getAllChildActivities, getAllMotherActivities, motherIsCheckedIn, summarizeChildActivity, summarizeChildParticipant, summarizeMotherActivity, summarizeMotherParticipant)
import App.Model
import App.Utils exposing (triggerRollbarOnFailure)
import AssocList as Dict
import Backend.AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import Backend.AcuteIllnessEncounter.Model exposing (emptyAcuteIllnessEncounter)
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis(..))
import Backend.AcuteIllnessEncounter.Update
import Backend.ChildScoreboardActivity.Utils
import Backend.ChildScoreboardEncounter.Model
import Backend.ChildScoreboardEncounter.Update
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Counseling.Decoder exposing (combineCounselingSchedules)
import Backend.Dashboard.Model exposing (DashboardStatsRaw)
import Backend.EducationSession.Model
import Backend.EducationSession.Update
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.HIVEncounter.Model
import Backend.HIVEncounter.Update
import Backend.HomeVisitEncounter.Model exposing (emptyHomeVisitEncounter)
import Backend.HomeVisitEncounter.Update
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..), IndividualParticipantExtraData(..), IndividualParticipantInitiator(..))
import Backend.IndividualEncounterParticipant.Update
import Backend.Measurement.Model
    exposing
        ( BloodSmearResult(..)
        , ChildMeasurements
        , HistoricalMeasurements
        , LaboratoryTest(..)
        , LabsResultsReviewState(..)
        , Measurements
        , TestExecutionNote(..)
        , TestPrerequisite
        , WellChildSymptom(..)
        )
import Backend.Measurement.Utils
    exposing
        ( getMeasurementValueFunc
        , labExpirationPeriod
        , mapChildMeasurementsAtOfflineSession
        , mapMeasurementData
        , splitChildMeasurements
        , splitMotherMeasurements
        )
import Backend.Model exposing (..)
import Backend.NCDActivity.Model
import Backend.NCDEncounter.Model
import Backend.NCDEncounter.Update
import Backend.Nurse.Model
import Backend.Nurse.Update
import Backend.NutritionActivity.Model
import Backend.NutritionEncounter.Model exposing (emptyNutritionEncounter)
import Backend.NutritionEncounter.Update
import Backend.NutritionEncounter.Utils exposing (nutritionAssessmentForBackend)
import Backend.Person.Model exposing (Initiator(..), PatchPersonInitator(..), Person)
import Backend.Person.Utils exposing (ageInMonths, graduatingAgeInMonth)
import Backend.PmtctParticipant.Model exposing (AdultActivities(..))
import Backend.PrenatalActivity.Model
import Backend.PrenatalEncounter.Model
    exposing
        ( PrenatalEncounterPostCreateDestination(..)
        , PrenatalProgressReportInitiator(..)
        , emptyPrenatalEncounter
        )
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis(..))
import Backend.PrenatalEncounter.Update
import Backend.Relationship.Encoder exposing (encodeRelationshipChanges)
import Backend.Relationship.Model exposing (MyRelatedBy(..), MyRelationship, RelatedBy(..))
import Backend.Relationship.Utils exposing (toMyRelationship, toRelationship)
import Backend.ResilienceSurvey.Model
import Backend.ResilienceSurvey.Update
import Backend.Session.Model exposing (CheckedIn, EditableSession, OfflineSession)
import Backend.Session.Update
import Backend.Session.Utils exposing (getChildMeasurementData2, getMyMother)
import Backend.StockUpdate.Model
import Backend.StockUpdate.Update
import Backend.StockUpdate.Utils exposing (generateStockManagementData)
import Backend.TraceContact.Model
import Backend.TraceContact.Update
import Backend.TuberculosisEncounter.Model
import Backend.TuberculosisEncounter.Update
import Backend.Utils exposing (..)
import Backend.Village.Utils exposing (getVillageById, getVillageClinicId)
import Backend.WellChildEncounter.Model exposing (EncounterWarning(..), emptyWellChildEncounter)
import Backend.WellChildEncounter.Update
import Date exposing (Unit(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Json.Encode exposing (object)
import LocalData exposing (LocalData(..), ReadyStatus(..))
import Maybe.Extra exposing (isJust, isNothing)
import Measurement.Model
import Measurement.Utils exposing (bloodSmearResultSet, testPerformedByExecutionNote)
import Pages.AcuteIllness.Activity.Model
import Pages.AcuteIllness.Activity.Types
import Pages.AcuteIllness.Activity.Utils
    exposing
        ( activityCompleted
        , mandatoryActivitiesCompletedSubsequentVisit
        , noImprovementOnSubsequentVisit
        , resolveAcuteIllnessDiagnosis
        , resolveNextStepFirstEncounter
        , resolveNextStepSubsequentEncounter
        , respiratoryRateAbnormalForAge
        )
import Pages.AcuteIllness.Encounter.Model
import Pages.AcuteIllness.Encounter.Utils
import Pages.ChildScoreboard.Activity.Utils
import Pages.ChildScoreboard.Encounter.Utils
import Pages.Dashboard.Model
import Pages.Dashboard.Utils
import Pages.GlobalCaseManagement.Utils
import Pages.NCD.RecurrentActivity.Utils
import Pages.NCD.Utils
import Pages.NextSteps.Model
import Pages.Nutrition.Activity.Model
import Pages.Nutrition.Activity.Utils
import Pages.Nutrition.Encounter.Utils
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Participant.Model
import Pages.Person.Model
import Pages.Prenatal.Activity.Model
import Pages.Prenatal.Activity.Types exposing (WarningPopupType(..))
import Pages.Prenatal.Activity.Utils
import Pages.Prenatal.Encounter.Model
import Pages.Prenatal.Encounter.Utils
import Pages.Prenatal.RecurrentActivity.Model
import Pages.Prenatal.RecurrentActivity.Utils
import Pages.Prenatal.RecurrentEncounter.Utils
import Pages.Prenatal.Utils
import Pages.Relationship.Model
import Pages.Session.Model
import Pages.Tuberculosis.Encounter.Utils
import Pages.WellChild.Activity.Utils
import Pages.WellChild.Encounter.Model
import Pages.WellChild.Encounter.Utils
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (toCmd, toTask)
import SyncManager.Model exposing (Site, SiteFeature)
import Task
import Time
import Translate exposing (Language, translate)
import ZScore.Model


updateIndexedDb :
    Language
    -> NominalDate
    -> Time.Posix
    -> Maybe App.Model.GPSCoordinates
    -> ZScore.Model.Model
    -> Site
    -> EverySet SiteFeature
    -> Maybe NurseId
    -> Maybe HealthCenterId
    -> Maybe VillageId
    -> Bool
    -> Bool
    -> Page
    -> SyncManager.Model.Model
    -> MsgIndexedDb
    -> ModelIndexedDb
    -> ( ModelIndexedDb, Cmd MsgIndexedDb, List App.Model.Msg )
updateIndexedDb language currentDate currentTime coordinates zscores site features nurseId healthCenterId villageId isChw isLabTech activePage syncManager msg model =
    let
        noChange =
            ( model, Cmd.none, [] )
    in
    case msg of
        HandleLogout ->
            let
                -- On logout we want to clear the statistics dashboards loaded
                -- for health center.
                -- We do this to maintain proper loading rutine when switching
                -- between nurse and chw.
                updatedComputedDashboards =
                    Maybe.map
                        (\healthCenterId_ ->
                            Dict.remove healthCenterId_ model.computedDashboards
                        )
                        healthCenterId
                        |> Maybe.withDefault model.computedDashboards
            in
            ( { model | computedDashboards = updatedComputedDashboards }
            , Cmd.none
            , []
            )

        FetchChildMeasurements childId ->
            ( { model | childMeasurements = Dict.insert childId Loading model.childMeasurements }
            , sw.get childMeasurementListEndpoint childId
                |> toCmd (RemoteData.fromResult >> HandleFetchedChildMeasurements childId)
            , []
            )

        FetchChildrenMeasurements ids ->
            if List.isEmpty ids then
                noChange

            else
                let
                    childMeasurements =
                        List.foldl (\id accum -> Dict.insert id Loading accum) model.childMeasurements ids
                in
                ( { model | childMeasurements = childMeasurements }
                , sw.getMany childMeasurementListEndpoint ids
                    |> toCmd (RemoteData.fromResult >> RemoteData.map Dict.fromList >> HandleFetchedChildrenMeasurements)
                , []
                )

        HandleFetchedChildMeasurements childId data ->
            ( { model | childMeasurements = Dict.insert childId data model.childMeasurements }
            , Cmd.none
            , []
            )

        HandleFetchedChildrenMeasurements webData ->
            case RemoteData.toMaybe webData of
                Nothing ->
                    noChange

                Just dict ->
                    let
                        dictUpdated =
                            Dict.map (\_ v -> RemoteData.Success v) dict
                    in
                    ( { model | childMeasurements = Dict.union dictUpdated model.childMeasurements }
                    , Cmd.none
                    , []
                    )

        FetchClinics ->
            ( { model | clinics = Loading }
            , sw.select clinicEndpoint ()
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> List.sortBy (Tuple.second >> .name) >> Dict.fromList) >> HandleFetchedClinics)
            , []
            )

        HandleFetchedClinics clinics ->
            ( { model | clinics = clinics }
            , Cmd.none
            , []
            )

        FetchComputedDashboard healthCenterId_ ->
            ( { model | computedDashboardLastFetched = currentTime }
            , sw.select computedDashboardEndpoint (ComputedDashboardParams healthCenterId_)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedComputedDashboard healthCenterId_)
            , []
            )

        HandleFetchedComputedDashboard healthCenterId_ webData ->
            let
                modelUpdated =
                    RemoteData.toMaybe webData
                        |> Maybe.andThen (Dict.values >> List.head)
                        |> Maybe.map
                            (\statsRaw ->
                                { model
                                    | computedDashboards =
                                        Dict.insert healthCenterId_
                                            (generateInitialComputedDashboard currentDate healthCenterId_ villageId statsRaw model)
                                            model.computedDashboards
                                }
                            )
                        |> Maybe.withDefault model
            in
            ( modelUpdated
            , Cmd.none
            , []
            )

        FetchComputedDashboardAssembledPermutation healthCenterId_ programTypeFilter selectedVillage ->
            let
                modelUpdated =
                    Dict.get healthCenterId_ model.computedDashboards
                        |> Maybe.map
                            (\computedDashboard ->
                                if Dict.member ( programTypeFilter, selectedVillage ) computedDashboard.assembledPermutations then
                                    model

                                else
                                    let
                                        assembledPermutationsUpdated =
                                            Dict.insert ( programTypeFilter, selectedVillage )
                                                (Pages.Dashboard.Utils.generateAssembledData currentDate healthCenterId_ computedDashboard.statsRaw model programTypeFilter selectedVillage)
                                                computedDashboard.assembledPermutations

                                        computedDashboardUpdated =
                                            { computedDashboard
                                                | assembledPermutations = assembledPermutationsUpdated
                                            }
                                    in
                                    { model | computedDashboards = Dict.insert healthCenterId_ computedDashboardUpdated model.computedDashboards }
                            )
                        |> Maybe.withDefault model
            in
            ( modelUpdated
            , Cmd.none
            , []
            )

        FetchEditableSession id calculationMsgs ->
            let
                newEditable =
                    makeEditableSession id model

                extraMsgs =
                    if RemoteData.isSuccess newEditable then
                        calculationMsgs

                    else
                        []
            in
            -- This one is a bit special. What we're asking for is not a fetch
            -- from IndexedDB as such, but a certain kind of organization of
            -- the data.
            ( { model | editableSessions = Dict.insert id newEditable model.editableSessions }
            , Cmd.none
            , []
            )
                |> sequenceExtra
                    (updateIndexedDb language
                        currentDate
                        currentTime
                        coordinates
                        zscores
                        site
                        features
                        nurseId
                        healthCenterId
                        villageId
                        isChw
                        isLabTech
                        activePage
                        syncManager
                    )
                    extraMsgs

        FetchEditableSessionCheckedIn id ->
            Dict.get id model.editableSessions
                |> Maybe.withDefault NotAsked
                |> RemoteData.map
                    (\editable ->
                        let
                            checkedIn =
                                cacheCheckedIn editable.offlineSession

                            updatedEditable =
                                { editable | checkedIn = checkedIn }
                        in
                        ( { model | editableSessions = Dict.insert id (Success updatedEditable) model.editableSessions }
                        , Cmd.none
                        , []
                        )
                    )
                |> RemoteData.withDefault noChange

        FetchEditableSessionMeasurements id ->
            Dict.get id model.editableSessions
                |> Maybe.withDefault NotAsked
                |> RemoteData.map
                    (\editable ->
                        let
                            measurements =
                                calculateOfflineSessionMeasurements id editable.offlineSession model

                            updatedOffline =
                                editable.offlineSession
                                    |> (\offline -> { offline | measurements = measurements })

                            updatedEditable =
                                { editable | offlineSession = updatedOffline }
                        in
                        ( { model | editableSessions = Dict.insert id (Success updatedEditable) model.editableSessions }
                        , Cmd.none
                        , []
                        )
                    )
                |> RemoteData.withDefault noChange

        FetchEditableSessionSummaryByActivity id ->
            Dict.get id model.editableSessions
                |> Maybe.withDefault NotAsked
                |> RemoteData.map
                    (\editable ->
                        let
                            summaryByActivity =
                                summarizeByActivity currentDate zscores features editable.offlineSession editable.checkedIn isChw model

                            updatedEditable =
                                { editable | summaryByActivity = summaryByActivity }
                        in
                        ( { model | editableSessions = Dict.insert id (Success updatedEditable) model.editableSessions }
                        , Cmd.none
                        , []
                        )
                    )
                |> RemoteData.withDefault noChange

        FetchEditableSessionSummaryByParticipant id ->
            Dict.get id model.editableSessions
                |> Maybe.withDefault NotAsked
                |> RemoteData.map
                    (\editable ->
                        let
                            summaryByParticipant =
                                summarizeByParticipant currentDate zscores features editable.offlineSession editable.checkedIn isChw model

                            updatedEditable =
                                { editable | summaryByParticipant = summaryByParticipant }
                        in
                        ( { model | editableSessions = Dict.insert id (Success updatedEditable) model.editableSessions }
                        , Cmd.none
                        , []
                        )
                    )
                |> RemoteData.withDefault noChange

        FetchEveryCounselingSchedule ->
            let
                topicTask =
                    sw.select counselingTopicEndpoint ()
                        |> toTask
                        |> Task.map (.items >> Dict.fromList)
                        |> RemoteData.fromTask

                scheduleTask =
                    sw.select counselingScheduleEndpoint ()
                        |> toTask
                        |> Task.map (.items >> List.map Tuple.second)
                        |> RemoteData.fromTask
            in
            ( { model | everyCounselingSchedule = Loading }
            , Task.map2 (RemoteData.map2 combineCounselingSchedules) topicTask scheduleTask
                |> Task.perform HandleFetchedEveryCounselingSchedule
            , []
            )

        HandleFetchedEveryCounselingSchedule data ->
            ( { model | everyCounselingSchedule = data }
            , Cmd.none
            , []
            )

        FetchExpectedParticipants sessionId ->
            let
                merge value accum =
                    accum
                        |> Maybe.withDefault []
                        |> (::) value
                        |> Just

                indexBy accessor _ value accum =
                    Dict.update (accessor value) (merge value) accum

                processIndex byId =
                    { byId = byId
                    , byChildId = Dict.foldl (indexBy .child) Dict.empty byId
                    , byMotherId = Dict.foldl (indexBy .adult) Dict.empty byId
                    }
            in
            ( { model | expectedParticipants = Dict.insert sessionId Loading model.expectedParticipants }
            , sw.select pmtctParticipantEndpoint (ParticipantsForSession sessionId)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList >> processIndex) >> HandleFetchedExpectedParticipants sessionId)
            , []
            )

        HandleFetchedExpectedParticipants sessionId data ->
            let
                expectedParticipants =
                    Dict.insert sessionId data model.expectedParticipants

                childrenIds =
                    case RemoteData.toMaybe data of
                        Just dict ->
                            Dict.keys dict.byChildId

                        Nothing ->
                            []

                motherIds =
                    case RemoteData.toMaybe data of
                        Just dict ->
                            Dict.keys dict.byMotherId

                        Nothing ->
                            []

                peopleIds =
                    List.concat [ childrenIds, motherIds ]

                -- Mark people to load.
                people =
                    List.foldl (\id accum -> Dict.insert id RemoteData.NotAsked accum) model.people peopleIds

                -- Mark Mothers and Children measurements to load.
                motherMeasurements =
                    List.foldl (\id accum -> Dict.insert id RemoteData.NotAsked accum) model.motherMeasurements motherIds

                childMeasurements =
                    List.foldl (\id accum -> Dict.insert id RemoteData.NotAsked accum) model.childMeasurements childrenIds
            in
            ( { model
                | expectedParticipants = expectedParticipants
                , people = people
                , motherMeasurements = motherMeasurements
                , childMeasurements = childMeasurements
              }
            , Cmd.none
            , []
            )

        FetchIndividualEncounterParticipant id ->
            ( { model | individualParticipants = Dict.insert id Loading model.individualParticipants }
            , sw.get individualEncounterParticipantEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedIndividualEncounterParticipant id)
            , []
            )

        HandleFetchedIndividualEncounterParticipant id data ->
            ( { model | individualParticipants = Dict.insert id data model.individualParticipants }
            , Cmd.none
            , []
            )

        FetchIndividualEncounterParticipants ids ->
            if List.isEmpty ids then
                noChange

            else
                let
                    individualParticipantsUpdated =
                        List.foldl (\id accum -> Dict.insert id Loading accum) model.individualParticipants ids
                in
                ( { model | individualParticipants = individualParticipantsUpdated }
                , sw.getMany individualEncounterParticipantEndpoint ids
                    |> toCmd (RemoteData.fromResult >> RemoteData.map Dict.fromList >> HandleFetchedIndividualEncounterParticipants)
                , []
                )

        HandleFetchedIndividualEncounterParticipants webData ->
            case RemoteData.toMaybe webData of
                Nothing ->
                    noChange

                Just dict ->
                    let
                        dictUpdated =
                            Dict.map (\_ v -> RemoteData.Success v) dict
                    in
                    ( { model | individualParticipants = Dict.union dictUpdated model.individualParticipants }
                    , Cmd.none
                    , []
                    )

        FetchIndividualEncounterParticipantsForPerson id ->
            ( { model | individualParticipantsByPerson = Dict.insert id Loading model.individualParticipantsByPerson }
            , sw.select individualEncounterParticipantEndpoint [ id ]
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedIndividualEncounterParticipantsForPerson id)
            , []
            )

        HandleFetchedIndividualEncounterParticipantsForPerson id data ->
            ( { model | individualParticipantsByPerson = Dict.insert id data model.individualParticipantsByPerson }
            , Cmd.none
            , []
            )

        FetchIndividualEncounterParticipantsForPeople ids ->
            if List.isEmpty ids then
                noChange

            else
                let
                    individualParticipantsByPersonUpdated =
                        List.foldl (\id accum -> Dict.insert id Loading accum) model.individualParticipantsByPerson ids
                in
                ( { model | individualParticipantsByPerson = individualParticipantsByPersonUpdated }
                , sw.select individualEncounterParticipantEndpoint ids
                    |> toCmd
                        (RemoteData.fromResult
                            >> RemoteData.map
                                (.items
                                    >> List.foldl
                                        (\( participantId, participant ) accum ->
                                            let
                                                dictPeopleUpdated =
                                                    Dict.get participant.person accum
                                                        |> Maybe.map (Dict.insert participantId participant)
                                                        |> Maybe.withDefault (Dict.singleton participantId participant)
                                            in
                                            Dict.insert participant.person dictPeopleUpdated accum
                                        )
                                        Dict.empty
                                )
                            >> HandleFetchedIndividualEncounterParticipantsForPeople
                        )
                , []
                )

        HandleFetchedIndividualEncounterParticipantsForPeople webData ->
            case RemoteData.toMaybe webData of
                Nothing ->
                    noChange

                Just dict ->
                    let
                        dictUpdated =
                            Dict.map (\_ v -> RemoteData.Success v) dict
                    in
                    ( { model | individualParticipantsByPerson = Dict.union dictUpdated model.individualParticipantsByPerson }
                    , Cmd.none
                    , []
                    )

        FetchPrenatalEncountersForParticipant id ->
            ( { model | prenatalEncountersByParticipant = Dict.insert id Loading model.prenatalEncountersByParticipant }
            , sw.select prenatalEncounterEndpoint [ id ]
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedPrenatalEncountersForParticipant id)
            , []
            )

        HandleFetchedPrenatalEncountersForParticipant id data ->
            ( { model | prenatalEncountersByParticipant = Dict.insert id data model.prenatalEncountersByParticipant }
            , Cmd.none
            , []
            )

        FetchPrenatalEncountersForParticipants ids ->
            let
                prenatalEncountersByParticipantUpdated =
                    List.foldl (\id accum -> Dict.insert id Loading accum) model.prenatalEncountersByParticipant ids
            in
            ( { model | prenatalEncountersByParticipant = prenatalEncountersByParticipantUpdated }
            , sw.select prenatalEncounterEndpoint ids
                |> toCmd
                    (RemoteData.fromResult
                        >> RemoteData.map
                            (.items
                                >> List.foldl
                                    (\( encounterId, encounter ) accum ->
                                        let
                                            dictParticipantUpdated =
                                                Dict.get encounter.participant accum
                                                    |> Maybe.map (Dict.insert encounterId encounter)
                                                    |> Maybe.withDefault (Dict.singleton encounterId encounter)
                                        in
                                        Dict.insert encounter.participant dictParticipantUpdated accum
                                    )
                                    Dict.empty
                            )
                        >> HandleFetchedPrenatalEncountersForParticipants
                    )
            , []
            )

        HandleFetchedPrenatalEncountersForParticipants webData ->
            case RemoteData.toMaybe webData of
                Nothing ->
                    noChange

                Just dict ->
                    let
                        dictUpdated =
                            Dict.map (\_ v -> RemoteData.Success v) dict
                    in
                    ( { model | prenatalEncountersByParticipant = Dict.union dictUpdated model.prenatalEncountersByParticipant }
                    , Cmd.none
                    , []
                    )

        FetchNutritionEncountersForParticipant id ->
            ( { model | nutritionEncountersByParticipant = Dict.insert id Loading model.nutritionEncountersByParticipant }
            , sw.select nutritionEncounterEndpoint [ id ]
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedNutritionEncountersForParticipant id)
            , []
            )

        HandleFetchedNutritionEncountersForParticipant id data ->
            ( { model | nutritionEncountersByParticipant = Dict.insert id data model.nutritionEncountersByParticipant }
            , Cmd.none
            , []
            )

        FetchHomeVisitEncountersForParticipant id ->
            ( { model | homeVisitEncountersByParticipant = Dict.insert id Loading model.homeVisitEncountersByParticipant }
            , sw.select homeVisitEncounterEndpoint [ id ]
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedHomeVisitEncountersForParticipant id)
            , []
            )

        HandleFetchedHomeVisitEncountersForParticipant id data ->
            ( { model | homeVisitEncountersByParticipant = Dict.insert id data model.homeVisitEncountersByParticipant }
            , Cmd.none
            , []
            )

        FetchHomeVisitEncountersForParticipants ids ->
            let
                homeVisitEncountersByParticipantUpdated =
                    List.foldl (\id accum -> Dict.insert id Loading accum) model.homeVisitEncountersByParticipant ids
            in
            ( { model | homeVisitEncountersByParticipant = homeVisitEncountersByParticipantUpdated }
            , sw.select homeVisitEncounterEndpoint ids
                |> toCmd
                    (RemoteData.fromResult
                        >> RemoteData.map
                            (.items
                                >> List.foldl
                                    (\( encounterId, encounter ) accum ->
                                        let
                                            dictParticipantUpdated =
                                                Dict.get encounter.participant accum
                                                    |> Maybe.map (Dict.insert encounterId encounter)
                                                    |> Maybe.withDefault (Dict.singleton encounterId encounter)
                                        in
                                        Dict.insert encounter.participant dictParticipantUpdated accum
                                    )
                                    Dict.empty
                            )
                        >> HandleFetchedHomeVisitEncountersForParticipants
                    )
            , []
            )

        HandleFetchedHomeVisitEncountersForParticipants webData ->
            case RemoteData.toMaybe webData of
                Nothing ->
                    noChange

                Just dict ->
                    let
                        dictUpdated =
                            Dict.map (\_ v -> RemoteData.Success v) dict
                    in
                    ( { model | homeVisitEncountersByParticipant = Dict.union dictUpdated model.homeVisitEncountersByParticipant }
                    , Cmd.none
                    , []
                    )

        FetchWellChildEncountersForParticipant id ->
            ( { model | wellChildEncountersByParticipant = Dict.insert id Loading model.wellChildEncountersByParticipant }
            , sw.select wellChildEncounterEndpoint [ id ]
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedWellChildEncountersForParticipant id)
            , []
            )

        HandleFetchedWellChildEncountersForParticipant id data ->
            ( { model | wellChildEncountersByParticipant = Dict.insert id data model.wellChildEncountersByParticipant }
            , Cmd.none
            , []
            )

        FetchWellChildEncountersForParticipants ids ->
            let
                wellChildEncountersByParticipantUpdated =
                    List.foldl (\id accum -> Dict.insert id Loading accum) model.wellChildEncountersByParticipant ids
            in
            ( { model | wellChildEncountersByParticipant = wellChildEncountersByParticipantUpdated }
            , sw.select wellChildEncounterEndpoint ids
                |> toCmd
                    (RemoteData.fromResult
                        >> RemoteData.map
                            (.items
                                >> List.foldl
                                    (\( encounterId, encounter ) accum ->
                                        let
                                            dictParticipantUpdated =
                                                Dict.get encounter.participant accum
                                                    |> Maybe.map (Dict.insert encounterId encounter)
                                                    |> Maybe.withDefault (Dict.singleton encounterId encounter)
                                        in
                                        Dict.insert encounter.participant dictParticipantUpdated accum
                                    )
                                    Dict.empty
                            )
                        >> HandleFetchedWellChildEncountersForParticipants
                    )
            , []
            )

        HandleFetchedWellChildEncountersForParticipants webData ->
            case RemoteData.toMaybe webData of
                Nothing ->
                    noChange

                Just dict ->
                    let
                        dictUpdated =
                            Dict.map (\_ v -> RemoteData.Success v) dict
                    in
                    ( { model | wellChildEncountersByParticipant = Dict.union dictUpdated model.wellChildEncountersByParticipant }
                    , Cmd.none
                    , []
                    )

        FetchAcuteIllnessEncountersForParticipant id ->
            ( { model | acuteIllnessEncountersByParticipant = Dict.insert id Loading model.acuteIllnessEncountersByParticipant }
            , sw.select acuteIllnessEncounterEndpoint [ id ]
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedAcuteIllnessEncountersForParticipant id)
            , []
            )

        HandleFetchedAcuteIllnessEncountersForParticipant id data ->
            ( { model | acuteIllnessEncountersByParticipant = Dict.insert id data model.acuteIllnessEncountersByParticipant }
            , Cmd.none
            , []
            )

        FetchAcuteIllnessEncountersForParticipants ids ->
            let
                acuteIllnessEncountersByParticipantUpdated =
                    List.foldl (\id accum -> Dict.insert id Loading accum) model.acuteIllnessEncountersByParticipant ids
            in
            ( { model | acuteIllnessEncountersByParticipant = acuteIllnessEncountersByParticipantUpdated }
            , sw.select acuteIllnessEncounterEndpoint ids
                |> toCmd
                    (RemoteData.fromResult
                        >> RemoteData.map
                            (.items
                                >> List.foldl
                                    (\( encounterId, encounter ) accum ->
                                        let
                                            dictParticipantUpdated =
                                                Dict.get encounter.participant accum
                                                    |> Maybe.map (Dict.insert encounterId encounter)
                                                    |> Maybe.withDefault (Dict.singleton encounterId encounter)
                                        in
                                        Dict.insert encounter.participant dictParticipantUpdated accum
                                    )
                                    Dict.empty
                            )
                        >> HandleFetchedAcuteIllnessEncountersForParticipants
                    )
            , []
            )

        HandleFetchedAcuteIllnessEncountersForParticipants webData ->
            case RemoteData.toMaybe webData of
                Nothing ->
                    noChange

                Just dict ->
                    let
                        dictUpdated =
                            Dict.map (\_ v -> RemoteData.Success v) dict
                    in
                    ( { model | acuteIllnessEncountersByParticipant = Dict.union dictUpdated model.acuteIllnessEncountersByParticipant }
                    , Cmd.none
                    , []
                    )

        FetchNCDEncountersForParticipant id ->
            ( { model | ncdEncountersByParticipant = Dict.insert id Loading model.ncdEncountersByParticipant }
            , sw.select ncdEncounterEndpoint [ id ]
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedNCDEncountersForParticipant id)
            , []
            )

        HandleFetchedNCDEncountersForParticipant id data ->
            ( { model | ncdEncountersByParticipant = Dict.insert id data model.ncdEncountersByParticipant }
            , Cmd.none
            , []
            )

        FetchNCDEncountersForParticipants ids ->
            let
                ncdEncountersByParticipantUpdated =
                    List.foldl (\id accum -> Dict.insert id Loading accum) model.ncdEncountersByParticipant ids
            in
            ( { model | ncdEncountersByParticipant = ncdEncountersByParticipantUpdated }
            , sw.select ncdEncounterEndpoint ids
                |> toCmd
                    (RemoteData.fromResult
                        >> RemoteData.map
                            (.items
                                >> List.foldl
                                    (\( encounterId, encounter ) accum ->
                                        let
                                            dictParticipantUpdated =
                                                Dict.get encounter.participant accum
                                                    |> Maybe.map (Dict.insert encounterId encounter)
                                                    |> Maybe.withDefault (Dict.singleton encounterId encounter)
                                        in
                                        Dict.insert encounter.participant dictParticipantUpdated accum
                                    )
                                    Dict.empty
                            )
                        >> HandleFetchedNCDEncountersForParticipants
                    )
            , []
            )

        HandleFetchedNCDEncountersForParticipants webData ->
            case RemoteData.toMaybe webData of
                Nothing ->
                    noChange

                Just dict ->
                    let
                        dictUpdated =
                            Dict.map (\_ v -> RemoteData.Success v) dict
                    in
                    ( { model | ncdEncountersByParticipant = Dict.union dictUpdated model.ncdEncountersByParticipant }
                    , Cmd.none
                    , []
                    )

        FetchChildScoreboardEncountersForParticipant id ->
            ( { model | childScoreboardEncountersByParticipant = Dict.insert id Loading model.childScoreboardEncountersByParticipant }
            , sw.select childScoreboardEncounterEndpoint [ id ]
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedChildScoreboardEncountersForParticipant id)
            , []
            )

        HandleFetchedChildScoreboardEncountersForParticipant id data ->
            ( { model | childScoreboardEncountersByParticipant = Dict.insert id data model.childScoreboardEncountersByParticipant }
            , Cmd.none
            , []
            )

        FetchTuberculosisEncountersForParticipant id ->
            ( { model | tuberculosisEncountersByParticipant = Dict.insert id Loading model.tuberculosisEncountersByParticipant }
            , sw.select tuberculosisEncounterEndpoint [ id ]
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedTuberculosisEncountersForParticipant id)
            , []
            )

        HandleFetchedTuberculosisEncountersForParticipant id data ->
            ( { model | tuberculosisEncountersByParticipant = Dict.insert id data model.tuberculosisEncountersByParticipant }
            , Cmd.none
            , []
            )

        FetchTuberculosisEncountersForParticipants ids ->
            let
                tuberculosisEncountersByParticipantUpdated =
                    List.foldl (\id accum -> Dict.insert id Loading accum) model.tuberculosisEncountersByParticipant ids
            in
            ( { model | tuberculosisEncountersByParticipant = tuberculosisEncountersByParticipantUpdated }
            , sw.select tuberculosisEncounterEndpoint ids
                |> toCmd
                    (RemoteData.fromResult
                        >> RemoteData.map
                            (.items
                                >> List.foldl
                                    (\( encounterId, encounter ) accum ->
                                        let
                                            dictParticipantUpdated =
                                                Dict.get encounter.participant accum
                                                    |> Maybe.map (Dict.insert encounterId encounter)
                                                    |> Maybe.withDefault (Dict.singleton encounterId encounter)
                                        in
                                        Dict.insert encounter.participant dictParticipantUpdated accum
                                    )
                                    Dict.empty
                            )
                        >> HandleFetchedTuberculosisEncountersForParticipants
                    )
            , []
            )

        HandleFetchedTuberculosisEncountersForParticipants webData ->
            case RemoteData.toMaybe webData of
                Nothing ->
                    noChange

                Just dict ->
                    let
                        dictUpdated =
                            Dict.map (\_ v -> RemoteData.Success v) dict
                    in
                    ( { model | tuberculosisEncountersByParticipant = Dict.union dictUpdated model.tuberculosisEncountersByParticipant }
                    , Cmd.none
                    , []
                    )

        FetchHIVEncountersForParticipant id ->
            ( { model | hivEncountersByParticipant = Dict.insert id Loading model.hivEncountersByParticipant }
            , sw.select hivEncounterEndpoint [ id ]
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedHIVEncountersForParticipant id)
            , []
            )

        HandleFetchedHIVEncountersForParticipant id data ->
            ( { model | hivEncountersByParticipant = Dict.insert id data model.hivEncountersByParticipant }
            , Cmd.none
            , []
            )

        FetchHIVEncountersForParticipants ids ->
            let
                hivEncountersByParticipantUpdated =
                    List.foldl (\id accum -> Dict.insert id Loading accum) model.hivEncountersByParticipant ids
            in
            ( { model | hivEncountersByParticipant = hivEncountersByParticipantUpdated }
            , sw.select hivEncounterEndpoint ids
                |> toCmd
                    (RemoteData.fromResult
                        >> RemoteData.map
                            (.items
                                >> List.foldl
                                    (\( encounterId, encounter ) accum ->
                                        let
                                            dictParticipantUpdated =
                                                Dict.get encounter.participant accum
                                                    |> Maybe.map (Dict.insert encounterId encounter)
                                                    |> Maybe.withDefault (Dict.singleton encounterId encounter)
                                        in
                                        Dict.insert encounter.participant dictParticipantUpdated accum
                                    )
                                    Dict.empty
                            )
                        >> HandleFetchedHIVEncountersForParticipants
                    )
            , []
            )

        HandleFetchedHIVEncountersForParticipants webData ->
            case RemoteData.toMaybe webData of
                Nothing ->
                    noChange

                Just dict ->
                    let
                        dictUpdated =
                            Dict.map (\_ v -> RemoteData.Success v) dict
                    in
                    ( { model | hivEncountersByParticipant = Dict.union dictUpdated model.hivEncountersByParticipant }
                    , Cmd.none
                    , []
                    )

        FetchPrenatalMeasurements id ->
            ( { model | prenatalMeasurements = Dict.insert id Loading model.prenatalMeasurements }
            , sw.get prenatalMeasurementsEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedPrenatalMeasurements id)
            , []
            )

        HandleFetchedPrenatalMeasurements id data ->
            ( { model | prenatalMeasurements = Dict.insert id data model.prenatalMeasurements }
            , Cmd.none
            , []
            )

        FetchNutritionMeasurements id ->
            ( { model | nutritionMeasurements = Dict.insert id Loading model.nutritionMeasurements }
            , sw.get nutritionMeasurementsEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedNutritionMeasurements id)
            , []
            )

        HandleFetchedNutritionMeasurements id data ->
            ( { model | nutritionMeasurements = Dict.insert id data model.nutritionMeasurements }
            , Cmd.none
            , []
            )

        FetchAcuteIllnessMeasurements id ->
            ( { model | acuteIllnessMeasurements = Dict.insert id Loading model.acuteIllnessMeasurements }
            , sw.get acuteIllnessMeasurementsEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedAcuteIllnessMeasurements id)
            , []
            )

        HandleFetchedAcuteIllnessMeasurements id data ->
            ( { model | acuteIllnessMeasurements = Dict.insert id data model.acuteIllnessMeasurements }
            , Cmd.none
            , []
            )

        FetchFollowUpMeasurements id ->
            ( { model | followUpMeasurements = Dict.insert id Loading model.followUpMeasurements }
            , sw.get followUpMeasurementsEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedFollowUpMeasurements id)
            , []
            )

        HandleFetchedFollowUpMeasurements id data ->
            ( { model | followUpMeasurements = Dict.insert id data model.followUpMeasurements }
            , Cmd.none
            , []
            )

        FetchStockManagementMeasurements id ->
            ( { model | stockManagementMeasurements = Dict.insert id Loading model.stockManagementMeasurements }
            , sw.get stockManagementMeasurementsEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedStockManagementMeasurements id)
            , []
            )

        HandleFetchedStockManagementMeasurements id data ->
            ( { model | stockManagementMeasurements = Dict.insert id data model.stockManagementMeasurements }
            , Cmd.none
            , []
            )

        FetchStockManagementData id ->
            let
                updatedModel =
                    case Dict.get id model.stockManagementData of
                        Just (Success _) ->
                            -- Data already calculated, and there's no need to recalculate.
                            model

                        _ ->
                            let
                                data =
                                    Dict.get id model.stockManagementMeasurements
                                        |> Maybe.andThen RemoteData.toMaybe
                                        |> Maybe.map (generateStockManagementData currentDate >> Success)
                                        |> Maybe.withDefault NotAsked
                            in
                            { model | stockManagementData = Dict.insert id data model.stockManagementData }
            in
            ( updatedModel
            , Cmd.none
            , []
            )

        MarkForRecalculationStockManagementData id ->
            ( { model | stockManagementData = Dict.insert id NotAsked model.stockManagementData }
            , Cmd.none
            , []
            )

        FetchHomeVisitMeasurements id ->
            ( { model | homeVisitMeasurements = Dict.insert id Loading model.homeVisitMeasurements }
            , sw.get homeVisitMeasurementsEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedHomeVisitMeasurements id)
            , []
            )

        HandleFetchedHomeVisitMeasurements id data ->
            ( { model | homeVisitMeasurements = Dict.insert id data model.homeVisitMeasurements }
            , Cmd.none
            , []
            )

        FetchWellChildMeasurements id ->
            ( { model | wellChildMeasurements = Dict.insert id Loading model.wellChildMeasurements }
            , sw.get wellChildMeasurementsEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedWellChildMeasurements id)
            , []
            )

        HandleFetchedWellChildMeasurements id data ->
            ( { model | wellChildMeasurements = Dict.insert id data model.wellChildMeasurements }
            , Cmd.none
            , []
            )

        FetchNCDMeasurements id ->
            ( { model | ncdMeasurements = Dict.insert id Loading model.ncdMeasurements }
            , sw.get ncdMeasurementsEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedNCDMeasurements id)
            , []
            )

        HandleFetchedNCDMeasurements id data ->
            ( { model | ncdMeasurements = Dict.insert id data model.ncdMeasurements }
            , Cmd.none
            , []
            )

        FetchChildScoreboardMeasurements id ->
            ( { model | childScoreboardMeasurements = Dict.insert id Loading model.childScoreboardMeasurements }
            , sw.get childScoreboardMeasurementsEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedChildScoreboardMeasurements id)
            , []
            )

        HandleFetchedChildScoreboardMeasurements id data ->
            ( { model | childScoreboardMeasurements = Dict.insert id data model.childScoreboardMeasurements }
            , Cmd.none
            , []
            )

        FetchTuberculosisMeasurements id ->
            ( { model | tuberculosisMeasurements = Dict.insert id Loading model.tuberculosisMeasurements }
            , sw.get tuberculosisMeasurementsEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedTuberculosisMeasurements id)
            , []
            )

        HandleFetchedTuberculosisMeasurements id data ->
            ( { model | tuberculosisMeasurements = Dict.insert id data model.tuberculosisMeasurements }
            , Cmd.none
            , []
            )

        FetchHIVMeasurements id ->
            ( { model | hivMeasurements = Dict.insert id Loading model.hivMeasurements }
            , sw.get hivMeasurementsEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedHIVMeasurements id)
            , []
            )

        HandleFetchedHIVMeasurements id data ->
            ( { model | hivMeasurements = Dict.insert id data model.hivMeasurements }
            , Cmd.none
            , []
            )

        FetchFollowUpParticipants ids ->
            if List.isEmpty ids then
                noChange

            else
                let
                    peopleUpdated =
                        List.foldl (\id accum -> Dict.insert id Loading accum) model.people ids
                in
                ( { model | people = peopleUpdated }
                , sw.getMany personEndpoint ids
                    |> toCmd (RemoteData.fromResult >> RemoteData.map Dict.fromList >> HandleFetchFollowUpParticipants)
                , []
                )

        HandleFetchFollowUpParticipants webData ->
            case RemoteData.toMaybe webData of
                Nothing ->
                    noChange

                Just dict ->
                    let
                        dictUpdated =
                            Dict.map (\_ v -> RemoteData.Success v) dict
                    in
                    ( { model | people = Dict.union dictUpdated model.people }
                    , Cmd.none
                    , []
                    )

        FetchParticipantsForPerson personId ->
            let
                query1 =
                    sw.select pmtctParticipantEndpoint (ParticipantsForChild personId)
                        |> toTask
                        |> Task.map (.items >> Dict.fromList)
                        |> RemoteData.fromTask

                query2 =
                    sw.select pmtctParticipantEndpoint (ParticipantsForAdult personId)
                        |> toTask
                        |> Task.map (.items >> Dict.fromList)
                        |> RemoteData.fromTask
            in
            ( { model | participantsByPerson = Dict.insert personId Loading model.participantsByPerson }
            , Task.map2 (RemoteData.map2 Dict.union) query1 query2
                |> Task.perform (HandleFetchedParticipantsForPerson personId)
            , []
            )

        HandleFetchedParticipantsForPerson personId data ->
            ( { model | participantsByPerson = Dict.insert personId data model.participantsByPerson }
            , Cmd.none
            , []
            )

        FetchRelationshipsForPerson personId ->
            let
                -- We run two queries, one for the `person` field, and one for
                -- `related_to` One could do this as an OR in the service
                -- worker instead, but it would basically run two queries
                -- anyway, so it's no more efficient.
                query1 =
                    sw.select relationshipEndpoint { person = Just personId, relatedTo = Nothing }
                        |> toTask
                        |> Task.map
                            (\val ->
                                val.items
                                    |> List.filterMap
                                        (\( id, relationship ) ->
                                            toMyRelationship personId relationship
                                                |> Maybe.map (\myRelationship -> ( id, myRelationship ))
                                        )
                                    |> Dict.fromList
                            )
                        |> RemoteData.fromTask

                query2 =
                    sw.select relationshipEndpoint { person = Nothing, relatedTo = Just personId }
                        |> toTask
                        |> Task.map
                            (\val ->
                                val.items
                                    |> List.filterMap
                                        (\( id, relationship ) ->
                                            toMyRelationship personId relationship
                                                |> Maybe.map (\myRelationship -> ( id, myRelationship ))
                                        )
                                    |> Dict.fromList
                            )
                        |> RemoteData.fromTask
            in
            ( { model | relationshipsByPerson = Dict.insert personId Loading model.relationshipsByPerson }
            , Task.map2 (RemoteData.map2 Dict.union) query1 query2
                |> Task.perform (HandleFetchedRelationshipsForPerson personId)
            , []
            )

        HandleFetchedRelationshipsForPerson personId data ->
            ( { model | relationshipsByPerson = Dict.insert personId data model.relationshipsByPerson }
            , Cmd.none
            , []
            )

        FetchExpectedSessions childId ->
            ( { model | expectedSessions = Dict.insert childId Loading model.expectedSessions }
            , sw.select sessionEndpoint (ForChild childId)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedExpectedSessions childId)
            , []
            )

        HandleFetchedExpectedSessions childId data ->
            ( { model | expectedSessions = Dict.insert childId data model.expectedSessions }
            , Cmd.none
            , []
            )

        FetchHealthCenters ->
            ( { model | healthCenters = Loading }
            , sw.select healthCenterEndpoint ()
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedHealthCenters)
            , []
            )

        HandleFetchedHealthCenters data ->
            ( { model | healthCenters = data }
            , Cmd.none
            , []
            )

        FetchMotherMeasurements motherId ->
            ( { model | motherMeasurements = Dict.insert motherId Loading model.motherMeasurements }
            , sw.get motherMeasurementListEndpoint motherId
                |> toCmd (RemoteData.fromResult >> HandleFetchedMotherMeasurements motherId)
            , []
            )

        FetchMothersMeasurements ids ->
            if List.isEmpty ids then
                noChange

            else
                let
                    motherMeasurements =
                        List.foldl (\id accum -> Dict.insert id Loading accum) model.motherMeasurements ids
                in
                ( { model | motherMeasurements = motherMeasurements }
                , sw.getMany motherMeasurementListEndpoint ids
                    |> toCmd (RemoteData.fromResult >> RemoteData.map Dict.fromList >> HandleFetchedMothersMeasurements)
                , []
                )

        HandleFetchedMotherMeasurements motherId data ->
            ( { model | motherMeasurements = Dict.insert motherId data model.motherMeasurements }
            , Cmd.none
            , []
            )

        HandleFetchedMothersMeasurements webData ->
            case RemoteData.toMaybe webData of
                Nothing ->
                    noChange

                Just dict ->
                    let
                        dictUpdated =
                            Dict.map (\_ v -> RemoteData.Success v) dict
                    in
                    ( { model | motherMeasurements = Dict.union dictUpdated model.motherMeasurements }
                    , Cmd.none
                    , []
                    )

        FetchParticipantForms ->
            ( { model | participantForms = Loading }
            , sw.select participantFormEndpoint ()
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedParticipantForms)
            , []
            )

        HandleFetchedParticipantForms data ->
            ( { model | participantForms = data }
            , Cmd.none
            , []
            )

        FetchPeople ids ->
            if List.isEmpty ids then
                noChange

            else
                let
                    peopleUpdated =
                        List.foldl (\id accum -> Dict.insert id Loading accum) model.people ids
                in
                ( { model | people = peopleUpdated }
                , sw.getMany personEndpoint ids
                    |> toCmd (RemoteData.fromResult >> RemoteData.map Dict.fromList >> HandleFetchedPeople)
                , []
                )

        HandleFetchedPeople webData ->
            case RemoteData.toMaybe webData of
                Nothing ->
                    noChange

                Just dict ->
                    let
                        dictUpdated =
                            Dict.map (\_ v -> RemoteData.Success v) dict
                    in
                    ( { model | people = Dict.union dictUpdated model.people }
                    , Cmd.none
                    , []
                    )

        FetchPeopleByName name ->
            let
                trimmed =
                    String.trim name
            in
            -- We'll limit the search to 500 each for now ... basically,
            -- just to avoid truly pathological cases.
            ( { model | personSearchesByName = Dict.insert trimmed Loading model.personSearchesByName }
            , sw.selectRange personEndpoint (ParamsNameContains trimmed) 0 (Just 500)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedPeopleByName trimmed)
            , []
            )

        HandleFetchedPeopleByName trimmed data ->
            ( { model | personSearchesByName = Dict.insert trimmed data model.personSearchesByName }
            , Cmd.none
            , []
            )

        FetchPeopleByNationalId nationalId ->
            let
                trimmed =
                    String.trim nationalId
            in
            -- We'll limit the search to 500 each for now ... basically,
            -- just to avoid truly pathological cases.
            ( { model | personSearchesByNationalId = Dict.insert trimmed Loading model.personSearchesByNationalId }
            , sw.selectRange personEndpoint (ParamsNationalIdContains trimmed) 0 (Just 500)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedPeopleByNationalId trimmed)
            , []
            )

        HandleFetchedPeopleByNationalId trimmed data ->
            ( { model | personSearchesByNationalId = Dict.insert trimmed data model.personSearchesByNationalId }
            , Cmd.none
            , []
            )

        FetchPeopleInVillage id ->
            getVillageById model id
                |> Maybe.map
                    (\village ->
                        let
                            geoFields =
                                String.join "|"
                                    [ village.province
                                    , village.district
                                    , village.sector
                                    , village.cell
                                    , village.village
                                    ]
                        in
                        ( { model | peopleInVillage = Dict.insert id Loading model.peopleInVillage }
                        , sw.selectRange personEndpoint (ParamsGeoFields geoFields) 0 (Just 5000)
                            |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedPeopleInVillage id)
                        , []
                        )
                    )
                |> Maybe.withDefault noChange

        HandleFetchedPeopleInVillage id data ->
            ( { model | peopleInVillage = Dict.insert id data model.peopleInVillage }
            , Cmd.none
            , []
            )
                |> sequenceExtra
                    (updateIndexedDb language
                        currentDate
                        currentTime
                        coordinates
                        zscores
                        site
                        features
                        nurseId
                        healthCenterId
                        villageId
                        isChw
                        isLabTech
                        activePage
                        syncManager
                    )
                    [ HandleFetchedPeople data ]

        FetchPerson id ->
            ( { model | people = Dict.insert id Loading model.people }
            , sw.get personEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedPerson id)
            , []
            )

        HandleFetchedPerson id data ->
            ( { model | people = Dict.insert id data model.people }
            , Cmd.none
            , []
            )

        FetchPrenatalEncounter id ->
            ( { model | prenatalEncounters = Dict.insert id Loading model.prenatalEncounters }
            , sw.get prenatalEncounterEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedPrenatalEncounter id)
            , []
            )

        HandleFetchedPrenatalEncounter id data ->
            ( { model | prenatalEncounters = Dict.insert id data model.prenatalEncounters }
            , Cmd.none
            , []
            )

        FetchPrenatalEncounters ids ->
            if List.isEmpty ids then
                noChange

            else
                let
                    prenatalEncountersUpdated =
                        List.foldl (\id accum -> Dict.insert id Loading accum) model.prenatalEncounters ids
                in
                ( { model | prenatalEncounters = prenatalEncountersUpdated }
                , sw.getMany prenatalEncounterEndpoint ids
                    |> toCmd (RemoteData.fromResult >> RemoteData.map Dict.fromList >> HandleFetchedPrenatalEncounters)
                , []
                )

        HandleFetchedPrenatalEncounters webData ->
            case RemoteData.toMaybe webData of
                Nothing ->
                    noChange

                Just dict ->
                    let
                        dictUpdated =
                            Dict.map (\_ v -> RemoteData.Success v) dict
                    in
                    ( { model | prenatalEncounters = Dict.union dictUpdated model.prenatalEncounters }
                    , Cmd.none
                    , []
                    )

        FetchNutritionEncounter id ->
            ( { model | nutritionEncounters = Dict.insert id Loading model.nutritionEncounters }
            , sw.get nutritionEncounterEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedNutritionEncounter id)
            , []
            )

        HandleFetchedNutritionEncounter id data ->
            ( { model | nutritionEncounters = Dict.insert id data model.nutritionEncounters }
            , Cmd.none
            , []
            )

        FetchHomeVisitEncounter id ->
            ( { model | homeVisitEncounters = Dict.insert id Loading model.homeVisitEncounters }
            , sw.get homeVisitEncounterEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedHomeVisitEncounter id)
            , []
            )

        HandleFetchedHomeVisitEncounter id data ->
            ( { model | homeVisitEncounters = Dict.insert id data model.homeVisitEncounters }
            , Cmd.none
            , []
            )

        FetchWellChildEncounter id ->
            ( { model | wellChildEncounters = Dict.insert id Loading model.wellChildEncounters }
            , sw.get wellChildEncounterEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedWellChildEncounter id)
            , []
            )

        HandleFetchedWellChildEncounter id data ->
            ( { model | wellChildEncounters = Dict.insert id data model.wellChildEncounters }
            , Cmd.none
            , []
            )

        FetchAcuteIllnessEncounter id ->
            ( { model | acuteIllnessEncounters = Dict.insert id Loading model.acuteIllnessEncounters }
            , sw.get acuteIllnessEncounterEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedAcuteIllnessEncounter id)
            , []
            )

        HandleFetchedAcuteIllnessEncounter id data ->
            ( { model | acuteIllnessEncounters = Dict.insert id data model.acuteIllnessEncounters }
            , Cmd.none
            , []
            )

        FetchAcuteIllnessEncounters ids ->
            if List.isEmpty ids then
                noChange

            else
                let
                    acuteIllnessEncountersUpdated =
                        List.foldl (\id accum -> Dict.insert id Loading accum) model.acuteIllnessEncounters ids
                in
                ( { model | acuteIllnessEncounters = acuteIllnessEncountersUpdated }
                , sw.getMany acuteIllnessEncounterEndpoint ids
                    |> toCmd (RemoteData.fromResult >> RemoteData.map Dict.fromList >> HandleFetchedAcuteIllnessEncounters)
                , []
                )

        HandleFetchedAcuteIllnessEncounters webData ->
            case RemoteData.toMaybe webData of
                Nothing ->
                    noChange

                Just dict ->
                    let
                        dictUpdated =
                            Dict.map (\_ v -> RemoteData.Success v) dict
                    in
                    ( { model | acuteIllnessEncounters = Dict.union dictUpdated model.acuteIllnessEncounters }
                    , Cmd.none
                    , []
                    )

        FetchNCDEncounter id ->
            ( { model | ncdEncounters = Dict.insert id Loading model.ncdEncounters }
            , sw.get ncdEncounterEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedNCDEncounter id)
            , []
            )

        HandleFetchedNCDEncounter id data ->
            ( { model | ncdEncounters = Dict.insert id data model.ncdEncounters }
            , Cmd.none
            , []
            )

        FetchChildScoreboardEncounter id ->
            ( { model | childScoreboardEncounters = Dict.insert id Loading model.childScoreboardEncounters }
            , sw.get childScoreboardEncounterEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedChildScoreboardEncounter id)
            , []
            )

        HandleFetchedChildScoreboardEncounter id data ->
            ( { model | childScoreboardEncounters = Dict.insert id data model.childScoreboardEncounters }
            , Cmd.none
            , []
            )

        FetchTuberculosisEncounter id ->
            ( { model | tuberculosisEncounters = Dict.insert id Loading model.tuberculosisEncounters }
            , sw.get tuberculosisEncounterEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedTuberculosisEncounter id)
            , []
            )

        HandleFetchedTuberculosisEncounter id data ->
            ( { model | tuberculosisEncounters = Dict.insert id data model.tuberculosisEncounters }
            , Cmd.none
            , []
            )

        FetchTuberculosisEncounters ids ->
            if List.isEmpty ids then
                noChange

            else
                let
                    tuberculosisEncountersUpdated =
                        List.foldl (\id accum -> Dict.insert id Loading accum) model.tuberculosisEncounters ids
                in
                ( { model | tuberculosisEncounters = tuberculosisEncountersUpdated }
                , sw.getMany tuberculosisEncounterEndpoint ids
                    |> toCmd (RemoteData.fromResult >> RemoteData.map Dict.fromList >> HandleFetchedTuberculosisEncounters)
                , []
                )

        HandleFetchedTuberculosisEncounters webData ->
            case RemoteData.toMaybe webData of
                Nothing ->
                    noChange

                Just dict ->
                    let
                        dictUpdated =
                            Dict.map (\_ v -> RemoteData.Success v) dict
                    in
                    ( { model | tuberculosisEncounters = Dict.union dictUpdated model.tuberculosisEncounters }
                    , Cmd.none
                    , []
                    )

        FetchHIVEncounter id ->
            ( { model | hivEncounters = Dict.insert id Loading model.hivEncounters }
            , sw.get hivEncounterEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedHIVEncounter id)
            , []
            )

        HandleFetchedHIVEncounter id data ->
            ( { model | hivEncounters = Dict.insert id data model.hivEncounters }
            , Cmd.none
            , []
            )

        FetchHIVEncounters ids ->
            if List.isEmpty ids then
                noChange

            else
                let
                    hivEncountersUpdated =
                        List.foldl (\id accum -> Dict.insert id Loading accum) model.hivEncounters ids
                in
                ( { model | hivEncounters = hivEncountersUpdated }
                , sw.getMany hivEncounterEndpoint ids
                    |> toCmd (RemoteData.fromResult >> RemoteData.map Dict.fromList >> HandleFetchedHIVEncounters)
                , []
                )

        HandleFetchedHIVEncounters webData ->
            case RemoteData.toMaybe webData of
                Nothing ->
                    noChange

                Just dict ->
                    let
                        dictUpdated =
                            Dict.map (\_ v -> RemoteData.Success v) dict
                    in
                    ( { model | hivEncounters = Dict.union dictUpdated model.hivEncounters }
                    , Cmd.none
                    , []
                    )

        FetchEducationSession id ->
            ( { model | educationSessions = Dict.insert id Loading model.educationSessions }
            , sw.get educationSessionEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedEducationSession id)
            , []
            )

        HandleFetchedEducationSession id data ->
            ( { model | educationSessions = Dict.insert id data model.educationSessions }
            , Cmd.none
            , []
            )

        FetchEducationSessionsForPerson id ->
            ( { model | educationSessionsByPerson = Dict.insert id Loading model.educationSessionsByPerson }
            , sw.select educationSessionEndpoint (Just id)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedEducationSessionsForPerson id)
            , []
            )

        HandleFetchedEducationSessionsForPerson id data ->
            ( { model | educationSessionsByPerson = Dict.insert id data model.educationSessionsByPerson }
            , Cmd.none
            , []
            )

        FetchSession sessionId ->
            ( { model | sessions = Dict.insert sessionId Loading model.sessions }
            , sw.get sessionEndpoint sessionId
                |> toCmd (RemoteData.fromResult >> HandleFetchedSession sessionId)
            , []
            )

        HandleFetchedSession sessionId data ->
            ( { model | sessions = Dict.insert sessionId data model.sessions }
            , Cmd.none
            , []
            )

        FetchSessionsByClinic clinicId ->
            ( { model | sessionsByClinic = Dict.insert clinicId Loading model.sessionsByClinic }
            , sw.select sessionEndpoint (ForClinic clinicId)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedSessionsByClinic clinicId)
            , []
            )

        HandleFetchedSessionsByClinic clinicId data ->
            ( { model | sessionsByClinic = Dict.insert clinicId data model.sessionsByClinic }
            , Cmd.none
            , []
            )

        FetchResilienceSurveysForNurse id ->
            ( { model | resilienceSurveysByNurse = Dict.insert id Loading model.resilienceSurveysByNurse }
            , sw.select resilienceSurveyEndpoint (Just id)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedResilienceSurveysForNurse id)
            , []
            )

        HandleFetchedResilienceSurveysForNurse id data ->
            ( { model | resilienceSurveysByNurse = Dict.insert id data model.resilienceSurveysByNurse }
            , Cmd.none
            , []
            )

        FetchVillages ->
            ( { model | villages = Loading }
            , sw.select villageEndpoint ()
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedVillages)
            , []
            )

        HandleFetchedVillages data ->
            ( { model | villages = data }
            , Cmd.none
            , []
            )

        FetchTraceContact id ->
            ( { model | traceContacts = Dict.insert id Loading model.traceContacts }
            , sw.get acuteIllnessTraceContactEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedTraceContact id)
            , []
            )

        HandleFetchedTraceContact id data ->
            ( { model | traceContacts = Dict.insert id data model.traceContacts }
            , Cmd.none
            , []
            )

        FetchPregnancyByNewborn id ->
            ( { model | pregnancyByNewborn = Dict.insert id Loading model.pregnancyByNewborn }
            , sw.get pregnancyByNewbornEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedPregnancyByNewborn id)
            , []
            )

        HandleFetchedPregnancyByNewborn id data ->
            ( { model | pregnancyByNewborn = Dict.insert id data model.pregnancyByNewborn }
            , Cmd.none
            , []
            )

        HandleRevisions revisions ->
            let
                downloadingContent =
                    case syncManager.syncStatus of
                        SyncManager.Model.SyncDownloadGeneral _ ->
                            True

                        SyncManager.Model.SyncDownloadAuthority _ ->
                            True

                        _ ->
                            False

                processRevisionAndDiagnoseAcuteIllness participantId encounterId =
                    let
                        person =
                            Dict.get participantId model.people
                                |> Maybe.withDefault NotAsked
                                |> RemoteData.toMaybe

                        ( newModel, _ ) =
                            List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                        extraMsgs =
                            Maybe.map2 (generateSuspectedDiagnosisMsgs currentDate features isChw model newModel)
                                encounterId
                                person
                                |> Maybe.withDefault []
                    in
                    ( newModel, extraMsgs )

                processRevisionAndAssessNutritionIndividual participantId encounterId =
                    let
                        ( newModel, _ ) =
                            List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                        extraMsgs =
                            Maybe.map (generateNutritionAssessmentIndividualMsgs currentDate zscores features isChw model newModel)
                                encounterId
                                |> Maybe.withDefault []
                    in
                    ( newModel, extraMsgs )

                processRevisionAndAssessNutritionGroup participantId sessionId updateFunc =
                    let
                        ( newModel, _ ) =
                            List.foldl (handleRevision currentDate healthCenterId villageId) ( model, True ) revisions
                    in
                    Maybe.map
                        (\sessionId_ ->
                            let
                                editableSessions =
                                    -- The `andThen` is so that we only recalculate
                                    -- the editable session if we already have a
                                    -- success.
                                    Dict.map
                                        (\id session ->
                                            RemoteData.andThen (\_ -> makeEditableSession id newModel) session
                                        )
                                        newModel.editableSessions

                                withRecalc =
                                    { newModel | editableSessions = editableSessions }

                                extraMsgs =
                                    -- Important: we pass model here, because we want to be examining the state before
                                    -- current editable session was set for recalculation with makeEditableSession.
                                    -- The reason for this is that at makeEditableSession, all measuerements are set to
                                    -- be refetched, and we are not able to determine if mandatory activities are completed
                                    -- or not.
                                    -- Therefore, we will be examining the 'before' state, taking into consideration
                                    -- that triggering activity is completed.
                                    generateNutritionAssessmentGroupMsgs currentDate
                                        zscores
                                        features
                                        isChw
                                        participantId
                                        sessionId_
                                        activePage
                                        updateFunc
                                        newModel
                            in
                            ( withRecalc, extraMsgs )
                        )
                        sessionId
                        |> Maybe.withDefault ( newModel, [] )

                processRevisionAndAssessWellChild participantId encounterId =
                    let
                        ( newModel, _ ) =
                            List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                        extraMsgs =
                            Maybe.map (generateNutritionAssessmentWellChildlMsgs currentDate zscores site isChw model newModel)
                                encounterId
                                |> Maybe.withDefault []
                    in
                    ( newModel, extraMsgs )

                processRevisionAndAssessPrenatal participantId encounterId updateAssesment =
                    processRevisionAndAssessPrenatalWithReportToOrigin participantId encounterId updateAssesment Nothing

                processRevisionAndAssessPrenatalWithReportToOrigin participantId encounterId updateAssesment originData =
                    let
                        ( newModel, _ ) =
                            List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                        extraMsgs =
                            Maybe.map (generatePrenatalAssessmentMsgs currentDate language site isChw isLabTech activePage updateAssesment originData newModel)
                                encounterId
                                |> Maybe.withDefault []
                    in
                    ( newModel, extraMsgs )

                processRevisionAndAssessNCD participantId encounterId =
                    let
                        ( newModel, _ ) =
                            List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                        extraMsgs =
                            Maybe.map (generateNCDAssessmentMsgs currentDate language activePage newModel)
                                encounterId
                                |> Maybe.withDefault []
                    in
                    ( newModel, extraMsgs )

                processRevisionAndUpdatePrenatalLabsResults participantId encounterId test executionNote resultsAdded testPrerequisites =
                    let
                        ( newModel, _ ) =
                            List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                        extraMsgs =
                            Maybe.map
                                (\encounterId_ ->
                                    let
                                        labsResultsMsgs =
                                            if
                                                resultsAdded
                                                    || -- If user is lab tech, we know for sure
                                                       -- the update is performed from recurrent phase of encounter,
                                                       -- even if actual test results are not set (which can happen
                                                       -- if lab tech indicates that test was not run).
                                                       isLabTech
                                            then
                                                generatePrenatalLabsResultsAddedMsgs currentDate isLabTech newModel test testPrerequisites encounterId_

                                            else
                                                generatePrenatalLabsTestAddedMsgs currentDate newModel test executionNote encounterId_

                                        possibleEndEncounterMsgs =
                                            if atPrenatalRecurrentPhase activePage then
                                                generatePrenatalRecurrentPhaseCompletedMsgs currentDate isLabTech newModel encounterId_

                                            else if atPrenatalInitialPhase activePage then
                                                generatePrenatalInitialPhaseCompletedMsgs currentDate site newModel encounterId_

                                            else
                                                []
                                    in
                                    labsResultsMsgs ++ possibleEndEncounterMsgs
                                )
                                encounterId
                                |> Maybe.withDefault []
                    in
                    ( newModel, extraMsgs )

                processVitalsRevisionAndUpdatePrenatalLabsResults participantId encounterId value =
                    let
                        ( newModel, _ ) =
                            List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                        extraMsgs =
                            Maybe.map2
                                (\dia sys ->
                                    Maybe.map
                                        (\encounterId_ ->
                                            let
                                                resultsAdded =
                                                    isJust value.diaRepeated

                                                labsResultsMsgs =
                                                    if resultsAdded then
                                                        generatePrenatalLabsResultsAddedMsgs
                                                            currentDate
                                                            isLabTech
                                                            newModel
                                                            Backend.Measurement.Model.TestVitalsRecheck
                                                            Nothing
                                                            encounterId_

                                                    else
                                                        let
                                                            executionNote =
                                                                if Pages.Prenatal.Activity.Utils.highBloodPressureCondition dia sys then
                                                                    -- When we have diagnosed Hypertension, we'll try to unschedule
                                                                    -- vitals recheck (as we do not know if it was scheduled before).
                                                                    -- generatePrenatalLabsTestAddedMsgs will unschedule only if needed.
                                                                    Backend.Measurement.Model.TestNoteNotIndicated

                                                                else
                                                                -- When we suspect hypertension, we'll try to schedule vitals recheck.
                                                                -- generatePrenatalLabsTestAddedMsgs will schedule only if needed.
                                                                if
                                                                    Pages.Prenatal.Utils.marginalBloodPressureCondition dia sys
                                                                then
                                                                    Backend.Measurement.Model.TestNoteRunToday

                                                                else
                                                                    -- Otherwise, we try to unschedule vitals recheck.
                                                                    -- generatePrenatalLabsTestAddedMsgs will unschedule only if needed.
                                                                    Backend.Measurement.Model.TestNoteNotIndicated
                                                        in
                                                        generatePrenatalLabsTestAddedMsgs
                                                            currentDate
                                                            newModel
                                                            Backend.Measurement.Model.TestVitalsRecheck
                                                            executionNote
                                                            encounterId_

                                                possibleEndEncounterMsgs =
                                                    if atPrenatalRecurrentPhase activePage then
                                                        generatePrenatalRecurrentPhaseCompletedMsgs currentDate isLabTech newModel encounterId_

                                                    else if atPrenatalInitialPhase activePage then
                                                        generatePrenatalInitialPhaseCompletedMsgs currentDate site newModel encounterId_

                                                    else
                                                        []
                                            in
                                            labsResultsMsgs ++ possibleEndEncounterMsgs
                                        )
                                        encounterId
                                        |> Maybe.withDefault []
                                )
                                value.dia
                                value.sys
                                |> Maybe.withDefault []
                    in
                    ( newModel, extraMsgs )

                processPrenatalRevisionPossiblyCompletingRecurrentPhase participantId encounterId =
                    let
                        ( newModel, _ ) =
                            List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions
                    in
                    if not <| atPrenatalRecurrentPhase activePage then
                        ( newModel, [] )

                    else
                        let
                            extraMsgs =
                                Maybe.map
                                    (\encounterId_ ->
                                        generatePrenatalRecurrentPhaseCompletedMsgs currentDate isLabTech newModel encounterId_
                                    )
                                    encounterId
                                    |> Maybe.withDefault []
                        in
                        ( newModel, extraMsgs )

                processRevisionAndUpdateNCDLabsResults participantId encounterId test executionNote resultsAdded =
                    let
                        ( newModel, _ ) =
                            List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                        extraMsgs =
                            Maybe.map
                                (\encounterId_ ->
                                    if resultsAdded then
                                        generateNCDLabsResultsAddedMsgs currentDate newModel test encounterId_

                                    else
                                        generateNCDLabsTestAddedMsgs currentDate newModel test executionNote encounterId_
                                )
                                encounterId
                                |> Maybe.withDefault []
                    in
                    ( newModel, extraMsgs )

                processWellChildSymptomsReviewRevision participantId encounterId value =
                    let
                        noSymptoms =
                            EverySet.isEmpty value || value == EverySet.singleton NoWellChildSymptoms
                    in
                    if noSymptoms then
                        []

                    else
                        generateWellChildDangerSignsAlertMsgs currentDate encounterId

                processWellChildVitalsRevision participantId encounterId value =
                    let
                        bodyTemperatureAlert =
                            value.bodyTemperature < 35 || value.bodyTemperature >= 37.5

                        respiratoryRateAlert =
                            Dict.get participantId model.people
                                |> Maybe.withDefault NotAsked
                                |> RemoteData.toMaybe
                                |> Maybe.andThen (ageInMonths currentDate)
                                |> (\ageMonths -> respiratoryRateAbnormalForAge ageMonths value.respiratoryRate)
                    in
                    if bodyTemperatureAlert || respiratoryRateAlert then
                        generateWellChildDangerSignsAlertMsgs currentDate encounterId

                    else
                        []

                processWellChildECDRevision participantId encounterId after =
                    encounterId
                        |> Maybe.andThen
                            (\id ->
                                Pages.WellChild.Encounter.Utils.generateAssembledData site id after
                                    |> RemoteData.toMaybe
                                    |> Maybe.map
                                        (\assembledAfter ->
                                            let
                                                warningsList =
                                                    assembledAfter.person.birthDate
                                                        |> Maybe.map
                                                            (\birthDate ->
                                                                let
                                                                    ageWeeks =
                                                                        Date.diff Weeks birthDate currentDate

                                                                    ageMonths =
                                                                        Date.diff Months birthDate currentDate
                                                                in
                                                                Pages.WellChild.Activity.Utils.generateRemianingECDSignsAfterCurrentEncounter currentDate assembledAfter
                                                                    |> List.filterMap
                                                                        (\sign ->
                                                                            if List.member sign Pages.WellChild.Activity.Utils.ecdSignsFrom5Weeks then
                                                                                if ageMonths >= 6 then
                                                                                    Just Pages.WellChild.Encounter.Model.ReferToSpecialist

                                                                                else if ageWeeks >= 14 then
                                                                                    Just Pages.WellChild.Encounter.Model.ChildBehind

                                                                                else
                                                                                    Nothing

                                                                            else if List.member sign Pages.WellChild.Activity.Utils.ecdSignsFrom13Weeks then
                                                                                if ageMonths >= 6 then
                                                                                    Just Pages.WellChild.Encounter.Model.ReferToSpecialist

                                                                                else
                                                                                    Nothing

                                                                            else if List.member sign Pages.WellChild.Activity.Utils.ecdSigns6To12MonthsMajors then
                                                                                -- Signs will be displayed until child is 13 months old.
                                                                                if ageMonths == 12 then
                                                                                    Just Pages.WellChild.Encounter.Model.ReferToSpecialist

                                                                                else if ageMonths >= 9 then
                                                                                    Just Pages.WellChild.Encounter.Model.ChildBehind

                                                                                else
                                                                                    Nothing

                                                                            else
                                                                                Nothing
                                                                        )
                                                            )
                                                        |> Maybe.withDefault []

                                                warning =
                                                    if List.member Pages.WellChild.Encounter.Model.ReferToSpecialist warningsList then
                                                        WarningECDMilestoneReferToSpecialist

                                                    else if List.member Pages.WellChild.Encounter.Model.ChildBehind warningsList then
                                                        WarningECDMilestoneBehind

                                                    else
                                                        NoECDMilstoneWarning

                                                setPopUpStateMsg popupType =
                                                    Pages.WellChild.Encounter.Model.PopupECD popupType
                                                        |> Pages.WellChild.Encounter.Model.DialogWarning
                                                        |> Just
                                                        |> Pages.WellChild.Encounter.Model.SetDialogState
                                                        |> App.Model.MsgPageWellChildEncounter id
                                                        |> App.Model.MsgLoggedIn

                                                popUpMsg =
                                                    case warning of
                                                        WarningECDMilestoneReferToSpecialist ->
                                                            [ setPopUpStateMsg Pages.WellChild.Encounter.Model.ReferToSpecialist ]

                                                        WarningECDMilestoneBehind ->
                                                            [ setPopUpStateMsg Pages.WellChild.Encounter.Model.ChildBehind ]

                                                        _ ->
                                                            []

                                                setEncounterWarningMsg =
                                                    Backend.WellChildEncounter.Model.SetWellChildEncounterWarning warning
                                                        |> Backend.Model.MsgWellChildEncounter id
                                                        |> App.Model.MsgIndexedDb
                                            in
                                            setEncounterWarningMsg :: popUpMsg
                                        )
                            )
                        |> Maybe.withDefault []

                processWellChildImmunisationRevision participantId encounterId after =
                    Maybe.andThen
                        (\id ->
                            Pages.WellChild.Encounter.Utils.generateAssembledData site id after
                                |> RemoteData.toMaybe
                                |> Maybe.andThen
                                    (\assembledAfter ->
                                        Maybe.map
                                            (\( measurementId, measurement ) ->
                                                let
                                                    immunisationDate =
                                                        Pages.WellChild.Activity.Utils.generateNextDateForImmunisationVisit currentDate site assembledAfter

                                                    asapImmunisationDate =
                                                        Pages.WellChild.Activity.Utils.generateASAPImmunisationDate currentDate site assembledAfter

                                                    value =
                                                        measurement.value
                                                in
                                                [ Backend.WellChildEncounter.Model.SaveNextVisit assembledAfter.participant.person
                                                    (Just measurementId)
                                                    { value | immunisationDate = immunisationDate, asapImmunisationDate = asapImmunisationDate }
                                                    |> Backend.Model.MsgWellChildEncounter id
                                                    |> App.Model.MsgIndexedDb
                                                ]
                                            )
                                            assembledAfter.measurements.nextVisit
                                    )
                        )
                        encounterId
                        |> Maybe.withDefault []

                processTuberculosisRevisionAndNavigateToProgressReport encounterId =
                    let
                        ( newModel, _ ) =
                            List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                        extraMsgs =
                            Maybe.map
                                (\encounterId_ ->
                                    generateTuberculosisEncounterCompletedMsgs currentDate newModel encounterId_
                                )
                                encounterId
                                |> Maybe.withDefault []
                    in
                    ( newModel, extraMsgs )

                processRevisions =
                    let
                        ( newModel, recalculateEditableSessions ) =
                            List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                        withRecalc =
                            -- If needed, we recalculate all editable sessions that we
                            -- actually have.
                            if recalculateEditableSessions then
                                let
                                    editableSessions =
                                        -- The `andThen` is so that we only recalculate
                                        -- the editable session if we already have a
                                        -- success.
                                        Dict.map
                                            (\id session ->
                                                RemoteData.andThen (\_ -> makeEditableSession id newModel) session
                                            )
                                            newModel.editableSessions
                                in
                                { newModel | editableSessions = editableSessions }

                            else
                                newModel
                    in
                    ( withRecalc
                    , Cmd.none
                    , []
                    )
            in
            if downloadingContent then
                processRevisions

            else
                case revisions of
                    -- Special handling for a single attendance revision, which means
                    -- there was a check in / check out in Attendance page.
                    -- Here we don't want to rebuild all Editable sessions, but only
                    -- the relevant one, and only the things that are needed.
                    [ AttendanceRevision uuid data ] ->
                        let
                            newModel =
                                mapMotherMeasurements
                                    data.participantId
                                    (\measurements ->
                                        { measurements
                                            | attendances =
                                                if data.deleted then
                                                    Dict.remove uuid measurements.attendances

                                                else
                                                    Dict.insert uuid data measurements.attendances
                                        }
                                    )
                                    model

                            withRecalc =
                                data.encounterId
                                    |> Maybe.map
                                        (\sessionId ->
                                            Dict.get sessionId newModel.editableSessions
                                                |> Maybe.andThen RemoteData.toMaybe
                                                |> Maybe.map
                                                    (\editableSession ->
                                                        let
                                                            newEditableSessions =
                                                                if data.deleted then
                                                                    Dict.remove sessionId newModel.editableSessions

                                                                else
                                                                    let
                                                                        updatedOffline =
                                                                            editableSession.offlineSession
                                                                                |> (\offline -> { offline | measurements = LocalData.setRecalculate offline.measurements })

                                                                        updatedEditable =
                                                                            Success
                                                                                { editableSession
                                                                                    | update = NotAsked
                                                                                    , offlineSession = updatedOffline
                                                                                    , checkedIn = LocalData.setRecalculate editableSession.checkedIn
                                                                                    , summaryByParticipant = LocalData.setRecalculate editableSession.summaryByParticipant
                                                                                    , summaryByActivity = LocalData.setRecalculate editableSession.summaryByActivity
                                                                                }
                                                                    in
                                                                    Dict.insert sessionId updatedEditable newModel.editableSessions
                                                        in
                                                        { newModel | editableSessions = newEditableSessions }
                                                    )
                                                |> Maybe.withDefault newModel
                                        )
                                    |> Maybe.withDefault newModel
                        in
                        ( withRecalc
                        , Cmd.none
                        , []
                        )

                    [ SymptomsGeneralRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ SymptomsRespiratoryRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ SymptomsGIRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ TravelHistoryRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ ExposureRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ AcuteIllnessVitalsRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ AcuteIllnessCoreExamRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ AcuteFindingsRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ MalariaTestingRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ CovidTestingRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ AcuteIllnessDangerSignsRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ AcuteIllnessMuacRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ AcuteIllnessNutritionRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ TreatmentOngoingRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ IsolationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                data.encounterId
                                    |> Maybe.map (generateAcuteIllnessAssesmentCompletedMsgs currentDate features isChw newModel)
                                    |> Maybe.withDefault []
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ Call114Revision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                data.encounterId
                                    |> Maybe.map (generateAcuteIllnessAssesmentCompletedMsgs currentDate features isChw newModel)
                                    |> Maybe.withDefault []
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ HCContactRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                data.encounterId
                                    |> Maybe.map (generateAcuteIllnessAssesmentCompletedMsgs currentDate features isChw newModel)
                                    |> Maybe.withDefault []
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ MedicationDistributionRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                data.encounterId
                                    |> Maybe.map (generateAcuteIllnessAssesmentCompletedMsgs currentDate features isChw newModel)
                                    |> Maybe.withDefault []
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ SendToHCRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                data.encounterId
                                    |> Maybe.map (generateAcuteIllnessAssesmentCompletedMsgs currentDate features isChw newModel)
                                    |> Maybe.withDefault []
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ HealthEducationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                data.encounterId
                                    |> Maybe.map (generateAcuteIllnessAssesmentCompletedMsgs currentDate features isChw newModel)
                                    |> Maybe.withDefault []
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ AcuteIllnessFollowUpRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                data.encounterId
                                    |> Maybe.map (generateAcuteIllnessAssesmentCompletedMsgs currentDate features isChw newModel)
                                    |> Maybe.withDefault []
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ NutritionHeightRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessNutritionIndividual data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ NutritionMuacRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessNutritionIndividual data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ NutritionNutritionRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessNutritionIndividual data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ NutritionWeightRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessNutritionIndividual data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ HeightRevision uuid data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessNutritionGroup data.participantId
                                    data.encounterId
                                    (\childMeasurements ->
                                        { childMeasurements
                                            | height =
                                                if data.deleted then
                                                    Nothing

                                                else
                                                    Just ( uuid, data )
                                        }
                                    )
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ MuacRevision uuid data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessNutritionGroup data.participantId
                                    data.encounterId
                                    (\childMeasurements ->
                                        { childMeasurements
                                            | muac =
                                                if data.deleted then
                                                    Nothing

                                                else
                                                    Just ( uuid, data )
                                        }
                                    )
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ ChildNutritionRevision uuid data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessNutritionGroup data.participantId
                                    data.encounterId
                                    (\childMeasurements ->
                                        { childMeasurements
                                            | nutrition =
                                                if data.deleted then
                                                    Nothing

                                                else
                                                    Just ( uuid, data )
                                        }
                                    )
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WeightRevision uuid data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessNutritionGroup data.participantId
                                    data.encounterId
                                    (\childMeasurements ->
                                        { childMeasurements
                                            | weight =
                                                if data.deleted then
                                                    Nothing

                                                else
                                                    Just ( uuid, data )
                                        }
                                    )
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ DangerSignsRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                -- This is the only place where we ask to update assessment for CHW, since
                                -- only thing that affects it is the Danger signs measurement.
                                processRevisionAndAssessPrenatal data.participantId data.encounterId True
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ PrenatalSymptomReviewRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessPrenatal data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ CorePhysicalExamRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessPrenatal data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ LastMenstrualPeriodRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessPrenatal data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ PregnancyTestRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessPrenatal data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ MedicationRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessPrenatal data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ VitalsRevision _ data ] ->
                        let
                            -- We do not catch changes done to model, because
                            -- it's handled by `processRevisionAndAssessPrenatal`
                            -- activation that comes bellow.
                            ( _, extraMsgsForLabsResults ) =
                                processVitalsRevisionAndUpdatePrenatalLabsResults
                                    data.participantId
                                    data.encounterId
                                    data.value

                            ( newModel, extraMsgsForAssessment ) =
                                processRevisionAndAssessPrenatal data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgsForLabsResults ++ extraMsgsForAssessment
                        )

                    [ MalariaPreventionRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processPrenatalRevisionPossiblyCompletingRecurrentPhase data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ PrenatalHealthEducationRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processPrenatalRevisionPossiblyCompletingRecurrentPhase data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ PrenatalSendToHCRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processPrenatalRevisionPossiblyCompletingRecurrentPhase data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ PrenatalMedicationDistributionRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processPrenatalRevisionPossiblyCompletingRecurrentPhase data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ PrenatalHIVTestRevision _ data ] ->
                        let
                            -- We do not catch changes done to model, because
                            -- it's handled by `processRevisionAndAssessPrenatal`
                            -- activation that comes bellow.
                            ( _, extraMsgsForLabsResults ) =
                                processRevisionAndUpdatePrenatalLabsResults
                                    data.participantId
                                    data.encounterId
                                    Backend.Measurement.Model.TestHIV
                                    data.value.executionNote
                                    (isJust data.value.testResult)
                                    data.value.testPrerequisites

                            ( newModel, extraMsgsForAssessment ) =
                                processRevisionAndAssessPrenatal data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgsForLabsResults ++ extraMsgsForAssessment
                        )

                    [ PrenatalPartnerHIVTestRevision _ data ] ->
                        let
                            ( newModel, extraMsg ) =
                                processRevisionAndUpdatePrenatalLabsResults
                                    data.participantId
                                    data.encounterId
                                    Backend.Measurement.Model.TestPartnerHIV
                                    data.value.executionNote
                                    (isJust data.value.testResult)
                                    data.value.testPrerequisites
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsg
                        )

                    [ PrenatalHIVPCRTestRevision _ data ] ->
                        let
                            -- We do not catch changes done to model, because
                            -- it's handled by `processRevisionAndAssessPrenatal`
                            -- activation that comes bellow.
                            ( _, extraMsgsForLabsResults ) =
                                processRevisionAndUpdatePrenatalLabsResults
                                    data.participantId
                                    data.encounterId
                                    Backend.Measurement.Model.TestHIVPCR
                                    data.value.executionNote
                                    (isJust data.value.hivViralLoadStatus)
                                    data.value.testPrerequisites

                            ( newModel, extraMsgsForAssessment ) =
                                processRevisionAndAssessPrenatal data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgsForLabsResults ++ extraMsgsForAssessment
                        )

                    [ PrenatalSyphilisTestRevision _ data ] ->
                        let
                            -- We do not catch changes done to model, because
                            -- it's handled by `processRevisionAndAssessPrenatal`
                            -- activation that comes bellow.
                            ( _, extraMsgsForLabsResults ) =
                                processRevisionAndUpdatePrenatalLabsResults
                                    data.participantId
                                    data.encounterId
                                    Backend.Measurement.Model.TestSyphilis
                                    data.value.executionNote
                                    (isJust data.value.testResult)
                                    data.value.testPrerequisites

                            ( newModel, extraMsgsForAssessment ) =
                                Maybe.map
                                    (\originatingEncounterId ->
                                        ( originatingEncounterId
                                        , Pages.Prenatal.Utils.syphilisDiagnosesIncludingNeurosyphilisRecurrentPhase
                                        )
                                    )
                                    data.value.originatingEncounter
                                    |> processRevisionAndAssessPrenatalWithReportToOrigin data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgsForLabsResults ++ extraMsgsForAssessment
                        )

                    [ PrenatalHepatitisBTestRevision _ data ] ->
                        let
                            -- We do not catch changes done to model, because
                            -- it's handled by `processRevisionAndAssessPrenatal`
                            -- activation that comes bellow.
                            ( _, extraMsgsForLabsResults ) =
                                processRevisionAndUpdatePrenatalLabsResults
                                    data.participantId
                                    data.encounterId
                                    Backend.Measurement.Model.TestHepatitisB
                                    data.value.executionNote
                                    (isJust data.value.testResult)
                                    data.value.testPrerequisites

                            ( newModel, extraMsgsForAssessment ) =
                                Maybe.map
                                    (\originatingEncounterId -> ( originatingEncounterId, [ DiagnosisHepatitisBRecurrentPhase ] ))
                                    data.value.originatingEncounter
                                    |> processRevisionAndAssessPrenatalWithReportToOrigin data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgsForLabsResults ++ extraMsgsForAssessment
                        )

                    [ PrenatalMalariaTestRevision _ data ] ->
                        let
                            -- We do not catch changes done to model, because
                            -- it's handled by `processRevisionAndAssessPrenatal`
                            -- activation that comes bellow.
                            ( _, extraMsgsForLabsResults ) =
                                let
                                    executionNote =
                                        if data.value.bloodSmearResult == BloodSmearPendingInput then
                                            TestNoteRunToday

                                        else
                                            data.value.executionNote

                                    resultsAdded =
                                        isJust data.value.testResult
                                            || bloodSmearResultSet data.value.bloodSmearResult
                                in
                                processRevisionAndUpdatePrenatalLabsResults
                                    data.participantId
                                    data.encounterId
                                    Backend.Measurement.Model.TestMalaria
                                    executionNote
                                    resultsAdded
                                    data.value.testPrerequisites

                            ( newModel, extraMsgsForAssessment ) =
                                processRevisionAndAssessPrenatal data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgsForLabsResults ++ extraMsgsForAssessment
                        )

                    [ PrenatalUrineDipstickTestRevision _ data ] ->
                        let
                            -- We do not catch changes done to model, because
                            -- it's handled by `processRevisionAndAssessPrenatal`
                            -- activation that comes bellow.
                            ( _, extraMsgsForLabsResults ) =
                                processRevisionAndUpdatePrenatalLabsResults
                                    data.participantId
                                    data.encounterId
                                    Backend.Measurement.Model.TestUrineDipstick
                                    data.value.executionNote
                                    (isJust data.value.protein)
                                    data.value.testPrerequisites

                            ( newModel, extraMsgsForAssessment ) =
                                processRevisionAndAssessPrenatal data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgsForLabsResults ++ extraMsgsForAssessment
                        )

                    [ PrenatalBloodGpRsTestRevision _ data ] ->
                        let
                            -- We do not catch changes done to the model, because
                            -- it's handled by `processRevisionAndAssessPrenatal`
                            -- activation that comes bellow.
                            ( _, extraMsgsForLabsResults ) =
                                processRevisionAndUpdatePrenatalLabsResults
                                    data.participantId
                                    data.encounterId
                                    Backend.Measurement.Model.TestBloodGpRs
                                    data.value.executionNote
                                    (isJust data.value.bloodGroup)
                                    data.value.testPrerequisites

                            ( newModel, extraMsgsForAssessment ) =
                                Maybe.map
                                    (\originatingEncounterId -> ( originatingEncounterId, [ DiagnosisRhesusNegativeRecurrentPhase ] ))
                                    data.value.originatingEncounter
                                    |> processRevisionAndAssessPrenatalWithReportToOrigin data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgsForLabsResults ++ extraMsgsForAssessment
                        )

                    [ PrenatalHemoglobinTestRevision _ data ] ->
                        let
                            -- We do not catch changes done to model, because
                            -- it's handled by `processRevisionAndAssessPrenatal`
                            -- activation that comes bellow.
                            ( _, extraMsgsForLabsResults ) =
                                processRevisionAndUpdatePrenatalLabsResults
                                    data.participantId
                                    data.encounterId
                                    Backend.Measurement.Model.TestHemoglobin
                                    data.value.executionNote
                                    (isJust data.value.hemoglobinCount)
                                    data.value.testPrerequisites

                            ( newModel, extraMsgsForAssessment ) =
                                processRevisionAndAssessPrenatal data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgsForLabsResults ++ extraMsgsForAssessment
                        )

                    [ PrenatalRandomBloodSugarTestRevision _ data ] ->
                        let
                            -- We do not catch changes done to model, because
                            -- it's handled by `processRevisionAndAssessPrenatal`
                            -- activation that comes bellow.
                            ( _, extraMsgsForLabsResults ) =
                                processRevisionAndUpdatePrenatalLabsResults
                                    data.participantId
                                    data.encounterId
                                    Backend.Measurement.Model.TestRandomBloodSugar
                                    data.value.executionNote
                                    (isJust data.value.sugarCount)
                                    data.value.testPrerequisites

                            ( newModel, extraMsgsForAssessment ) =
                                Maybe.map
                                    (\originatingEncounterId -> ( originatingEncounterId, Pages.Prenatal.Utils.diabetesDiagnosesRecurrentPhase ))
                                    data.value.originatingEncounter
                                    |> processRevisionAndAssessPrenatalWithReportToOrigin data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgsForLabsResults ++ extraMsgsForAssessment
                        )

                    [ PrenatalMentalHealthRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessPrenatal data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ BreastExamRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessPrenatal data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ PrenatalBreastfeedingRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessPrenatal data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ PrenatalGUExamRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessPrenatal data.participantId data.encounterId False
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WellChildHeightRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessWellChild data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WellChildHeadCircumferenceRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessWellChild data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WellChildMuacRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessWellChild data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WellChildNutritionRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessWellChild data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WellChildWeightRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessWellChild data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WellChildSymptomsReviewRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                processWellChildSymptomsReviewRevision data.participantId data.encounterId data.value
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WellChildVitalsRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                processWellChildVitalsRevision data.participantId data.encounterId data.value
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WellChildECDRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                processWellChildECDRevision data.participantId data.encounterId newModel
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WellChildBCGImmunisationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                processWellChildImmunisationRevision data.participantId data.encounterId newModel
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WellChildDTPImmunisationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                processWellChildImmunisationRevision data.participantId data.encounterId newModel
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WellChildDTPStandaloneImmunisationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                processWellChildImmunisationRevision data.participantId data.encounterId newModel
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WellChildHPVImmunisationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                processWellChildImmunisationRevision data.participantId data.encounterId newModel
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WellChildIPVImmunisationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                processWellChildImmunisationRevision data.participantId data.encounterId newModel
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WellChildMRImmunisationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                processWellChildImmunisationRevision data.participantId data.encounterId newModel
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WellChildOPVImmunisationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                processWellChildImmunisationRevision data.participantId data.encounterId newModel
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WellChildPCV13ImmunisationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                processWellChildImmunisationRevision data.participantId data.encounterId newModel
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ WellChildRotarixImmunisationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                processWellChildImmunisationRevision data.participantId data.encounterId newModel
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ NCDRandomBloodSugarTestRevision _ data ] ->
                        let
                            -- We do not catch changes done to model, because
                            -- it's handled by `processRevisionAndAssessNCD`
                            -- activation that comes bellow.
                            ( _, extraMsgsForLabsResults ) =
                                processRevisionAndUpdateNCDLabsResults
                                    data.participantId
                                    data.encounterId
                                    Backend.Measurement.Model.TestRandomBloodSugar
                                    data.value.executionNote
                                    (isJust data.value.sugarCount)

                            ( newModel, extraMsgsForAssessment ) =
                                processRevisionAndAssessNCD data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgsForLabsResults ++ extraMsgsForAssessment
                        )

                    [ NCDUrineDipstickTestRevision _ data ] ->
                        let
                            -- We do not catch changes done to model, because
                            -- it's handled by `processRevisionAndAssessNCD`
                            -- activation that comes bellow.
                            ( _, extraMsgsForLabsResults ) =
                                processRevisionAndUpdateNCDLabsResults
                                    data.participantId
                                    data.encounterId
                                    Backend.Measurement.Model.TestUrineDipstick
                                    data.value.executionNote
                                    (isJust data.value.protein)

                            ( newModel, extraMsgsForAssessment ) =
                                processRevisionAndAssessNCD data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgsForLabsResults ++ extraMsgsForAssessment
                        )

                    [ NCDCreatinineTestRevision _ data ] ->
                        let
                            -- We do not catch changes done to model, because
                            -- it's handled by `processRevisionAndAssessNCD`
                            -- activation that comes bellow.
                            ( _, extraMsgsForLabsResults ) =
                                processRevisionAndUpdateNCDLabsResults
                                    data.participantId
                                    data.encounterId
                                    Backend.Measurement.Model.TestCreatinine
                                    data.value.executionNote
                                    (isJust data.value.creatinineResult)

                            ( newModel, extraMsgsForAssessment ) =
                                processRevisionAndAssessNCD data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgsForLabsResults ++ extraMsgsForAssessment
                        )

                    [ NCDLiverFunctionTestRevision _ data ] ->
                        let
                            ( newModel, extraMsgsForLabsResults ) =
                                processRevisionAndUpdateNCDLabsResults
                                    data.participantId
                                    data.encounterId
                                    Backend.Measurement.Model.TestLiverFunction
                                    data.value.executionNote
                                    (isJust data.value.altResult)
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgsForLabsResults
                        )

                    [ NCDCoMorbiditiesRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessNCD data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ NCDVitalsRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processRevisionAndAssessNCD data.participantId data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ NCDLipidPanelTestRevision _ data ] ->
                        let
                            ( newModel, extraMsgsForLabsResults ) =
                                processRevisionAndUpdateNCDLabsResults
                                    data.participantId
                                    data.encounterId
                                    Backend.Measurement.Model.TestLipidPanel
                                    data.value.executionNote
                                    (isJust data.value.totalCholesterolResult)
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgsForLabsResults
                        )

                    [ ChildScoreboardNCDARevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                Maybe.map (generateChildScoreboardAssesmentCompletedMsgs currentDate site newModel) data.encounterId
                                    |> Maybe.withDefault []
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ ChildScoreboardBCGImmunisationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                Maybe.map (generateChildScoreboardAssesmentCompletedMsgs currentDate site newModel) data.encounterId
                                    |> Maybe.withDefault []
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ ChildScoreboardDTPImmunisationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                Maybe.map (generateChildScoreboardAssesmentCompletedMsgs currentDate site newModel) data.encounterId
                                    |> Maybe.withDefault []
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ ChildScoreboardDTPStandaloneImmunisationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                Maybe.map (generateChildScoreboardAssesmentCompletedMsgs currentDate site newModel) data.encounterId
                                    |> Maybe.withDefault []
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ ChildScoreboardIPVImmunisationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                Maybe.map (generateChildScoreboardAssesmentCompletedMsgs currentDate site newModel) data.encounterId
                                    |> Maybe.withDefault []
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ ChildScoreboardMRImmunisationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                Maybe.map (generateChildScoreboardAssesmentCompletedMsgs currentDate site newModel) data.encounterId
                                    |> Maybe.withDefault []
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ ChildScoreboardOPVImmunisationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                Maybe.map (generateChildScoreboardAssesmentCompletedMsgs currentDate site newModel) data.encounterId
                                    |> Maybe.withDefault []
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ ChildScoreboardPCV13ImmunisationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                Maybe.map (generateChildScoreboardAssesmentCompletedMsgs currentDate site newModel) data.encounterId
                                    |> Maybe.withDefault []
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ ChildScoreboardRotarixImmunisationRevision _ data ] ->
                        let
                            ( newModel, _ ) =
                                List.foldl (handleRevision currentDate healthCenterId villageId) ( model, False ) revisions

                            extraMsgs =
                                Maybe.map (generateChildScoreboardAssesmentCompletedMsgs currentDate site newModel) data.encounterId
                                    |> Maybe.withDefault []
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ TuberculosisDOTRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processTuberculosisRevisionAndNavigateToProgressReport data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ TuberculosisFollowUpRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processTuberculosisRevisionAndNavigateToProgressReport data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ TuberculosisHealthEducationRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processTuberculosisRevisionAndNavigateToProgressReport data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ TuberculosisMedicationRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processTuberculosisRevisionAndNavigateToProgressReport data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ TuberculosisReferralRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processTuberculosisRevisionAndNavigateToProgressReport data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ TuberculosisSymptomReviewRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processTuberculosisRevisionAndNavigateToProgressReport data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    [ TuberculosisTreatmentReviewRevision _ data ] ->
                        let
                            ( newModel, extraMsgs ) =
                                processTuberculosisRevisionAndNavigateToProgressReport data.encounterId
                        in
                        ( newModel
                        , Cmd.none
                        , extraMsgs
                        )

                    _ ->
                        processRevisions

        ResetFailedToFetchAuthorities ->
            let
                failureToNotAsked webData =
                    case webData of
                        Failure _ ->
                            NotAsked

                        _ ->
                            webData
            in
            ( { model | healthCenters = failureToNotAsked model.healthCenters, villages = failureToNotAsked model.villages }
            , Cmd.none
            , []
            )

        MsgPrenatalEncounter encounterId subMsg ->
            let
                encounter =
                    Dict.get encounterId model.prenatalEncounters
                        |> Maybe.andThen RemoteData.toMaybe

                requests =
                    Dict.get encounterId model.prenatalEncounterRequests
                        |> Maybe.withDefault Backend.PrenatalEncounter.Model.emptyModel

                ( subModel, subCmd, appMsgs ) =
                    Backend.PrenatalEncounter.Update.update currentDate nurseId healthCenterId encounterId encounter subMsg requests
            in
            ( { model | prenatalEncounterRequests = Dict.insert encounterId subModel model.prenatalEncounterRequests }
            , Cmd.map (MsgPrenatalEncounter encounterId) subCmd
            , appMsgs
            )

        MsgNutritionEncounter encounterId subMsg ->
            let
                encounter =
                    Dict.get encounterId model.nutritionEncounters
                        |> Maybe.andThen RemoteData.toMaybe

                requests =
                    Dict.get encounterId model.nutritionEncounterRequests
                        |> Maybe.withDefault Backend.NutritionEncounter.Model.emptyModel

                ( subModel, subCmd, appMsgs ) =
                    Backend.NutritionEncounter.Update.update currentDate
                        nurseId
                        healthCenterId
                        encounterId
                        encounter
                        subMsg
                        requests
            in
            ( { model | nutritionEncounterRequests = Dict.insert encounterId subModel model.nutritionEncounterRequests }
            , Cmd.map (MsgNutritionEncounter encounterId) subCmd
            , appMsgs
            )

        MsgAcuteIllnessEncounter encounterId subMsg ->
            let
                encounter =
                    Dict.get encounterId model.acuteIllnessEncounters
                        |> Maybe.andThen RemoteData.toMaybe

                requests =
                    Dict.get encounterId model.acuteIllnessEncounterRequests
                        |> Maybe.withDefault Backend.AcuteIllnessEncounter.Model.emptyModel

                ( subModel, subCmd, appMsgs ) =
                    Backend.AcuteIllnessEncounter.Update.update currentDate nurseId healthCenterId encounterId encounter subMsg requests
            in
            ( { model | acuteIllnessEncounterRequests = Dict.insert encounterId subModel model.acuteIllnessEncounterRequests }
            , Cmd.map (MsgAcuteIllnessEncounter encounterId) subCmd
            , appMsgs
            )

        MsgHomeVisitEncounter encounterId subMsg ->
            let
                encounter =
                    Dict.get encounterId model.homeVisitEncounters
                        |> Maybe.andThen RemoteData.toMaybe

                requests =
                    Dict.get encounterId model.homeVisitEncounterRequests
                        |> Maybe.withDefault Backend.HomeVisitEncounter.Model.emptyModel

                ( subModel, subCmd, appMsgs ) =
                    Backend.HomeVisitEncounter.Update.update currentDate nurseId healthCenterId encounterId encounter subMsg requests
            in
            ( { model | homeVisitEncounterRequests = Dict.insert encounterId subModel model.homeVisitEncounterRequests }
            , Cmd.map (MsgHomeVisitEncounter encounterId) subCmd
            , appMsgs
            )

        MsgWellChildEncounter encounterId subMsg ->
            let
                encounter =
                    Dict.get encounterId model.wellChildEncounters
                        |> Maybe.andThen RemoteData.toMaybe

                requests =
                    Dict.get encounterId model.wellChildEncounterRequests
                        |> Maybe.withDefault Backend.WellChildEncounter.Model.emptyModel

                ( subModel, subCmd, appMsgs ) =
                    Backend.WellChildEncounter.Update.update currentDate nurseId healthCenterId encounterId encounter subMsg requests
            in
            ( { model | wellChildEncounterRequests = Dict.insert encounterId subModel model.wellChildEncounterRequests }
            , Cmd.map (MsgWellChildEncounter encounterId) subCmd
            , appMsgs
            )

        MsgNCDEncounter encounterId subMsg ->
            let
                encounter =
                    Dict.get encounterId model.ncdEncounters
                        |> Maybe.andThen RemoteData.toMaybe

                requests =
                    Dict.get encounterId model.ncdEncounterRequests
                        |> Maybe.withDefault Backend.NCDEncounter.Model.emptyModel

                ( subModel, subCmd, appMsgs ) =
                    Backend.NCDEncounter.Update.update currentDate nurseId healthCenterId encounterId encounter subMsg requests
            in
            ( { model | ncdEncounterRequests = Dict.insert encounterId subModel model.ncdEncounterRequests }
            , Cmd.map (MsgNCDEncounter encounterId) subCmd
            , appMsgs
            )

        MsgChildScoreboardEncounter encounterId subMsg ->
            let
                encounter =
                    Dict.get encounterId model.childScoreboardEncounters
                        |> Maybe.andThen RemoteData.toMaybe

                requests =
                    Dict.get encounterId model.childScoreboardEncounterRequests
                        |> Maybe.withDefault Backend.ChildScoreboardEncounter.Model.emptyModel

                ( subModel, subCmd, appMsgs ) =
                    Backend.ChildScoreboardEncounter.Update.update currentDate nurseId healthCenterId encounterId encounter subMsg requests
            in
            ( { model | childScoreboardEncounterRequests = Dict.insert encounterId subModel model.childScoreboardEncounterRequests }
            , Cmd.map (MsgChildScoreboardEncounter encounterId) subCmd
            , appMsgs
            )

        MsgTuberculosisEncounter encounterId subMsg ->
            let
                encounter =
                    Dict.get encounterId model.tuberculosisEncounters
                        |> Maybe.andThen RemoteData.toMaybe

                requests =
                    Dict.get encounterId model.tuberculosisEncounterRequests
                        |> Maybe.withDefault Backend.TuberculosisEncounter.Model.emptyModel

                ( subModel, subCmd, appMsgs ) =
                    Backend.TuberculosisEncounter.Update.update currentDate nurseId healthCenterId encounterId encounter subMsg requests
            in
            ( { model | tuberculosisEncounterRequests = Dict.insert encounterId subModel model.tuberculosisEncounterRequests }
            , Cmd.map (MsgTuberculosisEncounter encounterId) subCmd
            , appMsgs
            )

        MsgHIVEncounter encounterId subMsg ->
            let
                encounter =
                    Dict.get encounterId model.hivEncounters
                        |> Maybe.andThen RemoteData.toMaybe

                requests =
                    Dict.get encounterId model.hivEncounterRequests
                        |> Maybe.withDefault Backend.HIVEncounter.Model.emptyModel

                ( subModel, subCmd, appMsgs ) =
                    Backend.HIVEncounter.Update.update currentDate nurseId healthCenterId encounterId encounter subMsg requests
            in
            ( { model | hivEncounterRequests = Dict.insert encounterId subModel model.hivEncounterRequests }
            , Cmd.map (MsgHIVEncounter encounterId) subCmd
            , appMsgs
            )

        MsgEducationSession sessionId subMsg ->
            let
                encounter =
                    Dict.get sessionId model.educationSessions
                        |> Maybe.andThen RemoteData.toMaybe

                requests =
                    Dict.get sessionId model.educationSessionRequests
                        |> Maybe.withDefault Backend.EducationSession.Model.emptyModel

                ( subModel, subCmd, appMsgs ) =
                    Backend.EducationSession.Update.update currentDate sessionId encounter subMsg requests
            in
            ( { model | educationSessionRequests = Dict.insert sessionId subModel model.educationSessionRequests }
            , Cmd.map (MsgEducationSession sessionId) subCmd
            , appMsgs
            )

        MsgTraceContact traceContactId subMsg ->
            let
                traceContact =
                    Dict.get traceContactId model.traceContacts
                        |> Maybe.andThen RemoteData.toMaybe

                requests =
                    Dict.get traceContactId model.traceContactRequests
                        |> Maybe.withDefault Backend.TraceContact.Model.emptyModel

                ( subModel, subCmd, appMsgs ) =
                    Backend.TraceContact.Update.update traceContactId traceContact subMsg requests
            in
            ( { model | traceContactRequests = Dict.insert traceContactId subModel model.traceContactRequests }
            , Cmd.map (MsgTraceContact traceContactId) subCmd
            , appMsgs
            )

        MsgIndividualEncounterParticipant participantId subMsg ->
            let
                participant =
                    Dict.get participantId model.individualParticipants
                        |> Maybe.andThen RemoteData.toMaybe

                requests =
                    Dict.get participantId model.individualEncounterParticipantRequests
                        |> Maybe.withDefault Backend.IndividualEncounterParticipant.Model.emptyModel

                ( subModel, subCmd, appMsgs ) =
                    Backend.IndividualEncounterParticipant.Update.update currentDate participantId participant subMsg requests
            in
            ( { model | individualEncounterParticipantRequests = Dict.insert participantId subModel model.individualEncounterParticipantRequests }
            , Cmd.map (MsgIndividualEncounterParticipant participantId) subCmd
            , appMsgs
            )

        MsgSession sessionId subMsg ->
            let
                session =
                    Dict.get sessionId model.editableSessions
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.map (.offlineSession >> .session)
                        |> RemoteData.toMaybe

                requests =
                    Dict.get sessionId model.sessionRequests
                        |> Maybe.withDefault Backend.Session.Model.emptyModel

                ( subModel, subCmd, appMsgs ) =
                    Backend.Session.Update.update currentDate nurseId sessionId session model subMsg requests
            in
            ( { model | sessionRequests = Dict.insert sessionId subModel model.sessionRequests }
            , Cmd.map (MsgSession sessionId) subCmd
            , appMsgs
            )

        MsgNurse updatedNurseId subMsg ->
            let
                requests =
                    Dict.get updatedNurseId model.nurseRequests
                        |> Maybe.withDefault Backend.Nurse.Model.emptyModel

                ( subModel, subCmd, appMsgs ) =
                    Backend.Nurse.Update.update currentDate subMsg requests
            in
            ( { model | nurseRequests = Dict.insert updatedNurseId subModel model.nurseRequests }
            , Cmd.map (MsgNurse updatedNurseId) subCmd
            , appMsgs
            )

        MsgResilienceSurvey surveyNurseId subMsg ->
            let
                requests =
                    Dict.get surveyNurseId model.resilienceSurveyRequests
                        |> Maybe.withDefault Backend.ResilienceSurvey.Model.emptyModel

                ( subModel, subCmd, appMsgs ) =
                    Backend.ResilienceSurvey.Update.update currentDate subMsg requests
            in
            ( { model | resilienceSurveyRequests = Dict.insert surveyNurseId subModel model.resilienceSurveyRequests }
            , Cmd.map (MsgResilienceSurvey surveyNurseId) subCmd
            , appMsgs
            )

        MsgStockUpdate updateNurseId subMsg ->
            let
                requests =
                    Dict.get updateNurseId model.stockUpdateRequests
                        |> Maybe.withDefault Backend.StockUpdate.Model.emptyModel

                ( subModel, subCmd, appMsgs ) =
                    Backend.StockUpdate.Update.update currentDate subMsg requests
            in
            ( { model | stockUpdateRequests = Dict.insert updateNurseId subModel model.stockUpdateRequests }
            , Cmd.map (MsgStockUpdate updateNurseId) subCmd
            , appMsgs
            )

        PostPmtctParticipant initiator data ->
            ( { model | postPmtctParticipant = Dict.insert data.child Loading model.postPmtctParticipant }
            , sw.post pmtctParticipantEndpoint data
                |> toCmd (RemoteData.fromResult >> HandlePostedPmtctParticipant data.child initiator)
            , []
            )

        HandlePostedPmtctParticipant id initiator data ->
            let
                rollbarOnFailure =
                    triggerRollbarOnFailure data

                appMsgs =
                    case initiator of
                        -- When in session context, automaticaly create
                        -- a new attendance for created participant.
                        GroupEncounterOrigin sessionId ->
                            data
                                |> RemoteData.map
                                    (Tuple.second
                                        >> .adult
                                        >> (\motherId ->
                                                Measurement.Model.SaveAttendance Nothing True
                                                    |> Backend.Session.Model.MeasurementOutMsgMother motherId
                                                    |> MsgSession sessionId
                                                    |> App.Model.MsgIndexedDb
                                                    |> List.singleton
                                           )
                                    )
                                |> RemoteData.withDefault []

                        _ ->
                            []
            in
            ( { model | postPmtctParticipant = Dict.insert id data model.postPmtctParticipant }
            , Cmd.none
            , rollbarOnFailure ++ appMsgs
            )

        PostRelationship personId myRelationship addGroup initiator ->
            let
                normalized =
                    toRelationship personId myRelationship healthCenterId

                -- If we'd also like to add these people to a group, construct
                -- a Msg to do that.
                extraMsgs =
                    addGroup
                        |> Maybe.map
                            (\clinicId ->
                                let
                                    defaultAdultActivities =
                                        case normalized.relatedBy of
                                            ParentOf ->
                                                MotherActivities

                                            CaregiverFor ->
                                                CaregiverActivities

                                    childBirthDate =
                                        Dict.get normalized.relatedTo model.people
                                            |> Maybe.withDefault NotAsked
                                            |> RemoteData.toMaybe
                                            |> Maybe.andThen .birthDate

                                    -- The start date determines when we start expecting this pair
                                    -- to be attending a group encounter. We'll look to see if we
                                    -- know the child's birth date. Normally, we will, because
                                    -- we've probably just entered it, or we've loaded the child
                                    -- for some other reason. We won't try to fetch the child here
                                    -- if we don't have the child, at least for now, because it
                                    -- would add complexity. If we don't know the child's
                                    -- birthdate, we'll default to 28 days ago. That should be
                                    -- enough so that, if we're in the middle of a group encounter,
                                    -- the child will be expected at that group encounter.
                                    defaultStartDate =
                                        childBirthDate
                                            |> Maybe.withDefault (Date.add Days -28 currentDate)

                                    -- For all groups but Sorwathe, we expect child to graduate from programm
                                    -- after 26 months. Therefore, if we can resolve clinic type and child birthday,
                                    -- we'll set expected graduation date.
                                    defaultEndDate =
                                        model.clinics
                                            |> RemoteData.toMaybe
                                            |> Maybe.andThen
                                                (Dict.get clinicId
                                                    >> Maybe.andThen
                                                        (\clinic ->
                                                            if List.member clinic.clinicType [ Sorwathe, Achi ] then
                                                                Nothing

                                                            else
                                                                Maybe.map (Date.add Months graduatingAgeInMonth) childBirthDate
                                                        )
                                                )
                                in
                                PostPmtctParticipant initiator
                                    { adult = normalized.person
                                    , child = normalized.relatedTo
                                    , adultActivities = defaultAdultActivities
                                    , start = defaultStartDate
                                    , end = defaultEndDate
                                    , clinic = clinicId
                                    , deleted = False
                                    }
                            )
                        |> Maybe.Extra.toList

                -- We want to patch any relationship between these two,
                -- whether or not reversed.
                query1 =
                    sw.select relationshipEndpoint
                        { person = Just normalized.person
                        , relatedTo = Just normalized.relatedTo
                        }
                        |> toTask
                        |> Task.map (.items >> Dict.fromList)

                query2 =
                    sw.select relationshipEndpoint
                        { person = Just normalized.relatedTo
                        , relatedTo = Just normalized.person
                        }
                        |> toTask
                        |> Task.map (.items >> Dict.fromList)

                existingRelationship =
                    Task.map2 Dict.union query1 query2
                        |> Task.map (Dict.toList >> List.head)

                relationshipCmd =
                    existingRelationship
                        |> Task.andThen
                            (\existing ->
                                case existing of
                                    Nothing ->
                                        sw.post relationshipEndpoint normalized
                                            |> toTask
                                            |> Task.map (always myRelationship)

                                    Just ( relationshipId, relationship ) ->
                                        let
                                            changes =
                                                encodeRelationshipChanges { old = relationship, new = normalized }
                                        in
                                        if List.isEmpty changes then
                                            -- If no changes, we just report success without posting to the DB
                                            Task.succeed myRelationship

                                        else
                                            object changes
                                                |> sw.patchAny relationshipEndpoint relationshipId
                                                |> toTask
                                                |> Task.map (always myRelationship)
                            )
                        |> RemoteData.fromTask
                        |> Task.perform (HandlePostedRelationship personId initiator)
            in
            ( { model | postRelationship = Dict.insert personId Loading model.postRelationship }
            , relationshipCmd
            , []
            )
                |> sequenceExtra
                    (updateIndexedDb language
                        currentDate
                        currentTime
                        coordinates
                        zscores
                        site
                        features
                        nurseId
                        healthCenterId
                        villageId
                        isChw
                        isLabTech
                        activePage
                        syncManager
                    )
                    extraMsgs

        HandlePostedRelationship personId initiator data ->
            let
                rollbarOnFailure =
                    triggerRollbarOnFailure data

                appMsgs =
                    data
                        |> RemoteData.map
                            (\relationship ->
                                let
                                    resetFormMsg =
                                        Pages.Relationship.Model.Reset initiator
                                            |> App.Model.MsgPageRelationship personId relationship.relatedTo
                                            |> App.Model.MsgLoggedIn
                                in
                                case initiator of
                                    -- We do not use the form in this scenario,
                                    -- therefore, no need to reset it.
                                    PrenatalNextStepsNewbornEnrolmentOrigin _ _ ->
                                        []

                                    -- When at session context, we navigate to session Attendance page.
                                    -- At that page, we should see newly created attendance.
                                    GroupEncounterOrigin sessionId ->
                                        [ resetFormMsg
                                        , App.Model.SetActivePage <| UserPage <| SessionPage sessionId AttendancePage
                                        ]

                                    -- For other cases, we navigate to the page of main person.
                                    _ ->
                                        [ resetFormMsg
                                        , App.Model.SetActivePage <| UserPage <| PersonPage personId initiator
                                        ]
                            )
                        |> RemoteData.withDefault []
            in
            ( { model | postRelationship = Dict.insert personId data model.postRelationship }
            , Cmd.none
            , rollbarOnFailure ++ appMsgs
            )

        PostPerson relation initiator person ->
            let
                -- Adding GPS coordinates.
                personWithCoordinates =
                    if gpsCoordinatesEnabled features && person.saveGPSLocation then
                        updatePersonWithCooridnates person coordinates

                    else
                        person
            in
            ( { model | postPerson = Loading }
            , sw.post personEndpoint personWithCoordinates
                |> toCmd (RemoteData.fromResult >> RemoteData.map Tuple.first >> HandlePostedPerson relation initiator)
            , []
            )

        HandlePostedPerson relation initiator data ->
            let
                rollbarOnFailure =
                    triggerRollbarOnFailure data

                ( appMsgs, extraMsgs ) =
                    -- If we succeed, we reset the form, and go to the page
                    -- showing the new person.
                    RemoteData.toMaybe data
                        |> Maybe.map
                            (\personId ->
                                let
                                    resetFormMsg =
                                        Pages.Person.Model.ResetCreateForm
                                            |> App.Model.MsgPageCreatePerson
                                            |> App.Model.MsgLoggedIn

                                    navigationMsg destination =
                                        UserPage destination
                                            |> App.Model.SetActivePage
                                in
                                case initiator of
                                    ParticipantDirectoryOrigin ->
                                        let
                                            nextPage =
                                                case relation of
                                                    Just id ->
                                                        RelationshipPage id personId initiator

                                                    Nothing ->
                                                        PersonPage personId initiator
                                        in
                                        ( [ resetFormMsg, navigationMsg nextPage ]
                                        , []
                                        )

                                    IndividualEncounterOrigin encounterType ->
                                        let
                                            nextPage =
                                                case encounterType of
                                                    AcuteIllnessEncounter ->
                                                        AcuteIllnessParticipantPage InitiatorParticipantsPage personId

                                                    AntenatalEncounter ->
                                                        PrenatalParticipantPage InitiatorParticipantsPage personId

                                                    ChildScoreboardEncounter ->
                                                        ChildScoreboardParticipantPage personId

                                                    HIVEncounter ->
                                                        HIVParticipantPage personId

                                                    -- We do not have a direct access to Home Visit
                                                    -- encounter, since it resides under Nutrition menu.
                                                    -- Providing 'default' page, to satisfy compiler.
                                                    HomeVisitEncounter ->
                                                        IndividualEncounterTypesPage

                                                    NCDEncounter ->
                                                        NCDParticipantPage InitiatorParticipantsPage personId

                                                    NutritionEncounter ->
                                                        NutritionParticipantPage InitiatorParticipantsPage personId

                                                    TuberculosisEncounter ->
                                                        TuberculosisParticipantPage personId

                                                    WellChildEncounter ->
                                                        WellChildParticipantPage InitiatorParticipantsPage personId

                                                    -- Note yet implemented. Providing 'default'
                                                    -- page, to satisfy compiler.
                                                    InmmunizationEncounter ->
                                                        IndividualEncounterTypesPage
                                        in
                                        ( [ resetFormMsg, navigationMsg nextPage ]
                                        , []
                                        )

                                    GroupEncounterOrigin _ ->
                                        let
                                            nextPage =
                                                case relation of
                                                    Just id ->
                                                        RelationshipPage id personId initiator

                                                    Nothing ->
                                                        PersonPage personId initiator
                                        in
                                        ( [ resetFormMsg, navigationMsg nextPage ]
                                        , []
                                        )

                                    PrenatalNextStepsNewbornEnrolmentOrigin _ encounterId ->
                                        let
                                            nextPage =
                                                PrenatalActivityPage encounterId Backend.PrenatalActivity.Model.NextSteps

                                            updateNewbornEnroledMsg =
                                                Dict.get encounterId model.prenatalEncounters
                                                    |> Maybe.andThen RemoteData.toMaybe
                                                    |> Maybe.map
                                                        (\encounter ->
                                                            [ Backend.IndividualEncounterParticipant.Model.SetNewborn personId
                                                                |> MsgIndividualEncounterParticipant encounter.participant
                                                            ]
                                                        )
                                                    |> Maybe.withDefault []

                                            createRelationshipMsg =
                                                relation
                                                    |> Maybe.map
                                                        (\motherId ->
                                                            let
                                                                villageGroupId =
                                                                    Maybe.andThen (\id -> getVillageClinicId id model) villageId

                                                                myRelationship =
                                                                    MyRelationship personId MyChild
                                                            in
                                                            [ PostRelationship motherId myRelationship villageGroupId initiator ]
                                                        )
                                                    |> Maybe.withDefault []
                                        in
                                        ( [ resetFormMsg, navigationMsg nextPage ]
                                        , updateNewbornEnroledMsg ++ createRelationshipMsg
                                        )

                                    AcuteIllnessContactsTracingActivityOrigin encounterId ->
                                        let
                                            setContactsTracingFormStateMsg =
                                                Pages.AcuteIllness.Activity.Model.ContactsTracingFormRecordContactDetails
                                                    personId
                                                    Pages.AcuteIllness.Activity.Model.emptyRecordContactDetailsData
                                                    |> Pages.AcuteIllness.Activity.Model.SetContactsTracingFormState
                                                    |> App.Model.MsgPageAcuteIllnessActivity encounterId Backend.AcuteIllnessActivity.Model.AcuteIllnessNextSteps
                                                    |> App.Model.MsgLoggedIn
                                                    |> List.singleton
                                        in
                                        ( setContactsTracingFormStateMsg
                                        , []
                                        )
                            )
                        |> Maybe.withDefault ( [], [] )
            in
            ( { model | postPerson = data }
            , Cmd.none
            , rollbarOnFailure ++ appMsgs
            )
                |> sequenceExtra
                    (updateIndexedDb language
                        currentDate
                        currentTime
                        coordinates
                        zscores
                        site
                        features
                        nurseId
                        healthCenterId
                        villageId
                        isChw
                        isLabTech
                        activePage
                        syncManager
                    )
                    extraMsgs

        PatchPerson origin personId person ->
            let
                -- Adding GPS coordinates.
                personWithCoordinates =
                    if gpsCoordinatesEnabled features && person.saveGPSLocation then
                        updatePersonWithCooridnates person coordinates

                    else
                        person
            in
            ( { model | postPerson = Loading }
            , sw.patchFull personEndpoint personId personWithCoordinates
                |> toCmd (RemoteData.fromResult >> HandlePatchedPerson origin personId)
            , []
            )

        HandlePatchedPerson origin personId data ->
            let
                rollbarOnFailure =
                    triggerRollbarOnFailure data

                appMsgs =
                    case origin of
                        InitiatorEditForm ->
                            -- If we succeed, we reset the form, and go to the page
                            -- showing the new person.
                            RemoteData.map
                                (\_ ->
                                    [ Pages.Person.Model.ResetEditForm
                                        |> App.Model.MsgPageEditPerson personId
                                        |> App.Model.MsgLoggedIn
                                    , PersonPage personId ParticipantDirectoryOrigin
                                        |> UserPage
                                        |> App.Model.SetActivePage
                                    ]
                                )
                                data
                                |> RemoteData.withDefault []

                        InitiatorProgressReport ->
                            []
            in
            ( { model | postPerson = Success personId }
            , Cmd.none
            , rollbarOnFailure ++ appMsgs
            )

        PostSession session ->
            ( { model | postSession = Loading }
            , sw.post sessionEndpoint session
                |> toCmd (RemoteData.fromResult >> RemoteData.map Tuple.first >> HandlePostedSession)
            , []
            )

        HandlePostedSession data ->
            let
                rollbarOnFailure =
                    triggerRollbarOnFailure data

                appMsgs =
                    RemoteData.map
                        (\sessionId ->
                            SessionPage sessionId AttendancePage
                                |> UserPage
                                |> App.Model.SetActivePage
                                |> List.singleton
                        )
                        data
                        |> RemoteData.withDefault []
            in
            ( { model | postSession = data }
            , Cmd.none
            , rollbarOnFailure ++ appMsgs
            )

        PostIndividualEncounterParticipant extraData session ->
            ( { model | postIndividualEncounterParticipant = Dict.insert session.person Loading model.postIndividualEncounterParticipant }
            , sw.post individualEncounterParticipantEndpoint session
                |> toCmd (RemoteData.fromResult >> HandlePostedIndividualEncounterParticipant session.person session.encounterType extraData)
            , []
            )

        HandlePostedIndividualEncounterParticipant personId encounterType extraData data ->
            let
                rollbarOnFailure =
                    triggerRollbarOnFailure data

                -- We automatically create new encounter for newly created  session.
                appMsgs =
                    RemoteData.map
                        (\( sessionId, _ ) ->
                            case encounterType of
                                AcuteIllnessEncounter ->
                                    case extraData of
                                        AcuteIllnessData acuteIllnessEncounterType ->
                                            [ emptyAcuteIllnessEncounter sessionId currentDate 1 acuteIllnessEncounterType healthCenterId
                                                |> Backend.Model.PostAcuteIllnessEncounter
                                                |> App.Model.MsgIndexedDb
                                            ]

                                        _ ->
                                            []

                                AntenatalEncounter ->
                                    case extraData of
                                        AntenatalData prenatalEncounterType ->
                                            [ emptyPrenatalEncounter sessionId currentDate prenatalEncounterType healthCenterId
                                                |> Backend.Model.PostPrenatalEncounter DestinationEncounterPage
                                                |> App.Model.MsgIndexedDb
                                            ]

                                        _ ->
                                            []

                                ChildScoreboardEncounter ->
                                    [ Backend.ChildScoreboardEncounter.Model.emptyChildScoreboardEncounter sessionId currentDate healthCenterId
                                        |> Backend.Model.PostChildScoreboardEncounter
                                        |> App.Model.MsgIndexedDb
                                    ]

                                HIVEncounter ->
                                    [ Backend.HIVEncounter.Model.emptyHIVEncounter sessionId currentDate healthCenterId
                                        |> Backend.Model.PostHIVEncounter
                                        |> App.Model.MsgIndexedDb
                                    ]

                                HomeVisitEncounter ->
                                    [ emptyHomeVisitEncounter sessionId currentDate healthCenterId
                                        |> Backend.Model.PostHomeVisitEncounter
                                        |> App.Model.MsgIndexedDb
                                    ]

                                NCDEncounter ->
                                    [ Backend.NCDEncounter.Model.emptyNCDEncounter sessionId currentDate healthCenterId
                                        |> Backend.Model.PostNCDEncounter
                                        |> App.Model.MsgIndexedDb
                                    ]

                                NutritionEncounter ->
                                    case extraData of
                                        NutritionData nutritionEncounterType ->
                                            [ emptyNutritionEncounter sessionId currentDate nutritionEncounterType healthCenterId
                                                |> Backend.Model.PostNutritionEncounter
                                                |> App.Model.MsgIndexedDb
                                            ]

                                        _ ->
                                            []

                                TuberculosisEncounter ->
                                    [ Backend.TuberculosisEncounter.Model.emptyTuberculosisEncounter sessionId currentDate healthCenterId
                                        |> Backend.Model.PostTuberculosisEncounter
                                        |> App.Model.MsgIndexedDb
                                    ]

                                WellChildEncounter ->
                                    case extraData of
                                        WellChildData wellChildEncounterType ->
                                            [ emptyWellChildEncounter sessionId currentDate wellChildEncounterType healthCenterId
                                                |> Backend.Model.PostWellChildEncounter
                                                |> App.Model.MsgIndexedDb
                                            ]

                                        _ ->
                                            []

                                InmmunizationEncounter ->
                                    []
                        )
                        data
                        |> RemoteData.withDefault []
            in
            ( { model | postIndividualEncounterParticipant = Dict.insert personId data model.postIndividualEncounterParticipant }
            , Cmd.none
            , rollbarOnFailure ++ appMsgs
            )

        PostPrenatalEncounter postCreateDestination prenatalEncounter ->
            ( { model | postPrenatalEncounter = Dict.insert prenatalEncounter.participant Loading model.postPrenatalEncounter }
            , sw.post prenatalEncounterEndpoint prenatalEncounter
                |> toCmd (RemoteData.fromResult >> HandlePostedPrenatalEncounter prenatalEncounter.participant postCreateDestination)
            , []
            )

        HandlePostedPrenatalEncounter participantId postCreateDestination data ->
            let
                rollbarOnFailure =
                    triggerRollbarOnFailure data

                appMsgs =
                    RemoteData.map
                        (\( prenatalEncounterId, _ ) ->
                            let
                                destinationPage =
                                    case postCreateDestination of
                                        DestinationEncounterPage ->
                                            UserPage <| Pages.Page.PrenatalEncounterPage prenatalEncounterId

                                        DestinationEncounterPageWithWarningPopup ->
                                            UserPage <| Pages.Page.PrenatalEncounterPage prenatalEncounterId

                                        DestinationClinicalProgressReportPage ->
                                            UserPage <| ClinicalProgressReportPage (InitiatorNewEncounter prenatalEncounterId) prenatalEncounterId

                                setWarningDialogStateMsg =
                                    if postCreateDestination == DestinationEncounterPageWithWarningPopup then
                                        [ Pages.Prenatal.Encounter.Model.SetChwWarningVisible True
                                            |> App.Model.MsgPagePrenatalEncounter prenatalEncounterId
                                            |> App.Model.MsgLoggedIn
                                        ]

                                    else
                                        []
                            in
                            App.Model.SetActivePage destinationPage :: setWarningDialogStateMsg
                        )
                        data
                        |> RemoteData.withDefault []
            in
            ( { model | postPrenatalEncounter = Dict.insert participantId data model.postPrenatalEncounter }
            , Cmd.none
            , rollbarOnFailure ++ appMsgs
            )

        PostNutritionEncounter nutritionEncounter ->
            ( { model | postNutritionEncounter = Dict.insert nutritionEncounter.participant Loading model.postNutritionEncounter }
            , sw.post nutritionEncounterEndpoint nutritionEncounter
                |> toCmd (RemoteData.fromResult >> HandlePostedNutritionEncounter nutritionEncounter.participant)
            , []
            )

        HandlePostedNutritionEncounter participantId data ->
            let
                rollbarOnFailure =
                    triggerRollbarOnFailure data

                appMsgs =
                    RemoteData.map
                        (\( nutritionEncounterId, _ ) ->
                            [ App.Model.SetActivePage <|
                                UserPage <|
                                    Pages.Page.NutritionEncounterPage nutritionEncounterId
                            ]
                        )
                        data
                        |> RemoteData.withDefault []
            in
            ( { model | postNutritionEncounter = Dict.insert participantId data model.postNutritionEncounter }
            , Cmd.none
            , rollbarOnFailure ++ appMsgs
            )

        PostHomeVisitEncounter homeVisitEncounter ->
            ( { model | postHomeVisitEncounter = Dict.insert homeVisitEncounter.participant Loading model.postHomeVisitEncounter }
            , sw.post homeVisitEncounterEndpoint homeVisitEncounter
                |> toCmd (RemoteData.fromResult >> HandlePostedHomeVisitEncounter homeVisitEncounter.participant)
            , []
            )

        HandlePostedHomeVisitEncounter participantId data ->
            let
                rollbarOnFailure =
                    triggerRollbarOnFailure data

                appMsgs =
                    RemoteData.map
                        (\( homeVisitEncounterId, _ ) ->
                            [ App.Model.SetActivePage <|
                                UserPage <|
                                    Pages.Page.HomeVisitEncounterPage homeVisitEncounterId
                            ]
                        )
                        data
                        |> RemoteData.withDefault []
            in
            ( { model | postHomeVisitEncounter = Dict.insert participantId data model.postHomeVisitEncounter }
            , Cmd.none
            , rollbarOnFailure ++ appMsgs
            )

        PostWellChildEncounter wellChildEncounter ->
            ( { model | postWellChildEncounter = Dict.insert wellChildEncounter.participant Loading model.postWellChildEncounter }
            , sw.post wellChildEncounterEndpoint wellChildEncounter
                |> toCmd (RemoteData.fromResult >> HandlePostedWellChildEncounter wellChildEncounter.participant)
            , []
            )

        HandlePostedWellChildEncounter participantId data ->
            let
                rollbarOnFailure =
                    triggerRollbarOnFailure data

                appMsgs =
                    RemoteData.map
                        (\( wellChildEncounterId, _ ) ->
                            [ App.Model.SetActivePage <|
                                UserPage <|
                                    Pages.Page.WellChildEncounterPage wellChildEncounterId
                            ]
                        )
                        data
                        |> RemoteData.withDefault []
            in
            ( { model | postWellChildEncounter = Dict.insert participantId data model.postWellChildEncounter }
            , Cmd.none
            , rollbarOnFailure ++ appMsgs
            )

        PostAcuteIllnessEncounter acuteIllnessEncounter ->
            ( { model | postAcuteIllnessEncounter = Dict.insert acuteIllnessEncounter.participant Loading model.postAcuteIllnessEncounter }
            , sw.post acuteIllnessEncounterEndpoint acuteIllnessEncounter
                |> toCmd (RemoteData.fromResult >> HandlePostedAcuteIllnessEncounter acuteIllnessEncounter.participant)
            , []
            )

        HandlePostedAcuteIllnessEncounter participantId data ->
            let
                rollbarOnFailure =
                    triggerRollbarOnFailure data

                appMsgs =
                    RemoteData.map
                        (\( acuteIllnessEncounterId, _ ) ->
                            [ App.Model.SetActivePage <|
                                UserPage <|
                                    Pages.Page.AcuteIllnessEncounterPage acuteIllnessEncounterId
                            ]
                        )
                        data
                        |> RemoteData.withDefault []
            in
            ( { model | postAcuteIllnessEncounter = Dict.insert participantId data model.postAcuteIllnessEncounter }
            , Cmd.none
            , rollbarOnFailure ++ appMsgs
            )

        PostNCDEncounter ncdEncounter ->
            ( { model | postNCDEncounter = Dict.insert ncdEncounter.participant Loading model.postNCDEncounter }
            , sw.post ncdEncounterEndpoint ncdEncounter
                |> toCmd (RemoteData.fromResult >> HandlePostedNCDEncounter ncdEncounter.participant)
            , []
            )

        HandlePostedNCDEncounter participantId data ->
            let
                rollbarOnFailure =
                    triggerRollbarOnFailure data

                appMsgs =
                    RemoteData.map
                        (\( ncdEncounterId, _ ) ->
                            [ App.Model.SetActivePage <|
                                UserPage <|
                                    Pages.Page.NCDEncounterPage ncdEncounterId
                            ]
                        )
                        data
                        |> RemoteData.withDefault []
            in
            ( { model | postNCDEncounter = Dict.insert participantId data model.postNCDEncounter }
            , Cmd.none
            , rollbarOnFailure ++ appMsgs
            )

        PostChildScoreboardEncounter childScoreboardEncounter ->
            ( { model | postChildScoreboardEncounter = Dict.insert childScoreboardEncounter.participant Loading model.postChildScoreboardEncounter }
            , sw.post childScoreboardEncounterEndpoint childScoreboardEncounter
                |> toCmd (RemoteData.fromResult >> HandlePostedChildScoreboardEncounter childScoreboardEncounter.participant)
            , []
            )

        HandlePostedChildScoreboardEncounter participantId data ->
            let
                rollbarOnFailure =
                    triggerRollbarOnFailure data

                appMsgs =
                    RemoteData.map
                        (\( childScoreboardEncounterId, _ ) ->
                            [ App.Model.SetActivePage <|
                                UserPage <|
                                    Pages.Page.ChildScoreboardEncounterPage childScoreboardEncounterId
                            ]
                        )
                        data
                        |> RemoteData.withDefault []
            in
            ( { model | postChildScoreboardEncounter = Dict.insert participantId data model.postChildScoreboardEncounter }
            , Cmd.none
            , rollbarOnFailure ++ appMsgs
            )

        PostTuberculosisEncounter tuberculosisEncounter ->
            ( { model | postTuberculosisEncounter = Dict.insert tuberculosisEncounter.participant Loading model.postTuberculosisEncounter }
            , sw.post tuberculosisEncounterEndpoint tuberculosisEncounter
                |> toCmd (RemoteData.fromResult >> HandlePostedTuberculosisEncounter tuberculosisEncounter.participant)
            , []
            )

        HandlePostedTuberculosisEncounter participantId data ->
            let
                rollbarOnFailure =
                    triggerRollbarOnFailure data

                appMsgs =
                    RemoteData.map
                        (\( tuberculosisEncounterId, _ ) ->
                            [ App.Model.SetActivePage <|
                                UserPage <|
                                    Pages.Page.TuberculosisEncounterPage tuberculosisEncounterId
                            ]
                        )
                        data
                        |> RemoteData.withDefault []
            in
            ( { model | postTuberculosisEncounter = Dict.insert participantId data model.postTuberculosisEncounter }
            , Cmd.none
            , rollbarOnFailure ++ appMsgs
            )

        PostHIVEncounter hivEncounter ->
            ( { model | postHIVEncounter = Dict.insert hivEncounter.participant Loading model.postHIVEncounter }
            , sw.post hivEncounterEndpoint hivEncounter
                |> toCmd (RemoteData.fromResult >> HandlePostedHIVEncounter hivEncounter.participant)
            , []
            )

        HandlePostedHIVEncounter participantId data ->
            let
                rollbarOnFailure =
                    triggerRollbarOnFailure data

                appMsgs =
                    RemoteData.map
                        (\( hivEncounterId, _ ) ->
                            [ App.Model.SetActivePage <|
                                UserPage <|
                                    Pages.Page.HIVEncounterPage hivEncounterId
                            ]
                        )
                        data
                        |> RemoteData.withDefault []
            in
            ( { model | postHIVEncounter = Dict.insert participantId data model.postHIVEncounter }
            , Cmd.none
            , rollbarOnFailure ++ appMsgs
            )

        PostEducationSession educationSession ->
            ( { model | postEducationSession = Loading }
            , sw.post educationSessionEndpoint educationSession
                |> toCmd (RemoteData.fromResult >> HandlePostedEducationSession)
            , []
            )

        HandlePostedEducationSession data ->
            let
                rollbarOnFailure =
                    triggerRollbarOnFailure data

                appMsgs =
                    RemoteData.map
                        (\( educationSessionId, _ ) ->
                            [ App.Model.SetActivePage <|
                                UserPage <|
                                    Pages.Page.EducationSessionPage educationSessionId
                            ]
                        )
                        data
                        |> RemoteData.withDefault []
            in
            ( { model | postEducationSession = data }
            , Cmd.none
            , rollbarOnFailure ++ appMsgs
            )


{-| The extra return value indicates whether we need to recalculate our
successful EditableSessions. Ideally, we would handle this in a more
nuanced way.
-}
handleRevision :
    NominalDate
    -> Maybe HealthCenterId
    -> Maybe VillageId
    -> Revision
    -> ( ModelIndexedDb, Bool )
    -> ( ModelIndexedDb, Bool )
handleRevision currentDate healthCenterId villageId revision (( model, recalc ) as noChange) =
    let
        encounterAction uuid data =
            if data.deleted then
                Dict.remove uuid

            else
                Dict.update uuid (Maybe.map (always (Success data)))

        measurementActionConsideringDeletedField uuid data =
            if data.deleted then
                Dict.remove uuid

            else
                Dict.insert uuid data
    in
    case revision of
        AcuteFindingsRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | acuteFindings =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        AcuteIllnessContactsTracingRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | contactsTracing =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        AcuteIllnessCoreExamRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | coreExam =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        AcuteIllnessDangerSignsRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | dangerSigns =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        AcuteIllnessEncounterRevision uuid data ->
            let
                acuteIllnessEncounters =
                    encounterAction uuid data model.acuteIllnessEncounters

                acuteIllnessEncountersByParticipant =
                    Dict.remove data.participant model.acuteIllnessEncountersByParticipant
            in
            ( { model
                | acuteIllnessEncounters = acuteIllnessEncounters
                , acuteIllnessEncountersByParticipant = acuteIllnessEncountersByParticipant
              }
            , recalc
            )

        AcuteIllnessFollowUpRevision uuid data ->
            let
                modelWithMappedFollowUp =
                    mapFollowUpMeasurements
                        healthCenterId
                        (\measurements -> { measurements | acuteIllness = measurementActionConsideringDeletedField uuid data measurements.acuteIllness })
                        model
            in
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | followUp =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                modelWithMappedFollowUp
            , recalc
            )

        AcuteIllnessMuacRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | muac =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        AcuteIllnessNutritionRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | nutrition =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        AcuteIllnessTraceContactRevision uuid data ->
            let
                modelWithMappedFollowUp =
                    mapFollowUpMeasurements
                        healthCenterId
                        (\measurements -> { measurements | traceContacts = measurementActionConsideringDeletedField uuid data measurements.traceContacts })
                        model

                traceContacts =
                    encounterAction uuid data model.traceContacts
            in
            ( { modelWithMappedFollowUp | traceContacts = traceContacts }
            , recalc
            )

        AcuteIllnessVitalsRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | vitals =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        AppointmentConfirmationRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | appointmentConfirmation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        AttendanceRevision uuid data ->
            ( mapMotherMeasurements
                data.participantId
                (\measurements -> { measurements | attendances = measurementActionConsideringDeletedField uuid data measurements.attendances })
                model
            , True
            )

        BreastExamRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | breastExam =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        BirthPlanRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | birthPlan =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        Call114Revision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | call114 =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        CatchmentAreaRevision _ _ ->
            noChange

        ChildFbfRevision uuid data ->
            let
                modelWithMappedStockManagement =
                    mapStockManagementMeasurements
                        healthCenterId
                        (\measurements -> { measurements | childFbf = measurementActionConsideringDeletedField uuid data measurements.childFbf })
                        modelWithStockUpdateRecalc

                -- This revision may cause stock management data to become obsolete,
                -- therefore, we 'mark' it for recalculation.
                modelWithStockUpdateRecalc =
                    Maybe.map
                        (\id ->
                            { model | stockManagementData = Dict.insert id NotAsked model.stockManagementData }
                        )
                        healthCenterId
                        |> Maybe.withDefault model
            in
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | fbfs = measurementActionConsideringDeletedField uuid data measurements.fbfs })
                modelWithMappedStockManagement
            , True
            )

        ChildNutritionRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | nutritions = measurementActionConsideringDeletedField uuid data measurements.nutritions })
                model
            , True
            )

        ChildScoreboardEncounterRevision uuid data ->
            let
                childScoreboardEncounters =
                    encounterAction uuid data model.childScoreboardEncounters

                childScoreboardEncountersByParticipant =
                    Dict.remove data.participant model.childScoreboardEncountersByParticipant
            in
            ( { model
                | childScoreboardEncounters = childScoreboardEncounters
                , childScoreboardEncountersByParticipant = childScoreboardEncountersByParticipant
              }
            , recalc
            )

        ChildScoreboardBCGImmunisationRevision uuid data ->
            ( mapChildScoreboardMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | bcgImmunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        ChildScoreboardDTPImmunisationRevision uuid data ->
            ( mapChildScoreboardMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | dtpImmunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        ChildScoreboardDTPStandaloneImmunisationRevision uuid data ->
            ( mapChildScoreboardMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | dtpStandaloneImmunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        ChildScoreboardIPVImmunisationRevision uuid data ->
            ( mapChildScoreboardMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | ipvImmunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        ChildScoreboardMRImmunisationRevision uuid data ->
            ( mapChildScoreboardMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | mrImmunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        ChildScoreboardNCDARevision uuid data ->
            ( mapChildScoreboardMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | ncda =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        ChildScoreboardOPVImmunisationRevision uuid data ->
            ( mapChildScoreboardMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | opvImmunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        ChildScoreboardPCV13ImmunisationRevision uuid data ->
            ( mapChildScoreboardMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | pcv13Immunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        ChildScoreboardRotarixImmunisationRevision uuid data ->
            ( mapChildScoreboardMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | rotarixImmunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        ClinicRevision uuid data ->
            let
                clinics =
                    RemoteData.map (Dict.insert uuid data) model.clinics
            in
            ( { model | clinics = clinics }
            , recalc
            )

        ContributingFactorsRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | contributingFactors = measurementActionConsideringDeletedField uuid data measurements.contributingFactors })
                model
            , True
            )

        CorePhysicalExamRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | corePhysicalExam =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        CounselingScheduleRevision _ _ ->
            -- Just invalidate our value ... if someone wants it, we'll refetch it.
            ( { model | everyCounselingSchedule = NotAsked }
            , True
            )

        CounselingSessionRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | counselingSessions = measurementActionConsideringDeletedField uuid data measurements.counselingSessions })
                model
            , True
            )

        CounselingTopicRevision _ _ ->
            ( { model | everyCounselingSchedule = NotAsked }
            , True
            )

        CovidTestingRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | covidTesting =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        DangerSignsRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | dangerSigns =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        DashboardStatsRevision uuid statsRaw ->
            let
                updatedComputedDashboard =
                    Dict.get uuid model.computedDashboards
                        |> Maybe.map
                            (\computedDashboard ->
                                { statsRaw = statsRaw
                                , assembledPermutations =
                                    Dict.map
                                        (\( programTypeFilter, selectedVillage ) _ ->
                                            Pages.Dashboard.Utils.generateAssembledData currentDate uuid statsRaw model programTypeFilter selectedVillage
                                        )
                                        computedDashboard.assembledPermutations
                                }
                            )
                        |> Maybe.withDefault (generateInitialComputedDashboard currentDate uuid villageId statsRaw model)
            in
            ( { model | computedDashboards = Dict.insert uuid updatedComputedDashboard model.computedDashboards }, recalc )

        EducationSessionRevision uuid data ->
            let
                educationSessions =
                    Dict.update uuid (Maybe.map (always (Success data))) model.educationSessions

                -- For every session participant, we check if it has data at
                -- educationSessionsByPerson dict. If so, we add the session to
                -- that data.
                updatedEducationSessionsByPerson =
                    EverySet.toList data.participants
                        |> List.foldl
                            (\personId accum ->
                                Dict.get personId model.educationSessionsByPerson
                                    |> Maybe.andThen RemoteData.toMaybe
                                    |> Maybe.map
                                        (\sessionsDict ->
                                            let
                                                updatedSessionsDict =
                                                    Dict.insert uuid data sessionsDict
                                            in
                                            Dict.insert personId (Success updatedSessionsDict) accum
                                        )
                                    |> Maybe.withDefault accum
                            )
                            model.educationSessionsByPerson
            in
            ( { model | educationSessions = educationSessions, educationSessionsByPerson = updatedEducationSessionsByPerson }
            , recalc
            )

        ExposureRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | exposure =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        FamilyPlanningRevision uuid data ->
            ( mapMotherMeasurements
                data.participantId
                (\measurements -> { measurements | familyPlannings = measurementActionConsideringDeletedField uuid data measurements.familyPlannings })
                model
            , True
            )

        FollowUpRevision uuid data ->
            let
                modelWithMappedFollowUp =
                    mapFollowUpMeasurements
                        healthCenterId
                        (\measurements -> { measurements | nutritionGroup = measurementActionConsideringDeletedField uuid data measurements.nutritionGroup })
                        model
            in
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | followUp = measurementActionConsideringDeletedField uuid data measurements.followUp })
                modelWithMappedFollowUp
            , True
            )

        GroupHealthEducationRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | healthEducation = measurementActionConsideringDeletedField uuid data measurements.healthEducation })
                model
            , True
            )

        GroupNCDARevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | ncda = measurementActionConsideringDeletedField uuid data measurements.ncda })
                model
            , True
            )

        GroupSendToHCRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | sendToHC = measurementActionConsideringDeletedField uuid data measurements.sendToHC })
                model
            , True
            )

        HCContactRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | hcContact =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        HealthCenterRevision uuid data ->
            let
                healthCenters =
                    RemoteData.map (Dict.insert uuid data) model.healthCenters
            in
            ( { model | healthCenters = healthCenters }
            , recalc
            )

        HealthEducationRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | healthEducation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        HeightRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | heights = measurementActionConsideringDeletedField uuid data measurements.heights })
                model
            , True
            )

        HIVDiagnosticsRevision uuid data ->
            ( mapHIVMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | diagnostics =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        HIVEncounterRevision uuid data ->
            let
                hivEncounters =
                    encounterAction uuid data model.hivEncounters

                hivEncountersByParticipant =
                    Dict.remove data.participant model.hivEncountersByParticipant
            in
            ( { model
                | hivEncounters = hivEncounters
                , hivEncountersByParticipant = hivEncountersByParticipant
              }
            , recalc
            )

        HIVFollowUpRevision uuid data ->
            let
                modelWithMappedFollowUp =
                    mapFollowUpMeasurements
                        healthCenterId
                        (\measurements -> { measurements | hiv = measurementActionConsideringDeletedField uuid data measurements.hiv })
                        model
            in
            ( mapHIVMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | followUp =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                modelWithMappedFollowUp
            , recalc
            )

        HIVHealthEducationRevision uuid data ->
            ( mapHIVMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | healthEducation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        HIVMedicationRevision uuid data ->
            ( mapHIVMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | medication =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        HIVReferralRevision uuid data ->
            ( mapHIVMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | referral =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        HIVSymptomReviewRevision uuid data ->
            ( mapHIVMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | symptomReview =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        HIVTreatmentReviewRevision uuid data ->
            ( mapHIVMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | treatmentReview =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        HomeVisitEncounterRevision uuid data ->
            let
                homeVisitEncounters =
                    encounterAction uuid data model.homeVisitEncounters

                homeVisitEncountersByParticipant =
                    Dict.remove data.participant model.homeVisitEncountersByParticipant
            in
            ( { model
                | homeVisitEncounters = homeVisitEncounters
                , homeVisitEncountersByParticipant = homeVisitEncountersByParticipant
              }
            , recalc
            )

        IsolationRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | isolation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        IndividualEncounterParticipantRevision uuid data ->
            let
                individualParticipants =
                    encounterAction uuid data model.individualParticipants

                individualParticipantsByPerson =
                    Dict.remove data.person model.individualParticipantsByPerson
            in
            ( { model
                | individualParticipants = individualParticipants
                , individualParticipantsByPerson = individualParticipantsByPerson
              }
            , recalc
            )

        LactationRevision uuid data ->
            ( mapMotherMeasurements
                data.participantId
                (\measurements -> { measurements | lactations = measurementActionConsideringDeletedField uuid data measurements.lactations })
                model
            , True
            )

        LastMenstrualPeriodRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | lastMenstrualPeriod =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        MalariaTestingRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | malariaTesting =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        MedicalHistoryRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | medicalHistory =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        MedicationRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | medication =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        MedicationDistributionRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | medicationDistribution =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        MotherFbfRevision uuid data ->
            let
                modelWithMappedStockManagement =
                    mapStockManagementMeasurements
                        healthCenterId
                        (\measurements -> { measurements | motherFbf = measurementActionConsideringDeletedField uuid data measurements.motherFbf })
                        modelWithStockUpdateRecalc

                -- This revision may cause stock management data to become obsolete,
                -- therefore, we 'mark' it for recalculation.
                modelWithStockUpdateRecalc =
                    Maybe.map
                        (\id ->
                            { model | stockManagementData = Dict.insert id NotAsked model.stockManagementData }
                        )
                        healthCenterId
                        |> Maybe.withDefault model
            in
            ( mapMotherMeasurements
                data.participantId
                (\measurements -> { measurements | fbfs = measurementActionConsideringDeletedField uuid data measurements.fbfs })
                modelWithMappedStockManagement
            , True
            )

        MuacRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | muacs = measurementActionConsideringDeletedField uuid data measurements.muacs })
                model
            , True
            )

        NCDCoMorbiditiesRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | coMorbidities =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDCoreExamRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | coreExam =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDCreatinineTestRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | creatinineTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDDangerSignsRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | dangerSigns =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDEncounterRevision uuid data ->
            let
                ncdEncounters =
                    encounterAction uuid data model.ncdEncounters

                ncdEncountersByParticipant =
                    Dict.remove data.participant model.ncdEncountersByParticipant
            in
            ( { model
                | ncdEncounters = ncdEncounters
                , ncdEncountersByParticipant = ncdEncountersByParticipant
              }
            , recalc
            )

        NCDFamilyHistoryRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | familyHistory =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDFamilyPlanningRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | familyPlanning =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDHbA1cTestRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | hba1cTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDHealthEducationRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | healthEducation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDHIVTestRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | hivTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDLabsResultsRevision uuid data ->
            let
                modelWithMappedFollowUp =
                    mapFollowUpMeasurements
                        healthCenterId
                        (\measurements -> { measurements | ncdLabs = measurementActionConsideringDeletedField uuid data measurements.ncdLabs })
                        model
            in
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | labsResults =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                modelWithMappedFollowUp
            , recalc
            )

        NCDLipidPanelTestRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | lipidPanelTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDLiverFunctionTestRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | liverFunctionTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDMedicationDistributionRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | medicationDistribution =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDMedicationHistoryRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | medicationHistory =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDOutsideCareRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | outsideCare =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDPregnancyTestRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | pregnancyTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDRandomBloodSugarTestRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | randomBloodSugarTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDReferralRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | referral =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDSocialHistoryRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | socialHistory =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDSymptomReviewRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | symptomReview =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDUrineDipstickTestRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | urineDipstickTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NCDVitalsRevision uuid data ->
            ( mapNCDMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | vitals =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NurseRevision _ _ ->
            -- Nothing to do in ModelIndexedDb yet. App.Update does do something with this one.
            noChange

        NutritionCaringRevision uuid data ->
            ( mapHomeVisitMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | caring =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NutritionContributingFactorsRevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | contributingFactors =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NutritionEncounterRevision uuid data ->
            let
                nutritionEncounters =
                    encounterAction uuid data model.nutritionEncounters

                nutritionEncountersByParticipant =
                    Dict.remove data.participant model.nutritionEncountersByParticipant
            in
            ( { model
                | nutritionEncounters = nutritionEncounters
                , nutritionEncountersByParticipant = nutritionEncountersByParticipant
              }
            , recalc
            )

        NutritionFeedingRevision uuid data ->
            ( mapHomeVisitMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | feeding =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NutritionFollowUpRevision uuid data ->
            let
                modelWithMappedFollowUp =
                    mapFollowUpMeasurements
                        healthCenterId
                        (\measurements -> { measurements | nutritionIndividual = measurementActionConsideringDeletedField uuid data measurements.nutritionIndividual })
                        model
            in
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | followUp =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                modelWithMappedFollowUp
            , recalc
            )

        NutritionFoodSecurityRevision uuid data ->
            ( mapHomeVisitMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | foodSecurity =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NutritionHealthEducationRevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | healthEducation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NutritionHeightRevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | height =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NutritionHygieneRevision uuid data ->
            ( mapHomeVisitMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | hygiene =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NutritionMuacRevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | muac =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NutritionNCDARevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | ncda =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NutritionNutritionRevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | nutrition =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NutritionPhotoRevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | photo =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NutritionSendToHCRevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | sendToHC =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        NutritionWeightRevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | weight =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        ObstetricalExamRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | obstetricalExam =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        ObstetricHistoryRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | obstetricHistory =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        ObstetricHistoryStep2Revision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | obstetricHistoryStep2 =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        ParticipantConsentRevision uuid data ->
            ( mapMotherMeasurements
                data.participantId
                (\measurements -> { measurements | consents = measurementActionConsideringDeletedField uuid data measurements.consents })
                model
            , True
            )

        ParticipantFormRevision uuid data ->
            ( { model | participantForms = RemoteData.map (Dict.insert uuid data) model.participantForms }
            , True
            )

        PersonRevision uuid data ->
            let
                people =
                    encounterAction uuid data model.people
            in
            ( { model
                | personSearchesByName = Dict.empty
                , personSearchesByNationalId = Dict.empty
                , people = people
              }
            , True
            )

        PhotoRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | photos = measurementActionConsideringDeletedField uuid data measurements.photos })
                model
            , True
            )

        PmtctParticipantRevision _ data ->
            ( { model
                | expectedSessions =
                    model.expectedSessions
                        |> Dict.remove data.child
                        |> Dict.remove data.adult
                , expectedParticipants =
                    Dict.empty
                , participantsByPerson =
                    model.participantsByPerson
                        |> Dict.remove data.child
                        |> Dict.remove data.adult
              }
            , True
            )

        PregnancyTestRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | pregnancyTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalAspirinRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | aspirin = Just ( uuid, data ) })
                model
            , recalc
            )

        PrenatalBloodGpRsTestRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | bloodGpRsTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalBreastfeedingRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | breastfeeding =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalCalciumRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | calcium =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalEncounterRevision uuid data ->
            let
                prenatalEncounters =
                    encounterAction uuid data model.prenatalEncounters

                prenatalEncountersByParticipant =
                    Dict.remove data.participant model.prenatalEncountersByParticipant
            in
            ( { model
                | prenatalEncounters = prenatalEncounters
                , prenatalEncountersByParticipant = prenatalEncountersByParticipant
              }
            , recalc
            )

        PrenatalFamilyPlanningRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | familyPlanning =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalFefolRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | fefol = Just ( uuid, data ) })
                model
            , recalc
            )

        PrenatalFolateRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | folate =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalFollowUpRevision uuid data ->
            let
                modelWithMappedFollowUp =
                    mapFollowUpMeasurements
                        healthCenterId
                        (\measurements -> { measurements | prenatal = measurementActionConsideringDeletedField uuid data measurements.prenatal })
                        model
            in
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | followUp =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                modelWithMappedFollowUp
            , recalc
            )

        PrenatalGUExamRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | guExam =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalHealthEducationRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | healthEducation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalHemoglobinTestRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | hemoglobinTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalHepatitisBTestRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | hepatitisBTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalHIVTestRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | hivTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalHIVPCRTestRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | hivPCRTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalIronRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | iron =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalLabsResultsRevision uuid data ->
            let
                modelWithMappedFollowUp =
                    mapFollowUpMeasurements
                        healthCenterId
                        (\measurements -> { measurements | prenatalLabs = measurementActionConsideringDeletedField uuid data measurements.prenatalLabs })
                        model
            in
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | labsResults =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                modelWithMappedFollowUp
            , recalc
            )

        PrenatalMalariaTestRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | malariaTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalMebendazoleRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | mebendazole =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalMentalHealthRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | mentalHealth =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalMedicationDistributionRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | medicationDistribution =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalMMSRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | mms =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalNutritionRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | nutrition =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalOutsideCareRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | outsideCare =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalPartnerHIVTestRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | partnerHIVTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalPhotoRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | prenatalPhoto =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalRandomBloodSugarTestRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | randomBloodSugarTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalSendToHCRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | sendToHC =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalSpecialityCareRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | specialityCare =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalSymptomReviewRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | symptomReview =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalSyphilisTestRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | syphilisTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalTetanusImmunisationRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | tetanusImmunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        PrenatalUrineDipstickTestRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | urineDipstickTest =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        RelationshipRevision _ _ ->
            ( { model | relationshipsByPerson = Dict.empty }
            , True
            )

        ResilienceSurveyRevision uuid data ->
            let
                nurseSurveys =
                    Dict.get data.nurse model.resilienceSurveysByNurse
                        |> Maybe.andThen RemoteData.toMaybe

                resilienceSurveysByNurse =
                    Maybe.map
                        (\surveys ->
                            let
                                updatedSurveys =
                                    Dict.insert uuid data surveys
                            in
                            Dict.insert data.nurse (Success updatedSurveys) model.resilienceSurveysByNurse
                        )
                        nurseSurveys
                        |> Maybe.withDefault (Dict.insert data.nurse (Success (Dict.singleton uuid data)) model.resilienceSurveysByNurse)
            in
            ( { model | resilienceSurveysByNurse = resilienceSurveysByNurse }
            , recalc
            )

        MalariaPreventionRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | malariaPrevention =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        SendToHCRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | sendToHC =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        SessionRevision uuid data ->
            let
                -- First, remove the session from all clinics (it might
                -- previously have been in). Then, add it in the right place.
                sessionsByClinic =
                    let
                        cleaned =
                            Dict.map (always (RemoteData.map (Dict.remove uuid))) model.sessionsByClinic
                    in
                    if data.deleted then
                        cleaned

                    else
                        Dict.update data.clinicId (Maybe.map (RemoteData.map (Dict.insert uuid data))) cleaned
            in
            ( { model
                | sessionsByClinic = sessionsByClinic
                , expectedParticipants = Dict.remove uuid model.expectedParticipants
                , expectedSessions = Dict.empty
                , sessions =
                    if data.deleted then
                        Dict.remove uuid model.sessions

                    else
                        Dict.insert uuid (Success data) model.sessions
              }
            , True
            )

        SocialHistoryRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | socialHistory =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        StockUpdateRevision uuid data ->
            let
                modelWithMappedStockManagement =
                    mapStockManagementMeasurements
                        healthCenterId
                        (\measurements -> { measurements | stockUpdate = Dict.insert uuid data measurements.stockUpdate })
                        modelWithStockUpdateRecalc

                -- This revision may cause stock management data to become obsolete,
                -- therefore, we 'mark' it for recalculation.
                modelWithStockUpdateRecalc =
                    Maybe.map
                        (\id ->
                            { model | stockManagementData = Dict.insert id NotAsked model.stockManagementData }
                        )
                        healthCenterId
                        |> Maybe.withDefault model
            in
            ( { modelWithMappedStockManagement
                | stockUpdates =
                    RemoteData.map (Dict.insert uuid data)
                        modelWithMappedStockManagement.stockUpdates
              }
            , recalc
            )

        SymptomsGeneralRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | symptomsGeneral =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        SymptomsGIRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | symptomsGI =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        SymptomsRespiratoryRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | symptomsRespiratory =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        TravelHistoryRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | travelHistory =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        TreatmentOngoingRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | treatmentOngoing =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        TreatmentReviewRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | treatmentReview =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        TuberculosisDiagnosticsRevision uuid data ->
            ( mapTuberculosisMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | diagnostics =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        TuberculosisDOTRevision uuid data ->
            ( mapTuberculosisMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | dot =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        TuberculosisEncounterRevision uuid data ->
            let
                tuberculosisEncounters =
                    encounterAction uuid data model.tuberculosisEncounters

                tuberculosisEncountersByParticipant =
                    Dict.remove data.participant model.tuberculosisEncountersByParticipant
            in
            ( { model
                | tuberculosisEncounters = tuberculosisEncounters
                , tuberculosisEncountersByParticipant = tuberculosisEncountersByParticipant
              }
            , recalc
            )

        TuberculosisFollowUpRevision uuid data ->
            let
                modelWithMappedFollowUp =
                    mapFollowUpMeasurements
                        healthCenterId
                        (\measurements -> { measurements | tuberculosis = measurementActionConsideringDeletedField uuid data measurements.tuberculosis })
                        model
            in
            ( mapTuberculosisMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | followUp =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                modelWithMappedFollowUp
            , recalc
            )

        TuberculosisHealthEducationRevision uuid data ->
            ( mapTuberculosisMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | healthEducation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        TuberculosisMedicationRevision uuid data ->
            ( mapTuberculosisMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | medication =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        TuberculosisReferralRevision uuid data ->
            ( mapTuberculosisMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | referral =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        TuberculosisSymptomReviewRevision uuid data ->
            ( mapTuberculosisMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | symptomReview =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        TuberculosisTreatmentReviewRevision uuid data ->
            ( mapTuberculosisMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | treatmentReview =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        VillageRevision uuid data ->
            let
                villages =
                    RemoteData.map (Dict.insert uuid data) model.villages
            in
            ( { model | villages = villages }
            , recalc
            )

        VitalsRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | vitals =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WeightRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | weights = measurementActionConsideringDeletedField uuid data measurements.weights })
                model
            , True
            )

        WellChildAlbendazoleRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | albendazole =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildBCGImmunisationRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | bcgImmunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildCaringRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | caring =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildContributingFactorsRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | contributingFactors =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildDTPImmunisationRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | dtpImmunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildDTPStandaloneImmunisationRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | dtpStandaloneImmunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildECDRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | ecd =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildEncounterRevision uuid data ->
            let
                wellChildEncounters =
                    encounterAction uuid data model.wellChildEncounters

                wellChildEncountersByParticipant =
                    Dict.remove data.participant model.wellChildEncountersByParticipant
            in
            ( { model
                | wellChildEncounters = wellChildEncounters
                , wellChildEncountersByParticipant = wellChildEncountersByParticipant
              }
            , recalc
            )

        WellChildFeedingRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | feeding =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildFollowUpRevision uuid data ->
            let
                modelWithMappedFollowUp =
                    mapFollowUpMeasurements
                        healthCenterId
                        (\measurements -> { measurements | wellChild = measurementActionConsideringDeletedField uuid data measurements.wellChild })
                        model
            in
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | followUp =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                modelWithMappedFollowUp
            , recalc
            )

        WellChildFoodSecurityRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | foodSecurity =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildHeadCircumferenceRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | headCircumference =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildHealthEducationRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | healthEducation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildHeightRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | height =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildHygieneRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | hygiene =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildHPVImmunisationRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | hpvImmunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildIPVImmunisationRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | ipvImmunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildMebendezoleRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | mebendezole =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildMRImmunisationRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | mrImmunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildMuacRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | muac =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildNCDARevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | ncda =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildNextVisitRevision uuid data ->
            let
                modelWithMappedFollowUp =
                    mapFollowUpMeasurements
                        healthCenterId
                        (\measurements ->
                            { measurements | nextVisit = measurementActionConsideringDeletedField uuid data measurements.nextVisit }
                        )
                        model
            in
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | nextVisit =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                modelWithMappedFollowUp
            , recalc
            )

        WellChildNutritionRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | nutrition =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildOPVImmunisationRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | opvImmunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildPCV13ImmunisationRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | pcv13Immunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildPhotoRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | photo =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildPregnancySummaryRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | pregnancySummary =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildRotarixImmunisationRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | rotarixImmunisation =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildSendToHCRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | sendToHC =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildSymptomsReviewRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | symptomsReview =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildVitalsRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | vitals =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildVitaminARevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | vitaminA =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )

        WellChildWeightRevision uuid data ->
            ( mapWellChildMeasurements
                data.encounterId
                (\measurements ->
                    { measurements
                        | weight =
                            if data.deleted then
                                Nothing

                            else
                                Just ( uuid, data )
                    }
                )
                model
            , recalc
            )


generatePrenatalAssessmentMsgs :
    NominalDate
    -> Language
    -> Site
    -> Bool
    -> Bool
    -> Page
    -> Bool
    -> Maybe ( PrenatalEncounterId, List PrenatalDiagnosis )
    -> ModelIndexedDb
    -> PrenatalEncounterId
    -> List App.Model.Msg
generatePrenatalAssessmentMsgs currentDate language site isChw isLabTech activePage updateAssesment originData after id =
    Maybe.map
        (\assembledAfter ->
            let
                mandatoryActivitiesCompleted =
                    Pages.Prenatal.Activity.Utils.mandatoryActivitiesForAssessmentCompleted
                        currentDate
                        site
                        assembledAfter

                initialEncounterNextStepsMsg =
                    PrenatalActivityPage id Backend.PrenatalActivity.Model.NextSteps
                        |> UserPage
                        |> App.Model.SetActivePage

                initialEncounterWarningPopupMsg state =
                    Pages.Prenatal.Activity.Model.SetWarningPopupState (Just <| WarningPopupUrgent state)
                        |> App.Model.MsgPagePrenatalActivity id Backend.PrenatalActivity.Model.NextSteps
                        |> App.Model.MsgLoggedIn

                recurrentEncounterWarningPopupMsg state =
                    Pages.Prenatal.RecurrentActivity.Model.SetWarningPopupState (Just <| WarningPopupUrgent state)
                        |> App.Model.MsgPagePrenatalRecurrentActivity id Backend.PrenatalActivity.Model.RecurrentNextSteps
                        |> App.Model.MsgLoggedIn
            in
            if isChw then
                let
                    updateAssesmentMsg =
                        if updateAssesment then
                            assembledAfter.measurements.followUp
                                |> Maybe.map
                                    (\( measurementId, measurement ) ->
                                        let
                                            updatedValue =
                                                measurement.value
                                                    |> (\value ->
                                                            { value
                                                                | assesment = Pages.Prenatal.Activity.Utils.generatePrenatalAssesmentForChw assembledAfter
                                                            }
                                                       )
                                        in
                                        Backend.PrenatalEncounter.Model.SaveFollowUp assembledAfter.participant.person (Just measurementId) updatedValue
                                            |> Backend.Model.MsgPrenatalEncounter id
                                            |> App.Model.MsgIndexedDb
                                            |> List.singleton
                                    )
                                |> Maybe.withDefault []

                        else
                            []
                in
                if not mandatoryActivitiesCompleted then
                    -- Assement is done only when all mandatory measurements were recorded.
                    -- However, since there are more mandatory activities when there are
                    -- no danger signs, we will try to update assement of follow up anyway.
                    updateAssesmentMsg

                else
                    let
                        dangerSignsList =
                            Pages.Prenatal.Activity.Utils.generateDangerSignsListForChw language
                                assembledAfter
                    in
                    if List.isEmpty dangerSignsList then
                        updateAssesmentMsg

                    else
                        updateAssesmentMsg
                            ++ [ -- Navigate to Next Steps activty page.
                                 initialEncounterNextStepsMsg
                               , initialEncounterWarningPopupMsg
                                    ( String.join ", " dangerSignsList
                                    , translate language Translate.EmergencyReferralHelperReferToHC
                                    )
                               ]

            else
            -- Logic for nurse.
            if
                mandatoryActivitiesCompleted
            then
                let
                    diagnosesBefore =
                        -- At this stage new diagnoses were not updated yet, therefore,
                        -- we can use the diganoses set for the encounter.
                        assembledAfter.encounter.diagnoses

                    diagnosesAfter =
                        Pages.Prenatal.Activity.Utils.generatePrenatalDiagnosesForNurse currentDate assembledAfter
                in
                if everySetsEqual diagnosesBefore diagnosesAfter then
                    []

                else
                    -- Here we know that trigger for update came form another encounter.
                    -- Therefore, we only need to perform actions for that originating encounter.
                    let
                        addedDiagnoses =
                            EverySet.diff diagnosesAfter diagnosesBefore
                                |> EverySet.toList
                    in
                    Maybe.map
                        (\( originatingEncounterId, targetDiagnoses ) ->
                            let
                                reportedDiagnoses =
                                    List.filter (\diagnosis -> List.member diagnosis targetDiagnoses) addedDiagnoses
                                        |> EverySet.fromList
                            in
                            if EverySet.isEmpty reportedDiagnoses then
                                []

                            else
                                let
                                    diabetesDiagnosed =
                                        EverySet.toList reportedDiagnoses
                                            |> List.any
                                                (\diagnosis ->
                                                    List.member diagnosis Pages.Prenatal.Utils.diabetesDiagnoses
                                                )

                                    rhNegativeDiagnosis =
                                        EverySet.member DiagnosisRhesusNegativeRecurrentPhase reportedDiagnoses
                                in
                                if
                                    (-- Reporting back about previous diagnosis results in hospital referral
                                     -- at Next steps. For Diabetes, we have logic saying that when it's first
                                     -- diagnosed, the patient is referred to hospital.
                                     -- On next occasions, no next steps are required.
                                     -- Therefore, if we know that Diabetes was already diagnosed, we will not
                                     -- report back about this diagnosis, to prevent unnecessary referral to the hospital.
                                     diabetesDiagnosed
                                        && Pages.Prenatal.Utils.diagnosedPreviouslyAnyOf Pages.Prenatal.Utils.diabetesDiagnoses assembledAfter
                                    )
                                        || (-- Reporting back about previous diagnosis results in hospital referral
                                            -- at Next steps.
                                            -- Even though the Blood group and rhesus test are supposed to be run once,
                                            -- we continue offering it until we get a result.
                                            -- This way, theoretically, it's possible to have multiple tests pending,
                                            -- and results can be entered multiple times.
                                            -- Therefore, if we know that Rhesus Negative was already diagnosed, we will not
                                            -- report back about this diagnosis, to prevent unnecessary referral to the hospital.
                                            rhNegativeDiagnosis
                                                && Pages.Prenatal.Utils.diagnosedPreviouslyAnyOf
                                                    [ DiagnosisRhesusNegativeInitialPhase
                                                    , DiagnosisRhesusNegativeRecurrentPhase
                                                    ]
                                                    assembledAfter
                                           )
                                then
                                    []

                                else
                                    Backend.PrenatalEncounter.Model.SetPastPrenatalDiagnoses reportedDiagnoses
                                        |> Backend.Model.MsgPrenatalEncounter originatingEncounterId
                                        |> App.Model.MsgIndexedDb
                                        |> List.singleton
                        )
                        originData
                        |> -- No originating encounter data, therefore, perform
                           -- required actions for current encounter.
                           Maybe.withDefault
                            (let
                                updateDiagnosesMsg =
                                    Backend.PrenatalEncounter.Model.SetPrenatalDiagnoses diagnosesAfter
                                        |> Backend.Model.MsgPrenatalEncounter id
                                        |> App.Model.MsgIndexedDb

                                initialEncounterMsgs =
                                    let
                                        urgentDiagnoses =
                                            List.filter
                                                Pages.Prenatal.Encounter.Utils.diagnosisRequiresEmergencyReferal
                                                addedDiagnoses

                                        additionalMsgs =
                                            if isLabTech || List.isEmpty urgentDiagnoses then
                                                []

                                            else
                                                -- View warning popup with instructions for Emergency
                                                -- Referral and navigate to Next Steps activity.
                                                [ Pages.Prenatal.Activity.Utils.resolveWarningPopupContentForUrgentDiagnoses
                                                    language
                                                    urgentDiagnoses
                                                    |> initialEncounterWarningPopupMsg
                                                , initialEncounterNextStepsMsg
                                                ]
                                    in
                                    -- These messages are sent when diagnoses set has changed.
                                    -- Therefore, in any case, we need to send command to update
                                    -- diagnoses set.
                                    updateDiagnosesMsg
                                        :: -- For urgent diagnoses, we show warning popup and
                                           -- navigate to Next Steps activity.
                                           additionalMsgs

                                recurrentEncounterMsgs =
                                    let
                                        urgentDiagnoses =
                                            List.filter
                                                Pages.Prenatal.RecurrentActivity.Utils.diagnosisRequiresEmergencyReferal
                                                addedDiagnoses

                                        additionalMsgs =
                                            if isLabTech || List.isEmpty urgentDiagnoses then
                                                []

                                            else
                                                [ PrenatalRecurrentActivityPage id Backend.PrenatalActivity.Model.RecurrentNextSteps
                                                    |> UserPage
                                                    |> App.Model.SetActivePage
                                                , Pages.Prenatal.Activity.Utils.resolveWarningPopupContentForUrgentDiagnoses
                                                    language
                                                    urgentDiagnoses
                                                    |> recurrentEncounterWarningPopupMsg
                                                ]
                                    in
                                    -- These messages are sent when diagnoses set has changed.
                                    -- Therefore, in any case, we need to send command to update
                                    -- diagnoses set.
                                    updateDiagnosesMsg
                                        :: -- For urgent diagnoses, we show warning popup and
                                           -- navigate to Next Steps activity.
                                           additionalMsgs
                             in
                             case activePage of
                                UserPage (PrenatalEncounterPage _) ->
                                    initialEncounterMsgs

                                UserPage (PrenatalActivityPage _ _) ->
                                    initialEncounterMsgs

                                UserPage (ClinicalProgressReportPage (InitiatorEncounterPage _) _) ->
                                    initialEncounterMsgs

                                UserPage (PrenatalRecurrentEncounterPage _) ->
                                    recurrentEncounterMsgs

                                UserPage (PrenatalRecurrentActivityPage _ _) ->
                                    recurrentEncounterMsgs

                                UserPage (ClinicalProgressReportPage (InitiatorRecurrentEncounterPage _) _) ->
                                    recurrentEncounterMsgs

                                _ ->
                                    []
                            )

            else
                []
        )
        (RemoteData.toMaybe <| Pages.Prenatal.Encounter.Utils.generateAssembledData id after)
        |> Maybe.withDefault []


generatePrenatalLabsTestAddedMsgs :
    NominalDate
    -> ModelIndexedDb
    -> Backend.Measurement.Model.LaboratoryTest
    -> Backend.Measurement.Model.TestExecutionNote
    -> PrenatalEncounterId
    -> List App.Model.Msg
generatePrenatalLabsTestAddedMsgs currentDate after test executionNote id =
    Pages.Prenatal.Encounter.Utils.generateAssembledData id after
        |> RemoteData.toMaybe
        |> Maybe.map
            (\assembled ->
                if
                    -- We allow Vitals recheck only during initial encounter.
                    -- At subsequent encounter, no need to retake blood preasure.
                    (test == Backend.Measurement.Model.TestVitalsRecheck)
                        && (not <| List.isEmpty assembled.nursePreviousEncountersData)
                then
                    []

                else
                    let
                        testExecuted =
                            testPerformedByExecutionNote executionNote
                    in
                    Maybe.map
                        (\( resultsId, measurement ) ->
                            let
                                updatedValue =
                                    (\value ->
                                        { value
                                            | performedTests =
                                                if testExecuted then
                                                    EverySet.insert test value.performedTests

                                                else
                                                    EverySet.remove test value.performedTests

                                            -- When we have universal form, it's possible to change if test is performed
                                            -- at lab, or at point of care.
                                            -- Therefore, we need to make sure that test does not appear as completed,
                                            -- which can happen, when nurse switches from point of care to lab.
                                            , completedTests = EverySet.remove test value.completedTests
                                            , resolutionDate = Date.add Days labExpirationPeriod currentDate
                                        }
                                    )
                                        measurement.value
                            in
                            [ savePrenatalLabsResultsMsg id assembled.participant.person (Just resultsId) updatedValue ]
                        )
                        assembled.measurements.labsResults
                        |> Maybe.withDefault
                            (if testExecuted then
                                let
                                    resultsValue =
                                        Backend.Measurement.Model.LabsResultsValue
                                            (EverySet.singleton test)
                                            EverySet.empty
                                            (Date.add Days labExpirationPeriod currentDate)
                                            False
                                            Nothing
                                            Nothing
                                in
                                [ savePrenatalLabsResultsMsg id assembled.participant.person Nothing resultsValue ]

                             else
                                []
                            )
            )
        |> Maybe.withDefault []


generatePrenatalLabsResultsAddedMsgs :
    NominalDate
    -> Bool
    -> ModelIndexedDb
    -> Backend.Measurement.Model.LaboratoryTest
    -> Maybe (EverySet TestPrerequisite)
    -> PrenatalEncounterId
    -> List App.Model.Msg
generatePrenatalLabsResultsAddedMsgs currentDate isLabTech after test testPrerequisites id =
    Pages.Prenatal.Encounter.Utils.generateAssembledData id after
        |> RemoteData.toMaybe
        |> Maybe.andThen
            (\assembled ->
                Maybe.map
                    (\( resultsId, results ) ->
                        let
                            ( performedTests, completedTests ) =
                                Pages.GlobalCaseManagement.Utils.labsResultsTestData currentDate results

                            updatedValue =
                                (\value ->
                                    let
                                        -- Since lab results can be entered right away (at poit of care),
                                        -- or at latter stage (after test was ordered at lab), we
                                        -- update both performed and completed tests.
                                        -- Since data structure is EverySet, it will not create duplicacies.
                                        updatedPerformedTests =
                                            EverySet.insert test performedTests

                                        updatedCompletedTests =
                                            EverySet.insert test completedTests

                                        allLabsCompleted =
                                            EverySet.size updatedCompletedTests == EverySet.size updatedPerformedTests

                                        updatedTestsWithFollowUp =
                                            -- Mark tests which results were entered by Lab Tech, and got
                                            -- follow up questions that will have to be completed by nurse.
                                            if isLabTech && List.member test [ TestHIV, TestSyphilis, TestPartnerHIV ] then
                                                Maybe.map (EverySet.insert test >> Just) value.testsWithFollowUp
                                                    |> Maybe.withDefault (Just <| EverySet.singleton test)

                                            else
                                                value.testsWithFollowUp

                                        reviewState =
                                            if isLabTech && isNothing value.reviewState && allLabsCompleted then
                                                -- For lab technician, request review if all labs were
                                                -- completed, and review state was not set previously.
                                                Just LabsResultsReviewRequested

                                            else if not isLabTech && allLabsCompleted then
                                                -- For nurse, set review state to completed,
                                                -- if all labs were completed.
                                                Just LabsResultsReviewCompleted

                                            else
                                                value.reviewState

                                        resolutionDate =
                                            if not isLabTech && allLabsCompleted then
                                                currentDate

                                            else
                                                value.resolutionDate
                                    in
                                    { value
                                        | performedTests = updatedPerformedTests
                                        , completedTests = updatedCompletedTests
                                        , testsWithFollowUp = updatedTestsWithFollowUp
                                        , reviewState = reviewState
                                        , resolutionDate = resolutionDate
                                    }
                                )
                                    results.value
                        in
                        [ savePrenatalLabsResultsMsg id assembled.participant.person (Just resultsId) updatedValue ]
                    )
                    assembled.measurements.labsResults
            )
        |> Maybe.withDefault []


generatePrenatalInitialPhaseCompletedMsgs :
    NominalDate
    -> Site
    -> ModelIndexedDb
    -> PrenatalEncounterId
    -> List App.Model.Msg
generatePrenatalInitialPhaseCompletedMsgs currentDate site after id =
    Pages.Prenatal.Encounter.Utils.generateAssembledData id after
        |> RemoteData.toMaybe
        |> Maybe.map
            (\assembled_ ->
                let
                    -- Since diagnostics is performed at Backend.Update, and set using Msg,
                    -- we don't have it set at generated assembled data.
                    -- Therefore, we update diagnoses manually and set them into data.
                    encounterWithDiagnoses =
                        (\encounter ->
                            { encounter
                                | diagnoses =
                                    Pages.Prenatal.Activity.Utils.generatePrenatalDiagnosesForNurse currentDate assembled_
                            }
                        )
                            assembled_.encounter

                    assembled =
                        { assembled_ | encounter = encounterWithDiagnoses }

                    ( _, pendingActivities ) =
                        Pages.Prenatal.Encounter.Utils.getAllActivities assembled
                            |> List.filter (Pages.Prenatal.Activity.Utils.expectActivity currentDate site assembled)
                            |> List.partition (Pages.Prenatal.Activity.Utils.activityCompleted currentDate site assembled)
                in
                if List.isEmpty pendingActivities then
                    [ App.Model.SetActivePage <|
                        UserPage <|
                            ClinicalProgressReportPage
                                (Backend.PrenatalEncounter.Model.InitiatorEncounterPage id)
                                id
                    ]

                else
                    []
            )
        |> Maybe.withDefault []


generatePrenatalRecurrentPhaseCompletedMsgs :
    NominalDate
    -> Bool
    -> ModelIndexedDb
    -> PrenatalEncounterId
    -> List App.Model.Msg
generatePrenatalRecurrentPhaseCompletedMsgs currentDate isLabTech after id =
    if isLabTech then
        []

    else
        Pages.Prenatal.Encounter.Utils.generateAssembledData id after
            |> RemoteData.toMaybe
            |> Maybe.andThen
                (\assembled_ ->
                    let
                        -- Since diagnostics is performed at Backend.Update, and set using Msg,
                        -- we don't have it set at generated assembled data.
                        -- Therefore, we update diagnoses manually and set them into data.
                        encounterWithDiagnoses =
                            (\encounter ->
                                { encounter
                                    | diagnoses =
                                        Pages.Prenatal.Activity.Utils.generatePrenatalDiagnosesForNurse currentDate assembled_
                                }
                            )
                                assembled_.encounter

                        assembled =
                            { assembled_ | encounter = encounterWithDiagnoses }

                        ( _, pendingActivities ) =
                            Pages.Prenatal.RecurrentEncounter.Utils.getAllActivities isLabTech
                                |> List.filter (Pages.Prenatal.RecurrentActivity.Utils.expectActivity currentDate isLabTech assembled)
                                |> List.partition (Pages.Prenatal.RecurrentActivity.Utils.activityCompleted currentDate isLabTech assembled)
                    in
                    if List.isEmpty pendingActivities then
                        Maybe.map
                            (\( resultsId, results ) ->
                                let
                                    value =
                                        results.value
                                in
                                [ savePrenatalLabsResultsMsg id
                                    assembled.participant.person
                                    (Just resultsId)
                                    { value | resolutionDate = currentDate }
                                , App.Model.SetActivePage <|
                                    UserPage <|
                                        ClinicalProgressReportPage
                                            (Backend.PrenatalEncounter.Model.InitiatorRecurrentEncounterPage id)
                                            id
                                ]
                            )
                            assembled.measurements.labsResults

                    else
                        Nothing
                )
            |> Maybe.withDefault []


atPrenatalInitialPhase : Page -> Bool
atPrenatalInitialPhase activePage =
    case activePage of
        UserPage (PrenatalEncounterPage _) ->
            True

        UserPage (PrenatalActivityPage _ _) ->
            True

        _ ->
            False


atPrenatalRecurrentPhase : Page -> Bool
atPrenatalRecurrentPhase activePage =
    case activePage of
        UserPage (PrenatalRecurrentEncounterPage _) ->
            True

        UserPage (PrenatalRecurrentActivityPage _ _) ->
            True

        _ ->
            False


savePrenatalLabsResultsMsg :
    PrenatalEncounterId
    -> PersonId
    -> Maybe PrenatalLabsResultsId
    -> Backend.Measurement.Model.LabsResultsValue
    -> App.Model.Msg
savePrenatalLabsResultsMsg encounterId personId labsResultsId labsResultsValue =
    Backend.PrenatalEncounter.Model.SaveLabsResults personId labsResultsId labsResultsValue
        |> Backend.Model.MsgPrenatalEncounter encounterId
        |> App.Model.MsgIndexedDb


generateNCDAssessmentMsgs :
    NominalDate
    -> Language
    -> Page
    -> ModelIndexedDb
    -> NCDEncounterId
    -> List App.Model.Msg
generateNCDAssessmentMsgs currentDate language activePage after id =
    Maybe.map
        (\assembledAfter ->
            let
                diagnosesBefore =
                    -- At this stage new diagnoses were not updated yet, therefore,
                    -- we can use the diganoses set for the encounter.
                    assembledAfter.encounter.diagnoses

                diagnosesAfter =
                    Pages.NCD.Utils.generateNCDDiagnoses currentDate assembledAfter
            in
            if everySetsEqual diagnosesBefore diagnosesAfter then
                []

            else
                [ Backend.NCDEncounter.Model.SetNCDDiagnoses diagnosesAfter
                    |> Backend.Model.MsgNCDEncounter id
                    |> App.Model.MsgIndexedDb
                ]
        )
        (RemoteData.toMaybe <| Pages.NCD.Utils.generateAssembledData id after)
        |> Maybe.withDefault []


generateNCDLabsTestAddedMsgs :
    NominalDate
    -> ModelIndexedDb
    -> Backend.Measurement.Model.LaboratoryTest
    -> Backend.Measurement.Model.TestExecutionNote
    -> NCDEncounterId
    -> List App.Model.Msg
generateNCDLabsTestAddedMsgs currentDate after test executionNote id =
    Pages.NCD.Utils.generateAssembledData id after
        |> RemoteData.toMaybe
        |> Maybe.map
            (\assembled ->
                let
                    testExecuted =
                        testPerformedByExecutionNote executionNote
                in
                Maybe.map
                    (\( resultsId, measurement ) ->
                        let
                            updatedValue =
                                measurement.value
                                    |> (\value ->
                                            { value
                                                | performedTests =
                                                    if testExecuted then
                                                        EverySet.insert test value.performedTests

                                                    else
                                                        EverySet.remove test value.performedTests

                                                -- When we have universal form, it's possible to change if test is performed
                                                -- at lab, or at point of care.
                                                -- Therefore, we need to make sure that test does not appear as completed,
                                                -- which can happen, when nurse switches from point of care to lab.
                                                , completedTests = EverySet.remove test value.completedTests
                                                , resolutionDate = Date.add Days labExpirationPeriod currentDate
                                            }
                                       )
                        in
                        [ saveNCDLabsResultsMsg id assembled.participant.person (Just resultsId) updatedValue ]
                    )
                    assembled.measurements.labsResults
                    |> Maybe.withDefault
                        (if testExecuted then
                            let
                                resultsValue =
                                    Backend.Measurement.Model.LabsResultsValue
                                        (EverySet.singleton test)
                                        EverySet.empty
                                        (Date.add Days labExpirationPeriod currentDate)
                                        False
                                        Nothing
                                        Nothing
                            in
                            [ saveNCDLabsResultsMsg id assembled.participant.person Nothing resultsValue ]

                         else
                            []
                        )
            )
        |> Maybe.withDefault []


generateNCDLabsResultsAddedMsgs :
    NominalDate
    -> ModelIndexedDb
    -> Backend.Measurement.Model.LaboratoryTest
    -> NCDEncounterId
    -> List App.Model.Msg
generateNCDLabsResultsAddedMsgs currentDate after test id =
    Pages.NCD.Utils.generateAssembledData id after
        |> RemoteData.toMaybe
        |> Maybe.andThen
            (\assembled ->
                Maybe.map
                    (\( resultsId, results ) ->
                        if
                            EverySet.member test results.value.completedTests
                                && EverySet.member test results.value.performedTests
                        then
                            -- Do not update value if we have it set up properly already.
                            []

                        else
                            let
                                ( performedTests, completedTests ) =
                                    Pages.GlobalCaseManagement.Utils.labsResultsTestData currentDate results

                                updatedValue =
                                    (\value ->
                                        let
                                            -- Since lab results can be entered right away (at poit of care),
                                            -- or at latter stage (after test was ordered at lab), we
                                            -- update both performed and completed tests.
                                            -- Since data structure is EverySet, it will not create duplicacies.
                                            updatedPerformedTests =
                                                EverySet.insert test performedTests

                                            updatedCompletedTests =
                                                EverySet.insert test completedTests

                                            resolutionDate =
                                                -- When all performed tests are completed, and Next Steps are either
                                                -- completed, or not required, setting today as resolution date.
                                                if
                                                    (EverySet.size updatedCompletedTests == EverySet.size updatedPerformedTests)
                                                        && Pages.NCD.RecurrentActivity.Utils.activityCompleted
                                                            currentDate
                                                            assembled
                                                            Backend.NCDActivity.Model.RecurrentNextSteps
                                                then
                                                    currentDate

                                                else
                                                    value.resolutionDate
                                        in
                                        { value
                                            | performedTests = updatedPerformedTests
                                            , completedTests = updatedCompletedTests
                                            , resolutionDate = resolutionDate
                                        }
                                    )
                                        results.value
                            in
                            [ saveNCDLabsResultsMsg id assembled.participant.person (Just resultsId) updatedValue ]
                    )
                    assembled.measurements.labsResults
            )
        |> Maybe.withDefault []


saveNCDLabsResultsMsg :
    NCDEncounterId
    -> PersonId
    -> Maybe NCDLabsResultsId
    -> Backend.Measurement.Model.LabsResultsValue
    -> App.Model.Msg
saveNCDLabsResultsMsg encounterId personId labsResultsId labsResultsValue =
    Backend.NCDEncounter.Model.SaveLabsResults personId labsResultsId labsResultsValue
        |> Backend.Model.MsgNCDEncounter encounterId
        |> App.Model.MsgIndexedDb


generateNutritionAssessmentIndividualMsgs :
    NominalDate
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> Bool
    -> ModelIndexedDb
    -> ModelIndexedDb
    -> NutritionEncounterId
    -> List App.Model.Msg
generateNutritionAssessmentIndividualMsgs currentDate zscores features isChw before after id =
    Maybe.map2
        (\assembledBefore assembledAfter ->
            let
                mandatoryActivitiesCompleted =
                    Pages.Nutrition.Activity.Utils.mandatoryActivitiesCompleted
                        currentDate
                        zscores
                        features
                        assembledAfter.person
                        isChw
                        assembledAfter
                        after
            in
            if not mandatoryActivitiesCompleted then
                -- Assement is done only when all mandatory measurements were recorded.
                []

            else
                let
                    assessmentBefore =
                        Pages.Nutrition.Activity.Utils.generateNutritionAssessment currentDate zscores after assembledBefore
                            |> nutritionAssessmentForBackend

                    assessmentAfter =
                        Pages.Nutrition.Activity.Utils.generateNutritionAssessment currentDate zscores after assembledAfter

                    assessmentForBackend =
                        nutritionAssessmentForBackend assessmentAfter

                    updateAssesmentMsgs =
                        if assessmentChanged then
                            let
                                updateAssesmentOnFollowUpMsg =
                                    assembledAfter.measurements.followUp
                                        |> Maybe.map
                                            (\( measurementId, measurement ) ->
                                                let
                                                    updatedValue =
                                                        measurement.value
                                                            |> (\value -> { value | assesment = assessmentForBackend })
                                                in
                                                Backend.NutritionEncounter.Model.SaveFollowUp assembledAfter.participant.person (Just measurementId) updatedValue
                                                    |> Backend.Model.MsgNutritionEncounter id
                                                    |> App.Model.MsgIndexedDb
                                                    |> List.singleton
                                            )
                                        |> Maybe.withDefault []

                                -- Update the assesment field on Nutrition measurement.
                                updateAssesmentOnNutritionMsg =
                                    assembledAfter.measurements.nutrition
                                        |> Maybe.map
                                            (\( measurementId, measurement ) ->
                                                let
                                                    updatedValue =
                                                        measurement.value
                                                            |> (\value -> { value | assesment = assessmentForBackend })
                                                in
                                                Backend.NutritionEncounter.Model.SaveNutrition assembledAfter.participant.person (Just measurementId) updatedValue
                                                    |> Backend.Model.MsgNutritionEncounter id
                                                    |> App.Model.MsgIndexedDb
                                                    |> List.singleton
                                            )
                                        |> Maybe.withDefault []
                            in
                            updateAssesmentOnFollowUpMsg ++ updateAssesmentOnNutritionMsg

                        else
                            []

                    assessmentChanged =
                        not (everySetsEqual assessmentBefore assessmentForBackend)
                in
                if List.isEmpty assessmentAfter then
                    -- No assesment, so, only thing we want to update is the
                    -- assesment field on Nutrition measurement and FollowUp
                    -- measurement (if it exists already).
                    updateAssesmentMsgs

                else
                    updateAssesmentMsgs
                        ++ [ -- Navigate to Nutrition encounter page.
                             App.Model.SetActivePage (UserPage (NutritionActivityPage id Backend.NutritionActivity.Model.NextSteps))

                           -- Show warning popup with new assesment.
                           , Pages.Nutrition.Activity.Model.SetWarningPopupState assessmentAfter
                                |> App.Model.MsgPageNutritionActivity id Backend.NutritionActivity.Model.NextSteps
                                |> App.Model.MsgLoggedIn
                           ]
        )
        (RemoteData.toMaybe <| Pages.Nutrition.Encounter.Utils.generateAssembledData id before)
        (RemoteData.toMaybe <| Pages.Nutrition.Encounter.Utils.generateAssembledData id after)
        |> Maybe.withDefault []


generateNutritionAssessmentGroupMsgs :
    NominalDate
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> Bool
    -> PersonId
    -> SessionId
    -> Page
    -> (ChildMeasurements -> ChildMeasurements)
    -> ModelIndexedDb
    -> List App.Model.Msg
generateNutritionAssessmentGroupMsgs currentDate zscores features isChw childId sessionId activePage updateFunc db =
    Dict.get sessionId db.editableSessions
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map
            (\session ->
                let
                    -- We simulate session data after measurement data is performed.
                    offlineSessionAfter =
                        mapChildMeasurementsAtOfflineSession childId updateFunc session.offlineSession

                    mandatoryActivitiesCompleted =
                        Activity.Utils.mandatoryActivitiesCompleted
                            currentDate
                            zscores
                            features
                            offlineSessionAfter
                            childId
                            isChw
                            db
                in
                if not mandatoryActivitiesCompleted then
                    []

                else
                    let
                        assessmentBefore =
                            Activity.Utils.generateNutritionAssessment
                                currentDate
                                zscores
                                childId
                                db
                                session.offlineSession
                                |> nutritionAssessmentForBackend

                        assessmentAfter =
                            Activity.Utils.generateNutritionAssessment
                                currentDate
                                zscores
                                childId
                                db
                                offlineSessionAfter

                        assessmentForBackend =
                            nutritionAssessmentForBackend assessmentAfter

                        updateAssesmentMsgs =
                            if assessmentChanged then
                                let
                                    measurementsAfter =
                                        getChildMeasurementData2 childId offlineSessionAfter

                                    updateAssesmentOnFollowUpMsg =
                                        measurementsAfter
                                            |> LocalData.unwrap
                                                []
                                                (\measurements ->
                                                    let
                                                        followUp =
                                                            mapMeasurementData .followUp measurements
                                                                |> .current

                                                        followUpValue =
                                                            getMeasurementValueFunc followUp
                                                    in
                                                    Maybe.map
                                                        (\value ->
                                                            let
                                                                followUpId =
                                                                    Maybe.map Tuple.first followUp

                                                                updatedValue =
                                                                    { value | assesment = assessmentForBackend }
                                                            in
                                                            Measurement.Model.SaveFollowUp followUpId updatedValue
                                                                |> Backend.Session.Model.MeasurementOutMsgChild childId
                                                                |> Backend.Model.MsgSession sessionId
                                                                |> App.Model.MsgIndexedDb
                                                                |> List.singleton
                                                        )
                                                        followUpValue
                                                        |> Maybe.withDefault []
                                                )

                                    updateAssesmentOnNutritionMsg =
                                        measurementsAfter
                                            |> LocalData.unwrap
                                                []
                                                (\measurements ->
                                                    let
                                                        nutrition =
                                                            mapMeasurementData .nutrition measurements
                                                                |> .current

                                                        nutritionValue =
                                                            getMeasurementValueFunc nutrition
                                                    in
                                                    Maybe.map
                                                        (\value ->
                                                            let
                                                                nutritionId =
                                                                    Maybe.map Tuple.first nutrition

                                                                updatedValue =
                                                                    { value | assesment = assessmentForBackend }
                                                            in
                                                            Measurement.Model.SaveNutrition nutritionId updatedValue
                                                                |> Backend.Session.Model.MeasurementOutMsgChild childId
                                                                |> Backend.Model.MsgSession sessionId
                                                                |> App.Model.MsgIndexedDb
                                                                |> List.singleton
                                                        )
                                                        nutritionValue
                                                        |> Maybe.withDefault []
                                                )
                                in
                                updateAssesmentOnFollowUpMsg ++ updateAssesmentOnNutritionMsg

                            else
                                []

                        assessmentChanged =
                            not (everySetsEqual assessmentBefore assessmentForBackend)

                        activityByActivityMsgs childActivity =
                            updateAssesmentMsgs
                                ++ [ App.Model.SetActivePage (UserPage (SessionPage sessionId (NextStepsPage childId (ChildActivity childActivity))))
                                   , Pages.NextSteps.Model.SetWarningPopupState assessmentAfter
                                        |> Pages.Session.Model.MsgNextSteps childId (ChildActivity childActivity)
                                        |> App.Model.MsgPageSession sessionId
                                        |> App.Model.MsgLoggedIn
                                   ]
                    in
                    if List.isEmpty assessmentAfter then
                        -- No assesment, so, only thing we want to update is the
                        -- assesment field on Nutrition measurement and FollowUp
                        -- measurement (if it exists already).
                        updateAssesmentMsgs

                    else
                        case activePage of
                            UserPage (SessionPage _ (ChildPage _)) ->
                                updateAssesmentMsgs
                                    ++ [ Pages.Participant.Model.DialogWarning assessmentAfter
                                            |> Just
                                            |> Pages.Participant.Model.SetDialogState
                                            |> Pages.Session.Model.MsgChild childId
                                            |> App.Model.MsgPageSession sessionId
                                            |> App.Model.MsgLoggedIn
                                       ]

                            UserPage (SessionPage _ (ActivityPage (ChildActivity Muac))) ->
                                activityByActivityMsgs Muac

                            UserPage (SessionPage _ (ActivityPage (ChildActivity NutritionSigns))) ->
                                activityByActivityMsgs NutritionSigns

                            UserPage (SessionPage _ (ActivityPage (ChildActivity Weight))) ->
                                activityByActivityMsgs Weight

                            _ ->
                                []
            )
        |> Maybe.withDefault []


generateNutritionAssessmentWellChildlMsgs :
    NominalDate
    -> ZScore.Model.Model
    -> Site
    -> Bool
    -> ModelIndexedDb
    -> ModelIndexedDb
    -> WellChildEncounterId
    -> List App.Model.Msg
generateNutritionAssessmentWellChildlMsgs currentDate zscores site isChw before after id =
    Maybe.map2
        (\assembledBefore assembledAfter ->
            let
                mandatoryActivitiesCompleted =
                    Pages.WellChild.Activity.Utils.mandatoryNutritionAssessmentTasksCompleted
                        currentDate
                        assembledAfter
            in
            if not mandatoryActivitiesCompleted then
                -- Assement is done only when all mandatory measurements were recorded.
                []

            else
                let
                    assessmentBefore =
                        Pages.WellChild.Activity.Utils.generateNutritionAssessment currentDate zscores before assembledBefore
                            |> nutritionAssessmentForBackend

                    assessmentForBackend =
                        Pages.WellChild.Activity.Utils.generateNutritionAssessment currentDate zscores after assembledAfter
                            |> nutritionAssessmentForBackend

                    assessmentChanged =
                        not (everySetsEqual assessmentBefore assessmentForBackend)
                in
                if assessmentChanged then
                    let
                        -- Update the assesment field on Follow Up measurement (if it exists already).
                        updateAssesmentOnFollowUpMsg =
                            assembledAfter.measurements.followUp
                                |> Maybe.map
                                    (\( measurementId, measurement ) ->
                                        let
                                            updatedValue =
                                                measurement.value
                                                    |> (\value -> { value | assesment = assessmentForBackend })
                                        in
                                        Backend.WellChildEncounter.Model.SaveFollowUp assembledAfter.participant.person (Just measurementId) updatedValue
                                            |> Backend.Model.MsgWellChildEncounter id
                                            |> App.Model.MsgIndexedDb
                                            |> List.singleton
                                    )
                                |> Maybe.withDefault []

                        -- Update the assesment field on Nutrition measurement.
                        updateAssesmentOnNutritionMsg =
                            assembledAfter.measurements.nutrition
                                |> Maybe.map
                                    (\( measurementId, measurement ) ->
                                        let
                                            updatedValue =
                                                measurement.value
                                                    |> (\value -> { value | assesment = assessmentForBackend })
                                        in
                                        Backend.WellChildEncounter.Model.SaveNutrition assembledAfter.participant.person (Just measurementId) updatedValue
                                            |> Backend.Model.MsgWellChildEncounter id
                                            |> App.Model.MsgIndexedDb
                                            |> List.singleton
                                    )
                                |> Maybe.withDefault []
                    in
                    updateAssesmentOnFollowUpMsg ++ updateAssesmentOnNutritionMsg

                else
                    []
        )
        (RemoteData.toMaybe <| Pages.WellChild.Encounter.Utils.generateAssembledData site id before)
        (RemoteData.toMaybe <| Pages.WellChild.Encounter.Utils.generateAssembledData site id after)
        |> Maybe.withDefault []


generateSuspectedDiagnosisMsgs :
    NominalDate
    -> EverySet SiteFeature
    -> Bool
    -> ModelIndexedDb
    -> ModelIndexedDb
    -> AcuteIllnessEncounterId
    -> Person
    -> List App.Model.Msg
generateSuspectedDiagnosisMsgs currentDate features isChw before after id person =
    Maybe.map2
        (\assembledBefore assembledAfter ->
            if assembledAfter.initialEncounter then
                generateSuspectedDiagnosisMsgsFirstEncounter currentDate isChw id person assembledBefore assembledAfter

            else
                generateSuspectedDiagnosisMsgsSubsequentEncounter currentDate features isChw assembledAfter
        )
        (RemoteData.toMaybe <| Pages.AcuteIllness.Encounter.Utils.generateAssembledData currentDate features id isChw before)
        (RemoteData.toMaybe <| Pages.AcuteIllness.Encounter.Utils.generateAssembledData currentDate features id isChw after)
        |> Maybe.withDefault []


generateSuspectedDiagnosisMsgsFirstEncounter :
    NominalDate
    -> Bool
    -> AcuteIllnessEncounterId
    -> Person
    -> Pages.AcuteIllness.Encounter.Model.AssembledData
    -> Pages.AcuteIllness.Encounter.Model.AssembledData
    -> List App.Model.Msg
generateSuspectedDiagnosisMsgsFirstEncounter currentDate isChw id person assembledBefore assembledAfter =
    let
        diagnosisBeforeChange =
            Maybe.map Tuple.second assembledBefore.diagnosis

        diagnosisAfterChange =
            Maybe.map Tuple.second assembledAfter.diagnosis
    in
    if diagnosisBeforeChange /= diagnosisAfterChange then
        case diagnosisAfterChange of
            Just newDiagnosis ->
                updateAcuteIllnessDiagnosisMsg id newDiagnosis
                    :: (resolveNextStepFirstEncounter currentDate isChw assembledAfter
                            |> generateMsgsForNewDiagnosis currentDate isChw id newDiagnosis
                       )

            Nothing ->
                [ updateAcuteIllnessDiagnosisMsg id NoAcuteIllnessDiagnosis ]

    else
        []


generateMsgsForNewDiagnosis :
    NominalDate
    -> Bool
    -> AcuteIllnessEncounterId
    -> AcuteIllnessDiagnosis
    -> Maybe Pages.AcuteIllness.Activity.Types.NextStepsTask
    -> List App.Model.Msg
generateMsgsForNewDiagnosis currentDate isChw id diagnosis nextStep =
    if isChw then
        generateCustomMsgsForNewDiagnosis currentDate id diagnosis nextStep

    else
        generateMsgsForNewDiagnosisForNurse currentDate id diagnosis nextStep


generateMsgsForNewDiagnosisForNurse :
    NominalDate
    -> AcuteIllnessEncounterId
    -> AcuteIllnessDiagnosis
    -> Maybe Pages.AcuteIllness.Activity.Types.NextStepsTask
    -> List App.Model.Msg
generateMsgsForNewDiagnosisForNurse currentDate id diagnosis nextStep =
    case diagnosis of
        DiagnosisCovid19Suspect ->
            [ -- Navigate to Acute Ilness Laboratory activty page.
              App.Model.SetActivePage (UserPage (AcuteIllnessActivityPage id AcuteIllnessLaboratory))
            , -- Focus on Covid testing task.
              Pages.AcuteIllness.Activity.Model.SetActiveLaboratoryTask Pages.AcuteIllness.Activity.Types.LaboratoryCovidTesting
                |> App.Model.MsgPageAcuteIllnessActivity id AcuteIllnessLaboratory
                |> App.Model.MsgLoggedIn

            -- Show warning popup with new diagnosis.
            , Pages.AcuteIllness.Activity.Model.SetWarningPopupState (Just diagnosis)
                |> App.Model.MsgPageAcuteIllnessActivity id AcuteIllnessLaboratory
                |> App.Model.MsgLoggedIn
            ]

        _ ->
            generateCustomMsgsForNewDiagnosis currentDate id diagnosis nextStep


generateCustomMsgsForNewDiagnosis :
    NominalDate
    -> AcuteIllnessEncounterId
    -> AcuteIllnessDiagnosis
    -> Maybe Pages.AcuteIllness.Activity.Types.NextStepsTask
    -> List App.Model.Msg
generateCustomMsgsForNewDiagnosis currentDate id diagnosis nextStep =
    case nextStep of
        Just step ->
            [ -- Navigate to Acute Ilness NextSteps activty page.
              App.Model.SetActivePage (UserPage (AcuteIllnessActivityPage id AcuteIllnessNextSteps))
            , -- Focus on first task on that page.
              Pages.AcuteIllness.Activity.Model.SetActiveNextStepsTask step
                |> App.Model.MsgPageAcuteIllnessActivity id AcuteIllnessNextSteps
                |> App.Model.MsgLoggedIn

            -- Show warning popup with new diagnosis.
            , Pages.AcuteIllness.Activity.Model.SetWarningPopupState (Just diagnosis)
                |> App.Model.MsgPageAcuteIllnessActivity id AcuteIllnessNextSteps
                |> App.Model.MsgLoggedIn
            ]

        Nothing ->
            [ -- Navigate to Acute Ilness encounter page.
              App.Model.SetActivePage (UserPage (AcuteIllnessEncounterPage id))

            -- Focus on 'Todo' tab.
            , Pages.AcuteIllness.Encounter.Model.SetSelectedTab Pages.AcuteIllness.Encounter.Model.Pending
                |> App.Model.MsgPageAcuteIllnessEncounter id
                |> App.Model.MsgLoggedIn

            -- Show warning popup with new diagnosis.
            , Pages.AcuteIllness.Encounter.Model.SetWarningPopupState (Just diagnosis)
                |> App.Model.MsgPageAcuteIllnessEncounter id
                |> App.Model.MsgLoggedIn
            ]


generateSuspectedDiagnosisMsgsSubsequentEncounter :
    NominalDate
    -> EverySet SiteFeature
    -> Bool
    -> Pages.AcuteIllness.Encounter.Model.AssembledData
    -> List App.Model.Msg
generateSuspectedDiagnosisMsgsSubsequentEncounter currentDate features isChw data =
    if mandatoryActivitiesCompletedSubsequentVisit currentDate isChw data then
        let
            diagnosisByCurrentEncounterMeasurements =
                resolveAcuteIllnessDiagnosis currentDate features isChw data
                    |> Maybe.withDefault NoAcuteIllnessDiagnosis

            setDiagnosisMsg =
                -- We have an update to diagnosis based on current measurements,
                -- and it is not yet set for the encounter.
                if data.encounter.diagnosis == NoAcuteIllnessDiagnosis && diagnosisByCurrentEncounterMeasurements /= NoAcuteIllnessDiagnosis then
                    [ updateAcuteIllnessDiagnosisMsg data.id diagnosisByCurrentEncounterMeasurements ]

                else
                    []

            setActiveTaskMsg =
                resolveNextStepSubsequentEncounter currentDate isChw data
                    |> Maybe.map
                        (Pages.AcuteIllness.Activity.Model.SetActiveNextStepsTask
                            >> App.Model.MsgPageAcuteIllnessActivity data.id AcuteIllnessNextSteps
                            >> App.Model.MsgLoggedIn
                            >> List.singleton
                        )
                    |> Maybe.withDefault []
        in
        [ -- Navigate to Acute Ilness NextSteps activty page.
          App.Model.SetActivePage (UserPage (AcuteIllnessActivityPage data.id AcuteIllnessNextSteps))

        -- Show warning popup with new diagnosis.
        , Maybe.map Tuple.second data.diagnosis
            |> Pages.AcuteIllness.Activity.Model.SetWarningPopupState
            |> App.Model.MsgPageAcuteIllnessActivity data.id AcuteIllnessNextSteps
            |> App.Model.MsgLoggedIn
        ]
            ++ -- Set diagnosis for this encounter.
               setDiagnosisMsg
            ++ -- Focus on first task on that page.
               setActiveTaskMsg

    else
        []


updateAcuteIllnessDiagnosisMsg : AcuteIllnessEncounterId -> AcuteIllnessDiagnosis -> App.Model.Msg
updateAcuteIllnessDiagnosisMsg id diagnosis =
    Backend.AcuteIllnessEncounter.Model.SetAcuteIllnessDiagnosis diagnosis
        |> Backend.Model.MsgAcuteIllnessEncounter id
        |> App.Model.MsgIndexedDb


generateAcuteIllnessAssesmentCompletedMsgs : NominalDate -> EverySet SiteFeature -> Bool -> ModelIndexedDb -> AcuteIllnessEncounterId -> List App.Model.Msg
generateAcuteIllnessAssesmentCompletedMsgs currentDate features isChw after id =
    Pages.AcuteIllness.Encounter.Utils.generateAssembledData currentDate features id isChw after
        |> RemoteData.toMaybe
        |> Maybe.map
            (\assembled ->
                if not <| activityCompleted currentDate isChw assembled AcuteIllnessNextSteps then
                    []

                else
                    let
                        navigateToProgressReportMsg =
                            App.Model.SetActivePage (UserPage (AcuteIllnessProgressReportPage Backend.AcuteIllnessEncounter.Types.InitiatorEncounterPage id))
                    in
                    if assembled.initialEncounter then
                        [ navigateToProgressReportMsg ]

                    else if noImprovementOnSubsequentVisit currentDate assembled.person assembled.measurements then
                        [ navigateToProgressReportMsg ]

                    else
                        [ App.Model.SetActivePage (UserPage (AcuteIllnessOutcomePage assembled.encounter.participant)) ]
            )
        |> Maybe.withDefault []


generateWellChildDangerSignsAlertMsgs : NominalDate -> Maybe WellChildEncounterId -> List App.Model.Msg
generateWellChildDangerSignsAlertMsgs currentDate maybeId =
    Maybe.map
        (\id ->
            [ -- Navigate to Well Child encounter page, because that's where we show alert popup.
              App.Model.SetActivePage (UserPage (WellChildEncounterPage id))

            -- Show danger signs alert popup.
            , Pages.WellChild.Encounter.Model.DialogWarning Pages.WellChild.Encounter.Model.PopupDangerSigns
                |> Just
                |> Pages.WellChild.Encounter.Model.SetDialogState
                |> App.Model.MsgPageWellChildEncounter id
                |> App.Model.MsgLoggedIn
            ]
        )
        maybeId
        |> Maybe.withDefault []


generateChildScoreboardAssesmentCompletedMsgs : NominalDate -> Site -> ModelIndexedDb -> ChildScoreboardEncounterId -> List App.Model.Msg
generateChildScoreboardAssesmentCompletedMsgs currentDate site after id =
    Pages.ChildScoreboard.Encounter.Utils.generateAssembledData site id after
        |> RemoteData.toMaybe
        |> Maybe.map
            (\assembled ->
                if
                    List.all (Pages.ChildScoreboard.Activity.Utils.activityCompleted currentDate site assembled after)
                        Backend.ChildScoreboardActivity.Utils.allActivities
                then
                    [ App.Model.SetActivePage (UserPage (ChildScoreboardProgressReportPage id)) ]

                else
                    []
            )
        |> Maybe.withDefault []


{-| Construct an EditableSession from our data, if we have all the needed data.

This is a convenience, because so many functions work in terms of an
EditableSession. In future, we might refactor that, or it might prove to
continue to be convenient. (It's probably not efficient to calculate all of
this on the fly every time, but it's much easier for now to work within
existing types).

-}
makeEditableSession : SessionId -> ModelIndexedDb -> WebData EditableSession
makeEditableSession sessionId db =
    let
        hasNoSuccessValues dict =
            dict
                |> Dict.values
                |> List.filter (\v -> RemoteData.isLoading v || RemoteData.isNotAsked v)
                |> List.isEmpty
                |> not

        hasMothersMeasurementsNotSuccess =
            hasNoSuccessValues db.motherMeasurements

        hasChildrenMeasurementsNotSuccess =
            hasNoSuccessValues db.childMeasurements

        noPeopleLoaded =
            -- There are groups that got all participants graduated,
            -- which causes noone to be loaded.
            -- In such cases, Attendance page shows constant spinner.
            -- Therefore, we will not raise red flag when no people
            -- need loading.
            (Dict.size db.people > 0)
                && (Dict.values db.people
                        |> List.filter (\v -> RemoteData.isSuccess v)
                        |> List.isEmpty
                   )
    in
    -- Make sure we don't still have measurements being lazy loaded, and at least
    -- some of the people have loaded. Otherwise, allow rebuilding the `EditableSession`.
    if hasMothersMeasurementsNotSuccess || hasChildrenMeasurementsNotSuccess || noPeopleLoaded then
        Loading

    else
        let
            sessionData =
                Dict.get sessionId db.sessions
                    |> Maybe.withDefault NotAsked

            allParticipantFormsData =
                db.participantForms

            everyCounselingScheduleData =
                db.everyCounselingSchedule

            participantsData =
                Dict.get sessionId db.expectedParticipants
                    |> Maybe.withDefault NotAsked

            mothersData =
                RemoteData.andThen
                    (\participants ->
                        Dict.keys participants.byMotherId
                            |> List.filterMap
                                (\id ->
                                    Dict.get id db.people
                                        |> Maybe.withDefault NotAsked
                                        |> RemoteData.toMaybe
                                        |> Maybe.map (\data -> Success ( id, data ))
                                )
                            |> RemoteData.fromList
                            |> RemoteData.map (List.sortBy (Tuple.second >> .name) >> Dict.fromList)
                    )
                    participantsData

            childrenData =
                RemoteData.andThen
                    (\participants ->
                        Dict.keys participants.byChildId
                            |> List.filterMap
                                (\id ->
                                    Dict.get id db.people
                                        |> Maybe.withDefault NotAsked
                                        |> RemoteData.toMaybe
                                        |> Maybe.map (\data -> Success ( id, data ))
                                )
                            |> RemoteData.fromList
                            |> RemoteData.map Dict.fromList
                    )
                    participantsData

            measurementData =
                Success NotNeeded

            offlineSession =
                RemoteData.map OfflineSession sessionData
                    |> RemoteData.andMap allParticipantFormsData
                    |> RemoteData.andMap everyCounselingScheduleData
                    |> RemoteData.andMap participantsData
                    |> RemoteData.andMap mothersData
                    |> RemoteData.andMap childrenData
                    |> RemoteData.andMap measurementData

            ( previousCheckedIn, previousSummaryByParticipant, previousSummaryByActivity ) =
                Dict.get sessionId db.editableSessions
                    |> Maybe.andThen RemoteData.toMaybe
                    |> Maybe.map
                        (\editableSessions ->
                            ( LocalData.setRecalculate editableSessions.checkedIn
                            , LocalData.setRecalculate editableSessions.summaryByParticipant
                            , LocalData.setRecalculate editableSessions.summaryByActivity
                            )
                        )
                    |> Maybe.withDefault ( NotNeeded, NotNeeded, NotNeeded )
        in
        RemoteData.map
            (\offline ->
                { offlineSession = offline
                , update = NotAsked
                , checkedIn = previousCheckedIn
                , summaryByParticipant = previousSummaryByParticipant
                , summaryByActivity = previousSummaryByActivity
                }
            )
            offlineSession


{-| Summarize our data for the editable session in a way that is useful
for our UI, when we're focused on participants. This only considers children &
mothers who are checked in to the session.
-}
summarizeByParticipant :
    NominalDate
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> OfflineSession
    -> LocalData CheckedIn
    -> Bool
    -> ModelIndexedDb
    -> LocalData SummaryByParticipant
summarizeByParticipant currentDate zscores features session checkedIn_ isChw db =
    LocalData.map
        (\checkedIn ->
            let
                children =
                    Dict.map
                        (\childId _ -> summarizeChildParticipant currentDate zscores features childId session isChw db)
                        checkedIn.children

                mothers =
                    Dict.map
                        (\motherId _ -> summarizeMotherParticipant currentDate zscores features motherId session isChw db)
                        checkedIn.mothers
            in
            { children = children
            , mothers = mothers
            }
        )
        checkedIn_


{-| Summarize our data for the editable session in a way that is useful
for our UI, when we're focused on activities. This only considers children &
mothers who are checked in to the session.
-}
summarizeByActivity :
    NominalDate
    -> ZScore.Model.Model
    -> EverySet SiteFeature
    -> OfflineSession
    -> LocalData CheckedIn
    -> Bool
    -> ModelIndexedDb
    -> LocalData SummaryByActivity
summarizeByActivity currentDate zscores features session checkedIn_ isChw db =
    LocalData.map
        (\checkedIn ->
            let
                children =
                    getAllChildActivities session
                        |> List.map
                            (\activity ->
                                ( activity
                                , summarizeChildActivity currentDate zscores features activity session isChw db checkedIn
                                )
                            )
                        |> Dict.fromList

                mothers =
                    getAllMotherActivities session
                        |> List.map
                            (\activity ->
                                ( activity
                                , summarizeMotherActivity currentDate zscores features activity session isChw db checkedIn
                                )
                            )
                        |> Dict.fromList
            in
            { children = children
            , mothers = mothers
            }
        )
        checkedIn_


{-| Who is checked in, considering both explicit check in and anyone who has
any completed activity?

It depends on Measurements at OfflineSession being fully loaded,
and for this reason we start with 'LocalData.map'

-}
cacheCheckedIn : OfflineSession -> LocalData CheckedIn
cacheCheckedIn session =
    LocalData.map
        (\_ ->
            let
                -- A mother is checked in if explicitly checked in or has any completed
                -- activities.
                mothers =
                    Dict.filter
                        (\motherId _ -> motherIsCheckedIn motherId session)
                        session.mothers

                -- A child is checked in if the mother is checked in.
                children =
                    Dict.filter
                        (\childId _ ->
                            getMyMother childId session
                                |> Maybe.map (\( motherId, _ ) -> Dict.member motherId mothers)
                                |> Maybe.withDefault False
                        )
                        session.children
            in
            { mothers = mothers
            , children = children
            }
        )
        session.measurements


calculateOfflineSessionMeasurements :
    SessionId
    -> OfflineSession
    -> ModelIndexedDb
    ->
        LocalData
            { historical : HistoricalMeasurements
            , current : Measurements
            , previous : Measurements
            }
calculateOfflineSessionMeasurements sessionId offlineSession db =
    let
        childMeasurementListData =
            Dict.keys offlineSession.children
                |> List.map
                    (\childId ->
                        Dict.get childId db.childMeasurements
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map (\data -> ( childId, data ))
                    )
                |> List.filter RemoteData.isSuccess
                |> RemoteData.fromList
                |> RemoteData.map Dict.fromList

        adultMeasurementListData =
            Dict.keys offlineSession.mothers
                |> List.map
                    (\motherId ->
                        Dict.get motherId db.motherMeasurements
                            |> Maybe.withDefault NotAsked
                            |> RemoteData.map (\data -> ( motherId, data ))
                    )
                |> List.filter RemoteData.isSuccess
                |> RemoteData.fromList
                |> RemoteData.map Dict.fromList

        childMeasurementsSplitData =
            RemoteData.map (\list -> splitChildMeasurements sessionId list) childMeasurementListData

        adultMeasurementsSplitData =
            RemoteData.map (\list -> splitMotherMeasurements sessionId list) adultMeasurementListData

        historicalMeasurementData =
            RemoteData.map2 HistoricalMeasurements adultMeasurementListData childMeasurementListData

        currentAndPrevious =
            RemoteData.map2
                (\childData motherData ->
                    { current =
                        { mothers = Dict.map (always .current) motherData
                        , children = Dict.map (always .current) childData
                        }
                    , previous =
                        { mothers = Dict.map (always .previous) motherData
                        , children = Dict.map (always .previous) childData
                        }
                    }
                )
                childMeasurementsSplitData
                adultMeasurementsSplitData

        currentMeasurementData =
            RemoteData.map .current currentAndPrevious

        previousMeasurementData =
            RemoteData.map .previous currentAndPrevious
    in
    RemoteData.map3
        (\historical current previous ->
            Ready
                { historical = historical
                , current = current
                , previous = previous
                }
                NoRecalculate
        )
        historicalMeasurementData
        currentMeasurementData
        previousMeasurementData
        |> RemoteData.withDefault NotNeeded


{-| When we get dashboard data from backend, we store it, and
generate a variation of assembled data needed to display Dashboar Main page.
@see: ComputedDashboard definition.
-}
generateInitialComputedDashboard : NominalDate -> HealthCenterId -> Maybe VillageId -> DashboardStatsRaw -> ModelIndexedDb -> ComputedDashboard
generateInitialComputedDashboard currentDate healthCenterId villageId statsRaw db =
    let
        ( programTypeFilter, selectedVillage ) =
            if isJust villageId then
                -- This is CHW Nurse, as only CHW work with villages.
                ( Pages.Dashboard.Model.FilterProgramCommunity
                , villageId
                )

            else
                ( Pages.Dashboard.Model.FilterAllPrograms
                , Nothing
                )
    in
    { statsRaw = statsRaw
    , assembledPermutations =
        Dict.singleton
            ( programTypeFilter, selectedVillage )
            (Pages.Dashboard.Utils.generateAssembledData currentDate healthCenterId statsRaw db programTypeFilter selectedVillage)
    }


generateTuberculosisEncounterCompletedMsgs :
    NominalDate
    -> ModelIndexedDb
    -> TuberculosisEncounterId
    -> List App.Model.Msg
generateTuberculosisEncounterCompletedMsgs currentDate after id =
    Pages.Tuberculosis.Encounter.Utils.generateAssembledData id after
        |> RemoteData.toMaybe
        |> Maybe.map
            (\assembled ->
                let
                    ( _, pendingActivities ) =
                        Pages.Tuberculosis.Encounter.Utils.partitionActivities currentDate assembled
                in
                if List.isEmpty pendingActivities then
                    [ App.Model.SetActivePage <| UserPage <| TuberculosisProgressReportPage id ]

                else
                    []
            )
        |> Maybe.withDefault []


updatePersonWithCooridnates : Person -> Maybe App.Model.GPSCoordinates -> Person
updatePersonWithCooridnates person =
    Maybe.map
        (\coordinates ->
            { person
                | registrationLatitude = String.fromFloat coordinates.latitude |> Just
                , registrationLongitude = String.fromFloat coordinates.longitude |> Just
            }
        )
        >> Maybe.withDefault person
