module Backend.Update exposing (updateIndexedDb)

import Activity.Model exposing (Activity(..), ChildActivity(..), SummaryByActivity, SummaryByParticipant)
import Activity.Utils exposing (getAllChildActivities, getAllMotherActivities, motherIsCheckedIn, summarizeChildActivity, summarizeChildParticipant, summarizeMotherActivity, summarizeMotherParticipant)
import App.Model
import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessActivity.Model exposing (AcuteIllnessActivity(..))
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..), emptyAcuteIllnessEncounter)
import Backend.AcuteIllnessEncounter.Update
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Counseling.Decoder exposing (combineCounselingSchedules)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Fetch
import Backend.HomeVisitActivity.Model
import Backend.HomeVisitEncounter.Model exposing (emptyHomeVisitEncounter)
import Backend.HomeVisitEncounter.Update
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..), IndividualParticipantExtraData(..))
import Backend.IndividualEncounterParticipant.Update
import Backend.Measurement.Model exposing (ChildMeasurements, HistoricalMeasurements, Measurements)
import Backend.Measurement.Utils exposing (mapChildMeasurementsAtOfflineSession, mapMeasurementData, splitChildMeasurements, splitMotherMeasurements)
import Backend.Model exposing (..)
import Backend.NutritionActivity.Model
import Backend.NutritionEncounter.Model exposing (emptyNutritionEncounter)
import Backend.NutritionEncounter.Update
import Backend.NutritionEncounter.Utils exposing (nutritionAssesmentForBackend)
import Backend.Person.Model exposing (Initiator(..), Person)
import Backend.Person.Utils exposing (ageInMonths, graduatingAgeInMonth)
import Backend.PmtctParticipant.Model exposing (AdultActivities(..))
import Backend.PrenatalActivity.Model
import Backend.PrenatalEncounter.Model exposing (ClinicalProgressReportInitiator(..), PrenatalEncounterPostCreateDestination(..), PrenatalEncounterType(..), emptyPrenatalEncounter)
import Backend.PrenatalEncounter.Update
import Backend.Relationship.Encoder exposing (encodeRelationshipChanges)
import Backend.Relationship.Model exposing (MyRelatedBy(..), MyRelationship, RelatedBy(..))
import Backend.Relationship.Utils exposing (toMyRelationship, toRelationship)
import Backend.Session.Model exposing (CheckedIn, EditableSession, OfflineSession, Session)
import Backend.Session.Update
import Backend.Session.Utils exposing (getChildMeasurementData2, getMyMother)
import Backend.Utils
    exposing
        ( mapAcuteIllnessMeasurements
        , mapChildMeasurements
        , mapFollowUpMeasurements
        , mapHomeVisitMeasurements
        , mapMotherMeasurements
        , mapNutritionMeasurements
        , mapPrenatalMeasurements
        , sw
        )
import Backend.Village.Utils exposing (getVillageClinicId)
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Json.Encode exposing (object)
import LocalData exposing (LocalData(..), ReadyStatus(..))
import Maybe.Extra exposing (isJust, isNothing, unwrap)
import Measurement.Model exposing (OutMsgMother(..))
import Pages.AcuteIllnessActivity.Model
import Pages.AcuteIllnessEncounter.Model
import Pages.AcuteIllnessEncounter.Utils
    exposing
        ( activityCompleted
        , mandatoryActivitiesCompletedSubsequentVisit
        , noImprovementOnSubsequentVisit
        , resolveAcuteIllnessDiagnosis
        , resolveNextStepFirstEncounter
        , resolveNextStepSubsequentEncounter
        )
import Pages.NextSteps.Model
import Pages.NutritionActivity.Model
import Pages.NutritionActivity.Utils
import Pages.NutritionEncounter.Model
import Pages.NutritionEncounter.Utils
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Participant.Model
import Pages.Person.Model
import Pages.PrenatalActivity.Model
import Pages.PrenatalEncounter.Model
import Pages.PrenatalEncounter.Utils
import Pages.Relationship.Model
import Pages.Session.Model
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (EntityUuid, ReadOnlyEndPoint, ReadWriteEndPoint, applyBackendUrl, toCmd, toTask, withoutDecoder)
import SyncManager.Model exposing (IndexDbQueryType(..))
import Task
import Translate exposing (Language, translate)
import ZScore.Model


updateIndexedDb :
    NominalDate
    -> Language
    -> ZScore.Model.Model
    -> Maybe NurseId
    -> Maybe HealthCenterId
    -> Maybe VillageId
    -> Bool
    -> Page
    -> MsgIndexedDb
    -> ModelIndexedDb
    -> ( ModelIndexedDb, Cmd MsgIndexedDb, List App.Model.Msg )
updateIndexedDb currentDate language zscores nurseId healthCenterId villageId isChw activePage msg model =
    let
        noChange =
            ( model, Cmd.none, [] )
    in
    case msg of
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

        HandleFetchedComputedDashboard healthCenterId_ webData ->
            let
                modelUpdated =
                    RemoteData.toMaybe webData
                        |> Maybe.map (\data -> { model | computedDashboard = data })
                        |> Maybe.withDefault model
            in
            ( modelUpdated
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

        FetchComputedDashboard healthCenterId_ ->
            ( model
            , sw.select computedDashboardEndpoint ()
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedComputedDashboard healthCenterId_)
            , []
            )

        HandleFetchedClinics clinics ->
            ( { model | clinics = clinics }
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
                |> sequenceExtra (updateIndexedDb currentDate language zscores nurseId healthCenterId villageId isChw activePage) extraMsgs

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
                                summarizeByActivity currentDate zscores editable.offlineSession editable.checkedIn isChw model

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
                                summarizeByParticipant currentDate zscores editable.offlineSession editable.checkedIn isChw model

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

        FetchPeopleByName name ->
            let
                trimmed =
                    String.trim name
            in
            -- We'll limit the search to 500 each for now ... basically,
            -- just to avoid truly pathological cases.
            ( { model | personSearches = Dict.insert trimmed Loading model.personSearches }
            , sw.selectRange personEndpoint { nameContains = Just trimmed } 0 (Just 500)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedPeopleByName trimmed)
            , []
            )

        HandleFetchedPeopleByName name data ->
            ( { model | personSearches = Dict.insert (String.trim name) data model.personSearches }
            , Cmd.none
            , []
            )

        FetchIndividualEncounterParticipantsForPerson id ->
            ( { model | individualParticipantsByPerson = Dict.insert id Loading model.individualParticipantsByPerson }
            , sw.select individualEncounterParticipantEndpoint (Just id)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedIndividualEncounterParticipantsForPerson id)
            , []
            )

        HandleFetchedIndividualEncounterParticipantsForPerson id data ->
            ( { model | individualParticipantsByPerson = Dict.insert id data model.individualParticipantsByPerson }
            , Cmd.none
            , []
            )

        FetchPrenatalEncountersForParticipant id ->
            ( { model | prenatalEncountersByParticipant = Dict.insert id Loading model.prenatalEncountersByParticipant }
            , sw.select prenatalEncounterEndpoint (Just id)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedPrenatalEncountersForParticipant id)
            , []
            )

        HandleFetchedPrenatalEncountersForParticipant id data ->
            ( { model | prenatalEncountersByParticipant = Dict.insert id data model.prenatalEncountersByParticipant }
            , Cmd.none
            , []
            )

        FetchNutritionEncountersForParticipant id ->
            ( { model | nutritionEncountersByParticipant = Dict.insert id Loading model.nutritionEncountersByParticipant }
            , sw.select nutritionEncounterEndpoint (Just id)
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
            , sw.select homeVisitEncounterEndpoint (Just id)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedHomeVisitEncountersForParticipant id)
            , []
            )

        HandleFetchedHomeVisitEncountersForParticipant id data ->
            ( { model | homeVisitEncountersByParticipant = Dict.insert id data model.homeVisitEncountersByParticipant }
            , Cmd.none
            , []
            )

        FetchAcuteIllnessEncountersForParticipant id ->
            ( { model | acuteIllnessEncountersByParticipant = Dict.insert id Loading model.acuteIllnessEncountersByParticipant }
            , sw.select acuteIllnessEncounterEndpoint (Just id)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedAcuteIllnessEncountersForParticipant id)
            , []
            )

        HandleFetchedAcuteIllnessEncountersForParticipant id data ->
            ( { model | acuteIllnessEncountersByParticipant = Dict.insert id data model.acuteIllnessEncountersByParticipant }
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
                    |> toCmd (RemoteData.fromResult >> RemoteData.map Dict.fromList >> HandleFetchPeople)
                , []
                )

        HandleFetchPeople webData ->
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

        HandleRevisions revisions ->
            let
                processRevisionAndDiagnoseAcuteIllness participantId encounterId =
                    let
                        person =
                            Dict.get participantId model.people
                                |> Maybe.withDefault NotAsked
                                |> RemoteData.toMaybe

                        ( newModel, _ ) =
                            List.foldl (handleRevision healthCenterId) ( model, False ) revisions

                        extraMsgs =
                            Maybe.map2 (generateSuspectedDiagnosisMsgs currentDate model newModel)
                                encounterId
                                person
                                |> Maybe.withDefault []
                    in
                    ( newModel, extraMsgs )

                processRevisionAndAssessNutritionIndividual participantId encounterId =
                    let
                        person =
                            Dict.get participantId model.people
                                |> Maybe.withDefault NotAsked
                                |> RemoteData.toMaybe

                        ( newModel, _ ) =
                            List.foldl (handleRevision healthCenterId) ( model, False ) revisions

                        extraMsgs =
                            Maybe.map (generateNutritionAssessmentIndividualMsgs currentDate zscores isChw newModel)
                                encounterId
                                |> Maybe.withDefault []
                    in
                    ( newModel, extraMsgs )

                processRevisionAndAssessNutritionGroup data updateFunc =
                    let
                        participantId =
                            data.participantId

                        sessionId =
                            data.encounterId

                        ( newModel, _ ) =
                            List.foldl (handleRevision healthCenterId) ( model, True ) revisions
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
                                    generateNutritionAssessmentGroupMsgs currentDate zscores isChw participantId sessionId_ activePage updateFunc newModel
                            in
                            ( withRecalc, extraMsgs )
                        )
                        sessionId
                        |> Maybe.withDefault ( newModel, [] )

                processRevisionAndWarnOnPrenatalDangerSigns participantId encounterId =
                    let
                        person =
                            Dict.get participantId model.people
                                |> Maybe.withDefault NotAsked
                                |> RemoteData.toMaybe

                        ( newModel, _ ) =
                            List.foldl (handleRevision healthCenterId) ( model, False ) revisions

                        extraMsgs =
                            Maybe.map (generatePrenatalDangerSignsWarningMsgs currentDate language isChw newModel)
                                encounterId
                                |> Maybe.withDefault []
                    in
                    ( newModel, extraMsgs )
            in
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
                                (\measurements -> { measurements | attendances = Dict.insert uuid data measurements.attendances })
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

                                                        newEditableSessions =
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

                [ SymptomsGeneralRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ SymptomsRespiratoryRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ SymptomsGIRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ TravelHistoryRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ ExposureRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ AcuteIllnessVitalsRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ AcuteFindingsRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ MalariaTestingRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ AcuteIllnessDangerSignsRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ AcuteIllnessMuacRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ AcuteIllnessNutritionRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ TreatmentOngoingRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndDiagnoseAcuteIllness data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ IsolationRevision uuid data ] ->
                    let
                        ( newModel, _ ) =
                            List.foldl (handleRevision healthCenterId) ( model, False ) revisions

                        extraMsgs =
                            data.encounterId
                                |> Maybe.map (generateAcuteIllnessAssesmentCompletedMsgs currentDate newModel)
                                |> Maybe.withDefault []
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ Call114Revision uuid data ] ->
                    let
                        ( newModel, _ ) =
                            List.foldl (handleRevision healthCenterId) ( model, False ) revisions

                        extraMsgs =
                            data.encounterId
                                |> Maybe.map (generateAcuteIllnessAssesmentCompletedMsgs currentDate newModel)
                                |> Maybe.withDefault []
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ HCContactRevision uuid data ] ->
                    let
                        ( newModel, _ ) =
                            List.foldl (handleRevision healthCenterId) ( model, False ) revisions

                        extraMsgs =
                            data.encounterId
                                |> Maybe.map (generateAcuteIllnessAssesmentCompletedMsgs currentDate newModel)
                                |> Maybe.withDefault []
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ MedicationDistributionRevision uuid data ] ->
                    let
                        ( newModel, _ ) =
                            List.foldl (handleRevision healthCenterId) ( model, False ) revisions

                        extraMsgs =
                            data.encounterId
                                |> Maybe.map (generateAcuteIllnessAssesmentCompletedMsgs currentDate newModel)
                                |> Maybe.withDefault []
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ SendToHCRevision uuid data ] ->
                    let
                        ( newModel, _ ) =
                            List.foldl (handleRevision healthCenterId) ( model, False ) revisions

                        extraMsgs =
                            data.encounterId
                                |> Maybe.map (generateAcuteIllnessAssesmentCompletedMsgs currentDate newModel)
                                |> Maybe.withDefault []
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ HealthEducationRevision uuid data ] ->
                    let
                        ( newModel, _ ) =
                            List.foldl (handleRevision healthCenterId) ( model, False ) revisions

                        extraMsgs =
                            data.encounterId
                                |> Maybe.map (generateAcuteIllnessAssesmentCompletedMsgs currentDate newModel)
                                |> Maybe.withDefault []
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ AcuteIllnessFollowUpRevision uuid data ] ->
                    let
                        ( newModel, _ ) =
                            List.foldl (handleRevision healthCenterId) ( model, False ) revisions

                        extraMsgs =
                            data.encounterId
                                |> Maybe.map (generateAcuteIllnessAssesmentCompletedMsgs currentDate newModel)
                                |> Maybe.withDefault []
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ NutritionMuacRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndAssessNutritionIndividual data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ NutritionNutritionRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndAssessNutritionIndividual data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ NutritionWeightRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndAssessNutritionIndividual data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ MuacRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndAssessNutritionGroup data (\childMeasurements -> { childMeasurements | muac = Just ( uuid, data ) })
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ ChildNutritionRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndAssessNutritionGroup data (\childMeasurements -> { childMeasurements | nutrition = Just ( uuid, data ) })
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ WeightRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndAssessNutritionGroup data (\childMeasurements -> { childMeasurements | weight = Just ( uuid, data ) })
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ DangerSignsRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndWarnOnPrenatalDangerSigns data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ LastMenstrualPeriodRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndWarnOnPrenatalDangerSigns data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                [ PregnancyTestingRevision uuid data ] ->
                    let
                        ( newModel, extraMsgs ) =
                            processRevisionAndWarnOnPrenatalDangerSigns data.participantId data.encounterId
                    in
                    ( newModel
                    , Cmd.none
                    , extraMsgs
                    )

                _ ->
                    let
                        ( newModel, recalculateEditableSessions ) =
                            List.foldl (handleRevision healthCenterId) ( model, False ) revisions

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

        MsgPrenatalEncounter encounterId subMsg ->
            let
                encounter =
                    Dict.get encounterId model.prenatalEncounters
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe

                requests =
                    Dict.get encounterId model.prenatalEncounterRequests
                        |> Maybe.withDefault Backend.PrenatalEncounter.Model.emptyModel

                ( subModel, subCmd ) =
                    Backend.PrenatalEncounter.Update.update nurseId healthCenterId encounterId encounter currentDate subMsg requests
            in
            ( { model | prenatalEncounterRequests = Dict.insert encounterId subModel model.prenatalEncounterRequests }
            , Cmd.map (MsgPrenatalEncounter encounterId) subCmd
            , []
            )

        MsgNutritionEncounter encounterId subMsg ->
            let
                encounter =
                    Dict.get encounterId model.nutritionEncounters
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe

                requests =
                    Dict.get encounterId model.nutritionEncounterRequests
                        |> Maybe.withDefault Backend.NutritionEncounter.Model.emptyModel

                ( subModel, subCmd ) =
                    Backend.NutritionEncounter.Update.update nurseId healthCenterId encounterId encounter currentDate subMsg requests
            in
            ( { model | nutritionEncounterRequests = Dict.insert encounterId subModel model.nutritionEncounterRequests }
            , Cmd.map (MsgNutritionEncounter encounterId) subCmd
            , []
            )

        MsgAcuteIllnessEncounter encounterId subMsg ->
            let
                encounter =
                    Dict.get encounterId model.acuteIllnessEncounters
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe

                requests =
                    Dict.get encounterId model.acuteIllnessEncounterRequests
                        |> Maybe.withDefault Backend.AcuteIllnessEncounter.Model.emptyModel

                ( subModel, subCmd ) =
                    Backend.AcuteIllnessEncounter.Update.update nurseId healthCenterId encounterId encounter currentDate subMsg requests
            in
            ( { model | acuteIllnessEncounterRequests = Dict.insert encounterId subModel model.acuteIllnessEncounterRequests }
            , Cmd.map (MsgAcuteIllnessEncounter encounterId) subCmd
            , []
            )

        MsgHomeVisitEncounter encounterId subMsg ->
            let
                encounter =
                    Dict.get encounterId model.homeVisitEncounters
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe

                requests =
                    Dict.get encounterId model.homeVisitEncounterRequests
                        |> Maybe.withDefault Backend.HomeVisitEncounter.Model.emptyModel

                ( subModel, subCmd ) =
                    Backend.HomeVisitEncounter.Update.update nurseId healthCenterId encounterId encounter currentDate subMsg requests
            in
            ( { model | homeVisitEncounterRequests = Dict.insert encounterId subModel model.homeVisitEncounterRequests }
            , Cmd.map (MsgHomeVisitEncounter encounterId) subCmd
            , []
            )

        MsgIndividualSession participantId subMsg ->
            let
                participant =
                    Dict.get participantId model.individualParticipants
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe

                requests =
                    Dict.get participantId model.individualSessionRequests
                        |> Maybe.withDefault Backend.IndividualEncounterParticipant.Model.emptyModel

                ( subModel, subCmd ) =
                    Backend.IndividualEncounterParticipant.Update.update participantId participant currentDate subMsg requests
            in
            ( { model | individualSessionRequests = Dict.insert participantId subModel model.individualSessionRequests }
            , Cmd.map (MsgIndividualSession participantId) subCmd
            , []
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

                ( subModel, subCmd, fetchMsgs ) =
                    Backend.Session.Update.update nurseId sessionId session currentDate model subMsg requests
            in
            ( { model | sessionRequests = Dict.insert sessionId subModel model.sessionRequests }
            , Cmd.map (MsgSession sessionId) subCmd
            , fetchMsgs
                |> List.filter (Backend.Fetch.shouldFetch model)
                |> List.map App.Model.MsgIndexedDb
            )

        PostPmtctParticipant initiator data ->
            ( { model | postPmtctParticipant = Dict.insert data.child Loading model.postPmtctParticipant }
            , sw.post pmtctParticipantEndpoint data
                |> toCmd (RemoteData.fromResult >> HandlePostedPmtctParticipant data.child initiator)
            , []
            )

        HandlePostedPmtctParticipant id initiator data ->
            let
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
            , appMsgs
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
                                                            let
                                                                graduationDate =
                                                                    Maybe.map (Date.add Months graduatingAgeInMonth) childBirthDate
                                                            in
                                                            if List.member clinic.clinicType [ Sorwathe, Achi ] then
                                                                Nothing

                                                            else
                                                                graduationDate
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
                |> sequenceExtra (updateIndexedDb currentDate language zscores nurseId healthCenterId villageId isChw activePage) extraMsgs

        HandlePostedRelationship personId initiator data ->
            let
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
                                    PrenatalNextStepsActivityOrigin _ ->
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
            , appMsgs
            )

        PostPerson relation initiator person ->
            ( { model | postPerson = Loading }
            , sw.post personEndpoint person
                |> toCmd (RemoteData.fromResult >> RemoteData.map Tuple.first >> HandlePostedPerson relation initiator)
            , []
            )

        HandlePostedPerson relation initiator data ->
            let
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
                                                        AcuteIllnessParticipantPage personId

                                                    AntenatalEncounter ->
                                                        PrenatalParticipantPage personId

                                                    NutritionEncounter ->
                                                        NutritionParticipantPage personId

                                                    _ ->
                                                        -- This will change as we add support for
                                                        -- new encounter types.
                                                        IndividualEncounterTypesPage
                                        in
                                        ( [ resetFormMsg, navigationMsg nextPage ]
                                        , []
                                        )

                                    GroupEncounterOrigin sessionId ->
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

                                    PrenatalNextStepsActivityOrigin encounterId ->
                                        let
                                            nextPage =
                                                PrenatalActivityPage encounterId Backend.PrenatalActivity.Model.NextSteps

                                            updateNewbornEnroledMsg =
                                                Dict.get encounterId model.prenatalEncounters
                                                    |> Maybe.andThen RemoteData.toMaybe
                                                    |> Maybe.map
                                                        (\encounter ->
                                                            [ Backend.IndividualEncounterParticipant.Model.SetNewborn personId
                                                                |> MsgIndividualSession encounter.participant
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
                            )
                        |> Maybe.withDefault ( [], [] )
            in
            ( { model | postPerson = data }
            , Cmd.none
            , appMsgs
            )
                |> sequenceExtra (updateIndexedDb currentDate language zscores nurseId healthCenterId villageId isChw activePage) extraMsgs

        PatchPerson personId person ->
            ( { model | postPerson = Loading }
            , sw.patchFull personEndpoint personId person
                |> toCmd (RemoteData.fromResult >> HandlePatchedPerson personId)
            , []
            )

        HandlePatchedPerson personId data ->
            let
                appMsgs =
                    -- If we succeed, we reset the form, and go to the page
                    -- showing the new person.
                    data
                        |> RemoteData.map
                            (\person ->
                                [ Pages.Person.Model.ResetEditForm
                                    |> App.Model.MsgPageEditPerson
                                    |> App.Model.MsgLoggedIn
                                , PersonPage personId ParticipantDirectoryOrigin
                                    |> UserPage
                                    |> App.Model.SetActivePage
                                ]
                            )
                        |> RemoteData.withDefault []
            in
            ( { model | postPerson = Success personId }
            , Cmd.none
            , appMsgs
            )

        PostSession session ->
            ( { model | postSession = Loading }
            , sw.post sessionEndpoint session
                |> toCmd (RemoteData.fromResult >> RemoteData.map Tuple.first >> HandlePostedSession session.clinicType)
            , []
            )

        HandlePostedSession clinicType data ->
            let
                msgs =
                    if clinicType == Chw then
                        data
                            |> RemoteData.map
                                (\sessionId ->
                                    SessionPage sessionId AttendancePage
                                        |> UserPage
                                        |> App.Model.SetActivePage
                                        |> List.singleton
                                )
                            |> RemoteData.withDefault []

                    else
                        []
            in
            ( { model | postSession = data }
            , Cmd.none
            , msgs
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

        PostIndividualSession extraData session ->
            ( { model | postIndividualSession = Dict.insert session.person Loading model.postIndividualSession }
            , sw.post individualEncounterParticipantEndpoint session
                |> toCmd (RemoteData.fromResult >> HandlePostedIndividualSession session.person session.encounterType extraData)
            , []
            )

        HandlePostedIndividualSession personId encounterType extraData data ->
            let
                -- We automatically create new encounter for newly created  session.
                appMsgs =
                    RemoteData.map
                        (\( sessionId, _ ) ->
                            case encounterType of
                                AcuteIllnessEncounter ->
                                    [ emptyAcuteIllnessEncounter sessionId currentDate 1 healthCenterId
                                        |> Backend.Model.PostAcuteIllnessEncounter
                                        |> App.Model.MsgIndexedDb
                                    ]

                                AntenatalEncounter ->
                                    case extraData of
                                        AntenatalData prenatalEncounterType ->
                                            [ emptyPrenatalEncounter sessionId currentDate prenatalEncounterType healthCenterId
                                                |> Backend.Model.PostPrenatalEncounter DestinationEncounterPage
                                                |> App.Model.MsgIndexedDb
                                            ]

                                        _ ->
                                            []

                                NutritionEncounter ->
                                    [ emptyNutritionEncounter sessionId currentDate healthCenterId
                                        |> Backend.Model.PostNutritionEncounter
                                        |> App.Model.MsgIndexedDb
                                    ]

                                HomeVisitEncounter ->
                                    [ emptyHomeVisitEncounter sessionId currentDate healthCenterId
                                        |> Backend.Model.PostHomeVisitEncounter
                                        |> App.Model.MsgIndexedDb
                                    ]

                                InmmunizationEncounter ->
                                    []
                        )
                        data
                        |> RemoteData.withDefault []
            in
            ( { model | postIndividualSession = Dict.insert personId data model.postIndividualSession }
            , Cmd.none
            , appMsgs
            )

        PostPrenatalEncounter postCreateDestination prenatalEncounter ->
            ( { model | postPrenatalEncounter = Dict.insert prenatalEncounter.participant Loading model.postPrenatalEncounter }
            , sw.post prenatalEncounterEndpoint prenatalEncounter
                |> toCmd (RemoteData.fromResult >> HandlePostedPrenatalEncounter prenatalEncounter.participant postCreateDestination)
            , []
            )

        HandlePostedPrenatalEncounter participantId postCreateDestination data ->
            ( { model | postPrenatalEncounter = Dict.insert participantId data model.postPrenatalEncounter }
            , Cmd.none
            , RemoteData.map
                (\( prenatalEncounterId, _ ) ->
                    let
                        destinationPage =
                            case postCreateDestination of
                                DestinationEncounterPage ->
                                    UserPage <| Pages.Page.PrenatalEncounterPage prenatalEncounterId

                                DestinationClinicalProgressReportPage ->
                                    UserPage <| ClinicalProgressReportPage (InitiatorNewEncounter prenatalEncounterId) prenatalEncounterId
                    in
                    [ App.Model.SetActivePage destinationPage ]
                )
                data
                |> RemoteData.withDefault []
            )

        PostNutritionEncounter nutritionEncounter ->
            ( { model | postNutritionEncounter = Dict.insert nutritionEncounter.participant Loading model.postNutritionEncounter }
            , sw.post nutritionEncounterEndpoint nutritionEncounter
                |> toCmd (RemoteData.fromResult >> HandlePostedNutritionEncounter nutritionEncounter.participant)
            , []
            )

        HandlePostedNutritionEncounter participantId data ->
            ( { model | postNutritionEncounter = Dict.insert participantId data model.postNutritionEncounter }
            , Cmd.none
            , RemoteData.map
                (\( nutritionEncounterId, _ ) ->
                    [ App.Model.SetActivePage <|
                        UserPage <|
                            Pages.Page.NutritionEncounterPage nutritionEncounterId
                    ]
                )
                data
                |> RemoteData.withDefault []
            )

        PostHomeVisitEncounter homeVisitEncounter ->
            ( { model | postHomeVisitEncounter = Dict.insert homeVisitEncounter.participant Loading model.postHomeVisitEncounter }
            , sw.post homeVisitEncounterEndpoint homeVisitEncounter
                |> toCmd (RemoteData.fromResult >> HandlePostedHomeVisitEncounter homeVisitEncounter.participant)
            , []
            )

        HandlePostedHomeVisitEncounter participantId data ->
            ( { model | postHomeVisitEncounter = Dict.insert participantId data model.postHomeVisitEncounter }
            , Cmd.none
            , RemoteData.map
                (\( homeVisitEncounterId, _ ) ->
                    [ App.Model.SetActivePage <|
                        UserPage <|
                            Pages.Page.HomeVisitEncounterPage homeVisitEncounterId
                    ]
                )
                data
                |> RemoteData.withDefault []
            )

        PostAcuteIllnessEncounter acuteIllnessEncounter ->
            ( { model | postAcuteIllnessEncounter = Dict.insert acuteIllnessEncounter.participant Loading model.postAcuteIllnessEncounter }
            , sw.post acuteIllnessEncounterEndpoint acuteIllnessEncounter
                |> toCmd (RemoteData.fromResult >> HandlePostedAcuteIllnessEncounter acuteIllnessEncounter.participant)
            , []
            )

        HandlePostedAcuteIllnessEncounter participantId data ->
            ( { model | postAcuteIllnessEncounter = Dict.insert participantId data model.postAcuteIllnessEncounter }
            , Cmd.none
            , RemoteData.map
                (\( acuteIllnessEncounterId, _ ) ->
                    [ App.Model.SetActivePage <|
                        UserPage <|
                            Pages.Page.AcuteIllnessEncounterPage acuteIllnessEncounterId
                    ]
                )
                data
                |> RemoteData.withDefault []
            )

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


{-| The extra return value indicates whether we need to recalculate our
successful EditableSessions. Ideally, we would handle this in a more
nuanced way.
-}
handleRevision : Maybe HealthCenterId -> Revision -> ( ModelIndexedDb, Bool ) -> ( ModelIndexedDb, Bool )
handleRevision healthCenterId revision (( model, recalc ) as noChange) =
    case revision of
        AcuteFindingsRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | acuteFindings = Just ( uuid, data ) })
                model
            , recalc
            )

        AcuteIllnessDangerSignsRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | dangerSigns = Just ( uuid, data ) })
                model
            , recalc
            )

        AcuteIllnessEncounterRevision uuid data ->
            let
                acuteIllnessEncounters =
                    Dict.update uuid (Maybe.map (always (Success data))) model.acuteIllnessEncounters

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
                        (\measurements -> { measurements | acuteIllness = Dict.insert uuid data measurements.acuteIllness })
                        model
            in
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | followUp = Just ( uuid, data ) })
                modelWithMappedFollowUp
            , recalc
            )

        AcuteIllnessMuacRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | muac = Just ( uuid, data ) })
                model
            , recalc
            )

        AcuteIllnessNutritionRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | nutrition = Just ( uuid, data ) })
                model
            , recalc
            )

        AcuteIllnessVitalsRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | vitals = Just ( uuid, data ) })
                model
            , recalc
            )

        AppointmentConfirmationRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | appointmentConfirmation = Just ( uuid, data ) })
                model
            , recalc
            )

        AttendanceRevision uuid data ->
            ( mapMotherMeasurements
                data.participantId
                (\measurements -> { measurements | attendances = Dict.insert uuid data measurements.attendances })
                model
            , True
            )

        BreastExamRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | breastExam = Just ( uuid, data ) })
                model
            , recalc
            )

        BirthPlanRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | birthPlan = Just ( uuid, data ) })
                model
            , recalc
            )

        Call114Revision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | call114 = Just ( uuid, data ) })
                model
            , recalc
            )

        CatchmentAreaRevision uuid data ->
            noChange

        ChildFbfRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | fbfs = Dict.insert uuid data measurements.fbfs })
                model
            , True
            )

        ChildNutritionRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | nutritions = Dict.insert uuid data measurements.nutritions })
                model
            , True
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
                (\measurements -> { measurements | contributingFactors = Dict.insert uuid data measurements.contributingFactors })
                model
            , True
            )

        CorePhysicalExamRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | corePhysicalExam = Just ( uuid, data ) })
                model
            , recalc
            )

        CounselingScheduleRevision uuid data ->
            -- Just invalidate our value ... if someone wants it, we'll refetch it.
            ( { model | everyCounselingSchedule = NotAsked }
            , True
            )

        CounselingSessionRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | counselingSessions = Dict.insert uuid data measurements.counselingSessions })
                model
            , True
            )

        CounselingTopicRevision uuid data ->
            ( { model | everyCounselingSchedule = NotAsked }
            , True
            )

        DangerSignsRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | dangerSigns = Just ( uuid, data ) })
                model
            , recalc
            )

        DashboardStatsRevision uuid data ->
            ( { model | computedDashboard = Dict.insert uuid data model.computedDashboard }, recalc )

        ExposureRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | exposure = Just ( uuid, data ) })
                model
            , recalc
            )

        FamilyPlanningRevision uuid data ->
            ( mapMotherMeasurements
                data.participantId
                (\measurements -> { measurements | familyPlannings = Dict.insert uuid data measurements.familyPlannings })
                model
            , True
            )

        FollowUpRevision uuid data ->
            let
                modelWithMappedFollowUp =
                    mapFollowUpMeasurements
                        healthCenterId
                        (\measurements -> { measurements | nutritionGroup = Dict.insert uuid data measurements.nutritionGroup })
                        model
            in
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | followUp = Dict.insert uuid data measurements.followUp })
                modelWithMappedFollowUp
            , True
            )

        GroupHealthEducationRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | healthEducation = Dict.insert uuid data measurements.healthEducation })
                model
            , True
            )

        GroupSendToHCRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | sendToHC = Dict.insert uuid data measurements.sendToHC })
                model
            , True
            )

        HCContactRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | hcContact = Just ( uuid, data ) })
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
                (\measurements -> { measurements | healthEducation = Just ( uuid, data ) })
                model
            , recalc
            )

        HeightRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | heights = Dict.insert uuid data measurements.heights })
                model
            , True
            )

        HomeVisitEncounterRevision uuid data ->
            let
                homeVisitEncounters =
                    Dict.update uuid (Maybe.map (always (Success data))) model.homeVisitEncounters

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
                (\measurements -> { measurements | isolation = Just ( uuid, data ) })
                model
            , recalc
            )

        IndividualEncounterParticipantRevision uuid data ->
            let
                individualParticipants =
                    Dict.update uuid (Maybe.map (always (Success data))) model.individualParticipants

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
                (\measurements -> { measurements | lactations = Dict.insert uuid data measurements.lactations })
                model
            , True
            )

        LastMenstrualPeriodRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | lastMenstrualPeriod = Just ( uuid, data ) })
                model
            , recalc
            )

        MalariaTestingRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | malariaTesting = Just ( uuid, data ) })
                model
            , recalc
            )

        MedicalHistoryRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | medicalHistory = Just ( uuid, data ) })
                model
            , recalc
            )

        MedicationRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | medication = Just ( uuid, data ) })
                model
            , recalc
            )

        MedicationDistributionRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | medicationDistribution = Just ( uuid, data ) })
                model
            , recalc
            )

        MotherFbfRevision uuid data ->
            ( mapMotherMeasurements
                data.participantId
                (\measurements -> { measurements | fbfs = Dict.insert uuid data measurements.fbfs })
                model
            , True
            )

        MuacRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | muacs = Dict.insert uuid data measurements.muacs })
                model
            , True
            )

        NurseRevision uuid data ->
            -- Nothing to do in ModelIndexedDb yet. App.Update does do something with this one.
            noChange

        NutritionCaringRevision uuid data ->
            ( mapHomeVisitMeasurements
                data.encounterId
                (\measurements -> { measurements | caring = Just ( uuid, data ) })
                model
            , recalc
            )

        NutritionContributingFactorsRevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements -> { measurements | contributingFactors = Just ( uuid, data ) })
                model
            , recalc
            )

        NutritionEncounterRevision uuid data ->
            let
                nutritionEncounters =
                    Dict.update uuid (Maybe.map (always (Success data))) model.nutritionEncounters

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
                (\measurements -> { measurements | feeding = Just ( uuid, data ) })
                model
            , recalc
            )

        NutritionFollowUpRevision uuid data ->
            let
                modelWithMappedFollowUp =
                    mapFollowUpMeasurements
                        healthCenterId
                        (\measurements -> { measurements | nutritionIndividual = Dict.insert uuid data measurements.nutritionIndividual })
                        model
            in
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements -> { measurements | followUp = Just ( uuid, data ) })
                modelWithMappedFollowUp
            , recalc
            )

        NutritionFoodSecurityRevision uuid data ->
            ( mapHomeVisitMeasurements
                data.encounterId
                (\measurements -> { measurements | foodSecurity = Just ( uuid, data ) })
                model
            , recalc
            )

        NutritionHealthEducationRevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements -> { measurements | healthEducation = Just ( uuid, data ) })
                model
            , recalc
            )

        NutritionHeightRevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements -> { measurements | height = Just ( uuid, data ) })
                model
            , recalc
            )

        NutritionHygieneRevision uuid data ->
            ( mapHomeVisitMeasurements
                data.encounterId
                (\measurements -> { measurements | hygiene = Just ( uuid, data ) })
                model
            , recalc
            )

        NutritionMuacRevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements -> { measurements | muac = Just ( uuid, data ) })
                model
            , recalc
            )

        NutritionNutritionRevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements -> { measurements | nutrition = Just ( uuid, data ) })
                model
            , recalc
            )

        NutritionPhotoRevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements -> { measurements | photo = Just ( uuid, data ) })
                model
            , recalc
            )

        NutritionSendToHCRevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements -> { measurements | sendToHC = Just ( uuid, data ) })
                model
            , recalc
            )

        NutritionWeightRevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements -> { measurements | weight = Just ( uuid, data ) })
                model
            , recalc
            )

        ObstetricalExamRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | obstetricalExam = Just ( uuid, data ) })
                model
            , recalc
            )

        ObstetricHistoryRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | obstetricHistory = Just ( uuid, data ) })
                model
            , recalc
            )

        ObstetricHistoryStep2Revision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | obstetricHistoryStep2 = Just ( uuid, data ) })
                model
            , recalc
            )

        ParticipantConsentRevision uuid data ->
            ( mapMotherMeasurements
                data.participantId
                (\measurements -> { measurements | consents = Dict.insert uuid data measurements.consents })
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
                    Dict.update uuid (Maybe.map (always (Success data))) model.people
            in
            ( { model
                | personSearches = Dict.empty
                , people = people
              }
            , True
            )

        PhotoRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | photos = Dict.insert uuid data measurements.photos })
                model
            , True
            )

        PmtctParticipantRevision uuid data ->
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

        PregnancyTestingRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | pregnancyTest = Just ( uuid, data ) })
                model
            , recalc
            )

        PrenatalHealthEducationRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | healthEducation = Just ( uuid, data ) })
                model
            , recalc
            )

        PrenatalEncounterRevision uuid data ->
            let
                prenatalEncounters =
                    Dict.update uuid (Maybe.map (always (Success data))) model.prenatalEncounters

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
                (\measurements -> { measurements | familyPlanning = Just ( uuid, data ) })
                model
            , recalc
            )

        PrenatalFollowUpRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | followUp = Just ( uuid, data ) })
                model
            , recalc
            )

        PrenatalNutritionRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | nutrition = Just ( uuid, data ) })
                model
            , recalc
            )

        PrenatalPhotoRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | prenatalPhoto = Just ( uuid, data ) })
                model
            , recalc
            )

        PrenatalSendToHCRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | sendToHC = Just ( uuid, data ) })
                model
            , recalc
            )

        RelationshipRevision uuid data ->
            ( { model | relationshipsByPerson = Dict.empty }
            , True
            )

        ResourceRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | resource = Just ( uuid, data ) })
                model
            , recalc
            )

        SendToHCRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | sendToHC = Just ( uuid, data ) })
                model
            , recalc
            )

        SessionRevision uuid data ->
            let
                -- First, remove the session from all clinics (it might
                -- previously have been in any). Then, add it in the right
                -- place.
                sessionsByClinic =
                    model.sessionsByClinic
                        |> Dict.map (always (RemoteData.map (Dict.remove uuid)))
                        |> Dict.update data.clinicId (Maybe.map (RemoteData.map (Dict.insert uuid data)))
            in
            ( { model
                | sessionsByClinic = sessionsByClinic
                , expectedParticipants = Dict.remove uuid model.expectedParticipants
                , expectedSessions = Dict.empty
                , sessions = Dict.insert uuid (Success data) model.sessions
              }
            , True
            )

        SocialHistoryRevision uuid data ->
            ( mapPrenatalMeasurements
                data.encounterId
                (\measurements -> { measurements | socialHistory = Just ( uuid, data ) })
                model
            , recalc
            )

        SymptomsGeneralRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | symptomsGeneral = Just ( uuid, data ) })
                model
            , recalc
            )

        SymptomsGIRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | symptomsGI = Just ( uuid, data ) })
                model
            , recalc
            )

        SymptomsRespiratoryRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | symptomsRespiratory = Just ( uuid, data ) })
                model
            , recalc
            )

        TravelHistoryRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | travelHistory = Just ( uuid, data ) })
                model
            , recalc
            )

        TreatmentOngoingRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | treatmentOngoing = Just ( uuid, data ) })
                model
            , recalc
            )

        TreatmentReviewRevision uuid data ->
            ( mapAcuteIllnessMeasurements
                data.encounterId
                (\measurements -> { measurements | treatmentReview = Just ( uuid, data ) })
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
                (\measurements -> { measurements | vitals = Just ( uuid, data ) })
                model
            , recalc
            )

        WeightRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | weights = Dict.insert uuid data measurements.weights })
                model
            , True
            )


generatePrenatalDangerSignsWarningMsgs : NominalDate -> Language -> Bool -> ModelIndexedDb -> PrenatalEncounterId -> List App.Model.Msg
generatePrenatalDangerSignsWarningMsgs currentDate language isChw after id =
    if not isChw then
        -- Assement is done only for CHW.
        []

    else
        Maybe.map
            (\assembledAfter ->
                let
                    mandatoryActivitiesCompleted =
                        Pages.PrenatalEncounter.Utils.mandatoryActivitiesForNextStepsCompleted
                            currentDate
                            assembledAfter
                in
                if not mandatoryActivitiesCompleted then
                    -- Assement is done only when all mandatory measurements were recorded.
                    []

                else
                    let
                        dangerSignsList =
                            Pages.PrenatalEncounter.Utils.generateDangerSignsList language
                                assembledAfter
                    in
                    if List.isEmpty dangerSignsList then
                        []

                    else
                        [ -- Navigate to NextSteps activty page.
                          PrenatalActivityPage id Backend.PrenatalActivity.Model.NextSteps
                            |> UserPage
                            |> App.Model.SetActivePage
                        , String.join ", " dangerSignsList
                            |> Just
                            |> Pages.PrenatalActivity.Model.SetWarningPopupState
                            |> App.Model.MsgPagePrenatalActivity id Backend.PrenatalActivity.Model.NextSteps
                            |> App.Model.MsgLoggedIn
                        ]
            )
            (RemoteData.toMaybe <| Pages.PrenatalEncounter.Utils.generateAssembledData id after)
            |> Maybe.withDefault []


generateNutritionAssessmentIndividualMsgs : NominalDate -> ZScore.Model.Model -> Bool -> ModelIndexedDb -> NutritionEncounterId -> List App.Model.Msg
generateNutritionAssessmentIndividualMsgs currentDate zscores isChw after id =
    if not isChw then
        -- Assement is done only for CHW.
        []

    else
        Maybe.map
            (\assembledAfter ->
                let
                    mandatoryActivitiesCompleted =
                        Pages.NutritionActivity.Utils.mandatoryActivitiesCompleted
                            currentDate
                            zscores
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
                        assesmentAfter =
                            Pages.NutritionActivity.Utils.generateNutritionAssesment currentDate zscores after assembledAfter

                        updateFollowUpAssesmentMsg =
                            assembledAfter.measurements.followUp
                                |> Maybe.map
                                    (\( measurementId, measurement ) ->
                                        let
                                            updatedValue =
                                                measurement.value
                                                    |> (\value -> { value | assesment = nutritionAssesmentForBackend assesmentAfter })
                                        in
                                        Backend.NutritionEncounter.Model.SaveFollowUp assembledAfter.participant.person (Just measurementId) updatedValue
                                            |> Backend.Model.MsgNutritionEncounter id
                                            |> App.Model.MsgIndexedDb
                                            |> List.singleton
                                    )
                                |> Maybe.withDefault []
                    in
                    if List.isEmpty assesmentAfter then
                        -- No assesment, so, only thing we want to update is the
                        -- assesment field on Follow Up measurement, if it exists already.
                        updateFollowUpAssesmentMsg

                    else
                        updateFollowUpAssesmentMsg
                            ++ [ -- Navigate to Nutrition encounter page.
                                 App.Model.SetActivePage (UserPage (NutritionActivityPage id Backend.NutritionActivity.Model.NextSteps))

                               -- Show warning popup with new assesment.
                               , Pages.NutritionActivity.Model.SetWarningPopupState assesmentAfter
                                    |> App.Model.MsgPageNutritionActivity id Backend.NutritionActivity.Model.NextSteps
                                    |> App.Model.MsgLoggedIn
                               ]
            )
            (RemoteData.toMaybe <| Pages.NutritionEncounter.Utils.generateAssembledData id after)
            |> Maybe.withDefault []


generateNutritionAssessmentGroupMsgs :
    NominalDate
    -> ZScore.Model.Model
    -> Bool
    -> PersonId
    -> SessionId
    -> Page
    -> (ChildMeasurements -> ChildMeasurements)
    -> ModelIndexedDb
    -> List App.Model.Msg
generateNutritionAssessmentGroupMsgs currentDate zscores isChw childId sessionId activePage updateFunc db =
    if not isChw then
        -- Assement is done only for CHW.
        []

    else
        Dict.get sessionId db.editableSessions
            |> Maybe.andThen RemoteData.toMaybe
            |> Maybe.map
                (\session ->
                    let
                        -- We simulate session data after measurement data is performed.
                        offlineSession =
                            mapChildMeasurementsAtOfflineSession childId updateFunc session.offlineSession

                        mandatoryActivitiesCompleted =
                            Activity.Utils.mandatoryActivitiesCompleted
                                currentDate
                                zscores
                                offlineSession
                                childId
                                isChw
                                db
                    in
                    if not mandatoryActivitiesCompleted then
                        []

                    else
                        let
                            assesment =
                                Activity.Utils.generateNutritionAssesment
                                    currentDate
                                    zscores
                                    childId
                                    db
                                    offlineSession

                            updateFollowUpAssesmentMsg =
                                getChildMeasurementData2 childId offlineSession
                                    |> LocalData.unwrap
                                        []
                                        (\measurements ->
                                            let
                                                followUp =
                                                    mapMeasurementData .followUp measurements
                                                        |> .current

                                                followUpId =
                                                    Maybe.map Tuple.first followUp

                                                followUpValue =
                                                    Maybe.map (Tuple.second >> .value) followUp
                                            in
                                            followUpValue
                                                |> Maybe.map
                                                    (\value ->
                                                        let
                                                            updatedValue =
                                                                { value | assesment = nutritionAssesmentForBackend assesment }
                                                        in
                                                        Measurement.Model.SaveFollowUp followUpId updatedValue
                                                            |> Backend.Session.Model.MeasurementOutMsgChild childId
                                                            |> Backend.Model.MsgSession sessionId
                                                            |> App.Model.MsgIndexedDb
                                                            |> List.singleton
                                                    )
                                                |> Maybe.withDefault []
                                        )

                            personByPersonMsgs =
                                updateFollowUpAssesmentMsg
                                    ++ [ Pages.Participant.Model.SetWarningPopupState assesment
                                            |> Pages.Session.Model.MsgChild childId
                                            |> App.Model.MsgPageSession sessionId
                                            |> App.Model.MsgLoggedIn
                                       ]

                            activityByActivityMsgs childActivity =
                                updateFollowUpAssesmentMsg
                                    ++ [ App.Model.SetActivePage (UserPage (SessionPage sessionId (NextStepsPage childId (ChildActivity childActivity))))
                                       , Pages.NextSteps.Model.SetWarningPopupState assesment
                                            |> Pages.Session.Model.MsgNextSteps childId (ChildActivity childActivity)
                                            |> App.Model.MsgPageSession sessionId
                                            |> App.Model.MsgLoggedIn
                                       ]
                        in
                        if List.isEmpty assesment then
                            -- No assesment, so, only thing we want to update is the
                            -- assesment field on Follow Up measurement, if it exists already.
                            updateFollowUpAssesmentMsg

                        else
                            case activePage of
                                UserPage (SessionPage _ (ChildPage _)) ->
                                    personByPersonMsgs

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


generateSuspectedDiagnosisMsgs : NominalDate -> ModelIndexedDb -> ModelIndexedDb -> AcuteIllnessEncounterId -> Person -> List App.Model.Msg
generateSuspectedDiagnosisMsgs currentDate before after id person =
    Maybe.map2
        (\assembledBefore assembledAfter ->
            if List.isEmpty assembledAfter.previousEncountersData then
                generateSuspectedDiagnosisMsgsFirstEncounter currentDate id person assembledBefore assembledAfter

            else
                generateSuspectedDiagnosisMsgsSubsequentEncounter currentDate assembledAfter
        )
        (RemoteData.toMaybe <| Pages.AcuteIllnessEncounter.Utils.generateAssembledData currentDate id before)
        (RemoteData.toMaybe <| Pages.AcuteIllnessEncounter.Utils.generateAssembledData currentDate id after)
        |> Maybe.withDefault []


generateSuspectedDiagnosisMsgsFirstEncounter :
    NominalDate
    -> AcuteIllnessEncounterId
    -> Person
    -> Pages.AcuteIllnessEncounter.Model.AssembledData
    -> Pages.AcuteIllnessEncounter.Model.AssembledData
    -> List App.Model.Msg
generateSuspectedDiagnosisMsgsFirstEncounter currentDate id person assembledBefore assembledAfter =
    let
        diagnosisBeforeChange =
            Maybe.map Tuple.second assembledBefore.diagnosis

        diagnosisAfterChange =
            Maybe.map Tuple.second assembledAfter.diagnosis

        msgsForDiagnosisUpdate =
            case diagnosisAfterChange of
                Just newDiagnosis ->
                    updateDiagnosisMsg id newDiagnosis
                        :: (case resolveNextStepFirstEncounter currentDate assembledAfter of
                                Just nextStep ->
                                    [ -- Navigate to Acute Ilness NextSteps activty page.
                                      App.Model.SetActivePage (UserPage (AcuteIllnessActivityPage id AcuteIllnessNextSteps))
                                    , -- Focus on first task on that page.
                                      Pages.AcuteIllnessActivity.Model.SetActiveNextStepsTask nextStep
                                        |> App.Model.MsgPageAcuteIllnessActivity id AcuteIllnessNextSteps
                                        |> App.Model.MsgLoggedIn

                                    -- Show warning popup with new diagnosis.
                                    , Pages.AcuteIllnessActivity.Model.SetWarningPopupState (Just newDiagnosis)
                                        |> App.Model.MsgPageAcuteIllnessActivity id AcuteIllnessNextSteps
                                        |> App.Model.MsgLoggedIn
                                    ]

                                Nothing ->
                                    [ -- Navigate to Acute Ilness encounter page.
                                      App.Model.SetActivePage (UserPage (AcuteIllnessEncounterPage id))

                                    -- Show warning popup with new diagnosis.
                                    , Pages.AcuteIllnessEncounter.Model.SetWarningPopupState (Just newDiagnosis)
                                        |> App.Model.MsgPageAcuteIllnessEncounter id
                                        |> App.Model.MsgLoggedIn
                                    ]
                           )

                Nothing ->
                    [ updateDiagnosisMsg id NoAcuteIllnessDiagnosis ]
    in
    if diagnosisBeforeChange /= diagnosisAfterChange then
        msgsForDiagnosisUpdate

    else
        []


generateSuspectedDiagnosisMsgsSubsequentEncounter : NominalDate -> Pages.AcuteIllnessEncounter.Model.AssembledData -> List App.Model.Msg
generateSuspectedDiagnosisMsgsSubsequentEncounter currentDate data =
    if mandatoryActivitiesCompletedSubsequentVisit currentDate data then
        let
            diagnosisByCurrentEncounterMeasurements =
                resolveAcuteIllnessDiagnosis currentDate data
                    |> Maybe.withDefault NoAcuteIllnessDiagnosis

            setDiagnosisMsg =
                -- We have an update to diagnosis based on current measurments,
                -- and it is not yet set for the encounter.
                if data.encounter.diagnosis == NoAcuteIllnessDiagnosis && diagnosisByCurrentEncounterMeasurements /= NoAcuteIllnessDiagnosis then
                    [ updateDiagnosisMsg data.id diagnosisByCurrentEncounterMeasurements ]

                else
                    []

            setActiveTaskMsg =
                resolveNextStepSubsequentEncounter currentDate data
                    |> Maybe.map
                        (Pages.AcuteIllnessActivity.Model.SetActiveNextStepsTask
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
            |> Pages.AcuteIllnessActivity.Model.SetWarningPopupState
            |> App.Model.MsgPageAcuteIllnessActivity data.id AcuteIllnessNextSteps
            |> App.Model.MsgLoggedIn
        ]
            ++ -- Set diagnosis for this encounter.
               setDiagnosisMsg
            ++ -- Focus on first task on that page.
               setActiveTaskMsg

    else
        []


updateDiagnosisMsg : AcuteIllnessEncounterId -> AcuteIllnessDiagnosis -> App.Model.Msg
updateDiagnosisMsg id diagnosis =
    Backend.AcuteIllnessEncounter.Model.SetAcuteIllnessDiagnosis diagnosis
        |> Backend.Model.MsgAcuteIllnessEncounter id
        |> App.Model.MsgIndexedDb


generateAcuteIllnessAssesmentCompletedMsgs : NominalDate -> ModelIndexedDb -> AcuteIllnessEncounterId -> List App.Model.Msg
generateAcuteIllnessAssesmentCompletedMsgs currentDate after id =
    Pages.AcuteIllnessEncounter.Utils.generateAssembledData currentDate id after
        |> RemoteData.toMaybe
        |> Maybe.map
            (\data ->
                let
                    navigateToProgressReportMsg =
                        App.Model.SetActivePage (UserPage (AcuteIllnessProgressReportPage id))

                    isFirstEncounter =
                        List.isEmpty data.previousEncountersData
                in
                if not <| activityCompleted currentDate isFirstEncounter data AcuteIllnessNextSteps then
                    []

                else if isFirstEncounter then
                    [ navigateToProgressReportMsg ]

                else if noImprovementOnSubsequentVisit currentDate data.person data.measurements then
                    [ navigateToProgressReportMsg ]

                else
                    [ App.Model.SetActivePage (UserPage (AcuteIllnessOutcomePage data.encounter.participant)) ]
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
            db.people
                |> Dict.values
                |> List.filter (\v -> RemoteData.isSuccess v)
                |> List.isEmpty
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
summarizeByParticipant : NominalDate -> ZScore.Model.Model -> OfflineSession -> LocalData CheckedIn -> Bool -> ModelIndexedDb -> LocalData SummaryByParticipant
summarizeByParticipant currentDate zscores session checkedIn_ isChw db =
    LocalData.map
        (\checkedIn ->
            let
                children =
                    Dict.map
                        (\childId _ -> summarizeChildParticipant currentDate zscores childId session isChw db)
                        checkedIn.children

                mothers =
                    Dict.map
                        (\motherId _ -> summarizeMotherParticipant currentDate zscores motherId session isChw db)
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
summarizeByActivity : NominalDate -> ZScore.Model.Model -> OfflineSession -> LocalData CheckedIn -> Bool -> ModelIndexedDb -> LocalData SummaryByActivity
summarizeByActivity currentDate zscores session checkedIn_ isChw db =
    LocalData.map
        (\checkedIn ->
            let
                children =
                    getAllChildActivities session
                        |> List.map
                            (\activity ->
                                ( activity
                                , summarizeChildActivity currentDate zscores activity session isChw db checkedIn
                                )
                            )
                        |> Dict.fromList

                mothers =
                    getAllMotherActivities session
                        |> List.map
                            (\activity ->
                                ( activity
                                , summarizeMotherActivity currentDate zscores activity session isChw db checkedIn
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
