module Backend.Update exposing (updateIndexedDb)

import Activity.Model exposing (SummaryByActivity, SummaryByParticipant)
import Activity.Utils exposing (getAllChildActivities, getAllMotherActivities, motherIsCheckedIn, summarizeChildActivity, summarizeChildParticipant, summarizeMotherActivity, summarizeMotherParticipant)
import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Clinic.Model exposing (ClinicType(..))
import Backend.Counseling.Decoder exposing (combineCounselingSchedules)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Fetch
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.IndividualEncounterParticipant.Update
import Backend.Measurement.Model exposing (HistoricalMeasurements, Measurements)
import Backend.Measurement.Utils exposing (splitChildMeasurements, splitMotherMeasurements)
import Backend.Model exposing (..)
import Backend.NutritionEncounter.Model
import Backend.NutritionEncounter.Update
import Backend.Person.Model exposing (RegistrationInitiator(..))
import Backend.PmtctParticipant.Model exposing (AdultActivities(..))
import Backend.PrenatalEncounter.Model
import Backend.PrenatalEncounter.Update
import Backend.Relationship.Encoder exposing (encodeRelationshipChanges)
import Backend.Relationship.Model exposing (RelatedBy(..))
import Backend.Relationship.Utils exposing (toMyRelationship, toRelationship)
import Backend.Session.Model exposing (CheckedIn, EditableSession, OfflineSession, Session)
import Backend.Session.Update
import Backend.Session.Utils exposing (getMyMother)
import Backend.Utils exposing (mapChildMeasurements, mapMotherMeasurements, mapNutritionMeasurements, mapPrenatalMeasurements)
import Date exposing (Unit(..))
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Json.Encode exposing (object)
import LocalData exposing (LocalData(..), ReadyStatus(..))
import Maybe.Extra exposing (isJust, unwrap)
import Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))
import Pages.Person.Model
import Pages.Relationship.Model
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (EntityUuid, ReadOnlyEndPoint, ReadWriteEndPoint, applyBackendUrl, toCmd, toTask, withoutDecoder)
import Task


updateIndexedDb : NominalDate -> Maybe NurseId -> Maybe HealthCenterId -> Bool -> MsgIndexedDb -> ModelIndexedDb -> ( ModelIndexedDb, Cmd MsgIndexedDb, List App.Model.Msg )
updateIndexedDb currentDate nurseId healthCenterId isChw msg model =
    let
        sw =
            applyBackendUrl "/sw"

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
                |> sequenceExtra (updateIndexedDb currentDate nurseId healthCenterId isChw) extraMsgs

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
                                summarizeByActivity currentDate editable.offlineSession editable.checkedIn isChw

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
                                summarizeByParticipant currentDate editable.offlineSession editable.checkedIn isChw

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
            -- We'll limit the search to 100 each for now ... basically,
            -- just to avoid truly pathological cases.
            ( { model | personSearches = Dict.insert trimmed Loading model.personSearches }
            , sw.selectRange personEndpoint { nameContains = Just trimmed } 0 (Just 100)
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

        FetchSyncData ->
            ( { model | syncData = Loading }
            , sw.select syncDataEndpoint ()
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> Dict.fromList) >> HandleFetchedSyncData)
            , []
            )

        HandleFetchedSyncData data ->
            ( { model | syncData = data }
            , Cmd.none
            , []
            )

        HandleRevisions revisions ->
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

                _ ->
                    let
                        ( newModel, recalculateEditableSessions ) =
                            List.foldl handleRevision ( model, False ) revisions

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

        SaveSyncData uuid data ->
            ( { model | saveSyncDataRequests = Dict.insert uuid Loading model.saveSyncDataRequests }
            , sw.put syncDataEndpoint uuid data
                |> withoutDecoder
                |> toCmd (RemoteData.fromResult >> HandleSavedSyncData uuid)
            , []
            )

        HandleSavedSyncData uuid data ->
            ( { model | saveSyncDataRequests = Dict.insert uuid data model.saveSyncDataRequests }
            , Cmd.none
            , []
            )

        DeleteSyncData uuid ->
            ( { model | deleteSyncDataRequests = Dict.insert uuid Loading model.deleteSyncDataRequests }
            , sw.delete syncDataEndpoint uuid
                |> toCmd (RemoteData.fromResult >> HandleDeletedSyncData uuid)
            , []
            )

        HandleDeletedSyncData uuid data ->
            ( { model | deleteSyncDataRequests = Dict.insert uuid data model.deleteSyncDataRequests }
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

        PostPmtctParticipant data ->
            ( { model | postPmtctParticipant = Dict.insert data.child Loading model.postPmtctParticipant }
            , sw.post pmtctParticipantEndpoint data
                |> toCmd (RemoteData.fromResult >> HandlePostedPmtctParticipant data.child)
            , []
            )

        HandlePostedPmtctParticipant id data ->
            ( { model | postPmtctParticipant = Dict.insert id data model.postPmtctParticipant }
            , Cmd.none
            , []
            )

        PostRelationship personId myRelationship addGroup ->
            let
                -- If we'd also like to add these people to a group, construct
                -- a Msg to do that.
                extraMsgs =
                    addGroup
                        |> Maybe.map
                            (\clinicId ->
                                PostPmtctParticipant
                                    { adult = normalized.person
                                    , child = normalized.relatedTo
                                    , adultActivities = defaultAdultActivities
                                    , start = defaultStartDate
                                    , end = Nothing
                                    , clinic = clinicId
                                    }
                            )
                        |> Maybe.Extra.toList

                -- The start date determines when we start expecting this pair
                -- to be attending a group encounter. We'll look to see if we
                -- know the child's birth date. Normally, we will, because
                -- we've probably just entered it, or we've loaded the child
                -- for some other reason. We won't try to fetch the child here
                -- if we don't have the child, at least for now, because it
                -- would add complexity.  If we don't know the child's
                -- birthdate, we'll default to 28 days ago. That should be
                -- enough so that, if we're in the middle of a group encounter,
                -- the child will be expected at that group encounter.
                defaultStartDate =
                    Dict.get normalized.relatedTo model.people
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe
                        |> Maybe.andThen .birthDate
                        |> Maybe.withDefault (Date.add Days -28 currentDate)

                defaultAdultActivities =
                    case normalized.relatedBy of
                        ParentOf ->
                            MotherActivities

                        CaregiverFor ->
                            CaregiverActivities

                normalized =
                    toRelationship personId myRelationship

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
                        |> Task.perform (HandlePostedRelationship personId)
            in
            ( { model | postRelationship = Dict.insert personId Loading model.postRelationship }
            , relationshipCmd
            , []
            )
                |> sequenceExtra (updateIndexedDb currentDate nurseId healthCenterId isChw) extraMsgs

        HandlePostedRelationship personId data ->
            let
                appMsgs =
                    data
                        |> RemoteData.map
                            (\relationship ->
                                [ Pages.Relationship.Model.Reset
                                    |> App.Model.MsgPageRelationship personId relationship.relatedTo
                                    |> App.Model.MsgLoggedIn
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
                appMsgs =
                    -- If we succeed, we reset the form, and go to the page
                    -- showing the new person.
                    data
                        |> RemoteData.map
                            (\personId ->
                                let
                                    nextPage =
                                        case initiator of
                                            ParticipantDirectoryOrigin ->
                                                case relation of
                                                    Just id ->
                                                        RelationshipPage id personId

                                                    Nothing ->
                                                        PersonPage personId

                                            IndividualEncounterOrigin encounterType ->
                                                case encounterType of
                                                    AntenatalEncounter ->
                                                        PrenatalParticipantPage personId

                                                    NutritionEncounter ->
                                                        NutritionParticipantPage personId

                                                    _ ->
                                                        -- This will change as we add support for
                                                        -- new encounter types.
                                                        IndividualEncounterTypesPage
                                in
                                [ Pages.Person.Model.ResetCreateForm
                                    |> App.Model.MsgPageCreatePerson
                                    |> App.Model.MsgLoggedIn
                                , nextPage
                                    |> UserPage
                                    |> App.Model.SetActivePage
                                ]
                            )
                        |> RemoteData.withDefault []
            in
            ( { model | postPerson = data }
            , Cmd.none
            , appMsgs
            )

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
                                , PersonPage personId
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

        PostIndividualSession session ->
            ( { model | postIndividualSession = Dict.insert session.person Loading model.postIndividualSession }
            , sw.post individualEncounterParticipantEndpoint session
                |> toCmd (RemoteData.fromResult >> HandlePostedIndividualSession session.person session.encounterType)
            , []
            )

        HandlePostedIndividualSession personId encounterType data ->
            let
                -- We automatically create new encounter for newly created  session.
                appMsgs =
                    RemoteData.map
                        (\( sessionId, _ ) ->
                            case encounterType of
                                AntenatalEncounter ->
                                    [ Backend.PrenatalEncounter.Model.PrenatalEncounter sessionId currentDate Nothing
                                        |> Backend.Model.PostPrenatalEncounter
                                        |> App.Model.MsgIndexedDb
                                    ]

                                NutritionEncounter ->
                                    [ Backend.NutritionEncounter.Model.NutritionEncounter sessionId currentDate Nothing
                                        |> Backend.Model.PostNutritionEncounter
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

        PostPrenatalEncounter prenatalEncounter ->
            ( { model | postPrenatalEncounter = Dict.insert prenatalEncounter.participant Loading model.postPrenatalEncounter }
            , sw.post prenatalEncounterEndpoint prenatalEncounter
                |> toCmd (RemoteData.fromResult >> HandlePostedPrenatalEncounter prenatalEncounter.participant)
            , []
            )

        HandlePostedPrenatalEncounter participantId data ->
            ( { model | postPrenatalEncounter = Dict.insert participantId data model.postPrenatalEncounter }
            , Cmd.none
            , RemoteData.map
                (\( prenatalEncounterId, _ ) ->
                    [ App.Model.SetActivePage <|
                        UserPage <|
                            Pages.Page.PrenatalEncounterPage prenatalEncounterId
                    ]
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


{-| The extra return value indicates whether we need to recalculate our
successful EditableSessions. Ideally, we would handle this in a more
nuanced way.
-}
handleRevision : Revision -> ( ModelIndexedDb, Bool ) -> ( ModelIndexedDb, Bool )
handleRevision revision (( model, recalc ) as noChange) =
    case revision of
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

        FamilyPlanningRevision uuid data ->
            ( mapMotherMeasurements
                data.participantId
                (\measurements -> { measurements | familyPlannings = Dict.insert uuid data measurements.familyPlannings })
                model
            , True
            )

        LactationRevision uuid data ->
            ( mapMotherMeasurements
                data.participantId
                (\measurements -> { measurements | lactations = Dict.insert uuid data measurements.lactations })
                model
            , True
            )

        HealthCenterRevision uuid data ->
            let
                healthCenters =
                    RemoteData.map (Dict.insert uuid data) model.healthCenters
            in
            ( { model | healthCenters = healthCenters }
            , recalc
            )

        HeightRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | heights = Dict.insert uuid data measurements.heights })
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

        NutritionHeightRevision uuid data ->
            ( mapNutritionMeasurements
                data.encounterId
                (\measurements -> { measurements | height = Just ( uuid, data ) })
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
    in
    -- Make sure we don't still have measurements being lazy loaded.
    -- If we do, allow rebuilding the `EditableSession`.
    if hasMothersMeasurementsNotSuccess || hasChildrenMeasurementsNotSuccess then
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
summarizeByParticipant : NominalDate -> OfflineSession -> LocalData CheckedIn -> Bool -> LocalData SummaryByParticipant
summarizeByParticipant currentDate session checkedIn_ isChw =
    LocalData.map
        (\checkedIn ->
            let
                children =
                    Dict.map
                        (\childId _ -> summarizeChildParticipant currentDate childId session isChw)
                        checkedIn.children

                mothers =
                    Dict.map
                        (\motherId _ -> summarizeMotherParticipant currentDate motherId session isChw)
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
summarizeByActivity : NominalDate -> OfflineSession -> LocalData CheckedIn -> Bool -> LocalData SummaryByActivity
summarizeByActivity currentDate session checkedIn_ isChw =
    LocalData.map
        (\checkedIn ->
            let
                children =
                    getAllChildActivities session
                        |> List.map
                            (\activity ->
                                ( activity
                                , summarizeChildActivity currentDate activity session isChw checkedIn
                                )
                            )
                        |> Dict.fromList

                mothers =
                    getAllMotherActivities session
                        |> List.map
                            (\activity ->
                                ( activity
                                , summarizeMotherActivity currentDate activity session isChw checkedIn
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
