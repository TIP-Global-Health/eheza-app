module Backend.Update exposing (updateIndexedDb)

import Activity.Model exposing (SummaryByActivity, SummaryByParticipant)
import Activity.Utils exposing (getAllChildActivities, getAllMotherActivities, motherIsCheckedIn, summarizeChildActivity, summarizeChildParticipant, summarizeMotherActivity, summarizeMotherParticipant)
import App.Model
import Backend.Counseling.Decoder exposing (combineCounselingSchedules)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (HistoricalMeasurements)
import Backend.Measurement.Utils exposing (splitChildMeasurements, splitMotherMeasurements)
import Backend.Model exposing (..)
import Backend.Person.Model exposing (RegistrationInitiator(..))
import Backend.PmtctParticipant.Model exposing (AdultActivities(..))
import Backend.PrenatalEncounter.Model
import Backend.PrenatalEncounter.Update
import Backend.PrenatalParticipant.Model
import Backend.PrenatalParticipant.Update
import Backend.Relationship.Encoder exposing (encodeRelationshipChanges)
import Backend.Relationship.Model exposing (RelatedBy(..))
import Backend.Relationship.Utils exposing (toMyRelationship, toRelationship)
import Backend.Session.Model exposing (CheckedIn, EditableSession, OfflineSession, Session)
import Backend.Session.Update
import Backend.Session.Utils exposing (getMyMother)
import Backend.Utils exposing (mapChildMeasurements, mapMotherMeasurements, mapPrenatalMeasurements)
import Dict
import EveryDict
import EveryDictList
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Json.Encode exposing (object)
import Lazy exposing (lazy)
import Maybe.Extra
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Person.Model
import Pages.Relationship.Model
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (EntityUuid, ReadOnlyEndPoint, ReadWriteEndPoint, applyAccessToken, applyBackendUrl, decodeEntityUuid, decodeSingleDrupalEntity, drupalBackend, drupalEndpoint, encodeEntityUuid, endpoint, fromEntityUuid, toCmd, toEntityUuid, toTask, withKeyEncoder, withParamsEncoder, withValueEncoder, withoutDecoder)
import Task
import Time.Date


updateIndexedDb : NominalDate -> Maybe NurseId -> Maybe HealthCenterId -> MsgIndexedDb -> ModelIndexedDb -> ( ModelIndexedDb, Cmd MsgIndexedDb, List App.Model.Msg )
updateIndexedDb currentDate nurseId healthCenterId msg model =
    let
        sw =
            applyBackendUrl "/sw"
    in
    case msg of
        FetchChildMeasurements childId ->
            ( { model | childMeasurements = EveryDict.insert childId Loading model.childMeasurements }
            , sw.get childMeasurementListEndpoint childId
                |> toCmd (RemoteData.fromResult >> HandleFetchedChildMeasurements childId)
            , []
            )

        HandleFetchedChildMeasurements childId data ->
            ( { model | childMeasurements = EveryDict.insert childId data model.childMeasurements }
            , Cmd.none
            , []
            )

        FetchClinics ->
            ( { model | clinics = Loading }
            , sw.select clinicEndpoint ()
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList >> EveryDictList.sortBy .name) >> HandleFetchedClinics)
            , []
            )

        HandleFetchedClinics clinics ->
            ( { model | clinics = clinics }
            , Cmd.none
            , []
            )

        FetchEditableSession id ->
            -- This one is a bit special. What we're asking for is not a fetch
            -- from IndexedDB as such, but a certain kind of organization of
            -- the data.
            ( { model | editableSessions = EveryDict.insert id (makeEditableSession id model) model.editableSessions }
            , Cmd.none
            , []
            )

        FetchEveryCounselingSchedule ->
            let
                topicTask =
                    sw.select counselingTopicEndpoint ()
                        |> toTask
                        |> Task.map (.items >> EveryDict.fromList)
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
                    EveryDict.update (accessor value) (merge value) accum

                processIndex byId =
                    { byId = byId
                    , byChildId = EveryDict.foldl (indexBy .child) EveryDict.empty byId
                    , byMotherId = EveryDict.foldl (indexBy .adult) EveryDict.empty byId
                    }
            in
            ( { model | expectedParticipants = EveryDict.insert sessionId Loading model.expectedParticipants }
            , sw.select pmtctParticipantEndpoint (ParticipantsForSession sessionId)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDict.fromList >> processIndex) >> HandleFetchedExpectedParticipants sessionId)
            , []
            )

        HandleFetchedExpectedParticipants sessionId data ->
            ( { model | expectedParticipants = EveryDict.insert sessionId data model.expectedParticipants }
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
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedPeopleByName trimmed)
            , []
            )

        HandleFetchedPeopleByName name data ->
            ( { model | personSearches = Dict.insert (String.trim name) data model.personSearches }
            , Cmd.none
            , []
            )

        -- Temporary
        GoToRandomPrenatalEncounter ->
            ( model
            , sw.selectRange prenatalEncounterEndpoint Nothing 0 (Just 1)
                |> toTask
                |> Task.map (.items >> List.head >> Maybe.map Tuple.first)
                |> Task.attempt HandleRandomPrenatalEncounter
            , []
            )

        -- Temporary
        HandleRandomPrenatalEncounter result ->
            let
                appMsgs =
                    case result of
                        Ok (Just id) ->
                            [ App.Model.SetActivePage <| UserPage <| Pages.Page.PrenatalEncounterPage id ]

                        _ ->
                            []
            in
            ( model
            , Cmd.none
            , appMsgs
            )

        FetchPrenatalParticipantsForPerson id ->
            ( { model | prenatalParticipantsByPerson = EveryDict.insert id Loading model.prenatalParticipantsByPerson }
            , sw.select prenatalParticipantEndpoint (Just id)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedPrenatalParticipantsForPerson id)
            , []
            )

        HandleFetchedPrenatalParticipantsForPerson id data ->
            ( { model | prenatalParticipantsByPerson = EveryDict.insert id data model.prenatalParticipantsByPerson }
            , Cmd.none
            , []
            )

        FetchPrenatalEncountersForParticipant id ->
            ( { model | prenatalEncountersByParticipant = EveryDict.insert id Loading model.prenatalEncountersByParticipant }
            , sw.select prenatalEncounterEndpoint (Just id)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedPrenatalEncountersForParticipant id)
            , []
            )

        HandleFetchedPrenatalEncountersForParticipant id data ->
            ( { model | prenatalEncountersByParticipant = EveryDict.insert id data model.prenatalEncountersByParticipant }
            , Cmd.none
            , []
            )

        FetchPrenatalMeasurements id ->
            ( { model | prenatalMeasurements = EveryDict.insert id Loading model.prenatalMeasurements }
            , sw.get prenatalMeasurementsEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedPrenatalMeasurements id)
            , []
            )

        HandleFetchedPrenatalMeasurements id data ->
            ( { model | prenatalMeasurements = EveryDict.insert id data model.prenatalMeasurements }
            , Cmd.none
            , []
            )

        FetchParticipantsForPerson personId ->
            let
                query1 =
                    sw.select pmtctParticipantEndpoint (ParticipantsForChild personId)
                        |> toTask
                        |> Task.map (.items >> EveryDict.fromList)
                        |> RemoteData.fromTask

                query2 =
                    sw.select pmtctParticipantEndpoint (ParticipantsForAdult personId)
                        |> toTask
                        |> Task.map (.items >> EveryDict.fromList)
                        |> RemoteData.fromTask
            in
            ( { model | participantsByPerson = EveryDict.insert personId Loading model.participantsByPerson }
            , Task.map2 (RemoteData.map2 EveryDict.union) query1 query2
                |> Task.perform (HandleFetchedParticipantsForPerson personId)
            , []
            )

        HandleFetchedParticipantsForPerson personId data ->
            ( { model | participantsByPerson = EveryDict.insert personId data model.participantsByPerson }
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
                        |> Task.map (.items >> EveryDictList.fromList >> EveryDictList.filterMap (always (toMyRelationship personId)))
                        |> RemoteData.fromTask

                query2 =
                    sw.select relationshipEndpoint { person = Nothing, relatedTo = Just personId }
                        |> toTask
                        |> Task.map (.items >> EveryDictList.fromList >> EveryDictList.filterMap (always (toMyRelationship personId)))
                        |> RemoteData.fromTask
            in
            ( { model | relationshipsByPerson = EveryDict.insert personId Loading model.relationshipsByPerson }
            , Task.map2 (RemoteData.map2 EveryDictList.union) query1 query2
                |> Task.perform (HandleFetchedRelationshipsForPerson personId)
            , []
            )

        HandleFetchedRelationshipsForPerson personId data ->
            ( { model | relationshipsByPerson = EveryDict.insert personId data model.relationshipsByPerson }
            , Cmd.none
            , []
            )

        FetchExpectedSessions childId ->
            ( { model | expectedSessions = EveryDict.insert childId Loading model.expectedSessions }
            , sw.select sessionEndpoint (ForChild childId)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedExpectedSessions childId)
            , []
            )

        HandleFetchedExpectedSessions childId data ->
            ( { model | expectedSessions = EveryDict.insert childId data model.expectedSessions }
            , Cmd.none
            , []
            )

        FetchHealthCenters ->
            ( { model | healthCenters = Loading }
            , sw.select healthCenterEndpoint ()
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedHealthCenters)
            , []
            )

        HandleFetchedHealthCenters data ->
            ( { model | healthCenters = data }
            , Cmd.none
            , []
            )

        FetchMotherMeasurements motherId ->
            ( { model | motherMeasurements = EveryDict.insert motherId Loading model.motherMeasurements }
            , sw.get motherMeasurementListEndpoint motherId
                |> toCmd (RemoteData.fromResult >> HandleFetchedMotherMeasurements motherId)
            , []
            )

        HandleFetchedMotherMeasurements motherId data ->
            ( { model | motherMeasurements = EveryDict.insert motherId data model.motherMeasurements }
            , Cmd.none
            , []
            )

        FetchParticipantForms ->
            ( { model | participantForms = Loading }
            , sw.select participantFormEndpoint ()
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedParticipantForms)
            , []
            )

        HandleFetchedParticipantForms data ->
            ( { model | participantForms = data }
            , Cmd.none
            , []
            )

        FetchPerson id ->
            ( { model | people = EveryDict.insert id Loading model.people }
            , sw.get personEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedPerson id)
            , []
            )

        HandleFetchedPerson id data ->
            ( { model | people = EveryDict.insert id data model.people }
            , Cmd.none
            , []
            )

        FetchPrenatalEncounter id ->
            ( { model | prenatalEncounters = EveryDict.insert id Loading model.prenatalEncounters }
            , sw.get prenatalEncounterEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedPrenatalEncounter id)
            , []
            )

        HandleFetchedPrenatalEncounter id data ->
            ( { model | prenatalEncounters = EveryDict.insert id data model.prenatalEncounters }
            , Cmd.none
            , []
            )

        FetchPrenatalParticipant id ->
            ( { model | prenatalParticipants = EveryDict.insert id Loading model.prenatalParticipants }
            , sw.get prenatalParticipantEndpoint id
                |> toCmd (RemoteData.fromResult >> HandleFetchedPrenatalParticipant id)
            , []
            )

        HandleFetchedPrenatalParticipant id data ->
            ( { model | prenatalParticipants = EveryDict.insert id data model.prenatalParticipants }
            , Cmd.none
            , []
            )

        FetchSession sessionId ->
            ( { model | sessions = EveryDict.insert sessionId Loading model.sessions }
            , sw.get sessionEndpoint sessionId
                |> toCmd (RemoteData.fromResult >> HandleFetchedSession sessionId)
            , []
            )

        HandleFetchedSession sessionId data ->
            ( { model | sessions = EveryDict.insert sessionId data model.sessions }
            , Cmd.none
            , []
            )

        FetchSessionsByClinic clinicId ->
            ( { model | sessionsByClinic = EveryDict.insert clinicId Loading model.sessionsByClinic }
            , sw.select sessionEndpoint (ForClinic clinicId)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedSessionsByClinic clinicId)
            , []
            )

        HandleFetchedSessionsByClinic clinicId data ->
            ( { model | sessionsByClinic = EveryDict.insert clinicId data model.sessionsByClinic }
            , Cmd.none
            , []
            )

        FetchSyncData ->
            ( { model | syncData = Loading }
            , sw.select syncDataEndpoint ()
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedSyncData)
            , []
            )

        HandleFetchedSyncData data ->
            ( { model | syncData = data }
            , Cmd.none
            , []
            )

        HandleRevisions revisions ->
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
                                EveryDict.map
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
            ( { model | saveSyncDataRequests = EveryDict.insert uuid Loading model.saveSyncDataRequests }
            , sw.put syncDataEndpoint uuid data
                |> withoutDecoder
                |> toCmd (RemoteData.fromResult >> HandleSavedSyncData uuid)
            , []
            )

        HandleSavedSyncData uuid data ->
            ( { model | saveSyncDataRequests = EveryDict.insert uuid data model.saveSyncDataRequests }
            , Cmd.none
            , []
            )

        DeleteSyncData uuid ->
            ( { model | deleteSyncDataRequests = EveryDict.insert uuid Loading model.deleteSyncDataRequests }
            , sw.delete syncDataEndpoint uuid
                |> toCmd (RemoteData.fromResult >> HandleDeletedSyncData uuid)
            , []
            )

        HandleDeletedSyncData uuid data ->
            ( { model | deleteSyncDataRequests = EveryDict.insert uuid data model.deleteSyncDataRequests }
            , Cmd.none
            , []
            )

        MsgPrenatalEncounter encounterId subMsg ->
            let
                encounter =
                    EveryDict.get encounterId model.prenatalEncounters
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe

                requests =
                    EveryDict.get encounterId model.prenatalEncounterRequests
                        |> Maybe.withDefault Backend.PrenatalEncounter.Model.emptyModel

                ( subModel, subCmd ) =
                    Backend.PrenatalEncounter.Update.update nurseId healthCenterId encounterId encounter currentDate subMsg requests
            in
            ( { model | prenatalEncounterRequests = EveryDict.insert encounterId subModel model.prenatalEncounterRequests }
            , Cmd.map (MsgPrenatalEncounter encounterId) subCmd
            , []
            )

        MsgPrenatalSession participantId subMsg ->
            let
                participant =
                    EveryDict.get participantId model.prenatalParticipants
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe

                requests =
                    EveryDict.get participantId model.prenatalSessionRequests
                        |> Maybe.withDefault Backend.PrenatalParticipant.Model.emptyModel

                ( subModel, subCmd ) =
                    Backend.PrenatalParticipant.Update.update participantId participant currentDate subMsg requests
            in
            ( { model | prenatalSessionRequests = EveryDict.insert participantId subModel model.prenatalSessionRequests }
            , Cmd.map (MsgPrenatalSession participantId) subCmd
            , []
            )

        MsgSession sessionId subMsg ->
            let
                session =
                    EveryDict.get sessionId model.editableSessions
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.map (.offlineSession >> .session)
                        |> RemoteData.toMaybe

                requests =
                    EveryDict.get sessionId model.sessionRequests
                        |> Maybe.withDefault Backend.Session.Model.emptyModel

                ( subModel, subCmd ) =
                    Backend.Session.Update.update nurseId sessionId session currentDate subMsg requests
            in
            ( { model | sessionRequests = EveryDict.insert sessionId subModel model.sessionRequests }
            , Cmd.map (MsgSession sessionId) subCmd
            , []
            )

        PostPmtctParticipant data ->
            ( { model | postPmtctParticipant = EveryDict.insert data.child Loading model.postPmtctParticipant }
            , sw.post pmtctParticipantEndpoint data
                |> toCmd (RemoteData.fromResult >> HandlePostedPmtctParticipant data.child)
            , []
            )

        HandlePostedPmtctParticipant id data ->
            ( { model | postPmtctParticipant = EveryDict.insert id data model.postPmtctParticipant }
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
                    EveryDict.get normalized.relatedTo model.people
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.toMaybe
                        |> Maybe.andThen .birthDate
                        |> Maybe.withDefault (Time.Date.addDays -28 currentDate)

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
                        |> Task.map (.items >> EveryDictList.fromList)

                query2 =
                    sw.select relationshipEndpoint
                        { person = Just normalized.relatedTo
                        , relatedTo = Just normalized.person
                        }
                        |> toTask
                        |> Task.map (.items >> EveryDictList.fromList)

                existingRelationship =
                    Task.map2 EveryDictList.union query1 query2
                        |> Task.map EveryDictList.head

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
            ( { model | postRelationship = EveryDict.insert personId Loading model.postRelationship }
            , relationshipCmd
            , []
            )
                |> sequenceExtra (updateIndexedDb currentDate nurseId healthCenterId) extraMsgs

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
            ( { model | postRelationship = EveryDict.insert personId data model.postRelationship }
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

                                            IndividualEncounterOrigin ->
                                                EncounterTypesPage personId
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

        PostSession session ->
            ( { model | postSession = Loading }
            , sw.post sessionEndpoint session
                |> toCmd (RemoteData.fromResult >> RemoteData.map Tuple.first >> HandlePostedSession)
            , []
            )

        HandlePostedSession data ->
            ( { model | postSession = data }
            , Cmd.none
            , []
            )

        PostPrenatalSession prenatalSession ->
            ( { model | postPrenatalSession = EveryDict.insert prenatalSession.person Loading model.postPrenatalSession }
            , sw.post prenatalParticipantEndpoint prenatalSession
                |> toCmd (RemoteData.fromResult >> HandlePostedPrenatalSession prenatalSession.person)
            , []
            )

        HandlePostedPrenatalSession personId data ->
            let
                -- We automatically create new encounter for newly created prenatal session.
                -- We'll probably need to change this once we allow to end prenatal session,
                -- but for now, this is sufficient.
                appMsgs =
                    RemoteData.map
                        (\( sessionId, _ ) ->
                            [ Backend.PrenatalEncounter.Model.PrenatalEncounter sessionId currentDate Nothing
                                |> Backend.Model.PostPrenatalEncounter
                                |> App.Model.MsgIndexedDb
                            ]
                        )
                        data
                        |> RemoteData.withDefault []
            in
            ( { model | postPrenatalSession = EveryDict.insert personId data model.postPrenatalSession }
            , Cmd.none
            , appMsgs
            )

        PostPrenatalEncounter prenatalEncounter ->
            ( { model | postPrenatalEncounter = EveryDict.insert prenatalEncounter.participant Loading model.postPrenatalEncounter }
            , sw.post prenatalEncounterEndpoint prenatalEncounter
                |> toCmd (RemoteData.fromResult >> HandlePostedPrenatalEncounter prenatalEncounter.participant)
            , []
            )

        HandlePostedPrenatalEncounter participantId data ->
            ( { model | postPrenatalEncounter = EveryDict.insert participantId data model.postPrenatalEncounter }
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
                (\measurements -> { measurements | attendances = EveryDictList.insert uuid data measurements.attendances })
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

        ChildNutritionRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | nutritions = EveryDictList.insert uuid data measurements.nutritions })
                model
            , True
            )

        ClinicRevision uuid data ->
            let
                clinics =
                    RemoteData.map (EveryDictList.insert uuid data) model.clinics
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
                (\measurements -> { measurements | counselingSessions = EveryDictList.insert uuid data measurements.counselingSessions })
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
                (\measurements -> { measurements | familyPlannings = EveryDictList.insert uuid data measurements.familyPlannings })
                model
            , True
            )

        HealthCenterRevision uuid data ->
            let
                healthCenters =
                    RemoteData.map (EveryDictList.insert uuid data) model.healthCenters
            in
            ( { model | healthCenters = healthCenters }
            , recalc
            )

        HeightRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | heights = EveryDictList.insert uuid data measurements.heights })
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

        MuacRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | muacs = EveryDictList.insert uuid data measurements.muacs })
                model
            , True
            )

        NurseRevision uuid data ->
            -- Nothing to do in ModelIndexedDb yet. App.Update does do something with this one.
            noChange

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
                (\measurements -> { measurements | consents = EveryDictList.insert uuid data measurements.consents })
                model
            , True
            )

        ParticipantFormRevision uuid data ->
            ( { model | participantForms = RemoteData.map (EveryDictList.insert uuid data) model.participantForms }
            , True
            )

        PersonRevision uuid data ->
            let
                people =
                    EveryDict.update uuid (Maybe.map (always (Success data))) model.people
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
                (\measurements -> { measurements | photos = EveryDictList.insert uuid data measurements.photos })
                model
            , True
            )

        PmtctParticipantRevision uuid data ->
            ( { model
                | expectedSessions =
                    model.expectedSessions
                        |> EveryDict.remove data.child
                        |> EveryDict.remove data.adult
                , expectedParticipants =
                    EveryDict.empty
                , participantsByPerson =
                    model.participantsByPerson
                        |> EveryDict.remove data.child
                        |> EveryDict.remove data.adult
              }
            , True
            )

        PrenatalParticipantRevision uuid data ->
            let
                prenatalParticipants =
                    EveryDict.update uuid (Maybe.map (always (Success data))) model.prenatalParticipants

                prenatalParticipantsByPerson =
                    EveryDict.remove data.person model.prenatalParticipantsByPerson
            in
            ( { model
                | prenatalParticipants = prenatalParticipants
                , prenatalParticipantsByPerson = prenatalParticipantsByPerson
              }
            , recalc
            )

        PrenatalEncounterRevision uuid data ->
            let
                prenatalEncounters =
                    EveryDict.update uuid (Maybe.map (always (Success data))) model.prenatalEncounters

                prenatalEncountersByParticipant =
                    EveryDict.remove data.participant model.prenatalEncountersByParticipant
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
            ( { model | relationshipsByPerson = EveryDict.empty }
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
                        |> EveryDict.map (always (RemoteData.map (EveryDictList.remove uuid)))
                        |> EveryDict.update data.clinicId (Maybe.map (RemoteData.map (EveryDictList.insert uuid data)))
            in
            ( { model
                | sessionsByClinic = sessionsByClinic
                , expectedParticipants = EveryDict.remove uuid model.expectedParticipants
                , expectedSessions = EveryDict.empty
                , sessions = EveryDict.insert uuid (Success data) model.sessions
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
                (\measurements -> { measurements | weights = EveryDictList.insert uuid data measurements.weights })
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
        sessionData =
            EveryDict.get sessionId db.sessions
                |> Maybe.withDefault NotAsked

        allParticipantFormsData =
            db.participantForms

        everyCounselingScheduleData =
            db.everyCounselingSchedule

        participantsData =
            EveryDict.get sessionId db.expectedParticipants
                |> Maybe.withDefault NotAsked

        mothersData =
            RemoteData.andThen
                (\participants ->
                    EveryDict.keys participants.byMotherId
                        |> List.map
                            (\id ->
                                EveryDict.get id db.people
                                    |> Maybe.withDefault NotAsked
                                    |> RemoteData.map (\data -> ( id, data ))
                            )
                        |> RemoteData.fromList
                        |> RemoteData.map (EveryDictList.fromList >> EveryDictList.sortBy .name)
                )
                participantsData

        childrenData =
            RemoteData.andThen
                (\participants ->
                    EveryDict.keys participants.byChildId
                        |> List.map
                            (\id ->
                                EveryDict.get id db.people
                                    |> Maybe.withDefault NotAsked
                                    |> RemoteData.map (\data -> ( id, data ))
                            )
                        |> RemoteData.fromList
                        |> RemoteData.map EveryDictList.fromList
                )
                participantsData

        childMeasurementListData =
            RemoteData.andThen
                (\children ->
                    EveryDictList.keys children
                        |> List.map
                            (\childId ->
                                EveryDict.get childId db.childMeasurements
                                    |> Maybe.withDefault NotAsked
                                    |> RemoteData.map (\data -> ( childId, data ))
                            )
                        |> RemoteData.fromList
                        |> RemoteData.map EveryDict.fromList
                )
                childrenData

        adultMeasurementListData =
            RemoteData.andThen
                (\mothers ->
                    EveryDictList.keys mothers
                        |> List.map
                            (\motherId ->
                                EveryDict.get motherId db.motherMeasurements
                                    |> Maybe.withDefault NotAsked
                                    |> RemoteData.map (\data -> ( motherId, data ))
                            )
                        |> RemoteData.fromList
                        |> RemoteData.map EveryDict.fromList
                )
                mothersData

        childMeasurementsSplitData =
            RemoteData.map (\list -> lazy <| \_ -> splitChildMeasurements sessionId list) childMeasurementListData

        adultMeasurementsSplitData =
            RemoteData.map (\list -> lazy <| \_ -> splitMotherMeasurements sessionId list) adultMeasurementListData

        historicalMeasurementData =
            RemoteData.map2 HistoricalMeasurements adultMeasurementListData childMeasurementListData

        currentAndPrevious =
            RemoteData.map2
                (Lazy.map2
                    (\childData motherData ->
                        { current =
                            { mothers = EveryDict.map (always .current) motherData
                            , children = EveryDict.map (always .current) childData
                            }
                        , previous =
                            { mothers = EveryDict.map (always .previous) motherData
                            , children = EveryDict.map (always .previous) childData
                            }
                        }
                    )
                )
                childMeasurementsSplitData
                adultMeasurementsSplitData

        currentMeasurementData =
            RemoteData.map (Lazy.map .current) currentAndPrevious

        previousMeasurementData =
            RemoteData.map (Lazy.map .previous) currentAndPrevious

        measurementData =
            RemoteData.map3
                (\historical ->
                    Lazy.map2
                        (\current previous ->
                            { historical = historical
                            , current = current
                            , previous = previous
                            }
                        )
                )
                historicalMeasurementData
                currentMeasurementData
                previousMeasurementData

        offlineSession =
            RemoteData.map OfflineSession sessionData
                |> RemoteData.andMap allParticipantFormsData
                |> RemoteData.andMap everyCounselingScheduleData
                |> RemoteData.andMap participantsData
                |> RemoteData.andMap mothersData
                |> RemoteData.andMap childrenData
                |> RemoteData.andMap measurementData
    in
    RemoteData.map
        (\offline ->
            let
                checkedIn =
                    lazy <|
                        \_ -> cacheCheckedIn offline

                summaryByParticipant =
                    Lazy.map (summarizeByParticipant offline) checkedIn

                summaryByActivity =
                    Lazy.map (summarizeByActivity offline) checkedIn
            in
            { offlineSession = offline
            , update = NotAsked
            , checkedIn = checkedIn
            , summaryByParticipant = summaryByParticipant
            , summaryByActivity = summaryByActivity
            }
        )
        offlineSession


{-| Summarize our data for the editable session in a way that is useful
for our UI, when we're focused on participants. This only considers children &
mothers who are checked in to the session.
-}
summarizeByParticipant : OfflineSession -> CheckedIn -> SummaryByParticipant
summarizeByParticipant session checkedIn =
    let
        children =
            EveryDictList.map
                (\childId _ -> summarizeChildParticipant childId session)
                checkedIn.children

        mothers =
            EveryDictList.map
                (\motherId _ -> summarizeMotherParticipant motherId session)
                checkedIn.mothers
    in
    { children = children
    , mothers = mothers
    }


{-| Summarize our data for the editable session in a way that is useful
for our UI, when we're focused on activities. This only considers children &
mothers who are checked in to the session.
-}
summarizeByActivity : OfflineSession -> CheckedIn -> SummaryByActivity
summarizeByActivity session checkedIn =
    let
        children =
            getAllChildActivities
                |> List.map
                    (\activity ->
                        ( activity
                        , summarizeChildActivity activity session checkedIn
                        )
                    )
                |> EveryDict.fromList

        mothers =
            getAllMotherActivities
                |> List.map
                    (\activity ->
                        ( activity
                        , summarizeMotherActivity activity session checkedIn
                        )
                    )
                |> EveryDict.fromList
    in
    { children = children
    , mothers = mothers
    }


{-| Who is checked in, considering both explicit check in and anyone who has
any completed activity?

Don't call this directly ... we store it lazily on EditableSession.checkedIn.
Ideally, we'd move it there and not expose it, but we'd have to rearrange a
bunch of stuff to avoid circular imports.

-}
cacheCheckedIn : OfflineSession -> CheckedIn
cacheCheckedIn session =
    let
        -- A mother is checked in if explicitly checked in or has any completed
        -- activites.
        mothers =
            EveryDictList.filter
                (\motherId _ -> motherIsCheckedIn motherId session)
                session.mothers

        -- A child is checked in if the mother is checked in.
        children =
            EveryDictList.filter
                (\childId _ ->
                    getMyMother childId session
                        |> Maybe.map (\( motherId, _ ) -> EveryDictList.member motherId mothers)
                        |> Maybe.withDefault False
                )
                session.children
    in
    { mothers = mothers
    , children = children
    }
