module Backend.Update exposing (updateIndexedDb)

import Activity.Model exposing (SummaryByActivity, SummaryByParticipant)
import Activity.Utils exposing (getAllChildActivities, getAllMotherActivities, motherIsCheckedIn, summarizeChildActivity, summarizeChildParticipant, summarizeMotherActivity, summarizeMotherParticipant)
import App.Model
import AssocList as Dict exposing (Dict)
import Backend.Counseling.Decoder exposing (combineCounselingSchedules)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (HistoricalMeasurements)
import Backend.Measurement.Utils exposing (splitChildMeasurements, splitMotherMeasurements)
import Backend.Model exposing (..)
import Backend.PmtctParticipant.Model exposing (AdultActivities(..))
import Backend.Relationship.Encoder exposing (encodeRelationshipChanges)
import Backend.Relationship.Model exposing (RelatedBy(..))
import Backend.Relationship.Utils exposing (toMyRelationship, toRelationship)
import Backend.Session.Model exposing (CheckedIn, EditableSession, OfflineSession, Session)
import Backend.Session.Update
import Backend.Session.Utils exposing (getMyMother)
import Backend.Utils exposing (mapChildMeasurements, mapMotherMeasurements)
import Gizra.NominalDate exposing (NominalDate, emptyNominalDate)
import Gizra.Update exposing (sequenceExtra)
import Json.Encode exposing (object)
import Maybe.Extra exposing (isJust)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Person.Model
import Pages.Relationship.Model
import RemoteData exposing (RemoteData(..), WebData)
import Restful.Endpoint exposing (EntityUuid, ReadOnlyEndPoint, ReadWriteEndPoint, applyBackendUrl, toCmd, toTask, withoutDecoder)
import Task


updateIndexedDb : NominalDate -> Maybe NurseId -> MsgIndexedDb -> ModelIndexedDb -> ( ModelIndexedDb, Cmd MsgIndexedDb, List App.Model.Msg )
updateIndexedDb currentDate nurseId msg model =
    let
        sw =
            applyBackendUrl "/sw"
    in
    case msg of
        FetchChildMeasurements childId ->
            ( { model | childMeasurements = Dict.insert childId Loading model.childMeasurements }
            , sw.get childMeasurementListEndpoint childId
                |> toCmd (RemoteData.fromResult >> HandleFetchedChildMeasurements childId)
            , []
            )

        HandleFetchedChildMeasurements childId data ->
            ( { model | childMeasurements = Dict.insert childId data model.childMeasurements }
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

        FetchEditableSession id ->
            -- This one is a bit special. What we're asking for is not a fetch
            -- from IndexedDB as such, but a certain kind of organization of
            -- the data.
            ( { model | editableSessions = Dict.insert id (makeEditableSession id model) model.editableSessions }
            , Cmd.none
            , []
            )

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
            ( { model | expectedParticipants = Dict.insert sessionId data model.expectedParticipants }
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
                        -- @todo: List.filterMap didn't work?
                        -- |> Task.map (.items >> Dict.fromList >> Dict.filterMap (always (toMyRelationship personId)))
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
                        -- @todo: List.filterMap didn't work?
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

        HandleFetchedMotherMeasurements motherId data ->
            ( { model | motherMeasurements = Dict.insert motherId data model.motherMeasurements }
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

        MsgSession sessionId subMsg ->
            let
                requests =
                    Dict.get sessionId model.sessionRequests
                        |> Maybe.withDefault Backend.Session.Model.emptyModel

                ( subModel, subCmd ) =
                    Backend.Session.Update.update nurseId sessionId currentDate subMsg requests
            in
            ( { model | sessionRequests = Dict.insert sessionId subModel model.sessionRequests }
            , Cmd.map (MsgSession sessionId) subCmd
            , []
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
                        -- @todo
                        -- |> Maybe.withDefault (Time.Date.addDays -28 currentDate)
                        |> Maybe.withDefault emptyNominalDate

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
                |> sequenceExtra (updateIndexedDb currentDate nurseId) extraMsgs

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

        PostPerson relation person ->
            ( { model | postPerson = Loading }
            , sw.post personEndpoint person
                |> toCmd (RemoteData.fromResult >> RemoteData.map Tuple.first >> HandlePostedPerson relation)
            , []
            )

        HandlePostedPerson relation data ->
            let
                appMsgs =
                    -- If we succeed, we reset the form, and go to the page
                    -- showing the new person.
                    data
                        |> RemoteData.map
                            (\personId ->
                                let
                                    nextPage =
                                        case relation of
                                            Just id ->
                                                RelationshipPage id personId

                                            Nothing ->
                                                PersonPage personId
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


{-| The extra return value indicates whether we need to recalculate our
successful EditableSessions. Ideally, we would handle this in a more
nuanced way.
-}
handleRevision : Revision -> ( ModelIndexedDb, Bool ) -> ( ModelIndexedDb, Bool )
handleRevision revision ( model, recalc ) =
    case revision of
        AttendanceRevision uuid data ->
            ( mapMotherMeasurements
                data.participantId
                (\measurements -> { measurements | attendances = Dict.insert uuid data measurements.attendances })
                model
            , True
            )

        CatchmentAreaRevision uuid data ->
            ( model
            , recalc
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

        FamilyPlanningRevision uuid data ->
            ( mapMotherMeasurements
                data.participantId
                (\measurements -> { measurements | familyPlannings = Dict.insert uuid data measurements.familyPlannings })
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

        MuacRevision uuid data ->
            ( mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | muacs = Dict.insert uuid data measurements.muacs })
                model
            , True
            )

        NurseRevision uuid data ->
            -- Nothing to do in ModelIndexedDb yet. App.Update does do something with this one.
            ( model
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

        RelationshipRevision uuid data ->
            ( { model | relationshipsByPerson = Dict.empty }
            , True
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
                        |> List.map
                            (\id ->
                                Dict.get id db.people
                                    |> Maybe.withDefault NotAsked
                                    |> RemoteData.map (\data -> ( id, data ))
                            )
                        |> RemoteData.fromList
                        |> RemoteData.map (List.sortBy (Tuple.second >> .name) >> Dict.fromList)
                )
                participantsData

        childrenData =
            RemoteData.andThen
                (\participants ->
                    Dict.keys participants.byChildId
                        |> List.map
                            (\id ->
                                Dict.get id db.people
                                    |> Maybe.withDefault NotAsked
                                    |> RemoteData.map (\data -> ( id, data ))
                            )
                        |> RemoteData.fromList
                        |> RemoteData.map Dict.fromList
                )
                participantsData

        childMeasurementListData =
            RemoteData.andThen
                (\children ->
                    Dict.keys children
                        |> List.map
                            (\childId ->
                                Dict.get childId db.childMeasurements
                                    |> Maybe.withDefault NotAsked
                                    |> RemoteData.map (\data -> ( childId, data ))
                            )
                        |> RemoteData.fromList
                        |> RemoteData.map Dict.fromList
                )
                childrenData

        adultMeasurementListData =
            RemoteData.andThen
                (\mothers ->
                    Dict.keys mothers
                        |> List.map
                            (\motherId ->
                                Dict.get motherId db.motherMeasurements
                                    |> Maybe.withDefault NotAsked
                                    |> RemoteData.map (\data -> ( motherId, data ))
                            )
                        |> RemoteData.fromList
                        |> RemoteData.map Dict.fromList
                )
                mothersData

        childMeasurementsSplitData =
            -- @todo: Keep lazy?
            -- RemoteData.map (\list -> lazy <| \_ -> splitChildMeasurements sessionId list) childMeasurementListData
            RemoteData.map (\list -> splitChildMeasurements sessionId list) childMeasurementListData

        adultMeasurementsSplitData =
            -- @todo: Keep lazy?
            -- RemoteData.map (\list -> lazy <| \_ -> splitMotherMeasurements sessionId list) adultMeasurementListData
            RemoteData.map (\list -> splitMotherMeasurements sessionId list) adultMeasurementListData

        historicalMeasurementData =
            RemoteData.map2 HistoricalMeasurements adultMeasurementListData childMeasurementListData

        currentAndPrevious =
            RemoteData.map2
                -- @todo: Keep lazy?
                -- Lazy.map2
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
            -- @todo: Keep lazy?
            -- RemoteData.map (Lazy.map .current) currentAndPrevious
            RemoteData.map .current currentAndPrevious

        previousMeasurementData =
            -- @todo: Keep lazy?
            -- RemoteData.map (Lazy.map .previous) currentAndPrevious
            RemoteData.map .previous currentAndPrevious

        measurementData =
            RemoteData.map3
                (\historical ->
                    -- @todo: Keep lazy?
                    -- Lazy.map2
                    \current previous ->
                        { historical = historical
                        , current = current
                        , previous = previous
                        }
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
                    -- @todo
                    -- lazy <|
                    -- \_ ->
                    cacheCheckedIn offline

                summaryByParticipant =
                    -- @todo: Keep lazy?
                    -- Lazy.map (summarizeByParticipant offline) checkedIn
                    summarizeByParticipant offline checkedIn

                summaryByActivity =
                    -- @todo: Keep lazy?
                    -- Lazy.map (summarizeByActivity offline) checkedIn
                    summarizeByActivity offline checkedIn
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
            Dict.map
                (\childId _ -> summarizeChildParticipant childId session)
                checkedIn.children

        mothers =
            Dict.map
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
                |> Dict.fromList

        mothers =
            getAllMotherActivities
                |> List.map
                    (\activity ->
                        ( activity
                        , summarizeMotherActivity activity session checkedIn
                        )
                    )
                |> Dict.fromList
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
