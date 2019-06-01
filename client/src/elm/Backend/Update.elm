module Backend.Update exposing (updateIndexedDb)

import App.Model
import Backend.Counseling.Decoder exposing (combineCounselingSchedules)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Model exposing (..)
import Backend.Relationship.Encoder exposing (encodeRelationshipChanges)
import Backend.Relationship.Utils exposing (toMyRelationship, toRelationship)
import Backend.Session.Model exposing (EditableSession, OfflineSession, Session)
import Backend.Session.Update
import Backend.Utils exposing (mapChildMeasurements, mapMotherMeasurements)
import Dict
import EveryDict
import EveryDictList
import Gizra.NominalDate exposing (NominalDate)
import Json.Encode exposing (object)
import Pages.Page exposing (Page(..), UserPage(..))
import Pages.Person.Model
import Pages.Relationship.Model
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (EntityUuid, ReadOnlyEndPoint, ReadWriteEndPoint, applyAccessToken, applyBackendUrl, decodeEntityUuid, decodeSingleDrupalEntity, drupalBackend, drupalEndpoint, encodeEntityUuid, endpoint, fromEntityUuid, toCmd, toEntityUuid, toTask, withKeyEncoder, withParamsEncoder, withValueEncoder, withoutDecoder)
import Task


updateIndexedDb : NominalDate -> Maybe NurseId -> MsgIndexedDb -> ModelIndexedDb -> ( ModelIndexedDb, Cmd MsgIndexedDb, List App.Model.Msg )
updateIndexedDb currentDate nurseId msg model =
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
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedClinics)
            , []
            )

        HandleFetchedClinics clinics ->
            ( { model | clinics = clinics }
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
            ( { model | expectedParticipants = EveryDict.insert sessionId Loading model.expectedParticipants }
            , sw.select pmtctParticipantEndpoint (ParticipantsForSession sessionId)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedExpectedParticipants sessionId)
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
            ( List.foldl handleRevision model revisions
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

        MsgSession sessionId subMsg ->
            let
                requests =
                    EveryDict.get sessionId model.sessionRequests
                        |> Maybe.withDefault Backend.Session.Model.emptyModel

                ( subModel, subCmd ) =
                    Backend.Session.Update.update nurseId sessionId currentDate subMsg requests
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

        PostRelationship personId myRelationship ->
            let
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

                cmd =
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
            , cmd
            , []
            )

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


handleRevision : Revision -> ModelIndexedDb -> ModelIndexedDb
handleRevision revision model =
    case revision of
        AttendanceRevision uuid data ->
            mapMotherMeasurements
                data.participantId
                (\measurements -> { measurements | attendances = EveryDictList.insert uuid data measurements.attendances })
                model

        CatchmentAreaRevision uuid data ->
            model

        ChildNutritionRevision uuid data ->
            mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | nutritions = EveryDictList.insert uuid data measurements.nutritions })
                model

        ClinicRevision uuid data ->
            let
                clinics =
                    RemoteData.map (EveryDictList.insert uuid data) model.clinics
            in
            { model | clinics = clinics }

        CounselingScheduleRevision uuid data ->
            -- Just invalidate our value ... if someone wants it, we'll refetch it.
            { model | everyCounselingSchedule = NotAsked }

        CounselingSessionRevision uuid data ->
            mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | counselingSessions = EveryDictList.insert uuid data measurements.counselingSessions })
                model

        CounselingTopicRevision uuid data ->
            { model | everyCounselingSchedule = NotAsked }

        FamilyPlanningRevision uuid data ->
            mapMotherMeasurements
                data.participantId
                (\measurements -> { measurements | familyPlannings = EveryDictList.insert uuid data measurements.familyPlannings })
                model

        HealthCenterRevision uuid data ->
            let
                healthCenters =
                    RemoteData.map (EveryDictList.insert uuid data) model.healthCenters
            in
            { model | healthCenters = healthCenters }

        HeightRevision uuid data ->
            mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | heights = EveryDictList.insert uuid data measurements.heights })
                model

        MuacRevision uuid data ->
            mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | muacs = EveryDictList.insert uuid data measurements.muacs })
                model

        NurseRevision uuid data ->
            -- Nothing to do in ModelIndexedDb yet. App.Update does do something with this one.
            model

        ParticipantConsentRevision uuid data ->
            mapMotherMeasurements
                data.participantId
                (\measurements -> { measurements | consents = EveryDictList.insert uuid data measurements.consents })
                model

        ParticipantFormRevision uuid data ->
            { model | participantForms = RemoteData.map (EveryDictList.insert uuid data) model.participantForms }

        PersonRevision uuid data ->
            let
                people =
                    EveryDict.update uuid (Maybe.map (always (Success data))) model.people
            in
            { model
                | personSearches = Dict.empty
                , people = people
            }

        PhotoRevision uuid data ->
            mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | photos = EveryDictList.insert uuid data measurements.photos })
                model

        PmtctParticipantRevision uuid data ->
            { model
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

        RelationshipRevision uuid data ->
            { model | relationshipsByPerson = EveryDict.empty }

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
            { model
                | sessionsByClinic = sessionsByClinic
                , expectedParticipants = EveryDict.remove uuid model.expectedParticipants
                , expectedSessions = EveryDict.empty
                , sessions = EveryDict.insert uuid (Success data) model.sessions
            }

        WeightRevision uuid data ->
            mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | weights = EveryDictList.insert uuid data measurements.weights })
                model
