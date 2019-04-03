module Backend.Update exposing (updateBackend, updateIndexedDb)

import Backend.Child.Encoder exposing (encodeMotherField)
import Backend.Counseling.Decoder exposing (combineCounselingSchedules)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Model exposing (..)
import Backend.Session.Model exposing (EditableSession, OfflineSession, Session)
import Backend.Session.Update
import Backend.Utils exposing (mapChildMeasurements, mapMotherMeasurements)
import Config.Model exposing (BackendUrl)
import Dict
import EveryDict
import EveryDictList
import Gizra.NominalDate exposing (NominalDate)
import Json.Encode exposing (object)
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (EntityUuid, ReadOnlyEndPoint, ReadWriteEndPoint, applyAccessToken, applyBackendUrl, decodeEntityUuid, decodeSingleDrupalEntity, drupalBackend, drupalEndpoint, encodeEntityUuid, endpoint, fromEntityUuid, toCmd, toEntityUuid, toTask, withKeyEncoder, withParamsEncoder, withValueEncoder, withoutDecoder)
import Task
import Utils.WebData exposing (resetError, resetSuccess)


updateIndexedDb : NominalDate -> Maybe NurseId -> MsgIndexedDb -> ModelIndexedDb -> ( ModelIndexedDb, Cmd MsgIndexedDb )
updateIndexedDb currentDate nurseId msg model =
    let
        sw =
            applyBackendUrl "/sw"
    in
    case msg of
        FetchChild childId ->
            ( { model | children = EveryDict.insert childId Loading model.children }
            , sw.get childEndpoint childId
                |> toCmd (RemoteData.fromResult >> HandleFetchedChild childId)
            )

        HandleFetchedChild childId data ->
            ( { model | children = EveryDict.insert childId data model.children }
            , Cmd.none
            )

        FetchChildMeasurements childId ->
            ( { model | childMeasurements = EveryDict.insert childId Loading model.childMeasurements }
            , sw.get childMeasurementListEndpoint childId
                |> toCmd (RemoteData.fromResult >> HandleFetchedChildMeasurements childId)
            )

        HandleFetchedChildMeasurements childId data ->
            ( { model | childMeasurements = EveryDict.insert childId data model.childMeasurements }
            , Cmd.none
            )

        FetchChildrenOfMother motherId ->
            ( { model | childrenOfMother = EveryDict.insert motherId Loading model.childrenOfMother }
            , sw.select childEndpoint { mother = Just motherId, nameContains = Nothing, session = Nothing }
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDict.fromList) >> HandleFetchedChildrenOfMother motherId)
            )

        HandleFetchedChildrenOfMother motherId data ->
            ( { model | childrenOfMother = EveryDict.insert motherId data model.childrenOfMother }
            , Cmd.none
            )

        FetchClinics ->
            ( { model | clinics = Loading }
            , sw.select clinicEndpoint ()
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedClinics)
            )

        HandleFetchedClinics clinics ->
            ( { model | clinics = clinics }
            , Cmd.none
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
            )

        HandleFetchedEveryCounselingSchedule data ->
            ( { model | everyCounselingSchedule = data }
            , Cmd.none
            )

        FetchExpectedParticipants sessionId ->
            let
                childTask =
                    sw.select childEndpoint { session = Just sessionId, nameContains = Nothing, mother = Nothing }
                        |> toTask
                        |> Task.map (.items >> EveryDictList.fromList)
                        |> RemoteData.fromTask

                motherTask =
                    sw.select motherEndpoint { session = Just sessionId, nameContains = Nothing }
                        |> toTask
                        |> Task.map (.items >> EveryDictList.fromList)
                        |> RemoteData.fromTask
            in
            ( { model | expectedParticipants = EveryDict.insert sessionId Loading model.expectedParticipants }
            , Task.map2 (RemoteData.map2 Participants) childTask motherTask
                |> Task.perform (HandleFetchedExpectedParticipants sessionId)
            )

        HandleFetchedExpectedParticipants sessionId data ->
            ( { model | expectedParticipants = EveryDict.insert sessionId data model.expectedParticipants }
            , Cmd.none
            )

        FetchParticipantsByName name ->
            let
                trimmed =
                    String.trim name

                -- We'll limit the search to 100 each for now ... basically,
                -- just to avoid truly pathological cases.
                childTask =
                    sw.selectRange childEndpoint { nameContains = Just trimmed, session = Nothing, mother = Nothing } 0 (Just 100)
                        |> toTask
                        |> Task.map (.items >> EveryDictList.fromList)
                        |> RemoteData.fromTask

                motherTask =
                    sw.selectRange motherEndpoint { nameContains = Just trimmed, session = Nothing } 0 (Just 100)
                        |> toTask
                        |> Task.map (.items >> EveryDictList.fromList)
                        |> RemoteData.fromTask
            in
            ( { model | nameSearches = Dict.insert trimmed Loading model.nameSearches }
            , Task.map2 (RemoteData.map2 Participants) childTask motherTask
                |> Task.perform (HandleFetchedParticipantsByName trimmed)
            )

        HandleFetchedParticipantsByName name data ->
            ( { model | nameSearches = Dict.insert (String.trim name) data model.nameSearches }
            , Cmd.none
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
            )

        HandleFetchedPeopleByName name data ->
            ( { model | personSearches = Dict.insert (String.trim name) data model.personSearches }
            , Cmd.none
            )

        FetchExpectedSessions childId ->
            ( { model | expectedSessions = EveryDict.insert childId Loading model.expectedSessions }
            , sw.select sessionEndpoint (ForChild childId)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedExpectedSessions childId)
            )

        HandleFetchedExpectedSessions childId data ->
            ( { model | expectedSessions = EveryDict.insert childId data model.expectedSessions }
            , Cmd.none
            )

        FetchHealthCenters ->
            ( { model | healthCenters = Loading }
            , sw.select healthCenterEndpoint ()
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedHealthCenters)
            )

        HandleFetchedHealthCenters data ->
            ( { model | healthCenters = data }
            , Cmd.none
            )

        FetchMother motherId ->
            ( { model | mothers = EveryDict.insert motherId Loading model.mothers }
            , sw.get motherEndpoint motherId
                |> toCmd (RemoteData.fromResult >> HandleFetchedMother motherId)
            )

        HandleFetchedMother motherId data ->
            ( { model | mothers = EveryDict.insert motherId data model.mothers }
            , Cmd.none
            )

        FetchMotherMeasurements motherId ->
            ( { model | motherMeasurements = EveryDict.insert motherId Loading model.motherMeasurements }
            , sw.get motherMeasurementListEndpoint motherId
                |> toCmd (RemoteData.fromResult >> HandleFetchedMotherMeasurements motherId)
            )

        HandleFetchedMotherMeasurements motherId data ->
            ( { model | motherMeasurements = EveryDict.insert motherId data model.motherMeasurements }
            , Cmd.none
            )

        FetchParticipantForms ->
            ( { model | participantForms = Loading }
            , sw.select participantFormEndpoint ()
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedParticipantForms)
            )

        HandleFetchedParticipantForms data ->
            ( { model | participantForms = data }
            , Cmd.none
            )

        FetchSession sessionId ->
            ( { model | sessions = EveryDict.insert sessionId Loading model.sessions }
            , sw.get sessionEndpoint sessionId
                |> toCmd (RemoteData.fromResult >> HandleFetchedSession sessionId)
            )

        HandleFetchedSession sessionId data ->
            ( { model | sessions = EveryDict.insert sessionId data model.sessions }
            , Cmd.none
            )

        FetchSessionsByClinic clinicId ->
            ( { model | sessionsByClinic = EveryDict.insert clinicId Loading model.sessionsByClinic }
            , sw.select sessionEndpoint (ForClinic clinicId)
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedSessionsByClinic clinicId)
            )

        HandleFetchedSessionsByClinic clinicId data ->
            ( { model | sessionsByClinic = EveryDict.insert clinicId data model.sessionsByClinic }
            , Cmd.none
            )

        FetchSyncData ->
            ( { model | syncData = Loading }
            , sw.select syncDataEndpoint ()
                |> toCmd (RemoteData.fromResult >> RemoteData.map (.items >> EveryDictList.fromList) >> HandleFetchedSyncData)
            )

        HandleFetchedSyncData data ->
            ( { model | syncData = data }
            , Cmd.none
            )

        HandleRevisions revisions ->
            ( List.foldl handleRevision model revisions
            , Cmd.none
            )

        SaveSyncData uuid data ->
            ( { model | saveSyncDataRequests = EveryDict.insert uuid Loading model.saveSyncDataRequests }
            , sw.put syncDataEndpoint uuid data
                |> withoutDecoder
                |> toCmd (RemoteData.fromResult >> HandleSavedSyncData uuid)
            )

        HandleSavedSyncData uuid data ->
            ( { model | saveSyncDataRequests = EveryDict.insert uuid data model.saveSyncDataRequests }
            , Cmd.none
            )

        DeleteSyncData uuid ->
            ( { model | deleteSyncDataRequests = EveryDict.insert uuid Loading model.deleteSyncDataRequests }
            , sw.delete syncDataEndpoint uuid
                |> toCmd (RemoteData.fromResult >> HandleDeletedSyncData uuid)
            )

        HandleDeletedSyncData uuid data ->
            ( { model | deleteSyncDataRequests = EveryDict.insert uuid data model.deleteSyncDataRequests }
            , Cmd.none
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
            )

        PostChild child ->
            ( { model | postChild = Loading }
            , sw.post childEndpoint child
                |> toCmd (RemoteData.fromResult >> RemoteData.map Tuple.first >> HandlePostChild)
            )

        HandlePostChild data ->
            ( { model | postChild = data }
            , Cmd.none
            )

        PostMother mother maybeChildId ->
            let
                motherTask =
                    sw.post motherEndpoint mother
                        |> toTask
                        |> Task.map Tuple.first

                childTask motherId =
                    case maybeChildId of
                        Just childId ->
                            object [ encodeMotherField (Just motherId) ]
                                |> sw.patchAny childEndpoint childId
                                |> toTask
                                |> Task.map (always motherId)

                        Nothing ->
                            Task.succeed motherId
            in
            ( { model | postMother = Loading }
            , motherTask
                |> Task.andThen childTask
                |> RemoteData.fromTask
                |> Task.perform HandlePostMother
            )

        HandlePostMother data ->
            ( { model | postMother = data }
            , Cmd.none
            )

        SetMotherOfChild childId motherId ->
            ( { model | setMotherOfChild = EveryDict.insert ( childId, motherId ) Loading model.setMotherOfChild }
            , object [ encodeMotherField (Just motherId) ]
                |> sw.patchAny childEndpoint childId
                |> withoutDecoder
                |> toCmd (RemoteData.fromResult >> HandleSetMotherOfChild childId motherId)
            )

        HandleSetMotherOfChild childId motherId data ->
            ( { model | setMotherOfChild = EveryDict.insert ( childId, motherId ) data model.setMotherOfChild }
            , Cmd.none
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

        ChildRevision uuid data ->
            -- Some of these we currently just invalidate, to let the query run
            -- again.  Others we adjust directly. We could do more of that, as
            -- an optimization.
            let
                -- Update with a Success value, but only if we've asked for it.
                children =
                    EveryDict.update uuid (Maybe.map (always (Success data))) model.children

                -- First, we remove this child from all mothers, and then add
                -- it to the correct mother (if we have data for that mother).
                childrenOfMother =
                    model.childrenOfMother
                        |> EveryDict.map (always (RemoteData.map (EveryDict.remove uuid)))
                        |> (case data.motherId of
                                Nothing ->
                                    identity

                                Just motherId ->
                                    EveryDict.update motherId (Maybe.map (RemoteData.map (EveryDict.insert uuid data)))
                           )
            in
            { model
                | expectedParticipants = EveryDict.empty
                , expectedSessions = EveryDict.remove uuid model.expectedSessions
                , nameSearches = Dict.empty
                , children = children
                , childrenOfMother = childrenOfMother
            }

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

        MotherRevision uuid data ->
            -- We'll need to refetch the expected participants, at least for now ... eventually
            -- we might be able to do something smaller.
            let
                mothers =
                    EveryDict.update uuid (Maybe.map (always (Success data))) model.mothers
            in
            { model
                | expectedParticipants = EveryDict.empty
                , expectedSessions = EveryDict.empty
                , nameSearches = Dict.empty
                , mothers = mothers
            }

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
            -- TODO
            model

        PhotoRevision uuid data ->
            mapChildMeasurements
                data.participantId
                (\measurements -> { measurements | photos = EveryDictList.insert uuid data measurements.photos })
                model

        PmtctParticipantRevision uuid data ->
            -- TODO
            model

        RelationshipRevision uuid data ->
            -- TODO
            model

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


updateBackend : BackendUrl -> String -> MsgBackend -> ModelBackend -> ( ModelBackend, Cmd MsgBackend )
updateBackend backendUrl accessToken msg model =
    let
        crud =
            applyBackendUrl backendUrl
                |> applyAccessToken accessToken
    in
    case msg of
        PostSession session ->
            ( { model | postSessionRequest = Loading }
            , crud.post sessionEndpoint session
                |> toCmd (RemoteData.fromResult >> HandlePostedSession)
            )

        PostTrainingSessionRequest request ->
            ( { model | postTrainingSessionRequest = Loading }
            , crud.post trainingSessionsEndpoint request
                -- We use the Tuple.second becausw we're only interested the
                -- value ... the backend doesn't (currently) send a key.
                |> toCmd (RemoteData.fromResult >> RemoteData.map Tuple.second >> HandleTrainingSessionResponse)
            )

        HandleTrainingSessionResponse webdata ->
            let
                newModel =
                    { model | postTrainingSessionRequest = webdata }
            in
            ( newModel, Cmd.none )

        HandlePostedSession webdata ->
            let
                newModel =
                    { model | postSessionRequest = webdata }
            in
            ( newModel, Cmd.none )

        ResetSessionRequests ->
            -- Reset session requests to `NotAsked` if `Error` or `Success`.
            -- This is for requests where we're showing an  indication in the
            -- UI, and we want to stop doing that at certain moments.
            ( { model
                | postSessionRequest = resetError <| resetSuccess model.postSessionRequest
                , postTrainingSessionRequest = resetError <| resetSuccess model.postTrainingSessionRequest
              }
            , Cmd.none
            )
