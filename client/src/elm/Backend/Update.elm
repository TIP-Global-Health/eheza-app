module Backend.Update exposing (updateBackend, updateIndexedDb)

import Activity.Utils
import App.Model
import Backend.Counseling.Decoder exposing (combineCounselingSchedules)
import Backend.Endpoints exposing (..)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (emptyMotherMeasurementList)
import Backend.Measurement.Utils exposing (mapMeasurementData)
import Backend.Model exposing (..)
import Backend.Session.Decoder exposing (decodeSession, decodeTrainingSessionRequest)
import Backend.Session.Encoder exposing (encodeOfflineSession, encodeOfflineSessionWithId, encodeSession, encodeTrainingSessionRequest)
import Backend.Session.Model exposing (EditableSession, OfflineSession, Session)
import Backend.Session.Update
import Backend.Session.Utils exposing (getChildMeasurementData, getMotherMeasurementData, makeEditableSession)
import Config.Model exposing (BackendUrl)
import Dict exposing (Dict)
import EveryDict
import EveryDictList
import Gizra.Json exposing (decodeInt)
import Gizra.NominalDate exposing (NominalDate)
import Gizra.Update exposing (sequenceExtra)
import Http exposing (Error)
import HttpBuilder
import Json.Decode exposing (Decoder, field, succeed)
import Json.Encode exposing (Value, object)
import Json.Encode.Extra
import Measurement.Model exposing (OutMsgChild(..), OutMsgMother(..))
import RemoteData exposing (RemoteData(..))
import Restful.Endpoint exposing (EntityUuid, ReadOnlyEndPoint, ReadWriteEndPoint, applyAccessToken, applyBackendUrl, decodeEntityUuid, decodeSingleDrupalEntity, drupalBackend, drupalEndpoint, encodeEntityUuid, endpoint, fromEntityUuid, toCmd, toEntityUuid, toTask, withKeyEncoder, withParamsEncoder, withValueEncoder, withoutDecoder)
import Rollbar
import Task
import Utils.WebData exposing (resetError, resetSuccess)


updateIndexedDb : NominalDate -> Maybe NurseId -> MsgIndexedDb -> ModelIndexedDb -> ( ModelIndexedDb, Cmd MsgIndexedDb )
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
            )

        HandleFetchedChildMeasurements childId data ->
            ( { model | childMeasurements = EveryDict.insert childId data model.childMeasurements }
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
                    sw.select childEndpoint { session = Just sessionId }
                        |> toTask
                        |> Task.map (.items >> EveryDictList.fromList)
                        |> RemoteData.fromTask

                motherTask =
                    sw.select motherEndpoint { session = Just sessionId }
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


handleRevision : Revision -> ModelIndexedDb -> ModelIndexedDb
handleRevision revision model =
    case revision of
        AttendanceRevision uuid data ->
            let
                motherMeasurements =
                    EveryDict.get data.participantId model.motherMeasurements
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.map (\measurements -> { measurements | attendances = EveryDictList.insert uuid data measurements.attendances })
            in
            { model | motherMeasurements = EveryDict.insert data.participantId motherMeasurements model.motherMeasurements }

        HealthCenterRevision uuid data ->
            let
                -- We don't do anything with revisions until we've fetched
                -- some original data.
                healthCenters =
                    RemoteData.map (EveryDictList.insert uuid data) model.healthCenters
            in
            { model | healthCenters = healthCenters }

        ClinicRevision uuid data ->
            let
                clinics =
                    RemoteData.map (EveryDictList.insert uuid data) model.clinics
            in
            { model | clinics = clinics }

        SessionRevision uuid data ->
            -- First, remove the session from all clinics (it might previously have been
            -- in any). Then, add it in the right place.
            let
                sessionsByClinic =
                    model.sessionsByClinic
                        |> EveryDict.map (always (RemoteData.map (EveryDictList.remove uuid)))
                        |> EveryDict.update data.clinicId (Maybe.map (RemoteData.map (EveryDictList.insert uuid data)))
            in
            { model | sessionsByClinic = sessionsByClinic }

        _ ->
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
