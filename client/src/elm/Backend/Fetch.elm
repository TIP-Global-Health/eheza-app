module Backend.Fetch exposing (forget, shouldFetch)

import Backend.Model exposing (..)
import EveryDict
import RemoteData exposing (RemoteData(..), isNotAsked)


{-| Given a `MsgIndexedDb`, do we need to fetch the data it would fetch?
We only answer `True` if the data is `NotAsked`. So, we don't automatically
re-fetch errors.

Note that the data need not literally be a `RemoteData`, but that will be
common. The answer does need to flip to `False` when a request is in progress,
or we will enter an infinite loop.

-}
shouldFetch : ModelIndexedDb -> MsgIndexedDb -> Bool
shouldFetch model msg =
    case msg of
        FetchChildMeasurements childId ->
            EveryDict.get childId model.childMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchClinics ->
            isNotAsked model.clinics

        FetchEveryCounselingSchedule ->
            isNotAsked model.everyCounselingSchedule

        FetchExpectedParticipants sessionId ->
            EveryDict.get sessionId model.expectedParticipants
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchExpectedSessions childId ->
            EveryDict.get childId model.expectedSessions
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchHealthCenters ->
            isNotAsked model.healthCenters

        FetchMotherMeasurements motherId ->
            EveryDict.get motherId model.motherMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchParticipantForms ->
            isNotAsked model.participantForms

        FetchSession sessionId ->
            EveryDict.get sessionId model.sessions
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchSessionsByClinic clinicId ->
            EveryDict.get clinicId model.sessionsByClinic
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchSyncData ->
            isNotAsked model.syncData

        -- For other messages, we answer false.
        _ ->
            False


{-| Given a Msg that would fetch some data, forget that data.
-}
forget : MsgIndexedDb -> ModelIndexedDb -> ModelIndexedDb
forget msg model =
    case msg of
        FetchChildMeasurements childId ->
            { model | childMeasurements = EveryDict.remove childId model.childMeasurements }

        FetchClinics ->
            { model | clinics = NotAsked }

        FetchEveryCounselingSchedule ->
            { model | everyCounselingSchedule = NotAsked }

        FetchExpectedParticipants sessionId ->
            { model | expectedParticipants = EveryDict.remove sessionId model.expectedParticipants }

        FetchExpectedSessions childId ->
            { model | expectedSessions = EveryDict.remove childId model.expectedSessions }

        FetchHealthCenters ->
            { model | healthCenters = NotAsked }

        FetchMotherMeasurements motherId ->
            { model | motherMeasurements = EveryDict.remove motherId model.motherMeasurements }

        FetchParticipantForms ->
            { model | participantForms = NotAsked }

        FetchSession sessionId ->
            { model | sessions = EveryDict.remove sessionId model.sessions }

        FetchSessionsByClinic clinicId ->
            { model | sessionsByClinic = EveryDict.remove clinicId model.sessionsByClinic }

        FetchSyncData ->
            { model | syncData = NotAsked }

        -- For other messages, we do nothing.
        _ ->
            model
