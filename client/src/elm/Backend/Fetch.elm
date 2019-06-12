module Backend.Fetch exposing (forget, shouldFetch)

import AllDict
import Backend.Model exposing (..)
import Dict
import RemoteData exposing (RemoteData(..), isNotAsked, isSuccess)


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
            AllDict.get childId model.childMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchClinics ->
            isNotAsked model.clinics

        FetchEditableSession id ->
            -- This one is a bit special because it is synthetic ...  what
            -- we're asking for here is not the fetch itself, but a certain
            -- organization of the fetched data. We want to re-run the
            -- organiztion in every case unless we have a success here.
            -- Which means, once we have a success, it's important to
            -- invalidate or modify our successful data if underlying data
            -- changes. (That is, our `handleRevisions` needs to keep the
            -- editable sessions in mind).
            AllDict.get id model.editableSessions
                |> Maybe.withDefault NotAsked
                |> isSuccess
                |> not

        FetchEveryCounselingSchedule ->
            isNotAsked model.everyCounselingSchedule

        FetchExpectedParticipants sessionId ->
            AllDict.get sessionId model.expectedParticipants
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchExpectedSessions childId ->
            AllDict.get childId model.expectedSessions
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchHealthCenters ->
            isNotAsked model.healthCenters

        FetchMotherMeasurements motherId ->
            AllDict.get motherId model.motherMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchParticipantForms ->
            isNotAsked model.participantForms

        FetchPeopleByName search ->
            Dict.get (String.trim search) model.personSearches
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPerson id ->
            AllDict.get id model.people
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchParticipantsForPerson id ->
            AllDict.get id model.participantsByPerson
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchRelationshipsForPerson id ->
            AllDict.get id model.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchSession sessionId ->
            AllDict.get sessionId model.sessions
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchSessionsByClinic clinicId ->
            AllDict.get clinicId model.sessionsByClinic
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
            { model | childMeasurements = AllDict.remove childId model.childMeasurements }

        FetchClinics ->
            { model | clinics = NotAsked }

        FetchEditableSession id ->
            { model | editableSessions = AllDict.remove id model.editableSessions }

        FetchEveryCounselingSchedule ->
            { model | everyCounselingSchedule = NotAsked }

        FetchExpectedParticipants sessionId ->
            { model | expectedParticipants = AllDict.remove sessionId model.expectedParticipants }

        FetchExpectedSessions childId ->
            { model | expectedSessions = AllDict.remove childId model.expectedSessions }

        FetchHealthCenters ->
            { model | healthCenters = NotAsked }

        FetchMotherMeasurements motherId ->
            { model | motherMeasurements = AllDict.remove motherId model.motherMeasurements }

        FetchParticipantForms ->
            { model | participantForms = NotAsked }

        FetchPeopleByName search ->
            { model | personSearches = Dict.remove (String.trim search) model.personSearches }

        FetchPerson id ->
            { model | people = AllDict.remove id model.people }

        FetchParticipantsForPerson id ->
            { model | participantsByPerson = AllDict.remove id model.participantsByPerson }

        FetchRelationshipsForPerson id ->
            { model | relationshipsByPerson = AllDict.remove id model.relationshipsByPerson }

        FetchSession sessionId ->
            { model | sessions = AllDict.remove sessionId model.sessions }

        FetchSessionsByClinic clinicId ->
            { model | sessionsByClinic = AllDict.remove clinicId model.sessionsByClinic }

        FetchSyncData ->
            { model | syncData = NotAsked }

        -- For other messages, we do nothing.
        _ ->
            model
