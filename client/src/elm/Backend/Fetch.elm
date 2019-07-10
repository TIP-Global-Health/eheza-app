module Backend.Fetch exposing (forget, shouldFetch)

import Backend.Model exposing (..)
import Dict
import EveryDict
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
            EveryDict.get childId model.childMeasurements
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
            EveryDict.get id model.editableSessions
                |> Maybe.withDefault NotAsked
                |> isSuccess
                |> not

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

        FetchPeopleByName search ->
            Dict.get (String.trim search) model.personSearches
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPerson id ->
            EveryDict.get id model.people
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchParticipantsForPerson id ->
            EveryDict.get id model.participantsByPerson
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPrenatalEncounter id ->
            AllDict.get id model.prenatalEncounters
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPrenatalEncountersForParticipant id ->
            AllDict.get id model.prenatalEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPrenatalMeasurements id ->
            AllDict.get id model.prenatalMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPrenatalParticipant id ->
            AllDict.get id model.prenatalParticipants
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPrenatalParticipantsForPerson id ->
            AllDict.get id model.prenatalParticipantsByPerson
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchRelationshipsForPerson id ->
            EveryDict.get id model.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> isNotAsked

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

        FetchEditableSession id ->
            { model | editableSessions = EveryDict.remove id model.editableSessions }

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

        FetchPeopleByName search ->
            { model | personSearches = Dict.remove (String.trim search) model.personSearches }

        FetchPerson id ->
            { model | people = EveryDict.remove id model.people }

        FetchPrenatalParticipantsForPerson id ->
            { model | prenatalParticipantsByPerson = AllDict.remove id model.prenatalParticipantsByPerson }

        FetchParticipantsForPerson id ->
            { model | participantsByPerson = EveryDict.remove id model.participantsByPerson }

        FetchPrenatalEncounter id ->
            { model | prenatalEncounters = AllDict.remove id model.prenatalEncounters }

        FetchPrenatalEncountersForParticipant id ->
            { model | prenatalEncountersByParticipant = AllDict.remove id model.prenatalEncountersByParticipant }

        FetchPrenatalMeasurements id ->
            { model | prenatalMeasurements = AllDict.remove id model.prenatalMeasurements }

        FetchPrenatalParticipant id ->
            { model | prenatalParticipants = AllDict.remove id model.prenatalParticipants }

        FetchRelationshipsForPerson id ->
            { model | relationshipsByPerson = EveryDict.remove id model.relationshipsByPerson }

        FetchSession sessionId ->
            { model | sessions = EveryDict.remove sessionId model.sessions }

        FetchSessionsByClinic clinicId ->
            { model | sessionsByClinic = EveryDict.remove clinicId model.sessionsByClinic }

        FetchSyncData ->
            { model | syncData = NotAsked }

        -- For other messages, we do nothing.
        _ ->
            model
