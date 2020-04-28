module Backend.Fetch exposing (forget, shouldFetch)

import AssocList as Dict
import Backend.Model exposing (..)
import LocalData exposing (isNotNeeded)
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
    let
        hasNoSuccessValues dict =
            dict
                |> Dict.values
                |> List.filter (\v -> RemoteData.isLoading v || RemoteData.isNotAsked v)
                |> List.isEmpty
                |> not
    in
    case msg of
        FetchChildMeasurements childId ->
            Dict.get childId model.childMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchChildrenMeasurements ids ->
            if List.isEmpty ids then
                False

            else
                hasNoSuccessValues model.childMeasurements

        FetchClinics ->
            isNotAsked model.clinics

        FetchEditableSession id _ ->
            -- This one is a bit special because it is synthetic ...  what
            -- we're asking for here is not the fetch itself, but a certain
            -- organization of the fetched data. We want to re-run the
            -- organization in every case unless we have a success here.
            -- Which means, once we have a success, it's important to
            -- invalidate or modify our successful data if underlying data
            -- changes. (That is, our `handleRevisions` needs to keep the
            -- editable sessions in mind).
            Dict.get id model.editableSessions
                |> Maybe.withDefault NotAsked
                |> isSuccess
                |> not

        FetchEditableSessionCheckedIn id ->
            Dict.get id model.editableSessions
                |> Maybe.withDefault NotAsked
                |> RemoteData.map (.checkedIn >> isNotNeeded)
                |> RemoteData.withDefault False

        FetchEditableSessionMeasurements id ->
            let
                -- Make sure we don't still have measurements being lazy loaded. If we do, allow rebuilding the
                -- `EditableSession`.
                hasMothersMeasurementsNotSuccess =
                    hasNoSuccessValues model.motherMeasurements

                hasChildrenMeasurementsNotSuccess =
                    hasNoSuccessValues model.childMeasurements

                measurementsNotSuccess =
                    Dict.get id model.editableSessions
                        |> Maybe.withDefault NotAsked
                        |> RemoteData.map (.offlineSession >> .measurements >> isNotNeeded)
                        |> RemoteData.withDefault False
            in
            measurementsNotSuccess
                || hasMothersMeasurementsNotSuccess
                || hasChildrenMeasurementsNotSuccess

        FetchEditableSessionSummaryByActivity id ->
            Dict.get id model.editableSessions
                |> Maybe.withDefault NotAsked
                |> RemoteData.map (.summaryByActivity >> isNotNeeded)
                |> RemoteData.withDefault False

        FetchEditableSessionSummaryByParticipant id ->
            Dict.get id model.editableSessions
                |> Maybe.withDefault NotAsked
                |> RemoteData.map (.summaryByParticipant >> isNotNeeded)
                |> RemoteData.withDefault False

        FetchEveryCounselingSchedule ->
            isNotAsked model.everyCounselingSchedule

        FetchExpectedParticipants sessionId ->
            Dict.get sessionId model.expectedParticipants
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchExpectedSessions childId ->
            Dict.get childId model.expectedSessions
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchHealthCenters ->
            isNotAsked model.healthCenters

        FetchVillages ->
            isNotAsked model.villages

        FetchMotherMeasurements motherId ->
            Dict.get motherId model.motherMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchMothersMeasurements ids ->
            if List.isEmpty ids then
                False

            else
                hasNoSuccessValues model.motherMeasurements

        FetchParticipantForms ->
            isNotAsked model.participantForms

        FetchPeopleByName search ->
            Dict.get (String.trim search) model.personSearches
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPerson id ->
            Dict.get id model.people
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPeople ids ->
            if List.isEmpty ids then
                False

            else
                hasNoSuccessValues model.people

        FetchParticipantsForPerson id ->
            Dict.get id model.participantsByPerson
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPrenatalEncounter id ->
            Dict.get id model.prenatalEncounters
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPrenatalEncountersForParticipant id ->
            Dict.get id model.prenatalEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPrenatalMeasurements id ->
            Dict.get id model.prenatalMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchIndividualEncounterParticipant id ->
            Dict.get id model.individualParticipants
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchIndividualEncounterParticipantsForPerson id ->
            Dict.get id model.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchRelationshipsForPerson id ->
            Dict.get id model.relationshipsByPerson
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchSession sessionId ->
            Dict.get sessionId model.sessions
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchSessionsByClinic clinicId ->
            Dict.get clinicId model.sessionsByClinic
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
            { model | childMeasurements = Dict.remove childId model.childMeasurements }

        FetchClinics ->
            { model | clinics = NotAsked }

        FetchEditableSession id _ ->
            { model | editableSessions = Dict.remove id model.editableSessions }

        FetchEveryCounselingSchedule ->
            { model | everyCounselingSchedule = NotAsked }

        FetchExpectedParticipants sessionId ->
            { model | expectedParticipants = Dict.remove sessionId model.expectedParticipants }

        FetchExpectedSessions childId ->
            { model | expectedSessions = Dict.remove childId model.expectedSessions }

        FetchHealthCenters ->
            { model | healthCenters = NotAsked }

        FetchVillages ->
            { model | villages = NotAsked }

        FetchMotherMeasurements motherId ->
            { model | motherMeasurements = Dict.remove motherId model.motherMeasurements }

        FetchParticipantForms ->
            { model | participantForms = NotAsked }

        FetchPeopleByName search ->
            { model | personSearches = Dict.remove (String.trim search) model.personSearches }

        FetchPerson id ->
            { model | people = Dict.remove id model.people }

        FetchIndividualEncounterParticipantsForPerson id ->
            { model | individualParticipantsByPerson = Dict.remove id model.individualParticipantsByPerson }

        FetchParticipantsForPerson id ->
            { model | participantsByPerson = Dict.remove id model.participantsByPerson }

        FetchPrenatalEncounter id ->
            { model | prenatalEncounters = Dict.remove id model.prenatalEncounters }

        FetchPrenatalEncountersForParticipant id ->
            { model | prenatalEncountersByParticipant = Dict.remove id model.prenatalEncountersByParticipant }

        FetchPrenatalMeasurements id ->
            { model | prenatalMeasurements = Dict.remove id model.prenatalMeasurements }

        FetchIndividualEncounterParticipant id ->
            { model | individualParticipants = Dict.remove id model.individualParticipants }

        FetchRelationshipsForPerson id ->
            { model | relationshipsByPerson = Dict.remove id model.relationshipsByPerson }

        FetchSession sessionId ->
            { model | sessions = Dict.remove sessionId model.sessions }

        FetchSessionsByClinic clinicId ->
            { model | sessionsByClinic = Dict.remove clinicId model.sessionsByClinic }

        FetchSyncData ->
            { model | syncData = NotAsked }

        -- For other messages, we do nothing.
        _ ->
            model
