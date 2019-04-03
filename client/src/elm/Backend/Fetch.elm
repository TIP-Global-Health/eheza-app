module Backend.Fetch exposing (forget, shouldFetch)

import Backend.Model exposing (..)
import Dict
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
        FetchChild childId ->
            EveryDict.get childId model.children
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchChildMeasurements childId ->
            EveryDict.get childId model.childMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchChildrenOfMother motherId ->
            EveryDict.get motherId model.childrenOfMother
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

        FetchMother motherId ->
            EveryDict.get motherId model.mothers
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchMotherMeasurements motherId ->
            EveryDict.get motherId model.motherMeasurements
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchParticipantForms ->
            isNotAsked model.participantForms

        FetchParticipantsByName search ->
            Dict.get (String.trim search) model.nameSearches
                |> Maybe.withDefault NotAsked
                |> isNotAsked

        FetchPeopleByName search ->
            Dict.get (String.trim search) model.personSearches
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
        FetchChild childId ->
            { model | children = EveryDict.remove childId model.children }

        FetchChildMeasurements childId ->
            { model | childMeasurements = EveryDict.remove childId model.childMeasurements }

        FetchChildrenOfMother motherId ->
            { model | childrenOfMother = EveryDict.remove motherId model.childrenOfMother }

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

        FetchMother motherId ->
            { model | mothers = EveryDict.remove motherId model.mothers }

        FetchMotherMeasurements motherId ->
            { model | motherMeasurements = EveryDict.remove motherId model.motherMeasurements }

        FetchParticipantForms ->
            { model | participantForms = NotAsked }

        FetchParticipantsByName search ->
            { model | nameSearches = Dict.remove (String.trim search) model.nameSearches }

        FetchPeopleByName search ->
            { model | personSearches = Dict.remove (String.trim search) model.personSearches }

        FetchSession sessionId ->
            { model | sessions = EveryDict.remove sessionId model.sessions }

        FetchSessionsByClinic clinicId ->
            { model | sessionsByClinic = EveryDict.remove clinicId model.sessionsByClinic }

        FetchSyncData ->
            { model | syncData = NotAsked }

        -- For other messages, we do nothing.
        _ ->
            model
