module Pages.PregnancyOutcome.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Maybe.Extra
import RemoteData exposing (RemoteData(..))


fetch : IndividualEncounterParticipantId -> ModelIndexedDb -> List MsgIndexedDb
fetch participantId db =
    let
        personId =
            Dict.get participantId db.individualParticipants
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .person

        encountersIds =
            Dict.get participantId db.prenatalEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> RemoteData.map Dict.keys
                |> RemoteData.withDefault []

        lastEncounterId =
            encountersIds
                |> List.reverse
                |> List.head

        -- We fetch measurements for all encounters, to be
        -- able to resolve EGA, EDD, Gravida and Para.
        fetchMeasurements =
            encountersIds
                |> List.map FetchPrenatalMeasurements
    in
    Maybe.Extra.values
        [ Maybe.map FetchPerson personId
        , Maybe.map FetchPrenatalEncounter lastEncounterId
        ]
        ++ [ FetchIndividualEncounterParticipant participantId, FetchPrenatalEncountersForParticipant participantId ]
        ++ fetchMeasurements
