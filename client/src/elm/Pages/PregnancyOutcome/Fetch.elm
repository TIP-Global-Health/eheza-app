module Pages.PregnancyOutcome.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EveryDict
import EveryDictList
import RemoteData exposing (RemoteData(..))


fetch : PrenatalParticipantId -> ModelIndexedDb -> List MsgIndexedDb
fetch participantId db =
    let
        personId =
            EveryDict.get participantId db.prenatalParticipants
                |> Maybe.withDefault NotAsked
                |> RemoteData.toMaybe
                |> Maybe.map .person

        encountersIds =
            EveryDict.get participantId db.prenatalEncountersByParticipant
                |> Maybe.withDefault NotAsked
                |> RemoteData.map EveryDictList.keys
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
    List.filterMap identity
        [ Maybe.map FetchPerson personId
        , Maybe.map FetchPrenatalEncounter lastEncounterId
        ]
        ++ [ FetchPrenatalParticipant participantId, FetchPrenatalEncountersForParticipant participantId ]
        ++ fetchMeasurements
