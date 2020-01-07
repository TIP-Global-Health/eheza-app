module Pages.PrenatalParticipant.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import EveryDict
import EveryDictList
import RemoteData exposing (RemoteData(..))


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        fetchEncounters =
            EveryDict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
                |> RemoteData.map
                    (EveryDictList.keys
                        >> List.map FetchPrenatalEncountersForParticipant
                    )
                |> RemoteData.withDefault []
    in
    fetchEncounters
        ++ [ FetchPerson id
           , FetchIndividualEncounterParticipantsForPerson id
           ]
