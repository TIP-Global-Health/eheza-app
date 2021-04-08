module Pages.PrenatalParticipant.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import RemoteData exposing (RemoteData(..))


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        fetchEncounters =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked
                |> RemoteData.map
                    (Dict.toList
                        >> List.filterMap
                            (\( participantId, participant ) ->
                                if participant.encounterType == Backend.IndividualEncounterParticipant.Model.AntenatalEncounter then
                                    Just participantId

                                else
                                    Nothing
                            )
                        >> List.map FetchPrenatalEncountersForParticipant
                    )
                |> RemoteData.withDefault []
    in
    fetchEncounters
        ++ [ FetchPerson id
           , FetchIndividualEncounterParticipantsForPerson id
           ]
