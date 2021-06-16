module Pages.WellChildParticipant.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Utils exposing (resolveIndividualParticipantsForPerson)
import RemoteData exposing (RemoteData(..))


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        individualParticipants =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked

        fetchWellChildEncounters =
            resolveIndividualParticipantsForPerson id WellChildEncounter db
                |> List.map FetchWellChildEncountersForParticipant
    in
    [ FetchPerson id
    , FetchIndividualEncounterParticipantsForPerson id
    ]
        ++ fetchWellChildEncounters
