module Pages.NCD.Participant.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Utils exposing (resolveIndividualParticipantsForPerson)


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        fetchNCDEncounters =
            resolveIndividualParticipantsForPerson id NCDEncounter db
                |> FetchNCDEncountersForParticipants
    in
    [ FetchPerson id
    , FetchIndividualEncounterParticipantsForPerson id
    , fetchNCDEncounters
    ]
