module Pages.HIV.Participant.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Utils exposing (resolveIndividualParticipantsForPerson)


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        fetchHIVEncounters =
            resolveIndividualParticipantsForPerson id HIVEncounter db
                |> FetchHIVEncountersForParticipants
    in
    [ FetchPerson id
    , FetchIndividualEncounterParticipantsForPerson id
    , fetchHIVEncounters
    ]
