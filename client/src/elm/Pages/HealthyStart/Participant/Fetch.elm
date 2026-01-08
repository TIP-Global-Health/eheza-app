module Pages.HealthyStart.Participant.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Utils exposing (resolveIndividualParticipantsForPerson)


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        fetchHealthyStartEncounters =
            resolveIndividualParticipantsForPerson id HealthyStartEncounter db
                |> FetchHealthyStartEncountersForParticipants
    in
    [ FetchPerson id
    , FetchIndividualEncounterParticipantsForPerson id
    , fetchHealthyStartEncounters
    ]
