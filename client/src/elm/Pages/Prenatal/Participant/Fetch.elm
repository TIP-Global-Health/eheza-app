module Pages.Prenatal.Participant.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Utils exposing (resolveIndividualParticipantsForPerson)
import RemoteData exposing (RemoteData(..))


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        fetchEncountersMsgs =
            resolveIndividualParticipantsForPerson id AntenatalEncounter db
                |> List.map FetchPrenatalEncountersForParticipant
    in
    [ FetchPerson id
    , FetchIndividualEncounterParticipantsForPerson id
    ]
        ++ fetchEncountersMsgs
