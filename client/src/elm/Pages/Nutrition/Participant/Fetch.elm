module Pages.Nutrition.Participant.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Utils exposing (resolveIndividualParticipantsForPerson)
import RemoteData exposing (RemoteData(..))


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        fetchNutritionEncounters =
            resolveIndividualParticipantsForPerson id NutritionEncounter db
                |> List.map FetchNutritionEncountersForParticipant

        fetchHomeVisitEncounters =
            resolveIndividualParticipantsForPerson id HomeVisitEncounter db
                |> List.map FetchHomeVisitEncountersForParticipant
    in
    [ FetchPerson id
    , FetchIndividualEncounterParticipantsForPerson id
    ]
        ++ fetchNutritionEncounters
        ++ fetchHomeVisitEncounters
