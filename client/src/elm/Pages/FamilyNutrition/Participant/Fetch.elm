module Pages.FamilyNutrition.Participant.Fetch exposing (fetch)

import Backend.Entities exposing (..)
import Backend.FamilyEncounterParticipant.Model
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import Backend.Utils exposing (resolveFamilyParticipantsForPerson)


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        fetchFamilyNutritionEncountersMsgs =
            resolveFamilyParticipantsForPerson id Backend.FamilyEncounterParticipant.Model.NutritionEncounter db
                |> List.map FetchFamilyNutritionEncountersForParticipant
    in
    [ FetchPerson id
    , FetchFamilyEncounterParticipantsForPerson id
    ]
        ++ fetchFamilyNutritionEncountersMsgs
