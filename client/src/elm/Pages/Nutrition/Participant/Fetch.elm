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
        individualParticipants =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked

        fetchNutritionEncountersMsgs =
            resolveIndividualParticipantsForPerson id NutritionEncounter db
                |> List.map FetchNutritionEncountersForParticipant

        fetchHomeVisitEncounterMsg =
            resolveIndividualParticipantsForPerson id HomeVisitEncounter db
                |> FetchHomeVisitEncountersForParticipants
    in
    [ FetchPerson id
    , FetchIndividualEncounterParticipantsForPerson id
    , fetchHomeVisitEncounterMsg
    ]
        ++ fetchNutritionEncountersMsgs
