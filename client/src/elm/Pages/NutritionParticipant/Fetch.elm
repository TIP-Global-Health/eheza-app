module Pages.NutritionParticipant.Fetch exposing (fetch)

import AssocList as Dict
import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model
import Backend.Model exposing (ModelIndexedDb, MsgIndexedDb(..))
import RemoteData exposing (RemoteData(..))


fetch : PersonId -> ModelIndexedDb -> List MsgIndexedDb
fetch id db =
    let
        individualParticipants =
            Dict.get id db.individualParticipantsByPerson
                |> Maybe.withDefault NotAsked

        fetchNutritionEncounters =
            individualParticipants
                |> RemoteData.map
                    (Dict.toList
                        >> List.filterMap
                            (\( participantId, participant ) ->
                                if participant.encounterType == Backend.IndividualEncounterParticipant.Model.NutritionEncounter then
                                    Just participantId

                                else
                                    Nothing
                            )
                        >> List.map FetchNutritionEncountersForParticipant
                    )
                |> RemoteData.withDefault []

        fetchHomeVisitEncounters =
            individualParticipants
                |> RemoteData.map
                    (Dict.toList
                        >> List.filterMap
                            (\( participantId, participant ) ->
                                if participant.encounterType == Backend.IndividualEncounterParticipant.Model.HomeVisitEncounter then
                                    Just participantId

                                else
                                    Nothing
                            )
                        >> List.map FetchHomeVisitEncountersForParticipant
                    )
                |> RemoteData.withDefault []
    in
    fetchNutritionEncounters
        ++ fetchHomeVisitEncounters
        ++ [ FetchPerson id
           , FetchIndividualEncounterParticipantsForPerson id
           ]
