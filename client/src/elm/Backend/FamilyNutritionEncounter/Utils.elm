module Backend.FamilyNutritionEncounter.Utils exposing (getFamilyNutritionEncountersForParticipant)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.FamilyNutritionEncounter.Model exposing (FamilyNutritionEncounter)
import Backend.Model exposing (ModelIndexedDb)
import RemoteData exposing (WebData)


getFamilyNutritionEncountersForParticipant : ModelIndexedDb -> FamilyEncounterParticipantId -> List ( FamilyNutritionEncounterId, FamilyNutritionEncounter )
getFamilyNutritionEncountersForParticipant =
    getParticipantEncountersByEncounterType .familyNutritionEncountersByParticipant


getParticipantEncountersByEncounterType :
    (ModelIndexedDb -> Dict FamilyEncounterParticipantId (WebData (Dict encounterId encounter)))
    -> ModelIndexedDb
    -> FamilyEncounterParticipantId
    -> List ( encounterId, encounter )
getParticipantEncountersByEncounterType dataStructureFunc db participantId =
    dataStructureFunc db
        |> Dict.get participantId
        |> Maybe.andThen RemoteData.toMaybe
        |> Maybe.map Dict.toList
        |> Maybe.withDefault []
