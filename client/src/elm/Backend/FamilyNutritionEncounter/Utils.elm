module Backend.FamilyNutritionEncounter.Utils exposing (getFamilyNutritionEncountersForParticipant)

import Backend.Entities exposing (..)
import Backend.FamilyNutritionEncounter.Model exposing (FamilyNutritionEncounter)
import Backend.Model exposing (ModelIndexedDb)
import Backend.NutritionEncounter.Utils exposing (getParticipantEncountersByEncounterType)


getFamilyNutritionEncountersForParticipant : ModelIndexedDb -> FamilyEncounterParticipantId -> List ( FamilyNutritionEncounterId, FamilyNutritionEncounter )
getFamilyNutritionEncountersForParticipant =
    getParticipantEncountersByEncounterType .familyNutritionEncountersByParticipant
