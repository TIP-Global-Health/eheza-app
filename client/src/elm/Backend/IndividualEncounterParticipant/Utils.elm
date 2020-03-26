module Backend.IndividualEncounterParticipant.Utils exposing (decodeIndividualEncounterTypeFromString, encoudeIndividualEncounterTypeAsString)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))


encoudeIndividualEncounterTypeAsString : IndividualEncounterType -> String
encoudeIndividualEncounterTypeAsString encounterType =
    case encounterType of
        AntenatalEncounter ->
            "antenatal"

        InmmunizationEncounter ->
            "inmmunization"

        NutritionEncounter ->
            "nutrition"


decodeIndividualEncounterTypeFromString : String -> Maybe IndividualEncounterType
decodeIndividualEncounterTypeFromString string =
    case string of
        "antenatal" ->
            Just AntenatalEncounter

        "inmmunization" ->
            Just InmmunizationEncounter

        "nutrition" ->
            Just NutritionEncounter

        _ ->
            Nothing
