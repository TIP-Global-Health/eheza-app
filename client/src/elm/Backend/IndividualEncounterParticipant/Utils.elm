module Backend.IndividualEncounterParticipant.Utils exposing (decodeIndividualEncounterTypeFromString, encoudeIndividualEncounterTypeAsString, isDailyEncounterActive)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterType(..))
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isNothing)


encoudeIndividualEncounterTypeAsString : IndividualEncounterType -> String
encoudeIndividualEncounterTypeAsString encounterType =
    case encounterType of
        AcuteIllnessEncounter ->
            "acute-illness"

        AntenatalEncounter ->
            "antenatal"

        InmmunizationEncounter ->
            "inmmunization"

        NutritionEncounter ->
            "nutrition"


decodeIndividualEncounterTypeFromString : String -> Maybe IndividualEncounterType
decodeIndividualEncounterTypeFromString string =
    case string of
        "acute-illness" ->
            Just AcuteIllnessEncounter

        "antenatal" ->
            Just AntenatalEncounter

        "inmmunization" ->
            Just InmmunizationEncounter

        "nutrition" ->
            Just NutritionEncounter

        _ ->
            Nothing


isDailyEncounterActive : NominalDate -> { a | startDate : NominalDate, endDate : Maybe NominalDate } -> Bool
isDailyEncounterActive currentDate encounter =
    let
        _ =
            Debug.log "encounter" encounter
    in
    encounter.startDate == currentDate && isNothing encounter.endDate
