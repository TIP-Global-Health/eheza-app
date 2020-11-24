module Backend.IndividualEncounterParticipant.Utils exposing (..)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isNothing)


encodeIndividualEncounterTypeAsString : IndividualEncounterType -> String
encodeIndividualEncounterTypeAsString encounterType =
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
    encounter.startDate == currentDate && isNothing encounter.endDate


emptyIndividualEncounterParticipant : NominalDate -> PersonId -> IndividualEncounterType -> HealthCenterId -> IndividualEncounterParticipant
emptyIndividualEncounterParticipant currentDate personId type_ healthCenterId =
    IndividualEncounterParticipant personId type_ currentDate Nothing Nothing Nothing Nothing Nothing False (Just healthCenterId)
