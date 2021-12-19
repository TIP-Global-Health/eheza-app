module Backend.IndividualEncounterParticipant.Utils exposing (..)

import Backend.Entities exposing (..)
import Backend.IndividualEncounterParticipant.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import Maybe.Extra exposing (isNothing)


individualEncounterTypeToString : IndividualEncounterType -> String
individualEncounterTypeToString encounterType =
    case encounterType of
        AcuteIllnessEncounter ->
            "acute-illness"

        AntenatalEncounter ->
            "antenatal"

        HomeVisitEncounter ->
            "home-visit"

        InmmunizationEncounter ->
            "inmmunization"

        NutritionEncounter ->
            "nutrition"

        WellChildEncounter ->
            "well-child"


individualEncounterTypeFromString : String -> Maybe IndividualEncounterType
individualEncounterTypeFromString string =
    case string of
        "acute-illness" ->
            Just AcuteIllnessEncounter

        "antenatal" ->
            Just AntenatalEncounter

        "home-visit" ->
            Just HomeVisitEncounter

        "inmmunization" ->
            Just InmmunizationEncounter

        "nutrition" ->
            Just NutritionEncounter

        "well-child" ->
            Just WellChildEncounter

        _ ->
            Nothing


isDailyEncounterActive : NominalDate -> { a | startDate : NominalDate, endDate : Maybe NominalDate } -> Bool
isDailyEncounterActive currentDate encounter =
    encounter.startDate == currentDate && isNothing encounter.endDate
