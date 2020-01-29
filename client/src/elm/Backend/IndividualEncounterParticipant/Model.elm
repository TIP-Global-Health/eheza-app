module Backend.IndividualEncounterParticipant.Model exposing (IndividualEncounterParticipant, IndividualEncounterType(..))

import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)


type alias IndividualEncounterParticipant =
    { person : PersonId
    , encounterType : IndividualEncounterType
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    }


type IndividualEncounterType
    = AntenatalEncounter
    | InmmunizationEncounter
    | NutritionEncounter
