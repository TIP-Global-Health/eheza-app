module Backend.PrenatalParticipant.Model exposing (EncounterType(..), PrenatalParticipant)

import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)


type alias PrenatalParticipant =
    { person : PersonId
    , encounterType : EncounterType
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    }


type EncounterType
    = AntenatalEncounter
    | InmmunizationEncounter
    | NutritionEncounter
