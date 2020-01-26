module Backend.PrenatalParticipant.Model exposing (EncounterType(..), PregnancyOutcome(..), PrenatalParticipant, allPregnancyOutcome)

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


type PregnancyOutcome
    = OutcomeLiveAtTerm
    | OutcomeLivePreTerm
    | OutcomeStillAtTerm
    | OutcomeStillPreTerm
    | OutcomeAbortions


allPregnancyOutcome : List PregnancyOutcome
allPregnancyOutcome =
    [ OutcomeLiveAtTerm
    , OutcomeLivePreTerm
    , OutcomeStillAtTerm
    , OutcomeStillPreTerm
    , OutcomeAbortions
    ]
