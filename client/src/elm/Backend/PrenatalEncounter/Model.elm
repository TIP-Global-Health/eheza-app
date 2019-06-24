module Backend.PrenatalEncounter.Model exposing (PrenatalEncounter)

import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)


type alias PrenatalEncounter =
    { startDate : NominalDate
    , endDate : Maybe NominalDate
    , participant : PrenatalParticipantId
    }
