module Backend.PrenatalParticipant.Model exposing (PrenatalParticipant)

import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)


type alias PrenatalParticipant =
    { person : PersonId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    }
