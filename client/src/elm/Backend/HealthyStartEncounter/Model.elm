module Backend.HealthyStartEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias HealthyStartEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , shard : Maybe HealthCenterId
    }


emptyHealthyStartEncounter : IndividualEncounterParticipantId -> NominalDate -> Maybe HealthCenterId -> HealthyStartEncounter
emptyHealthyStartEncounter participant startDate shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , shard = shard
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closeHealthyStartEncounter : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeHealthyStartEncounter = NotAsked
    }


type Msg
    = CloseHealthyStartEncounter
    | HandleClosedHealthyStartEncounter (WebData ())
