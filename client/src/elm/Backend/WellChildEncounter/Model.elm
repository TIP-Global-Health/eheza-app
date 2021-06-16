module Backend.WellChildEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias WellChildEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , shard : Maybe HealthCenterId
    }


emptyWellChildEncounter : IndividualEncounterParticipantId -> NominalDate -> Maybe HealthCenterId -> WellChildEncounter
emptyWellChildEncounter participant startDate shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , shard = shard
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closeWellChildEncounter : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeWellChildEncounter = NotAsked
    }


type Msg
    = CloseWellChildEncounter
    | HandleClosedWellChildEncounter (WebData ())
