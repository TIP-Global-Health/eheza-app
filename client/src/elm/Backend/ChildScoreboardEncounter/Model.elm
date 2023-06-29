module Backend.ChildScoreboardEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias ChildScoreboardEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , shard : Maybe HealthCenterId
    }


emptyChildScoreboardEncounter : IndividualEncounterParticipantId -> NominalDate -> Maybe HealthCenterId -> ChildScoreboardEncounter
emptyChildScoreboardEncounter participant startDate shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , shard = shard
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closeChildScoreboardEncounter : WebData ()
    , saveNCDA : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeChildScoreboardEncounter = NotAsked
    , saveNCDA = NotAsked
    }


type Msg
    = CloseChildScoreboardEncounter
    | HandleClosedChildScoreboardEncounter (WebData ())
    | SaveNCDA PersonId (Maybe ChildScoreboardNCDAId) NCDAValueNEW
    | HandleSavedNCDA (WebData ())
