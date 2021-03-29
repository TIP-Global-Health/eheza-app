module Backend.HomeVisitEncounter.Model exposing (HomeVisitEncounter, Model, Msg(..), emptyModel)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias HomeVisitEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , shard : Maybe HealthCenterId
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closeHomeVisitEncounter : WebData () }


emptyModel : Model
emptyModel =
    { closeHomeVisitEncounter = NotAsked }


type Msg
    = CloseHomeVisitEncounter
    | HandleClosedHomeVisitEncounter (WebData ())
