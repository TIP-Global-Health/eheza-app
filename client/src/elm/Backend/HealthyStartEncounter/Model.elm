module Backend.HealthyStartEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.HealthyStartEncounter.Types exposing (HealthyStartDiagnosis(..), HealthyStartEncounterType(..))
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias HealthyStartEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , encounterType : HealthyStartEncounterType
    , diagnoses : EverySet HealthyStartDiagnosis
    , pastDiagnoses : EverySet HealthyStartDiagnosis
    , indicators : EverySet HealthyStartIndicator
    , nextVisitDate : Maybe NominalDate
    , shard : Maybe HealthCenterId
    }


emptyHealthyStartEncounter : IndividualEncounterParticipantId -> NominalDate -> HealthyStartEncounterType -> Maybe HealthCenterId -> HealthyStartEncounter
emptyHealthyStartEncounter participant startDate encounterType shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , encounterType = encounterType
    , diagnoses = EverySet.empty
    , pastDiagnoses = EverySet.empty
    , indicators = EverySet.empty
    , nextVisitDate = Nothing
    , shard = shard
    }


type HealthyStartIndicator
    = NoHealthyStartIndicators


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
