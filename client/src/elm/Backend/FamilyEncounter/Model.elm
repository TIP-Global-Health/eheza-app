module Backend.FamilyEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias FamilyEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , participants : List PersonId
    , deleted : Bool
    , shard : Maybe HealthCenterId
    }


emptyFamilyEncounter : IndividualEncounterParticipantId -> NominalDate -> Maybe HealthCenterId -> FamilyEncounter
emptyFamilyEncounter participant startDate shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , participants = []
    , deleted = False
    , shard = shard
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closeFamilyEncounter : WebData ()
    , saveFBFMother : WebData ()
    , saveFBFChild : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeFamilyEncounter = NotAsked
    , saveFBFMother = NotAsked
    , saveFBFChild = NotAsked
    }


type Msg
    = CloseFamilyEncounter
    | HandleClosedFamilyEncounter (WebData ())
    | SaveFBFMother PersonId (Maybe FamilyFBFMotherId) FamilyFBFValue
    | HandleSavedFBFMother (WebData ())
    | SaveFBFChild PersonId (Maybe FamilyFBFChildId) FamilyFBFValue
    | HandleSavedFBFChild (WebData ())
