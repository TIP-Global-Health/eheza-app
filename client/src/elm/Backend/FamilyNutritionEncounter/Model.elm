module Backend.FamilyNutritionEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias FamilyNutritionEncounter =
    { participant : FamilyEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , deleted : Bool
    , shard : Maybe HealthCenterId
    }


emptyFamilyNutritionEncounter : FamilyEncounterParticipantId -> NominalDate -> Maybe HealthCenterId -> FamilyNutritionEncounter
emptyFamilyNutritionEncounter participant startDate shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , deleted = False
    , shard = shard
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { updateFamilyNutritionEncounter : WebData ()
    }


emptyModel : Model
emptyModel =
    { updateFamilyNutritionEncounter = NotAsked
    }


type Msg
    = CloseFamilyNutritionEncounter
    | HandleUpdatedFamilyNutritionEncounter (WebData ())
