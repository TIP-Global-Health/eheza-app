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
    { saveAhezaChild : WebData ()
    , saveAhezaMother : WebData ()
    , saveMuacChild : WebData ()
    , saveMuacMother : WebData ()
    , updateFamilyNutritionEncounter : WebData ()
    }


emptyModel : Model
emptyModel =
    { saveAhezaChild = NotAsked
    , saveAhezaMother = NotAsked
    , saveMuacChild = NotAsked
    , saveMuacMother = NotAsked
    , updateFamilyNutritionEncounter = NotAsked
    }


type Msg
    = CloseFamilyNutritionEncounter
    | HandleSavedAhezaChild (WebData ())
    | HandleSavedAhezaMother (WebData ())
    | HandleSavedMuacChild (WebData ())
    | HandleSavedMuacMother (WebData ())
    | HandleUpdatedFamilyNutritionEncounter (WebData ())
    | SaveAhezaChild PersonId (Maybe AhezaChildId) Float
    | SaveAhezaMother PersonId (Maybe AhezaMotherId) AhezaMotherValue
    | SaveMuacChild PersonId (Maybe FamilyNutritionMuacChildId) MuacInCm
    | SaveMuacMother PersonId (Maybe FamilyNutritionMuacMotherId) MuacInCm
