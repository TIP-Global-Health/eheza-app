module Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounter, Model, Msg(..), emptyModel)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias AcuteIllnessEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closeAcuteIllnessEncounter : WebData ()
    , saveNutrition : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeAcuteIllnessEncounter = NotAsked
    , saveNutrition = NotAsked
    }


type Msg
    = CloseAcuteIllnessEncounter
    | HandleClosedAcuteIllnessEncounter (WebData ())
