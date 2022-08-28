module Backend.NCDEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias NCDEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , shard : Maybe HealthCenterId
    }


emptyNCDEncounter : IndividualEncounterParticipantId -> NominalDate -> Maybe HealthCenterId -> NCDEncounter
emptyNCDEncounter participant startDate shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , shard = shard
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closeNCDEncounter : WebData ()
    , saveDangerSigns : WebData ()
    , saveSymptomReview : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeNCDEncounter = NotAsked
    , saveDangerSigns = NotAsked
    , saveSymptomReview = NotAsked
    }


type Msg
    = CloseNCDEncounter
    | HandleClosedNCDEncounter (WebData ())
    | SaveDangerSigns PersonId (Maybe NCDDangerSignsId) NCDDangerSignsValue
    | HandleSavedDangerSigns (WebData ())
    | SaveSymptomReview PersonId (Maybe NCDSymptomReviewId) NCDSymptomReviewValue
    | HandleSavedSymptomReview (WebData ())
