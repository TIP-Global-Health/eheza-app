module Backend.TuberculosisEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias TuberculosisEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , shard : Maybe HealthCenterId
    }


emptyTuberculosisEncounter : IndividualEncounterParticipantId -> NominalDate -> Maybe HealthCenterId -> TuberculosisEncounter
emptyTuberculosisEncounter participant startDate shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , shard = shard
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closeTuberculosisEncounter : WebData ()
    , saveDiagnostics : WebData ()
    , saveSymptomReview : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeTuberculosisEncounter = NotAsked
    , saveDiagnostics = NotAsked
    , saveSymptomReview = NotAsked
    }


type Msg
    = CloseTuberculosisEncounter
    | HandleClosedTuberculosisEncounter (WebData ())
    | SaveDiagnostics PersonId (Maybe TuberculosisDiagnosticsId) TuberculosisDiagnosticsValue
    | HandleSavedDiagnostics (WebData ())
    | SaveSymptomReview PersonId (Maybe TuberculosisSymptomReviewId) TuberculosisSymptomReviewValue
    | HandleSavedSymptomReview (WebData ())
