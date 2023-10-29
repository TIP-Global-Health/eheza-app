module Backend.ChildScoreboardEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
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
    , saveBCGImmunisation : WebData ()
    , saveDTPImmunisation : WebData ()
    , saveDTPStandaloneImmunisation : WebData ()
    , saveIPVImmunisation : WebData ()
    , saveMRImmunisation : WebData ()
    , saveOPVImmunisation : WebData ()
    , savePCV13Immunisation : WebData ()
    , saveRotarixImmunisation : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeChildScoreboardEncounter = NotAsked
    , saveNCDA = NotAsked
    , saveBCGImmunisation = NotAsked
    , saveDTPImmunisation = NotAsked
    , saveDTPStandaloneImmunisation = NotAsked
    , saveIPVImmunisation = NotAsked
    , saveMRImmunisation = NotAsked
    , saveOPVImmunisation = NotAsked
    , savePCV13Immunisation = NotAsked
    , saveRotarixImmunisation = NotAsked
    }


type Msg
    = CloseChildScoreboardEncounter
    | HandleClosedChildScoreboardEncounter (WebData ())
    | SaveNCDA PersonId (Maybe ChildScoreboardNCDAId) NCDAValue
    | HandleSavedNCDA (WebData ())
    | SaveBCGImmunisation PersonId (Maybe ChildScoreboardBCGImmunisationId) VaccinationValue
    | HandleSavedBCGImmunisation (WebData ())
    | SaveDTPImmunisation PersonId (Maybe ChildScoreboardDTPImmunisationId) VaccinationValue
    | HandleSavedDTPImmunisation (WebData ())
    | SaveDTPStandaloneImmunisation PersonId (Maybe ChildScoreboardDTPStandaloneImmunisationId) VaccinationValue
    | HandleSavedDTPStandaloneImmunisation (WebData ())
    | SaveIPVImmunisation PersonId (Maybe ChildScoreboardIPVImmunisationId) VaccinationValue
    | HandleSavedIPVImmunisation (WebData ())
    | SaveMRImmunisation PersonId (Maybe ChildScoreboardMRImmunisationId) VaccinationValue
    | HandleSavedMRImmunisation (WebData ())
    | SaveOPVImmunisation PersonId (Maybe ChildScoreboardOPVImmunisationId) VaccinationValue
    | HandleSavedOPVImmunisation (WebData ())
    | SavePCV13Immunisation PersonId (Maybe ChildScoreboardPCV13ImmunisationId) VaccinationValue
    | HandleSavedPCV13Immunisation (WebData ())
    | SaveRotarixImmunisation PersonId (Maybe ChildScoreboardRotarixImmunisationId) VaccinationValue
    | HandleSavedRotarixImmunisation (WebData ())
