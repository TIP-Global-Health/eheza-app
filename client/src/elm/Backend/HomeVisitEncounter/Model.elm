module Backend.HomeVisitEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias HomeVisitEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , shard : Maybe HealthCenterId
    }


emptyHomeVisitEncounter : IndividualEncounterParticipantId -> NominalDate -> Maybe HealthCenterId -> HomeVisitEncounter
emptyHomeVisitEncounter participant startDate shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , shard = shard
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closeHomeVisitEncounter : WebData ()
    , saveFeeding : WebData ()
    , saveHygiene : WebData ()
    , saveFoodSecurity : WebData ()
    , saveCaring : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeHomeVisitEncounter = NotAsked
    , saveFeeding = NotAsked
    , saveHygiene = NotAsked
    , saveFoodSecurity = NotAsked
    , saveCaring = NotAsked
    }


type Msg
    = CloseHomeVisitEncounter
    | HandleClosedHomeVisitEncounter (WebData ())
    | SaveFeeding PersonId (Maybe NutritionFeedingId) NutritionFeedingValue
    | HandleSavedFeeding (WebData ())
    | SaveHygiene PersonId (Maybe NutritionHygieneId) NutritionHygieneValue
    | HandleSavedHygiene (WebData ())
    | SaveFoodSecurity PersonId (Maybe NutritionFoodSecurityId) NutritionFoodSecurityValue
    | HandleSavedFoodSecurity (WebData ())
    | SaveCaring PersonId (Maybe NutritionCaringId) NutritionCaringValue
    | HandleSavedCaring (WebData ())
