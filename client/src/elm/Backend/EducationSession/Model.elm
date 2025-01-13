module Backend.EducationSession.Model exposing (..)

import Backend.Entities exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias EducationSession =
    { startDate : NominalDate
    , nurse : NurseId
    , village : VillageId
    , topics : EverySet EducationTopic
    , participants : EverySet PersonId
    , endDate : Maybe NominalDate
    , shard : Maybe HealthCenterId
    }


emptyEducationSession : NominalDate -> NurseId -> VillageId -> Maybe HealthCenterId -> EducationSession
emptyEducationSession startDate nurse village shard =
    { startDate = startDate
    , nurse = nurse
    , village = village
    , topics = EverySet.empty
    , participants = EverySet.empty
    , endDate = Nothing
    , shard = shard
    }


type EducationTopic
    = TopicTuberculosis
    | TopicSTD
    | TopicMentalHealth
    | TopicMalaria
    | TopicChildhoodIllnesses
    | TopicMalnutrition
    | TopicANCPostpartum
    | TopicFamilyPlanning
    | TopicGender
    | TopicNCD


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { updateEducationSession : WebData ()
    }


emptyModel : Model
emptyModel =
    { updateEducationSession = NotAsked
    }


type Msg
    = Update (EducationSession -> EducationSession)
    | HandleUpdated (WebData ())
