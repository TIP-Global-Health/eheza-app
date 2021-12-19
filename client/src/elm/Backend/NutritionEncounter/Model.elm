module Backend.NutritionEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias NutritionEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , shard : Maybe HealthCenterId
    }


emptyNutritionEncounter : IndividualEncounterParticipantId -> NominalDate -> Maybe HealthCenterId -> NutritionEncounter
emptyNutritionEncounter participant startDate shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , shard = shard
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closeNutritionEncounter : WebData ()
    , saveHeight : WebData ()
    , saveMuac : WebData ()
    , saveNutrition : WebData ()
    , savePhoto : WebData ()
    , saveWeight : WebData ()
    , saveSendToHC : WebData ()
    , saveHealthEducation : WebData ()
    , saveContributingFactors : WebData ()
    , saveFollowUp : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeNutritionEncounter = NotAsked
    , saveHeight = NotAsked
    , saveMuac = NotAsked
    , saveNutrition = NotAsked
    , savePhoto = NotAsked
    , saveWeight = NotAsked
    , saveSendToHC = NotAsked
    , saveHealthEducation = NotAsked
    , saveContributingFactors = NotAsked
    , saveFollowUp = NotAsked
    }


type Msg
    = CloseNutritionEncounter
    | HandleClosedNutritionEncounter (WebData ())
    | SaveHeight PersonId (Maybe NutritionHeightId) HeightInCm
    | HandleSavedHeight (WebData ())
    | SaveMuac PersonId (Maybe NutritionMuacId) MuacInCm
    | HandleSavedMuac (WebData ())
    | SaveNutrition PersonId (Maybe NutritionNutritionId) NutritionValue
    | HandleSavedNutrition (WebData ())
    | SavePhoto PersonId (Maybe NutritionPhotoId) PhotoUrl
    | HandleSavedPhoto (WebData ())
    | SaveWeight PersonId (Maybe NutritionWeightId) WeightInKg
    | HandleSavedWeight (WebData ())
    | SaveSendToHC PersonId (Maybe NutritionSendToHCId) SendToHCValue
    | HandleSavedSendToHC (WebData ())
    | SaveHealthEducation PersonId (Maybe NutritionHealthEducationId) HealthEducationValue
    | HandleSavedHealthEducation (WebData ())
    | SaveContributingFactors PersonId (Maybe NutritionContributingFactorsId) (EverySet ContributingFactorsSign)
    | HandleSavedContributingFactors (WebData ())
    | SaveFollowUp PersonId (Maybe NutritionFollowUpId) FollowUpValue
    | HandleSavedFollowUp (WebData ())
