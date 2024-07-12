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
    , encounterType : NutritionEncounterType
    , shard : Maybe HealthCenterId
    }


emptyNutritionEncounter : IndividualEncounterParticipantId -> NominalDate -> NutritionEncounterType -> Maybe HealthCenterId -> NutritionEncounter
emptyNutritionEncounter participant startDate encounterType shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , encounterType = encounterType
    , shard = shard
    }


type NutritionEncounterType
    = NutritionEncounterNurse
    | NutritionEncounterCHW
    | NutritionEncounterUnknown


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
    , saveNCDA : WebData ()
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
    , saveNCDA = NotAsked
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
    | SavePhoto PersonId (Maybe NutritionPhotoId) ImageUrl
    | HandleSavedPhoto (WebData ())
    | SaveWeight PersonId (Maybe NutritionWeightId) WeightInKg
    | HandleSavedWeight (WebData ())
    | SaveNCDA PersonId (Maybe NutritionNCDAId) NCDAValue
    | HandleSavedNCDA (WebData ())
    | SaveSendToHC PersonId (Maybe NutritionSendToHCId) SendToHCValue
    | HandleSavedSendToHC (WebData ())
    | SaveHealthEducation PersonId (Maybe NutritionHealthEducationId) HealthEducationValue
    | HandleSavedHealthEducation (WebData ())
    | SaveContributingFactors PersonId (Maybe NutritionContributingFactorsId) (EverySet ContributingFactorsSign)
    | HandleSavedContributingFactors (WebData ())
    | SaveFollowUp PersonId (Maybe NutritionFollowUpId) NutritionFollowUpValue
    | HandleSavedFollowUp (WebData ())
