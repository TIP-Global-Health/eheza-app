module Backend.WellChildEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias WellChildEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , encounterType : WellChildEncounterType
    , shard : Maybe HealthCenterId
    }


emptyWellChildEncounter : IndividualEncounterParticipantId -> NominalDate -> WellChildEncounterType -> Maybe HealthCenterId -> WellChildEncounter
emptyWellChildEncounter participant startDate encounterType shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , encounterType = encounterType
    , shard = shard
    }


type WellChildEncounterType
    = PediatricCare
    | NewbornExam


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closeWellChildEncounter : WebData ()
    , saveSymptomsReview : WebData ()
    , saveVitals : WebData ()
    , saveHeight : WebData ()
    , saveMuac : WebData ()
    , saveNutrition : WebData ()
    , savePhoto : WebData ()
    , saveWeight : WebData ()
    , saveContributingFactors : WebData ()
    , saveHealthEducation : WebData ()
    , saveFollowUp : WebData ()
    , saveSendToHC : WebData ()
    , saveHeadCircumference : WebData ()
    , saveECD : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeWellChildEncounter = NotAsked
    , saveSymptomsReview = NotAsked
    , saveVitals = NotAsked
    , saveHeight = NotAsked
    , saveMuac = NotAsked
    , saveNutrition = NotAsked
    , savePhoto = NotAsked
    , saveWeight = NotAsked
    , saveContributingFactors = NotAsked
    , saveHealthEducation = NotAsked
    , saveFollowUp = NotAsked
    , saveSendToHC = NotAsked
    , saveHeadCircumference = NotAsked
    , saveECD = NotAsked
    }


type Msg
    = CloseWellChildEncounter
    | HandleClosedWellChildEncounter (WebData ())
    | SaveSymptomsReview PersonId (Maybe WellChildSymptomsReviewId) (EverySet WellChildSymptom)
    | HandleSavedSymptomsReview (WebData ())
    | SaveVitals PersonId (Maybe WellChildVitalsId) BasicVitalsValue
    | HandleSavedVitals (WebData ())
    | SaveHeight PersonId (Maybe WellChildHeightId) HeightInCm
    | HandleSavedHeight (WebData ())
    | SaveMuac PersonId (Maybe WellChildMuacId) MuacInCm
    | HandleSavedMuac (WebData ())
    | SaveNutrition PersonId (Maybe WellChildNutritionId) (EverySet ChildNutritionSign)
    | HandleSavedNutrition (WebData ())
    | SavePhoto PersonId (Maybe WellChildPhotoId) PhotoUrl
    | HandleSavedPhoto (WebData ())
    | SaveWeight PersonId (Maybe WellChildWeightId) WeightInKg
    | HandleSavedWeight (WebData ())
    | SaveContributingFactors PersonId (Maybe WellChildContributingFactorsId) (EverySet ContributingFactorsSign)
    | HandleSavedContributingFactors (WebData ())
    | SaveHealthEducation PersonId (Maybe WellChildHealthEducationId) HealthEducationValue
    | HandleSavedHealthEducation (WebData ())
    | SaveFollowUp PersonId (Maybe WellChildFollowUpId) FollowUpValue
    | HandleSavedFollowUp (WebData ())
    | SaveSendToHC PersonId (Maybe WellChildSendToHCId) SendToHCValue
    | HandleSavedSendToHC (WebData ())
    | SaveHeadCircumference PersonId (Maybe WellChildHeadCircumferenceId) HeadCircumferenceValue
    | HandleSavedHeadCircumference (WebData ())
    | SaveECD PersonId (Maybe WellChildECDId) (EverySet ECDSign)
    | HandleSavedECD (WebData ())
