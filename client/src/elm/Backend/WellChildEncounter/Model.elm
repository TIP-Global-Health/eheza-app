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
    , savePregnancySummary : WebData ()
    , saveSymptomsReview : WebData ()
    , saveVitals : WebData ()
    , saveHeight : WebData ()
    , saveHeadCircumference : WebData ()
    , saveMuac : WebData ()
    , saveNutrition : WebData ()
    , savePhoto : WebData ()
    , saveWeight : WebData ()
    , saveContributingFactors : WebData ()
    , saveHealthEducation : WebData ()
    , saveFollowUp : WebData ()
    , saveSendToHC : WebData ()
    , saveImmunisation : WebData ()
    , saveECD : WebData ()
    , saveMebendezole : WebData ()
    , saveVitaminA : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeWellChildEncounter = NotAsked
    , savePregnancySummary = NotAsked
    , saveSymptomsReview = NotAsked
    , saveVitals = NotAsked
    , saveHeight = NotAsked
    , saveHeadCircumference = NotAsked
    , saveMuac = NotAsked
    , saveNutrition = NotAsked
    , savePhoto = NotAsked
    , saveWeight = NotAsked
    , saveContributingFactors = NotAsked
    , saveHealthEducation = NotAsked
    , saveFollowUp = NotAsked
    , saveSendToHC = NotAsked
    , saveImmunisation = NotAsked
    , saveECD = NotAsked
    , saveMebendezole = NotAsked
    , saveVitaminA = NotAsked
    }


type Msg
    = CloseWellChildEncounter
    | HandleClosedWellChildEncounter (WebData ())
    | SavePregnancySummary PersonId (Maybe WellChildPregnancySummaryId) PregnancySummaryValue
    | HandleSavedPregnancySummary (WebData ())
    | SaveSymptomsReview PersonId (Maybe WellChildSymptomsReviewId) (EverySet WellChildSymptom)
    | HandleSavedSymptomsReview (WebData ())
    | SaveVitals PersonId (Maybe WellChildVitalsId) BasicVitalsValue
    | HandleSavedVitals (WebData ())
    | SaveHeight PersonId (Maybe WellChildHeightId) HeightInCm
    | SaveHeadCircumference PersonId (Maybe WellChildHeadCircumferenceId) HeadCircumferenceValue
    | HandleSavedHeadCircumference (WebData ())
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
    | SaveECD PersonId (Maybe WellChildECDId) (EverySet ECDSign)
    | SaveImmunisation PersonId (Maybe WellChildImmunisationId) ImmunisationValue
    | HandleSavedImmunisation (WebData ())
    | HandleSavedECD (WebData ())
    | SaveMebendezole PersonId (Maybe WellChildMebendezoleId) AdministrationNote
    | HandleSavedMebendezole (WebData ())
    | SaveVitaminA PersonId (Maybe WellChildVitaminAId) AdministrationNote
    | HandleSavedVitaminA (WebData ())
