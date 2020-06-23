module Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessEncounter, Model, Msg(..), emptyModel)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias AcuteIllnessEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , shard : Maybe HealthCenterId
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closeAcuteIllnessEncounter : WebData ()
    , saveSymptomsGeneral : WebData ()
    , saveSymptomsRespiratory : WebData ()
    , saveSymptomsGI : WebData ()
    , saveVitals : WebData ()
    , saveAcuteFindings : WebData ()
    , saveMalariaTesting : WebData ()
    , saveTravelHistory : WebData ()
    , saveExposure : WebData ()
    , saveIsolation : WebData ()
    , saveHCContact : WebData ()
    , saveTreatmentReview : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeAcuteIllnessEncounter = NotAsked
    , saveSymptomsGeneral = NotAsked
    , saveSymptomsRespiratory = NotAsked
    , saveSymptomsGI = NotAsked
    , saveVitals = NotAsked
    , saveAcuteFindings = NotAsked
    , saveMalariaTesting = NotAsked
    , saveTravelHistory = NotAsked
    , saveExposure = NotAsked
    , saveIsolation = NotAsked
    , saveHCContact = NotAsked
    , saveTreatmentReview = NotAsked
    }


type Msg
    = CloseAcuteIllnessEncounter
    | HandleClosedAcuteIllnessEncounter (WebData ())
    | SaveSymptomsGeneral PersonId (Maybe SymptomsGeneralId) (Dict SymptomsGeneralSign Int)
    | HandleSavedSymptomsGeneral (WebData ())
    | SaveSymptomsRespiratory PersonId (Maybe SymptomsRespiratoryId) (Dict SymptomsRespiratorySign Int)
    | HandleSavedSymptomsRespiratory (WebData ())
    | SaveSymptomsGI PersonId (Maybe SymptomsGIId) SymptomsGIValue
    | HandleSavedSymptomsGI (WebData ())
    | SaveVitals PersonId (Maybe AcuteIllnessVitalsId) AcuteIllnessVitalsValue
    | HandleSavedVitals (WebData ())
    | SaveAcuteFindings PersonId (Maybe AcuteFindingsId) AcuteFindingsValue
    | HandleSavedAcuteFindings (WebData ())
    | SaveMalariaTesting PersonId (Maybe MalariaTestingId) MalariaRapidTestResult
    | HandleSavedMalariaTesting (WebData ())
    | SaveTravelHistory PersonId (Maybe TravelHistoryId) (EverySet TravelHistorySign)
    | HandleSavedTravelHistory (WebData ())
    | SaveExposure PersonId (Maybe ExposureId) (EverySet ExposureSign)
    | HandleSavedExposure (WebData ())
    | SaveIsolation PersonId (Maybe IsolationId) IsolationValue
    | HandleSavedIsolation (WebData ())
    | SaveHCContact PersonId (Maybe HCContactId) HCContactValue
    | HandleSavedHCContact (WebData ())
    | SaveTreatmentReview PersonId (Maybe TreatmentReviewId) (EverySet TreatmentReviewSign)
    | HandleSavedTreatmentReview (WebData ())
