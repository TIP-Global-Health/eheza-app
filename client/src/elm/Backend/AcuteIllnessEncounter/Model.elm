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
    , saveMalariaTesting : WebData ()
    , saveTravelHistory : WebData ()
    , saveExposure : WebData ()
    , saveIsolation : WebData ()
    , saveHCContact : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeAcuteIllnessEncounter = NotAsked
    , saveSymptomsGeneral = NotAsked
    , saveSymptomsRespiratory = NotAsked
    , saveSymptomsGI = NotAsked
    , saveVitals = NotAsked
    , saveMalariaTesting = NotAsked
    , saveTravelHistory = NotAsked
    , saveExposure = NotAsked
    , saveIsolation = NotAsked
    , saveHCContact = NotAsked
    }


type Msg
    = CloseAcuteIllnessEncounter
    | HandleClosedAcuteIllnessEncounter (WebData ())
    | SaveSymptomsGeneral PersonId (Maybe SymptomsGeneralId) (Dict SymptomsGeneralSign Int)
    | HandleSavedSymptomsGeneral (WebData ())
    | SaveSymptomsRespiratory PersonId (Maybe SymptomsRespiratoryId) (Dict SymptomsRespiratorySign Int)
    | HandleSavedSymptomsRespiratory (WebData ())
    | SaveSymptomsGI PersonId (Maybe SymptomsGIId) (Dict SymptomsGISign Int)
    | HandleSavedSymptomsGI (WebData ())
    | SaveVitals PersonId (Maybe AcuteIllnessVitalsId) AcuteIllnessVitalsValue
    | HandleSavedVitals (WebData ())
    | SaveMalariaTesting PersonId (Maybe MalariaTestingId) (EverySet MalariaTestingSign)
    | HandleSavedMalariaTesting (WebData ())
    | SaveTravelHistory PersonId (Maybe TravelHistoryId) (EverySet TravelHistorySign)
    | HandleSavedTravelHistory (WebData ())
    | SaveExposure PersonId (Maybe ExposureId) (EverySet ExposureSign)
    | HandleSavedExposure (WebData ())
    | SaveIsolation PersonId (Maybe IsolationId) IsolationValue
    | HandleSavedIsolation (WebData ())
    | SaveHCContact PersonId (Maybe HCContactId) HCContactValue
    | HandleSavedHCContact (WebData ())
