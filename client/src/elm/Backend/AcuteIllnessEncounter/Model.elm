module Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis(..), AcuteIllnessEncounter, Model, Msg(..), emptyAcuteIllnessEncounter, emptyModel)

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
    , diagnosis : AcuteIllnessDiagnosis
    , shard : Maybe HealthCenterId
    }


emptyAcuteIllnessEncounter : IndividualEncounterParticipantId -> NominalDate -> Maybe HealthCenterId -> AcuteIllnessEncounter
emptyAcuteIllnessEncounter participant startDate shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , diagnosis = NoAcuteIllnessDiagnosis
    , shard = shard
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { updateAcuteIllnessEncounter : WebData ()
    , saveSymptomsGeneral : WebData ()
    , saveSymptomsRespiratory : WebData ()
    , saveSymptomsGI : WebData ()
    , saveVitals : WebData ()
    , saveAcuteFindings : WebData ()
    , saveMalariaTesting : WebData ()
    , saveSendToHC : WebData ()
    , saveMedicationDistribution : WebData ()
    , saveTravelHistory : WebData ()
    , saveExposure : WebData ()
    , saveIsolation : WebData ()
    , saveHCContact : WebData ()
    , saveCall114 : WebData ()
    , saveTreatmentReview : WebData ()
    , saveMuac : WebData ()
    , saveTreatmentOngoing : WebData ()
    }


emptyModel : Model
emptyModel =
    { updateAcuteIllnessEncounter = NotAsked
    , saveSymptomsGeneral = NotAsked
    , saveSymptomsRespiratory = NotAsked
    , saveSymptomsGI = NotAsked
    , saveVitals = NotAsked
    , saveAcuteFindings = NotAsked
    , saveMalariaTesting = NotAsked
    , saveSendToHC = NotAsked
    , saveMedicationDistribution = NotAsked
    , saveTravelHistory = NotAsked
    , saveExposure = NotAsked
    , saveIsolation = NotAsked
    , saveHCContact = NotAsked
    , saveCall114 = NotAsked
    , saveTreatmentReview = NotAsked
    , saveMuac = NotAsked
    , saveTreatmentOngoing = NotAsked
    }


type AcuteIllnessDiagnosis
    = DiagnosisCovid19
    | DiagnosisMalariaComplicated
    | DiagnosisMalariaUncomplicated
    | DiagnosisMalariaUncomplicatedAndPregnant
    | DiagnosisGastrointestinalInfectionComplicated
    | DiagnosisGastrointestinalInfectionUncomplicated
    | DiagnosisSimpleColdAndCough
    | DiagnosisRespiratoryInfectionComplicated
    | DiagnosisRespiratoryInfectionUncomplicated
    | DiagnosisFeverOfUnknownOrigin
    | DiagnosisUndeterminedMoreEvaluationNeeded
    | NoAcuteIllnessDiagnosis


type Msg
    = CloseAcuteIllnessEncounter
    | SetAcuteIllnessDiagnosis AcuteIllnessDiagnosis
    | HandleUpdatedAcuteIllnessEncounter (WebData ())
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
    | SaveSendToHC PersonId (Maybe SendToHCId) (EverySet SendToHCSign)
    | HandleSavedSendToHC (WebData ())
    | SaveMedicationDistribution PersonId (Maybe MedicationDistributionId) MedicationDistributionValue
    | HandleSavedMedicationDistribution (WebData ())
    | SaveTravelHistory PersonId (Maybe TravelHistoryId) (EverySet TravelHistorySign)
    | HandleSavedTravelHistory (WebData ())
    | SaveExposure PersonId (Maybe ExposureId) (EverySet ExposureSign)
    | HandleSavedExposure (WebData ())
    | SaveIsolation PersonId (Maybe IsolationId) IsolationValue
    | HandleSavedIsolation (WebData ())
    | SaveHCContact PersonId (Maybe HCContactId) HCContactValue
    | HandleSavedHCContact (WebData ())
    | SaveCall114 PersonId (Maybe Call114Id) Call114Value
    | HandleSavedCall114 (WebData ())
    | SaveTreatmentReview PersonId (Maybe TreatmentReviewId) (EverySet TreatmentReviewSign)
    | HandleSavedTreatmentReview (WebData ())
    | SaveMuac PersonId (Maybe AcuteIllnessMuacId) MuacInCm
    | HandleSavedMuac (WebData ())
    | SaveTreatmentOngoing PersonId (Maybe TreatmentOngoingId) TreatmentOngoingValue
    | HandleSavedTreatmentOngoing (WebData ())
