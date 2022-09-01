module Backend.NCDEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias NCDEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , shard : Maybe HealthCenterId
    }


emptyNCDEncounter : IndividualEncounterParticipantId -> NominalDate -> Maybe HealthCenterId -> NCDEncounter
emptyNCDEncounter participant startDate shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , shard = shard
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closeNCDEncounter : WebData ()
    , saveDangerSigns : WebData ()
    , saveSymptomReview : WebData ()
    , saveFamilyPlanning : WebData ()
    , saveCoreExam : WebData ()
    , saveVitals : WebData ()
    , saveCoMorbidities : WebData ()
    , saveMedicationHistory : WebData ()
    , saveSocialHistory : WebData ()
    , saveFamilyHistory : WebData ()
    , saveOutsideCare : WebData ()
    }


emptyModel : Model
emptyModel =
    { closeNCDEncounter = NotAsked
    , saveDangerSigns = NotAsked
    , saveSymptomReview = NotAsked
    , saveFamilyPlanning = NotAsked
    , saveCoreExam = NotAsked
    , saveVitals = NotAsked
    , saveCoMorbidities = NotAsked
    , saveMedicationHistory = NotAsked
    , saveSocialHistory = NotAsked
    , saveFamilyHistory = NotAsked
    , saveOutsideCare = NotAsked
    }


type Msg
    = CloseNCDEncounter
    | HandleClosedNCDEncounter (WebData ())
    | SaveDangerSigns PersonId (Maybe NCDDangerSignsId) NCDDangerSignsValue
    | HandleSavedDangerSigns (WebData ())
    | SaveSymptomReview PersonId (Maybe NCDSymptomReviewId) NCDSymptomReviewValue
    | HandleSavedSymptomReview (WebData ())
    | SaveFamilyPlanning PersonId (Maybe NCDFamilyPlanningId) (EverySet FamilyPlanningSign)
    | HandleSavedFamilyPlanning (WebData ())
    | SaveCoreExam PersonId (Maybe NCDCoreExamId) CorePhysicalExamValue
    | HandleSavedCoreExam (WebData ())
    | SaveVitals PersonId (Maybe NCDVitalsId) VitalsValue
    | HandleSavedVitals (WebData ())
    | SaveCoMorbidities PersonId (Maybe NCDCoMorbiditiesId) NCDCoMorbiditiesValue
    | HandleSavedCoMorbidities (WebData ())
    | SaveMedicationHistory PersonId (Maybe NCDMedicationHistoryId) NCDMedicationHistoryValue
    | HandleSavedMedicationHistory (WebData ())
    | SaveSocialHistory PersonId (Maybe NCDSocialHistoryId) NCDSocialHistoryValue
    | HandleSavedSocialHistory (WebData ())
    | SaveFamilyHistory PersonId (Maybe NCDFamilyHistoryId) NCDFamilyHistoryValue
    | HandleSavedFamilyHistory (WebData ())
    | SaveOutsideCare PersonId (Maybe NCDOutsideCareId) (OutsideCareValue MedicalCondition)
    | HandleSavedOutsideCare (WebData ())
