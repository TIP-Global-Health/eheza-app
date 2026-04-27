module Backend.NCDEncounter.Model exposing (Model, Msg(..), NCDEncounter, emptyModel, emptyNCDEncounter)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.NCDEncounter.Types exposing (NCDDiagnosis)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias NCDEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , diagnoses : EverySet NCDDiagnosis
    , deleted : Bool
    , shard : Maybe HealthCenterId
    }


emptyNCDEncounter : IndividualEncounterParticipantId -> NominalDate -> Maybe HealthCenterId -> NCDEncounter
emptyNCDEncounter participant startDate shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , diagnoses = EverySet.empty
    , deleted = False
    , shard = shard
    }


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { updateNCDEncounter : WebData ()
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
    , saveHIVTest : WebData ()
    , saveUrineDipstickTest : WebData ()
    , saveRandomBloodSugarTest : WebData ()
    , savePregnancyTest : WebData ()
    , saveCreatinineTest : WebData ()
    , saveLiverFunctionTest : WebData ()
    , saveLipidPanelTest : WebData ()
    , saveHbA1cTest : WebData ()
    , saveLabsResults : WebData ()
    , saveHealthEducation : WebData ()
    , saveMedicationDistribution : WebData ()
    , saveReferral : WebData ()
    }


emptyModel : Model
emptyModel =
    { updateNCDEncounter = NotAsked
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
    , saveHIVTest = NotAsked
    , saveUrineDipstickTest = NotAsked
    , saveRandomBloodSugarTest = NotAsked
    , savePregnancyTest = NotAsked
    , saveCreatinineTest = NotAsked
    , saveLiverFunctionTest = NotAsked
    , saveLipidPanelTest = NotAsked
    , saveHbA1cTest = NotAsked
    , saveLabsResults = NotAsked
    , saveHealthEducation = NotAsked
    , saveMedicationDistribution = NotAsked
    , saveReferral = NotAsked
    }


type Msg
    = CloseNCDEncounter
    | SetNCDDiagnoses (EverySet NCDDiagnosis)
    | HandleUpdatedNCDEncounter (WebData ())
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
    | SaveHIVTest PersonId (Maybe NCDHIVTestId) HIVTestValue
    | HandleSavedHIVTest (WebData ())
    | SaveUrineDipstickTest PersonId (Maybe NCDUrineDipstickTestId) UrineDipstickTestValue
    | HandleSavedUrineDipstickTest (WebData ())
    | SaveRandomBloodSugarTest PersonId (Maybe NCDRandomBloodSugarTestId) (RandomBloodSugarTestValue NCDEncounterId)
    | HandleSavedRandomBloodSugarTest (WebData ())
    | SaveLabsResults PersonId (Maybe NCDLabsResultsId) LabsResultsValue
    | HandleSavedLabsResults (WebData ())
    | SavePregnancyTest PersonId (Maybe NCDPregnancyTestId) PregnancyTestValue
    | HandleSavedPregnancyTest (WebData ())
    | SaveCreatinineTest PersonId (Maybe NCDCreatinineTestId) CreatinineTestValue
    | HandleSavedCreatinineTest (WebData ())
    | SaveLiverFunctionTest PersonId (Maybe NCDLiverFunctionTestId) LiverFunctionTestValue
    | HandleSavedLiverFunctionTest (WebData ())
    | SaveLipidPanelTest PersonId (Maybe NCDLipidPanelTestId) LipidPanelTestValue
    | HandleSavedLipidPanelTest (WebData ())
    | SaveHbA1cTest PersonId (Maybe NCDHbA1cTestId) HbA1cTestValue
    | HandleSavedHbA1cTest (WebData ())
    | SaveHealthEducation PersonId (Maybe NCDHealthEducationId) NCDHealthEducationValue
    | HandleSavedHealthEducation (WebData ())
    | SaveMedicationDistribution PersonId (Maybe NCDMedicationDistributionId) NCDMedicationDistributionValue
    | HandleSavedMedicationDistribution (WebData ())
    | SaveReferral PersonId (Maybe NCDReferralId) ReferralValue
    | HandleSavedReferral (WebData ())
