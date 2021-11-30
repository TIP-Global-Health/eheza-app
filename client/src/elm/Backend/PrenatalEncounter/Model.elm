module Backend.PrenatalEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias PrenatalEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , encounterType : PrenatalEncounterType
    , shard : Maybe HealthCenterId
    }


emptyPrenatalEncounter : IndividualEncounterParticipantId -> NominalDate -> PrenatalEncounterType -> Maybe HealthCenterId -> PrenatalEncounter
emptyPrenatalEncounter participant startDate encounterType shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , encounterType = encounterType
    , shard = shard
    }


type PrenatalEncounterType
    = NurseEncounter
    | ChwFirstEncounter
    | ChwSecondEncounter
    | ChwThirdPlusEncounter
    | ChwPostpartumEncounter


type RecordPreganancyInitiator
    = InitiatorParticipantPage
    | InitiatorWarningPopup
    | InitiatorPostpartumEncounter PrenatalEncounterId


type ClinicalProgressReportInitiator
    = InitiatorEncounterPage
    | InitiatorNewEncounter PrenatalEncounterId


type PrenatalEncounterPostCreateDestination
    = DestinationEncounterPage
    | DestinationEncounterPageWithWarningPopup
    | DestinationClinicalProgressReportPage


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { closePrenatalEncounter : WebData ()
    , saveBreastExam : WebData ()
    , saveCorePhysicalExam : WebData ()
    , saveDangerSigns : WebData ()
    , saveLastMenstrualPeriod : WebData ()
    , saveMedicalHistory : WebData ()
    , saveMedication : WebData ()
    , saveObstetricalExam : WebData ()
    , saveObstetricHistory : WebData ()
    , saveObstetricHistoryStep2 : WebData ()
    , saveFamilyPlanning : WebData ()
    , saveNutrition : WebData ()
    , saveMalariaPrevention : WebData ()
    , saveSocialHistory : WebData ()
    , saveVitals : WebData ()
    , savePrenatalPhoto : WebData ()
    , saveBirthPlan : WebData ()
    , savePregnancyTest : WebData ()
    , saveHealthEducation : WebData ()
    , saveFollowUp : WebData ()
    , saveSendToHC : WebData ()
    , saveAppointmentConfirmation : WebData ()
    }


emptyModel : Model
emptyModel =
    { closePrenatalEncounter = NotAsked
    , saveBreastExam = NotAsked
    , saveCorePhysicalExam = NotAsked
    , saveDangerSigns = NotAsked
    , saveLastMenstrualPeriod = NotAsked
    , saveMedicalHistory = NotAsked
    , saveMedication = NotAsked
    , saveObstetricalExam = NotAsked
    , saveObstetricHistory = NotAsked
    , saveObstetricHistoryStep2 = NotAsked
    , saveFamilyPlanning = NotAsked
    , saveNutrition = NotAsked
    , saveMalariaPrevention = NotAsked
    , saveSocialHistory = NotAsked
    , saveVitals = NotAsked
    , savePrenatalPhoto = NotAsked
    , saveBirthPlan = NotAsked
    , savePregnancyTest = NotAsked
    , saveHealthEducation = NotAsked
    , saveFollowUp = NotAsked
    , saveSendToHC = NotAsked
    , saveAppointmentConfirmation = NotAsked
    }


type Msg
    = ClosePrenatalEncounter
    | HandleClosedPrenatalEncounter (WebData ())
    | SaveBreastExam PersonId (Maybe BreastExamId) BreastExamValue
    | HandleSavedBreastExam (WebData ())
    | SaveCorePhysicalExam PersonId (Maybe CorePhysicalExamId) CorePhysicalExamValue
    | HandleSavedCorePhysicalExam (WebData ())
    | SaveDangerSigns PersonId (Maybe DangerSignsId) DangerSignsValue
    | HandleSavedDangerSigns (WebData ())
    | SaveLastMenstrualPeriod PersonId (Maybe LastMenstrualPeriodId) LastMenstrualPeriodValue
    | HandleSavedLastMenstrualPeriod (WebData ())
    | SaveMedicalHistory PersonId (Maybe MedicalHistoryId) (EverySet MedicalHistorySign)
    | HandleSavedMedicalHistory (WebData ())
    | SaveMedication PersonId (Maybe MedicationId) (EverySet MedicationSign)
    | HandleSavedMedication (WebData ())
    | SaveObstetricalExam PersonId (Maybe ObstetricalExamId) ObstetricalExamValue
    | HandleSavedObstetricalExam (WebData ())
    | SaveObstetricHistory PersonId (Maybe ObstetricHistoryId) ObstetricHistoryValue
    | HandleSavedObstetricHistory (WebData ())
    | SaveObstetricHistoryStep2 PersonId (Maybe ObstetricHistoryStep2Id) ObstetricHistoryStep2Value
    | HandleSavedObstetricHistoryStep2 (WebData ())
    | SaveFamilyPlanning PersonId (Maybe PrenatalFamilyPlanningId) (EverySet FamilyPlanningSign)
    | HandleSavedFamilyPlanning (WebData ())
    | SaveNutrition PersonId (Maybe PrenatalNutritionId) PrenatalNutritionValue
    | HandleSavedNutrition (WebData ())
    | SaveMalariaPrevention PersonId (Maybe MalariaPreventionId) (EverySet MalariaPreventionSign)
    | HandleSavedMalariaPrevention (WebData ())
    | SaveSocialHistory PersonId (Maybe SocialHistoryId) SocialHistoryValue
    | HandleSavedSocialHistory (WebData ())
    | SaveVitals PersonId (Maybe VitalsId) VitalsValue
    | HandleSavedVitals (WebData ())
    | SavePrenatalPhoto PersonId (Maybe PrenatalPhotoId) PhotoUrl
    | HandleSavedPrenatalPhoto (WebData ())
    | SaveBirthPlan PersonId (Maybe BirthPlanId) BirthPlanValue
    | HandleSavedBirthPlan (WebData ())
    | SavePregnancyTest PersonId (Maybe PregnancyTestId) PregnancyTestResult
    | HandleSavedPregnancyTest (WebData ())
    | SaveHealthEducation PersonId (Maybe PrenatalHealthEducationId) (EverySet PrenatalHealthEducationSign)
    | HandleSavedHealthEducation (WebData ())
    | SaveFollowUp PersonId (Maybe PrenatalFollowUpId) PrenatalFollowUpValue
    | HandleSavedFollowup (WebData ())
    | SaveSendToHC PersonId (Maybe PrenatalSendToHcId) SendToHCValue
    | HandleSavedSendToHC (WebData ())
    | SaveAppointmentConfirmation PersonId (Maybe PrenatalAppointmentConfirmationId) PrenatalAppointmentConfirmationValue
    | HandleSavedAppointmentConfirmation (WebData ())
