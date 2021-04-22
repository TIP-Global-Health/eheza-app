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
    | ChwThirdEncounter
    | ChwPostpartumEncounter


type RecordPreganancyInitiator
    = InitiatorParticipantPage
    | InitiatorWarningPopup


type ClinicalProgressReportInitiator
    = InitiatorEncounterPage
    | InitiatorNewEncounter PrenatalEncounterId


type PrenatalEncounterPostCreateDestination
    = DestinationEncounterPage
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
    , saveResource : WebData ()
    , saveSocialHistory : WebData ()
    , saveVitals : WebData ()
    , savePrenatalPhoto : WebData ()
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
    , saveResource = NotAsked
    , saveSocialHistory = NotAsked
    , saveVitals = NotAsked
    , savePrenatalPhoto = NotAsked
    }


type Msg
    = ClosePrenatalEncounter
    | SaveBreastExam PersonId (Maybe BreastExamId) BreastExamValue
    | SaveCorePhysicalExam PersonId (Maybe CorePhysicalExamId) CorePhysicalExamValue
    | SaveDangerSigns PersonId (Maybe DangerSignsId) (EverySet DangerSign)
    | SaveLastMenstrualPeriod PersonId (Maybe LastMenstrualPeriodId) LastMenstrualPeriodValue
    | SaveMedicalHistory PersonId (Maybe MedicalHistoryId) (EverySet MedicalHistorySign)
    | SaveMedication PersonId (Maybe MedicationId) (EverySet MedicationSign)
    | SaveObstetricalExam PersonId (Maybe ObstetricalExamId) ObstetricalExamValue
    | SaveObstetricHistory PersonId (Maybe ObstetricHistoryId) ObstetricHistoryValue
    | SaveObstetricHistoryStep2 PersonId (Maybe ObstetricHistoryStep2Id) ObstetricHistoryStep2Value
    | SaveFamilyPlanning PersonId (Maybe PrenatalFamilyPlanningId) (EverySet FamilyPlanningSign)
    | SaveNutrition PersonId (Maybe PrenatalNutritionId) PrenatalNutritionValue
    | SaveResource PersonId (Maybe ResourceId) (EverySet ResourceSign)
    | SaveSocialHistory PersonId (Maybe SocialHistoryId) SocialHistoryValue
    | SaveVitals PersonId (Maybe VitalsId) VitalsValue
    | SavePrenatalPhoto PersonId (Maybe PrenatalPhotoId) PhotoUrl
    | HandleClosedPrenatalEncounter (WebData ())
    | HandleSavedBreastExam (WebData ())
    | HandleSavedCorePhysicalExam (WebData ())
    | HandleSavedDangerSigns (WebData ())
    | HandleSavedLastMenstrualPeriod (WebData ())
    | HandleSavedMedicalHistory (WebData ())
    | HandleSavedMedication (WebData ())
    | HandleSavedObstetricalExam (WebData ())
    | HandleSavedObstetricHistory (WebData ())
    | HandleSavedObstetricHistoryStep2 (WebData ())
    | HandleSavedFamilyPlanning (WebData ())
    | HandleSavedNutrition (WebData ())
    | HandleSavedResource (WebData ())
    | HandleSavedSocialHistory (WebData ())
    | HandleSavedVitals (WebData ())
    | HandleSavedPrenatalPhoto (WebData ())
