module Backend.HealthyStartEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias HealthyStartEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , encounterType : HealthyStartEncounterType
    , diagnoses : EverySet PrenatalDiagnosis
    , pastDiagnoses : EverySet PrenatalDiagnosis
    , indicators : EverySet HealthyStartIndicator
    , nextVisitDate : Maybe NominalDate
    , shard : Maybe HealthCenterId
    }


emptyHealthyStartEncounter : IndividualEncounterParticipantId -> NominalDate -> HealthyStartEncounterType -> Maybe HealthCenterId -> HealthyStartEncounter
emptyHealthyStartEncounter participant startDate encounterType shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , encounterType = encounterType
    , diagnoses = EverySet.empty
    , pastDiagnoses = EverySet.empty
    , indicators = EverySet.empty
    , nextVisitDate = Nothing
    , shard = shard
    }


type HealthyStartEncounterType
    = NurseEncounter
    | NursePostpartumEncounter


type RecordPreganancyInitiator
    = InitiatorParticipantPage
    | InitiatorWarningPopup
    | InitiatorPostpartumEncounter HealthyStartEncounterId


type HealthyStartProgressReportInitiator
    = InitiatorEncounterPage HealthyStartEncounterId
    | InitiatorRecurrentEncounterPage HealthyStartEncounterId
    | InitiatorNewEncounter HealthyStartEncounterId
    | InitiatorPatientRecord PersonId
    | InitiatorCaseManagement HealthyStartEncounterId


type HealthyStartIndicator
    = IndicatorHistoryLabsCompleted
    | NoHealthyStartIndicators


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { updateHealthyStartEncounter : WebData ()

    -- , saveBreastExam : WebData ()
    -- , saveCorePhysicalExam : WebData ()
    -- , saveDangerSigns : WebData ()
    -- , saveLastMenstrualPeriod : WebData ()
    -- , saveMedicalHistory : WebData ()
    -- , saveMedication : WebData ()
    -- , saveObstetricalExam : WebData ()
    -- , saveObstetricHistory : WebData ()
    -- , saveObstetricHistoryStep2 : WebData ()
    -- , saveFamilyPlanning : WebData ()
    -- , saveNutrition : WebData ()
    -- , saveMalariaPrevention : WebData ()
    -- , saveSocialHistory : WebData ()
    -- , saveVitals : WebData ()
    -- , saveHealthyStartPhoto : WebData ()
    -- , saveBirthPlan : WebData ()
    -- , savePregnancyTest : WebData ()
    -- , saveHealthEducation : WebData ()
    -- , saveFollowUp : WebData ()
    -- , saveSendToHC : WebData ()
    -- , saveAppointmentConfirmation : WebData ()
    -- , saveHIVTest : WebData ()
    -- , saveSyphilisTest : WebData ()
    -- , saveHepatitisBTest : WebData ()
    -- , saveMalariaTest : WebData ()
    -- , saveBloodGpRsTest : WebData ()
    -- , saveUrineDipstickTest : WebData ()
    -- , saveHemoglobinTest : WebData ()
    -- , saveRandomBloodSugarTest : WebData ()
    -- , saveLabsResults : WebData ()
    -- , saveMedicationDistribution : WebData ()
    -- , saveSymptomReview : WebData ()
    -- , saveOutsideCare : WebData ()
    -- , saveHIVPCRTest : WebData ()
    -- , savePartnerHIVTest : WebData ()
    -- , saveMentalHealth : WebData ()
    -- , saveTetanusImmunisation : WebData ()
    -- , saveBreastfeeding : WebData ()
    -- , saveGUExam : WebData ()
    -- , saveSpecialityCare : WebData ()
    -- , saveAspirin : WebData ()
    -- , saveCalcium : WebData ()
    -- , saveFefol : WebData ()
    -- , saveFolate : WebData ()
    -- , saveIron : WebData ()
    -- , saveMMS : WebData ()
    -- , saveMebendazole : WebData ()
    }


emptyModel : Model
emptyModel =
    { updateHealthyStartEncounter = NotAsked

    -- , saveBreastExam = NotAsked
    -- , saveCorePhysicalExam = NotAsked
    -- , saveDangerSigns = NotAsked
    -- , saveLastMenstrualPeriod = NotAsked
    -- , saveMedicalHistory = NotAsked
    -- , saveMedication = NotAsked
    -- , saveObstetricalExam = NotAsked
    -- , saveObstetricHistory = NotAsked
    -- , saveObstetricHistoryStep2 = NotAsked
    -- , saveFamilyPlanning = NotAsked
    -- , saveNutrition = NotAsked
    -- , saveMalariaPrevention = NotAsked
    -- , saveSocialHistory = NotAsked
    -- , saveVitals = NotAsked
    -- , saveHealthyStartPhoto = NotAsked
    -- , saveBirthPlan = NotAsked
    -- , savePregnancyTest = NotAsked
    -- , saveHealthEducation = NotAsked
    -- , saveFollowUp = NotAsked
    -- , saveSendToHC = NotAsked
    -- , saveAppointmentConfirmation = NotAsked
    -- , saveHIVTest = NotAsked
    -- , saveSyphilisTest = NotAsked
    -- , saveHepatitisBTest = NotAsked
    -- , saveMalariaTest = NotAsked
    -- , saveBloodGpRsTest = NotAsked
    -- , saveUrineDipstickTest = NotAsked
    -- , saveHemoglobinTest = NotAsked
    -- , saveRandomBloodSugarTest = NotAsked
    -- , saveLabsResults = NotAsked
    -- , saveMedicationDistribution = NotAsked
    -- , saveSymptomReview = NotAsked
    -- , saveOutsideCare = NotAsked
    -- , saveHIVPCRTest = NotAsked
    -- , savePartnerHIVTest = NotAsked
    -- , saveMentalHealth = NotAsked
    -- , saveTetanusImmunisation = NotAsked
    -- , saveBreastfeeding = NotAsked
    -- , saveGUExam = NotAsked
    -- , saveSpecialityCare = NotAsked
    -- , saveAspirin = NotAsked
    -- , saveCalcium = NotAsked
    -- , saveFefol = NotAsked
    -- , saveFolate = NotAsked
    -- , saveIron = NotAsked
    -- , saveMMS = NotAsked
    -- , saveMebendazole = NotAsked
    }


type Msg
    = CloseEncounter
    | SetHealthyStartDiagnoses (EverySet PrenatalDiagnosis)
    | SetPastHealthyStartDiagnoses (EverySet PrenatalDiagnosis)
    | SetLabsHistoryCompleted
    | SetNextVisitDate NominalDate
    | HandleUpdatedHealthyStartEncounter (WebData ())
      -- | SaveBreastExam PersonId (Maybe BreastExamId) BreastExamValue
      -- | HandleSavedBreastExam (WebData ())
      -- | SaveCorePhysicalExam PersonId (Maybe CorePhysicalExamId) CorePhysicalExamValue
      -- | HandleSavedCorePhysicalExam (WebData ())
      -- | SaveDangerSigns PersonId (Maybe DangerSignsId) DangerSignsValue
      -- | HandleSavedDangerSigns (WebData ())
      -- | SaveLastMenstrualPeriod PersonId (Maybe LastMenstrualPeriodId) LastMenstrualPeriodValue
      -- | HandleSavedLastMenstrualPeriod (WebData ())
      -- | SaveMedicalHistory PersonId (Maybe MedicalHistoryId) MedicalHistoryValue
      -- | HandleSavedMedicalHistory (WebData ())
      -- | SaveMedication PersonId (Maybe MedicationId) MedicationValue
      -- | HandleSavedMedication (WebData ())
      -- | SaveAspirin PersonId (Maybe HealthyStartAspirinId) AdministrationNote
      -- | HandleSavedAspirin (WebData ())
      -- | SaveCalcium PersonId (Maybe HealthyStartCalciumId) AdministrationNote
      -- | HandleSavedCalcium (WebData ())
      -- | SaveFefol PersonId (Maybe HealthyStartFefolId) AdministrationNote
      -- | HandleSavedFefol (WebData ())
      -- | SaveFolate PersonId (Maybe HealthyStartFolateId) AdministrationNote
      -- | HandleSavedFolate (WebData ())
      -- | SaveIron PersonId (Maybe HealthyStartIronId) AdministrationNote
      -- | HandleSavedIron (WebData ())
      -- | SaveMMS PersonId (Maybe HealthyStartMMSId) AdministrationNote
      -- | HandleSavedMMS (WebData ())
      -- | SaveMebendazole PersonId (Maybe HealthyStartMebendazoleId) AdministrationNote
      -- | HandleSavedMebendazole (WebData ())
      -- | SaveObstetricalExam PersonId (Maybe ObstetricalExamId) ObstetricalExamValue
      -- | HandleSavedObstetricalExam (WebData ())
      -- | SaveObstetricHistory PersonId (Maybe ObstetricHistoryId) ObstetricHistoryValue
      -- | HandleSavedObstetricHistory (WebData ())
      -- | SaveObstetricHistoryStep2 PersonId (Maybe ObstetricHistoryStep2Id) ObstetricHistoryStep2Value
      -- | HandleSavedObstetricHistoryStep2 (WebData ())
      -- | SaveFamilyPlanning PersonId (Maybe HealthyStartFamilyPlanningId) (EverySet FamilyPlanningSign)
      -- | HandleSavedFamilyPlanning (WebData ())
      -- | SaveNutrition PersonId (Maybe HealthyStartNutritionId) HealthyStartNutritionValue
      -- | HandleSavedNutrition (WebData ())
      -- | SaveMalariaPrevention PersonId (Maybe MalariaPreventionId) MalariaPreventionValue
      -- | HandleSavedMalariaPrevention (WebData ())
      -- | SaveSocialHistory PersonId (Maybe SocialHistoryId) SocialHistoryValue
      -- | HandleSavedSocialHistory (WebData ())
      -- | SaveVitals PersonId (Maybe VitalsId) VitalsValue
      -- | HandleSavedVitals (WebData ())
      -- | SaveHealthyStartPhoto PersonId (Maybe HealthyStartPhotoId) ImageUrl
      -- | HandleSavedHealthyStartPhoto (WebData ())
      -- | SaveBirthPlan PersonId (Maybe BirthPlanId) BirthPlanValue
      -- | HandleSavedBirthPlan (WebData ())
      -- | SavePregnancyTest PersonId (Maybe PregnancyTestId) PregnancyTestResult
      -- | HandleSavedPregnancyTest (WebData ())
      -- | SaveHealthEducation PersonId (Maybe HealthyStartHealthEducationId) HealthyStartHealthEducationValue
      -- | HandleSavedHealthEducation (WebData ())
      -- | SaveFollowUp PersonId (Maybe HealthyStartFollowUpId) HealthyStartFollowUpValue
      -- | HandleSavedFollowUp (WebData ())
      -- | SaveSendToHC PersonId (Maybe HealthyStartSendToHCId) HealthyStartReferralValue
      -- | HandleSavedSendToHC (WebData ())
      -- | SaveAppointmentConfirmation PersonId (Maybe HealthyStartAppointmentConfirmationId) HealthyStartAppointmentConfirmationValue
      -- | HandleSavedAppointmentConfirmation (WebData ())
      -- | SaveHIVTest PersonId (Maybe HealthyStartHIVTestId) HIVTestValue
      -- | HandleSavedHIVTest (WebData ())
      -- | SaveSyphilisTest PersonId (Maybe HealthyStartSyphilisTestId) (SyphilisTestValue HealthyStartEncounterId)
      -- | HandleSavedSyphilisTest (WebData ())
      -- | SaveHepatitisBTest PersonId (Maybe HealthyStartHepatitisBTestId) (HepatitisBTestValue HealthyStartEncounterId)
      -- | HandleSavedHepatitisBTest (WebData ())
      -- | SaveMalariaTest PersonId (Maybe HealthyStartMalariaTestId) MalariaTestValue
      -- | HandleSavedMalariaTest (WebData ())
      -- | SaveBloodGpRsTest PersonId (Maybe HealthyStartBloodGpRsTestId) (BloodGpRsTestValue HealthyStartEncounterId)
      -- | HandleSavedBloodGpRsTest (WebData ())
      -- | SaveUrineDipstickTest PersonId (Maybe HealthyStartUrineDipstickTestId) UrineDipstickTestValue
      -- | HandleSavedUrineDipstickTest (WebData ())
      -- | SaveHemoglobinTest PersonId (Maybe HealthyStartHemoglobinTestId) HemoglobinTestValue
      -- | HandleSavedHemoglobinTest (WebData ())
      -- | SaveRandomBloodSugarTest PersonId (Maybe HealthyStartRandomBloodSugarTestId) (RandomBloodSugarTestValue HealthyStartEncounterId)
      -- | HandleSavedRandomBloodSugarTest (WebData ())
      -- | SaveLabsResults PersonId (Maybe HealthyStartLabsResultsId) LabsResultsValue
      -- | HandleSavedLabsResults (WebData ())
      -- | SaveMedicationDistribution PersonId (Maybe HealthyStartMedicationDistributionId) HealthyStartMedicationDistributionValue
      -- | HandleSavedMedicationDistribution (WebData ())
      -- | SaveSymptomReview PersonId (Maybe HealthyStartSymptomReviewId) HealthyStartSymptomReviewValue
      -- | HandleSavedSymptomReview (WebData ())
      -- | SaveOutsideCare PersonId (Maybe HealthyStartOutsideCareId) (OutsideCareValue PrenatalDiagnosis)
      -- | HandleSavedOutsideCare (WebData ())
      -- | SaveHIVPCRTest PersonId (Maybe HealthyStartHIVPCRTestId) HIVPCRTestValue
      -- | HandleSavedHIVPCRTest (WebData ())
      -- | SavePartnerHIVTest PersonId (Maybe HealthyStartPartnerHIVTestId) PartnerHIVTestValue
      -- | HandleSavedPartnerHIVTest (WebData ())
      -- | SaveMentalHealth PersonId (Maybe HealthyStartMentalHealthId) HealthyStartMentalHealthValue
      -- | HandleSavedMentalHealth (WebData ())
      -- | SaveTetanusImmunisation PersonId (Maybe HealthyStartTetanusImmunisationId) VaccinationValue
      -- | HandleSavedTetanusImmunisation (WebData ())
      -- | SaveBreastfeeding PersonId (Maybe HealthyStartBreastfeedingId) BreastfeedingValue
      -- | HandleSavedBreastfeeding (WebData ())
      -- | SaveGUExam PersonId (Maybe HealthyStartGUExamId) GUExamValue
      -- | HandleSavedGUExam (WebData ())
      -- | SaveSpecialityCare PersonId (Maybe HealthyStartSpecialityCareId) SpecialityCareValue
      -- | HandleSavedSpecialityCare (WebData ())
    | NoOp
