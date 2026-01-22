module Backend.PrenatalEncounter.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.PrenatalEncounter.Types exposing (PrenatalDiagnosis)
import EverySet exposing (EverySet)
import Gizra.NominalDate exposing (NominalDate)
import RemoteData exposing (RemoteData(..), WebData)


type alias PrenatalEncounter =
    { participant : IndividualEncounterParticipantId
    , startDate : NominalDate
    , endDate : Maybe NominalDate
    , encounterType : PrenatalEncounterType
    , diagnoses : EverySet PrenatalDiagnosis
    , pastDiagnoses : EverySet PrenatalDiagnosis
    , indicators : EverySet PrenatalIndicator
    , nextVisitDate : Maybe NominalDate
    , deleted : Bool
    , shard : Maybe HealthCenterId
    }


emptyPrenatalEncounter : IndividualEncounterParticipantId -> NominalDate -> PrenatalEncounterType -> Maybe HealthCenterId -> PrenatalEncounter
emptyPrenatalEncounter participant startDate encounterType shard =
    { participant = participant
    , startDate = startDate
    , endDate = Nothing
    , encounterType = encounterType
    , diagnoses = EverySet.empty
    , pastDiagnoses = EverySet.empty
    , indicators = EverySet.empty
    , nextVisitDate = Nothing
    , deleted = False
    , shard = shard
    }


type PrenatalEncounterType
    = NurseEncounter
    | NursePostpartumEncounter
    | ChwFirstEncounter
    | ChwSecondEncounter
    | ChwThirdPlusEncounter
    | ChwPostpartumEncounter


type RecordPreganancyInitiator
    = InitiatorParticipantPage
    | InitiatorWarningPopup
    | InitiatorPostpartumEncounter PrenatalEncounterId


type PrenatalProgressReportInitiator
    = InitiatorEncounterPage PrenatalEncounterId
    | InitiatorRecurrentEncounterPage PrenatalEncounterId
    | InitiatorNewEncounter PrenatalEncounterId
    | InitiatorPatientRecord PersonId
    | InitiatorCaseManagement PrenatalEncounterId


type PrenatalEncounterPostCreateDestination
    = DestinationEncounterPage
    | DestinationEncounterPageWithWarningPopup
    | DestinationClinicalProgressReportPage


type PrenatalIndicator
    = IndicatorHistoryLabsCompleted
    | IndicatorAdequateGWG
    | IndicatorInadequateGWG
    | NoPrenatalIndicators


{-| This is a subdivision of ModelIndexedDb that tracks requests in-progress
to peform the updates indicated by the `Msg` type below.
-}
type alias Model =
    { updatePrenatalEncounter : WebData ()
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
    , saveHIVTest : WebData ()
    , saveSyphilisTest : WebData ()
    , saveHepatitisBTest : WebData ()
    , saveMalariaTest : WebData ()
    , saveBloodGpRsTest : WebData ()
    , saveUrineDipstickTest : WebData ()
    , saveHemoglobinTest : WebData ()
    , saveRandomBloodSugarTest : WebData ()
    , saveLabsResults : WebData ()
    , saveMedicationDistribution : WebData ()
    , saveSymptomReview : WebData ()
    , saveOutsideCare : WebData ()
    , saveHIVPCRTest : WebData ()
    , savePartnerHIVTest : WebData ()
    , saveMentalHealth : WebData ()
    , saveTetanusImmunisation : WebData ()
    , saveBreastfeeding : WebData ()
    , saveGUExam : WebData ()
    , saveSpecialityCare : WebData ()
    , saveAspirin : WebData ()
    , saveCalcium : WebData ()
    , saveFefol : WebData ()
    , saveFolate : WebData ()
    , saveIron : WebData ()
    , saveMMS : WebData ()
    , saveMebendazole : WebData ()
    }


emptyModel : Model
emptyModel =
    { updatePrenatalEncounter = NotAsked
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
    , saveHIVTest = NotAsked
    , saveSyphilisTest = NotAsked
    , saveHepatitisBTest = NotAsked
    , saveMalariaTest = NotAsked
    , saveBloodGpRsTest = NotAsked
    , saveUrineDipstickTest = NotAsked
    , saveHemoglobinTest = NotAsked
    , saveRandomBloodSugarTest = NotAsked
    , saveLabsResults = NotAsked
    , saveMedicationDistribution = NotAsked
    , saveSymptomReview = NotAsked
    , saveOutsideCare = NotAsked
    , saveHIVPCRTest = NotAsked
    , savePartnerHIVTest = NotAsked
    , saveMentalHealth = NotAsked
    , saveTetanusImmunisation = NotAsked
    , saveBreastfeeding = NotAsked
    , saveGUExam = NotAsked
    , saveSpecialityCare = NotAsked
    , saveAspirin = NotAsked
    , saveCalcium = NotAsked
    , saveFefol = NotAsked
    , saveFolate = NotAsked
    , saveIron = NotAsked
    , saveMMS = NotAsked
    , saveMebendazole = NotAsked
    }


type Msg
    = CloseEncounter
    | SetPrenatalDiagnoses (EverySet PrenatalDiagnosis)
    | SetPastPrenatalDiagnoses (EverySet PrenatalDiagnosis)
    | SetLabsHistoryCompleted
    | SetNextVisitDate NominalDate
    | SetGWGIndicator Bool
    | HandleUpdatedPrenatalEncounter (WebData ())
    | SaveBreastExam PersonId (Maybe BreastExamId) BreastExamValue
    | HandleSavedBreastExam (WebData ())
    | SaveCorePhysicalExam PersonId (Maybe CorePhysicalExamId) CorePhysicalExamValue
    | HandleSavedCorePhysicalExam (WebData ())
    | SaveDangerSigns PersonId (Maybe DangerSignsId) DangerSignsValue
    | HandleSavedDangerSigns (WebData ())
    | SaveLastMenstrualPeriod PersonId (Maybe LastMenstrualPeriodId) LastMenstrualPeriodValue
    | HandleSavedLastMenstrualPeriod (WebData ())
    | SaveMedicalHistory PersonId (Maybe MedicalHistoryId) MedicalHistoryValue
    | HandleSavedMedicalHistory (WebData ())
    | SaveMedication PersonId (Maybe MedicationId) MedicationValue
    | HandleSavedMedication (WebData ())
    | SaveAspirin PersonId (Maybe PrenatalAspirinId) AdministrationNote
    | HandleSavedAspirin (WebData ())
    | SaveCalcium PersonId (Maybe PrenatalCalciumId) AdministrationNote
    | HandleSavedCalcium (WebData ())
    | SaveFefol PersonId (Maybe PrenatalFefolId) AdministrationNote
    | HandleSavedFefol (WebData ())
    | SaveFolate PersonId (Maybe PrenatalFolateId) AdministrationNote
    | HandleSavedFolate (WebData ())
    | SaveIron PersonId (Maybe PrenatalIronId) AdministrationNote
    | HandleSavedIron (WebData ())
    | SaveMMS PersonId (Maybe PrenatalMMSId) AdministrationNote
    | HandleSavedMMS (WebData ())
    | SaveMebendazole PersonId (Maybe PrenatalMebendazoleId) AdministrationNote
    | HandleSavedMebendazole (WebData ())
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
    | SaveMalariaPrevention PersonId (Maybe MalariaPreventionId) MalariaPreventionValue
    | HandleSavedMalariaPrevention (WebData ())
    | SaveSocialHistory PersonId (Maybe SocialHistoryId) SocialHistoryValue
    | HandleSavedSocialHistory (WebData ())
    | SaveVitals PersonId (Maybe VitalsId) VitalsValue
    | HandleSavedVitals (WebData ())
    | SavePrenatalPhoto PersonId (Maybe PrenatalPhotoId) ImageUrl
    | HandleSavedPrenatalPhoto (WebData ())
    | SaveBirthPlan PersonId (Maybe BirthPlanId) BirthPlanValue
    | HandleSavedBirthPlan (WebData ())
    | SavePregnancyTest PersonId (Maybe PregnancyTestId) PregnancyTestResult
    | HandleSavedPregnancyTest (WebData ())
    | SaveHealthEducation PersonId (Maybe PrenatalHealthEducationId) PrenatalHealthEducationValue
    | HandleSavedHealthEducation (WebData ())
    | SaveFollowUp PersonId (Maybe PrenatalFollowUpId) PrenatalFollowUpValue
    | HandleSavedFollowUp (WebData ())
    | SaveSendToHC PersonId (Maybe PrenatalSendToHCId) PrenatalReferralValue
    | HandleSavedSendToHC (WebData ())
    | SaveAppointmentConfirmation PersonId (Maybe PrenatalAppointmentConfirmationId) PrenatalAppointmentConfirmationValue
    | HandleSavedAppointmentConfirmation (WebData ())
    | SaveHIVTest PersonId (Maybe PrenatalHIVTestId) HIVTestValue
    | HandleSavedHIVTest (WebData ())
    | SaveSyphilisTest PersonId (Maybe PrenatalSyphilisTestId) (SyphilisTestValue PrenatalEncounterId)
    | HandleSavedSyphilisTest (WebData ())
    | SaveHepatitisBTest PersonId (Maybe PrenatalHepatitisBTestId) (HepatitisBTestValue PrenatalEncounterId)
    | HandleSavedHepatitisBTest (WebData ())
    | SaveMalariaTest PersonId (Maybe PrenatalMalariaTestId) MalariaTestValue
    | HandleSavedMalariaTest (WebData ())
    | SaveBloodGpRsTest PersonId (Maybe PrenatalBloodGpRsTestId) (BloodGpRsTestValue PrenatalEncounterId)
    | HandleSavedBloodGpRsTest (WebData ())
    | SaveUrineDipstickTest PersonId (Maybe PrenatalUrineDipstickTestId) UrineDipstickTestValue
    | HandleSavedUrineDipstickTest (WebData ())
    | SaveHemoglobinTest PersonId (Maybe PrenatalHemoglobinTestId) HemoglobinTestValue
    | HandleSavedHemoglobinTest (WebData ())
    | SaveRandomBloodSugarTest PersonId (Maybe PrenatalRandomBloodSugarTestId) (RandomBloodSugarTestValue PrenatalEncounterId)
    | HandleSavedRandomBloodSugarTest (WebData ())
    | SaveLabsResults PersonId (Maybe PrenatalLabsResultsId) LabsResultsValue
    | HandleSavedLabsResults (WebData ())
    | SaveMedicationDistribution PersonId (Maybe PrenatalMedicationDistributionId) PrenatalMedicationDistributionValue
    | HandleSavedMedicationDistribution (WebData ())
    | SaveSymptomReview PersonId (Maybe PrenatalSymptomReviewId) PrenatalSymptomReviewValue
    | HandleSavedSymptomReview (WebData ())
    | SaveOutsideCare PersonId (Maybe PrenatalOutsideCareId) (OutsideCareValue PrenatalDiagnosis)
    | HandleSavedOutsideCare (WebData ())
    | SaveHIVPCRTest PersonId (Maybe PrenatalHIVPCRTestId) HIVPCRTestValue
    | HandleSavedHIVPCRTest (WebData ())
    | SavePartnerHIVTest PersonId (Maybe PrenatalPartnerHIVTestId) PartnerHIVTestValue
    | HandleSavedPartnerHIVTest (WebData ())
    | SaveMentalHealth PersonId (Maybe PrenatalMentalHealthId) PrenatalMentalHealthValue
    | HandleSavedMentalHealth (WebData ())
    | SaveTetanusImmunisation PersonId (Maybe PrenatalTetanusImmunisationId) VaccinationValue
    | HandleSavedTetanusImmunisation (WebData ())
    | SaveBreastfeeding PersonId (Maybe PrenatalBreastfeedingId) BreastfeedingValue
    | HandleSavedBreastfeeding (WebData ())
    | SaveGUExam PersonId (Maybe PrenatalGUExamId) GUExamValue
    | HandleSavedGUExam (WebData ())
    | SaveSpecialityCare PersonId (Maybe PrenatalSpecialityCareId) SpecialityCareValue
    | HandleSavedSpecialityCare (WebData ())
    | NoOp
