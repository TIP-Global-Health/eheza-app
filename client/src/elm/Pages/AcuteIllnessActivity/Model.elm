module Pages.AcuteIllnessActivity.Model exposing (AcuteFindingsForm, ExposureData, ExposureForm, ExposureTask(..), HCContactForm, IsolationForm, LaboratoryData, LaboratoryTask(..), MalariaTestingForm, MedicationDistributionForm, Model, Msg(..), NextStepsData, NextStepsTask(..), PhysicalExamData, PhysicalExamTask(..), PriorTreatmentData, PriorTreatmentTask(..), SendToHCForm, SymptomsData, SymptomsGIForm, SymptomsGeneralForm, SymptomsRespiratoryForm, SymptomsTask(..), TravelHistoryForm, TreatmentReviewForm, VitalsForm, emptyExposureData, emptyLaboratoryData, emptyModel, emptyNextStepsData, emptyPhysicalExamData, emptyPriorTreatmentData, emptySymptomsData, emptyTreatmentReviewForm)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Model exposing (AcuteIllnessDiagnosis)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import EverySet exposing (EverySet)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
    | SetAlertsDialogState Bool
    | SetWarningPopupState (Maybe AcuteIllnessDiagnosis)
    | SetPertinentSymptomsPopupState Bool
      -- SYMPTOMS Msgs
    | SetActiveSymptomsTask SymptomsTask
    | ToggleSymptomsGeneralSign SymptomsGeneralSign
    | ToggleSymptomsGISign SymptomsGISign
    | ToggleSymptomsRespiratorySign SymptomsRespiratorySign
    | SetSymptomsGeneralSignValue SymptomsGeneralSign String
    | SetSymptomsGISignValue SymptomsGISign String
    | SetSymptomsRespiratorySignValue SymptomsRespiratorySign String
    | SetSymptomsGIIntractableVomiting Bool
    | SaveSymptomsGeneral PersonId (Maybe ( SymptomsGeneralId, SymptomsGeneral )) (Maybe SymptomsTask)
    | SaveSymptomsRespiratory PersonId (Maybe ( SymptomsRespiratoryId, SymptomsRespiratory )) (Maybe SymptomsTask)
    | SaveSymptomsGI PersonId (Maybe ( SymptomsGIId, SymptomsGI )) (Maybe SymptomsTask)
      -- PHYSICAL EXAM Msgs
    | SetActivePhysicalExamTask PhysicalExamTask
    | SetVitalsResporatoryRate String
    | SetVitalsBodyTemperature String
    | SetAcuteFindingsGeneralSign AcuteFindingsGeneralSign
    | SetAcuteFindingsRespiratorySign AcuteFindingsRespiratorySign
    | SaveVitals PersonId (Maybe ( AcuteIllnessVitalsId, AcuteIllnessVitals )) (Maybe PhysicalExamTask)
    | SaveAcuteFindings PersonId (Maybe ( AcuteFindingsId, AcuteFindings )) (Maybe PhysicalExamTask)
      -- LABORATORY Msgs
    | SetActiveLaboratoryTask LaboratoryTask
    | SetRapidTestResult String
    | SetIsPregnant Bool
    | SaveMalariaTesting PersonId (Maybe ( MalariaTestingId, MalariaTesting ))
      -- EXPOSURE Msgs
    | SetActiveExposureTask ExposureTask
    | SetCovid19Country Bool
    | SaveTravelHistory PersonId (Maybe ( TravelHistoryId, TravelHistory )) (Maybe ExposureTask)
    | SetCovid19Symptoms Bool
    | SaveExposure PersonId (Maybe ( ExposureId, Exposure )) (Maybe ExposureTask)
      -- PRIOR TREATMNENT
    | SetActivePriorTreatmentTask PriorTreatmentTask
    | SetTreatmentReviewBoolInput (Bool -> TreatmentReviewForm -> TreatmentReviewForm) Bool
    | SaveTreatmentReview PersonId (Maybe ( TreatmentReviewId, TreatmentReview ))
      -- NEXT STEPS
    | SetActiveNextStepsTask NextStepsTask
    | SetPatientIsolated Bool
    | SetHealthEducation Bool
    | SetSignOnDoor Bool
    | SetReasonForNotIsolating ReasonForNotIsolating
    | SaveIsolation PersonId (Maybe ( IsolationId, Isolation )) (Maybe NextStepsTask)
    | SetCalled114 Bool
    | SetContactedSite Bool
    | SetHCRecommendation HCRecommendation
    | SetSiteRecommendation SiteRecommendation
    | SaveHCContact PersonId (Maybe ( HCContactId, HCContact )) (Maybe NextStepsTask)
    | SetReferToHealthCenter Bool
    | SetHandReferralForm Bool
    | SaveSendToHC PersonId (Maybe ( SendToHCId, SendToHC ))
    | SetMedicationDistributionBoolInput (Bool -> MedicationDistributionForm -> MedicationDistributionForm) Bool
    | SaveMedicationDistribution PersonId (Maybe ( MedicationDistributionId, MedicationDistribution ))


type alias Model =
    { symptomsData : SymptomsData
    , physicalExamData : PhysicalExamData
    , laboratoryData : LaboratoryData
    , exposureData : ExposureData
    , priorTreatmentData : PriorTreatmentData
    , nextStepsData : NextStepsData
    , showAlertsDialog : Bool
    , showPertinentSymptomsPopup : Bool
    , warningPopupState : Maybe AcuteIllnessDiagnosis
    }


emptyModel : Model
emptyModel =
    { symptomsData = emptySymptomsData
    , physicalExamData = emptyPhysicalExamData
    , laboratoryData = emptyLaboratoryData
    , exposureData = emptyExposureData
    , priorTreatmentData = emptyPriorTreatmentData
    , nextStepsData = emptyNextStepsData
    , showAlertsDialog = False
    , showPertinentSymptomsPopup = False
    , warningPopupState = Nothing
    }



-- SYMPTOMS


type alias SymptomsData =
    { symptomsGeneralForm : SymptomsGeneralForm
    , symptomsRespiratoryForm : SymptomsRespiratoryForm
    , symptomsGIForm : SymptomsGIForm
    , activeTask : SymptomsTask
    }


emptySymptomsData : SymptomsData
emptySymptomsData =
    { symptomsGeneralForm = SymptomsGeneralForm Dict.empty False
    , symptomsRespiratoryForm = SymptomsRespiratoryForm Dict.empty False
    , symptomsGIForm = SymptomsGIForm Dict.empty False Nothing False
    , activeTask = SymptomsGeneral
    }


type SymptomsTask
    = SymptomsGeneral
    | SymptomsRespiratory
    | SymptomsGI


type alias SymptomsGeneralForm =
    { signs : Dict SymptomsGeneralSign Int
    , signsDirty : Bool
    }


type alias SymptomsRespiratoryForm =
    { signs : Dict SymptomsRespiratorySign Int
    , signsDirty : Bool
    }


type alias SymptomsGIForm =
    { signs : Dict SymptomsGISign Int
    , signsDirty : Bool
    , intractableVomiting : Maybe Bool
    , intractableVomitingDirty : Bool
    }



-- PHYSICAL EXAM


type alias PhysicalExamData =
    { vitalsForm : VitalsForm
    , acuteFindingsForm : AcuteFindingsForm
    , activeTask : PhysicalExamTask
    }


emptyPhysicalExamData : PhysicalExamData
emptyPhysicalExamData =
    { vitalsForm = VitalsForm Nothing False Nothing False
    , acuteFindingsForm = AcuteFindingsForm Nothing Nothing
    , activeTask = PhysicalExamVitals
    }


type PhysicalExamTask
    = PhysicalExamVitals
    | PhysicalExamAcuteFindings


type alias VitalsForm =
    { respiratoryRate : Maybe Int
    , respiratoryRateDirty : Bool
    , bodyTemperature : Maybe Float
    , bodyTemperatureDirty : Bool
    }


type alias AcuteFindingsForm =
    { signsGeneral : Maybe (List AcuteFindingsGeneralSign)
    , signsRespiratory : Maybe (List AcuteFindingsRespiratorySign)
    }



-- LABORATORY


type alias LaboratoryData =
    { malariaTestingForm : MalariaTestingForm
    , activeTask : LaboratoryTask
    }


emptyLaboratoryData : LaboratoryData
emptyLaboratoryData =
    { malariaTestingForm = MalariaTestingForm Nothing Nothing
    , activeTask = LaboratoryMalariaTesting
    }


type LaboratoryTask
    = LaboratoryMalariaTesting


type alias MalariaTestingForm =
    { rapidTestResult : Maybe MalariaRapidTestResult
    , isPregnant : Maybe Bool
    }



-- EXPOSURE


type alias ExposureData =
    { travelHistoryForm : TravelHistoryForm
    , exposureForm : ExposureForm
    , activeTask : ExposureTask
    }


emptyExposureData : ExposureData
emptyExposureData =
    { travelHistoryForm = TravelHistoryForm Nothing
    , exposureForm = ExposureForm Nothing
    , activeTask = ExposureTravel
    }


type ExposureTask
    = ExposureTravel
    | ExposureExposure


type alias TravelHistoryForm =
    { covid19Country : Maybe Bool
    }


type alias ExposureForm =
    { covid19Symptoms : Maybe Bool
    }



-- PRIOR TREATMENT


type alias PriorTreatmentData =
    { treatmentReviewForm : TreatmentReviewForm
    , activeTask : PriorTreatmentTask
    }


emptyPriorTreatmentData : PriorTreatmentData
emptyPriorTreatmentData =
    { treatmentReviewForm = emptyTreatmentReviewForm
    , activeTask = TreatmentReview
    }


type PriorTreatmentTask
    = TreatmentReview


type alias TreatmentReviewForm =
    { feverPast6Hours : Maybe Bool
    , feverPast6HoursHelped : Maybe Bool
    , malariaToday : Maybe Bool
    , malariaTodayHelped : Maybe Bool
    , malariaWithinPastMonth : Maybe Bool
    , malariaWithinPastMonthHelped : Maybe Bool
    }


emptyTreatmentReviewForm : TreatmentReviewForm
emptyTreatmentReviewForm =
    { feverPast6Hours = Nothing
    , feverPast6HoursHelped = Nothing
    , malariaToday = Nothing
    , malariaTodayHelped = Nothing
    , malariaWithinPastMonth = Nothing
    , malariaWithinPastMonthHelped = Nothing
    }



-- NEXT STEPS


type alias NextStepsData =
    { isolationForm : IsolationForm
    , hcContactForm : HCContactForm
    , sendToHCForm : SendToHCForm
    , medicationDistributionForm : MedicationDistributionForm
    , activeTask : Maybe NextStepsTask
    }


emptyNextStepsData : NextStepsData
emptyNextStepsData =
    { isolationForm = IsolationForm Nothing Nothing Nothing Nothing
    , hcContactForm = emptyHCContactForm
    , sendToHCForm = SendToHCForm Nothing Nothing
    , medicationDistributionForm = MedicationDistributionForm Nothing Nothing Nothing Nothing Nothing
    , activeTask = Nothing
    }


type NextStepsTask
    = NextStepsIsolation
    | NextStepsCall114
    | NextStepsMedicationDistribution
    | NextStepsSendToHC


type alias IsolationForm =
    { patientIsolated : Maybe Bool
    , signOnDoor : Maybe Bool
    , healthEducation : Maybe Bool
    , reasonsForNotIsolating : Maybe (List ReasonForNotIsolating)
    }


type alias HCContactForm =
    { called114 : Maybe Bool
    , hcRecommendation : Maybe HCRecommendation
    , hcRecommendationDirty : Bool
    , contactedSite : Maybe Bool
    , contactedSiteDirty : Bool
    , siteRecommendation : Maybe SiteRecommendation
    , siteRecommendationDirty : Bool
    }


emptyHCContactForm : HCContactForm
emptyHCContactForm =
    { called114 = Nothing
    , hcRecommendation = Nothing
    , hcRecommendationDirty = False
    , contactedSite = Nothing
    , contactedSiteDirty = False
    , siteRecommendation = Nothing
    , siteRecommendationDirty = False
    }


type alias SendToHCForm =
    { handReferralForm : Maybe Bool
    , referToHealthCenter : Maybe Bool
    }


type alias MedicationDistributionForm =
    { amoxicillin : Maybe Bool
    , coartem : Maybe Bool
    , ors : Maybe Bool
    , zinc : Maybe Bool
    , lemonJuiceOrHoney : Maybe Bool
    }
