module Pages.AcuteIllness.Activity.Model exposing (..)

import AssocList as Dict exposing (Dict)
import Backend.AcuteIllnessEncounter.Types exposing (AcuteIllnessDiagnosis)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Backend.Person.Form
import Components.PatientsSearchForm.Model
import Date exposing (Date)
import DateSelector.Model exposing (DateSelectorConfig)
import EverySet exposing (EverySet)
import Form
import Gizra.NominalDate exposing (NominalDate)
import Measurement.Model exposing (..)
import Pages.AcuteIllness.Activity.Types exposing (..)
import Pages.Page exposing (Page)
import SyncManager.Model exposing (Site)


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
    | SetSymptomsRespiratoryCough Bool
    | SetSymptomsGIIntractableVomiting Bool
    | SaveSymptomsGeneral PersonId (Maybe ( SymptomsGeneralId, SymptomsGeneral )) (Maybe SymptomsTask)
    | SaveSymptomsRespiratory PersonId (Maybe ( SymptomsRespiratoryId, SymptomsRespiratory )) (Maybe SymptomsTask)
    | SaveSymptomsGI PersonId (Maybe ( SymptomsGIId, SymptomsGI )) (Maybe SymptomsTask)
      -- PHYSICAL EXAM Msgs
    | SetActivePhysicalExamTask PhysicalExamTask
    | SetVitalsIntInput (Maybe Int -> VitalsForm -> VitalsForm) String
    | SetVitalsFloatInput (Maybe Float -> VitalsForm -> VitalsForm) String
    | SetAcuteFindingsGeneralSign AcuteFindingsGeneralSign
    | SetAcuteFindingsRespiratorySign AcuteFindingsRespiratorySign
    | SaveVitals PersonId (Maybe ( AcuteIllnessVitalsId, AcuteIllnessVitals )) (Maybe PhysicalExamTask)
    | SaveAcuteFindings PersonId (Maybe ( AcuteFindingsId, AcuteFindings )) (Maybe PhysicalExamTask)
    | SetMuac String
    | SaveMuac PersonId (Maybe ( AcuteIllnessMuacId, AcuteIllnessMuac )) (Maybe PhysicalExamTask)
    | SetNutritionSign ChildNutritionSign
    | SaveNutrition PersonId (Maybe ( AcuteIllnessNutritionId, AcuteIllnessNutrition )) (Maybe PhysicalExamTask)
    | SetCoreExamHeart HeartCPESign
    | SetCoreExamLungs LungsCPESign
    | SaveCoreExam PersonId (Maybe ( AcuteIllnessCoreExamId, AcuteIllnessCoreExam )) (Maybe PhysicalExamTask)
      -- LABORATORY Msgs
    | SetActiveLaboratoryTask AILaboratoryTask
    | SetRapidTestResult String
    | SetIsPregnant Bool
    | SaveMalariaTesting PersonId (Maybe ( MalariaTestingId, MalariaTesting )) (Maybe AILaboratoryTask)
    | SetCovidTestingBoolInput (Bool -> CovidTestingForm -> CovidTestingForm) Bool
    | SetCovidTestingAdministrationNote AdministrationNote
    | SaveCovidTesting PersonId (Maybe ( CovidTestingId, CovidTesting )) (Maybe AILaboratoryTask)
      -- EXPOSURE Msgs
    | SetActiveExposureTask ExposureTask
    | SetCovid19Country Bool
    | SaveTravelHistory PersonId (Maybe ( TravelHistoryId, TravelHistory )) (Maybe ExposureTask)
    | SetCovid19Symptoms Bool
    | SaveExposure PersonId (Maybe ( ExposureId, Exposure )) (Maybe ExposureTask)
      -- PRIOR TREATMENT
    | SetActivePriorTreatmentTask PriorTreatmentTask
    | SetTreatmentReviewBoolInput (Bool -> TreatmentReviewForm -> TreatmentReviewForm) Bool
    | SaveTreatmentReview PersonId (Maybe ( TreatmentReviewId, TreatmentReview ))
      -- NEXT STEPS
    | SetActiveNextStepsTask Pages.AcuteIllness.Activity.Types.NextStepsTask
    | SetPatientIsolated Bool
    | SetHealthEducation Bool
    | SetReasonForNotProvidingHealthEducation ReasonForNotProvidingHealthEducation
    | SetSignOnDoor Bool
    | SetReasonForNotIsolating ReasonForNotIsolating
    | SaveIsolation PersonId (Maybe ( IsolationId, Isolation )) (Maybe Pages.AcuteIllness.Activity.Types.NextStepsTask)
    | SetContactedHC Bool
    | SetHCRecommendation HCRecommendation
    | SetResponsePeriod ResponsePeriod
    | SetAmbulanceArrivalPeriod ResponsePeriod
    | SaveHCContact PersonId (Maybe ( HCContactId, HCContact )) (Maybe Pages.AcuteIllness.Activity.Types.NextStepsTask)
    | SetCalled114 Bool
    | SetContactedSite Bool
    | SetRecommendation114 Recommendation114
    | SetRecommendationSite RecommendationSite
    | SaveCall114 PersonId (Maybe ( Call114Id, Call114 )) (Maybe Pages.AcuteIllness.Activity.Types.NextStepsTask)
    | SetReferToHealthCenter Bool
    | SetHandReferralForm Bool
    | SaveSendToHC PersonId (Maybe ( SendToHCId, SendToHC )) (Maybe Pages.AcuteIllness.Activity.Types.NextStepsTask)
    | SetReasonForNonReferral ReasonForNonReferral
    | SetMedicationDistributionBoolInput (Bool -> MedicationDistributionForm -> MedicationDistributionForm) Bool
    | SetMedicationDistributionAdministrationNote (Maybe AdministrationNote) MedicationDistributionSign AdministrationNote
    | SaveMedicationDistribution PersonId (Maybe ( MedicationDistributionId, MedicationDistribution )) (Maybe Pages.AcuteIllness.Activity.Types.NextStepsTask)
    | SetProvidedEducationForDiagnosis Bool
    | SaveHealthEducation PersonId (Maybe ( HealthEducationId, HealthEducation )) (Maybe Pages.AcuteIllness.Activity.Types.NextStepsTask)
    | SetFollowUpOption FollowUpOption
    | SaveFollowUp PersonId (Maybe AcuteIllnessDiagnosis) (Maybe ( AcuteIllnessFollowUpId, AcuteIllnessFollowUp )) (Maybe Pages.AcuteIllness.Activity.Types.NextStepsTask)
    | SetContactsTracingFormState ContactsTracingFormState
    | MsgPatientsSearchForm Components.PatientsSearchForm.Model.Msg
    | SetContactsTracingDate Date
    | SetContactsTracingDateSelectorState (Maybe (DateSelectorConfig Msg))
    | SetContactsTracingPhoneNumber String
    | SetContactsTracingFinished
    | SaveTracedContact ContactTraceItem
    | DeleteTracedContact PersonId
    | RegisterContactMsgForm Form.Msg
    | SaveContactsTracing PersonId (Maybe ( AcuteIllnessContactsTracingId, AcuteIllnessContactsTracing )) (Maybe Pages.AcuteIllness.Activity.Types.NextStepsTask)
      -- ONGOIN TREATMENT
    | SetActiveOngoingTreatmentTask OngoingTreatmentTask
    | SetOngoingTreatmentReviewBoolInput (Bool -> OngoingTreatmentReviewForm -> OngoingTreatmentReviewForm) Bool
    | SetReasonForNotTaking ReasonForNotTaking
    | SetTotalMissedDoses String
    | SetAdverseEvent AdverseEvent
    | SaveOngoingTreatmentReview PersonId (Maybe ( TreatmentOngoingId, TreatmentOngoing ))
      -- DANGER SIGNS
    | SetActiveDangerSignsTask DangerSignsTask
    | SetConditionImproving Bool
    | SetDangerSign AcuteIllnessDangerSign
    | SaveReviewDangerSigns PersonId (Maybe ( AcuteIllnessDangerSignsId, AcuteIllnessDangerSigns ))


type alias Model =
    { symptomsData : SymptomsData
    , physicalExamData : PhysicalExamData
    , laboratoryData : LaboratoryData
    , exposureData : ExposureData
    , priorTreatmentData : PriorTreatmentData
    , nextStepsData : NextStepsData
    , ongoingTreatmentData : OngoingTreatmentData
    , dangerSignsData : DangerSignsData
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
    , ongoingTreatmentData = emptyOngoingTreatmentData
    , dangerSignsData = emptyDangerSignsData
    , showAlertsDialog = False
    , showPertinentSymptomsPopup = False
    , warningPopupState = Nothing
    }



-- SYMPTOMS


type alias SymptomsData =
    { symptomsGeneralForm : SymptomsGeneralForm
    , symptomsRespiratoryForm : SymptomsRespiratoryForm
    , symptomsGIForm : SymptomsGIForm
    , activeTask : Maybe SymptomsTask
    }


emptySymptomsData : SymptomsData
emptySymptomsData =
    { symptomsGeneralForm = SymptomsGeneralForm Dict.empty False
    , symptomsRespiratoryForm = SymptomsRespiratoryForm Dict.empty False
    , symptomsGIForm = SymptomsGIForm Dict.empty False Nothing False
    , activeTask = Nothing
    }


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
    , muacForm : MuacForm
    , nutritionForm : AcuteIllnessNutritionForm
    , coreExamForm : AcuteIllnessCoreExamForm
    , activeTask : Maybe PhysicalExamTask
    }


emptyPhysicalExamData : PhysicalExamData
emptyPhysicalExamData =
    { vitalsForm = emptyVitalsForm
    , acuteFindingsForm = AcuteFindingsForm Nothing Nothing
    , muacForm = emptyMuacForm
    , nutritionForm = AcuteIllnessNutritionForm Nothing
    , coreExamForm = AcuteIllnessCoreExamForm Nothing Nothing
    , activeTask = Nothing
    }


type alias AcuteFindingsForm =
    { signsGeneral : Maybe (List AcuteFindingsGeneralSign)
    , signsRespiratory : Maybe (List AcuteFindingsRespiratorySign)
    }


type alias AcuteIllnessNutritionForm =
    { signs : Maybe (List ChildNutritionSign)
    }


type alias AcuteIllnessCoreExamForm =
    { heart : Maybe HeartCPESign
    , lungs : Maybe (List LungsCPESign)
    }



-- LABORATORY


type alias LaboratoryData =
    { malariaTestingForm : MalariaTestingForm
    , covidTestingForm : CovidTestingForm
    , activeTask : Maybe AILaboratoryTask
    }


emptyLaboratoryData : LaboratoryData
emptyLaboratoryData =
    { malariaTestingForm = MalariaTestingForm Nothing Nothing
    , covidTestingForm = emptyCovidTestingForm
    , activeTask = Nothing
    }


type alias MalariaTestingForm =
    { rapidTestResult : Maybe RapidTestResult
    , isPregnant : Maybe Bool
    }


type alias CovidTestingForm =
    { testPerformed : Maybe Bool
    , testPositive : Maybe Bool
    , isPregnant : Maybe Bool
    , administrationNote : Maybe AdministrationNote
    }


emptyCovidTestingForm : CovidTestingForm
emptyCovidTestingForm =
    CovidTestingForm Nothing Nothing Nothing Nothing



-- EXPOSURE


type alias ExposureData =
    { travelHistoryForm : TravelHistoryForm
    , exposureForm : ExposureForm
    , activeTask : Maybe ExposureTask
    }


emptyExposureData : ExposureData
emptyExposureData =
    { travelHistoryForm = TravelHistoryForm Nothing
    , exposureForm = ExposureForm Nothing
    , activeTask = Nothing
    }


type alias TravelHistoryForm =
    { covid19Country : Maybe Bool
    }


type alias ExposureForm =
    { covid19Symptoms : Maybe Bool
    }



-- PRIOR TREATMENT


type alias PriorTreatmentData =
    { treatmentReviewForm : TreatmentReviewForm
    , activeTask : Maybe PriorTreatmentTask
    }


emptyPriorTreatmentData : PriorTreatmentData
emptyPriorTreatmentData =
    { treatmentReviewForm = emptyTreatmentReviewForm
    , activeTask = Nothing
    }


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
    , call114Form : Call114Form
    , sendToHCForm : SendToHCForm
    , medicationDistributionForm : MedicationDistributionForm
    , healthEducationForm : HealthEducationForm
    , followUpForm : FollowUpForm
    , contactsTracingForm : ContactsTracingForm
    , activeTask : Maybe Pages.AcuteIllness.Activity.Types.NextStepsTask
    }


type alias FollowUpForm =
    { option : Maybe FollowUpOption
    , diagnosis : Maybe AcuteIllnessDiagnosis
    , resolutionDate : Maybe NominalDate
    }


emptyNextStepsData : NextStepsData
emptyNextStepsData =
    { isolationForm = IsolationForm Nothing Nothing Nothing Nothing
    , hcContactForm = HCContactForm Nothing Nothing Nothing Nothing
    , call114Form = emptyCall114Form
    , sendToHCForm = emptySendToHCForm
    , medicationDistributionForm = MedicationDistributionForm Nothing Nothing Nothing Nothing Nothing Nothing
    , healthEducationForm = emptyHealthEducationForm
    , followUpForm = FollowUpForm Nothing Nothing Nothing
    , contactsTracingForm = emptyContactsTracingForm
    , activeTask = Nothing
    }


type alias IsolationForm =
    { patientIsolated : Maybe Bool
    , signOnDoor : Maybe Bool
    , healthEducation : Maybe Bool
    , reasonsForNotIsolating : Maybe (List ReasonForNotIsolating)
    }


type alias HCContactForm =
    { contactedHC : Maybe Bool
    , recommendations : Maybe HCRecommendation
    , responsePeriod : Maybe ResponsePeriod
    , ambulanceArrivalPeriod : Maybe ResponsePeriod
    }


type alias Call114Form =
    { called114 : Maybe Bool
    , recommendation114 : Maybe Recommendation114
    , recommendation114Dirty : Bool
    , contactedSite : Maybe Bool
    , contactedSiteDirty : Bool
    , recommendationSite : Maybe RecommendationSite
    , recommendationSiteDirty : Bool
    }


emptyCall114Form : Call114Form
emptyCall114Form =
    { called114 = Nothing
    , recommendation114 = Nothing
    , recommendation114Dirty = False
    , contactedSite = Nothing
    , contactedSiteDirty = False
    , recommendationSite = Nothing
    , recommendationSiteDirty = False
    }


type alias MedicationDistributionForm =
    { amoxicillin : Maybe Bool
    , coartem : Maybe Bool
    , ors : Maybe Bool
    , zinc : Maybe Bool
    , lemonJuiceOrHoney : Maybe Bool
    , nonAdministrationSigns : Maybe (EverySet MedicationNonAdministrationSign)
    }


type alias ContactsTracingForm =
    { state : ContactsTracingFormState
    , contacts : Maybe (Dict PersonId ContactTraceItem)
    , finished : Bool
    }


type ContactsTracingFormState
    = ContactsTracingFormSummary
    | ContactsTracingFormSearchParticipants Components.PatientsSearchForm.Model.Model
    | ContactsTracingFormRecordContactDetails PersonId RecordContactDetailsData
    | ContactsTracingFormRegisterContact RegisterContactData


emptyContactsTracingForm : ContactsTracingForm
emptyContactsTracingForm =
    { state = ContactsTracingFormSummary
    , contacts = Nothing
    , finished = False
    }


type alias RecordContactDetailsData =
    { contactDate : Maybe Date
    , dateSelectorPopupState : Maybe (DateSelectorConfig Msg)
    , phoneNumber : Maybe String
    }


emptyRecordContactDetailsData : RecordContactDetailsData
emptyRecordContactDetailsData =
    { contactDate = Nothing
    , dateSelectorPopupState = Nothing
    , phoneNumber = Nothing
    }


type alias RegisterContactData =
    Backend.Person.Form.ContactForm


emptyRegisterContactData : Site -> RegisterContactData
emptyRegisterContactData site =
    Backend.Person.Form.emptyContactForm site



-- ONGOING TREATMENT


type alias OngoingTreatmentData =
    { treatmentReviewForm : OngoingTreatmentReviewForm
    , activeTask : Maybe OngoingTreatmentTask
    }


emptyOngoingTreatmentData : OngoingTreatmentData
emptyOngoingTreatmentData =
    { treatmentReviewForm = emptyOngoingTreatmentReviewForm
    , activeTask = Nothing
    }



-- DANGER SIGNS


type alias DangerSignsData =
    { reviewDangerSignsForm : ReviewDangerSignsForm
    , activeTask : Maybe DangerSignsTask
    }


emptyDangerSignsData : DangerSignsData
emptyDangerSignsData =
    { reviewDangerSignsForm = emptyReviewDangerSignsForm
    , activeTask = Nothing
    }


type alias ReviewDangerSignsForm =
    { conditionImproving : Maybe Bool
    , symptoms : Maybe (List AcuteIllnessDangerSign)
    }


emptyReviewDangerSignsForm : ReviewDangerSignsForm
emptyReviewDangerSignsForm =
    { conditionImproving = Nothing
    , symptoms = Nothing
    }
