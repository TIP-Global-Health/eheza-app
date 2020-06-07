module Pages.AcuteIllnessActivity.Model exposing (ExposureData, ExposureForm, ExposureTask(..), HCContactForm, IsolationForm, LaboratoryData, LaboratoryTask(..), MalariaTestingForm, Model, Msg(..), PhysicalExamData, PhysicalExamTask(..), PriorTreatmentData, PriorTreatmentTask(..), SymptomsData, SymptomsGIForm, SymptomsGeneralForm, SymptomsRespiratoryForm, SymptomsTask(..), TravelHistoryForm, TreatmentReviewForm, VitalsForm, emptyExposureData, emptyLaboratoryData, emptyModel, emptyPhysicalExamData, emptyPriorTreatmentData, emptySymptomsData, emptyTreatmentReviewForm)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
    | SetAlertsDialogState Bool
    | SetCovid19PopupState Bool
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
    | SaveVitals PersonId (Maybe ( AcuteIllnessVitalsId, AcuteIllnessVitals ))
      -- LABORATORY Msgs
    | SetActiveLaboratoryTask LaboratoryTask
    | SetRapidTestPositive Bool
    | SaveMalariaTesting PersonId (Maybe ( MalariaTestingId, MalariaTesting ))
      -- EXPOSURE Msgs
    | SetActiveExposureTask ExposureTask
    | SetCovid19Country Bool
    | SaveTravelHistory PersonId (Maybe ( TravelHistoryId, TravelHistory )) (Maybe ExposureTask)
    | SetCovid19Symptoms Bool
    | SetSimilarSymptoms Bool
    | SaveExposure PersonId (Maybe ( ExposureId, Exposure )) (Maybe ExposureTask)
    | SetPatientIsolated Bool
    | SetHealthEducation Bool
    | SetSignOnDoor Bool
    | SetReasonForNotIsolating ReasonForNotIsolating
    | SaveIsolation PersonId (Maybe ( IsolationId, Isolation )) (Maybe ExposureTask)
    | SetContactedHC Bool
    | SetHCRecommendation HCRecomendation
    | SetResponsePeriod ResponsePeriod
    | SetAmbulanceArrivalPeriod ResponsePeriod
    | SaveHCContact PersonId (Maybe ( HCContactId, HCContact )) (Maybe ExposureTask)
      -- PRIOR TREATMNENT
    | SetActivePriorTreatmentTask PriorTreatmentTask
    | SetTreatmentReviewBoolInput (Bool -> TreatmentReviewForm -> TreatmentReviewForm) Bool


type alias Model =
    { symptomsData : SymptomsData
    , physicalExamData : PhysicalExamData
    , laboratoryData : LaboratoryData
    , exposureData : ExposureData
    , priorTreatmentData : PriorTreatmentData
    , showAlertsDialog : Bool
    , showCovid19Popup : Bool
    }


emptyModel : Model
emptyModel =
    { symptomsData = emptySymptomsData
    , physicalExamData = emptyPhysicalExamData
    , laboratoryData = emptyLaboratoryData
    , exposureData = emptyExposureData
    , priorTreatmentData = emptyPriorTreatmentData
    , showAlertsDialog = False
    , showCovid19Popup = False
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
    { symptomsGeneralForm = SymptomsGeneralForm Dict.empty
    , symptomsRespiratoryForm = SymptomsRespiratoryForm Dict.empty
    , symptomsGIForm = SymptomsGIForm Dict.empty Nothing
    , activeTask = SymptomsGeneral
    }


type SymptomsTask
    = SymptomsGeneral
    | SymptomsRespiratory
    | SymptomsGI


type alias SymptomsGeneralForm =
    { signs : Dict SymptomsGeneralSign Int
    }


type alias SymptomsRespiratoryForm =
    { signs : Dict SymptomsRespiratorySign Int
    }


type alias SymptomsGIForm =
    { signs : Dict SymptomsGISign Int
    , intractableVomiting : Maybe Bool
    }



-- PHYSICAL EXAM


type alias PhysicalExamData =
    { vitalsForm : VitalsForm
    , activeTask : PhysicalExamTask
    }


emptyPhysicalExamData : PhysicalExamData
emptyPhysicalExamData =
    { vitalsForm = VitalsForm Nothing Nothing
    , activeTask = PhysicalExamVitals
    }


type PhysicalExamTask
    = PhysicalExamVitals


type alias VitalsForm =
    { respiratoryRate : Maybe Int
    , bodyTemperature : Maybe Float
    }



-- LABORATORY


type alias LaboratoryData =
    { malariaTestingForm : MalariaTestingForm
    , activeTask : LaboratoryTask
    }


emptyLaboratoryData : LaboratoryData
emptyLaboratoryData =
    { malariaTestingForm = MalariaTestingForm Nothing
    , activeTask = LaboratoryMalariaTesting
    }


type LaboratoryTask
    = LaboratoryMalariaTesting


type alias MalariaTestingForm =
    { rapidTestPositive : Maybe Bool
    }



-- EXPOSURE


type alias ExposureData =
    { travelHistoryForm : TravelHistoryForm
    , exposureForm : ExposureForm
    , isolationForm : IsolationForm
    , hcContactForm : HCContactForm
    , activeTask : ExposureTask
    }


emptyExposureData : ExposureData
emptyExposureData =
    { travelHistoryForm = TravelHistoryForm Nothing
    , exposureForm = ExposureForm Nothing Nothing
    , isolationForm = IsolationForm Nothing Nothing Nothing Nothing
    , hcContactForm = HCContactForm Nothing Nothing Nothing Nothing
    , activeTask = ExposureTravel
    }


type ExposureTask
    = ExposureTravel
    | ExposureExposure
    | ExposureIsolation
    | ExposureContactHC


type alias TravelHistoryForm =
    { covid19Country : Maybe Bool
    }


type alias ExposureForm =
    { covid19Symptoms : Maybe Bool
    , similarSymptoms : Maybe Bool
    }


type alias IsolationForm =
    { patientIsolated : Maybe Bool
    , signOnDoor : Maybe Bool
    , healthEducation : Maybe Bool
    , reasonsForNotIsolating : Maybe (List ReasonForNotIsolating)
    }


type alias HCContactForm =
    { contactedHC : Maybe Bool
    , recomendations : Maybe HCRecomendation
    , responsePeriod : Maybe ResponsePeriod
    , ambulanceArrivalPeriod : Maybe ResponsePeriod
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
