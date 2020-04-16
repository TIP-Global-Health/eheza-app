module Pages.AcuteIllnessActivity.Model exposing (ExposureData, ExposureForm, ExposureTask(..), HCContactForm, IsolationForm, LaboratoryData, LaboratoryTask(..), MalariaTestingForm, Model, Msg(..), PhysicalExamData, PhysicalExamTask(..), SymptomsData, SymptomsGIForm, SymptomsGeneralForm, SymptomsRespiratoryForm, SymptomsTask(..), TravelHistoryForm, VitalsForm, emptyExposureData, emptyLaboratoryData, emptyModel, emptyPhysicalExamData, emptySymptomsData)

import AssocList as Dict exposing (Dict)
import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (..)
import Pages.Page exposing (Page)


type Msg
    = SetActivePage Page
      -- SYMPTOMS Msgs
    | SetActiveSymptomsTask SymptomsTask
    | ToggleSymptomsGeneralSign SymptomsGeneralSign
    | ToggleSymptomsGISign SymptomsGISign
    | ToggleSymptomsRespiratorySign SymptomsRespiratorySign
    | SetSymptomsGeneralSignValue SymptomsGeneralSign String
    | SetSymptomsGISignValue SymptomsGISign String
    | SetSymptomsRespiratorySignValue SymptomsRespiratorySign String
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
    | SetCovid19Symptoms Bool
    | SetSimilarSymptoms Bool
    | SetPatientIsolated Bool
    | SetHealthEducation Bool
    | SetSignOnDoor Bool
    | SetReasonForNotIsolating ReasonForNotIsolating
    | SetContactedHC Bool
    | SetHCRecommendation HCRecomendation
    | SetResponsePeriod ResponsePeriod
    | SetAmbulanceArrivalPeriod ResponsePeriod


type alias Model =
    { symptomsData : SymptomsData
    , physicalExamData : PhysicalExamData
    , laboratoryData : LaboratoryData
    , exposureData : ExposureData
    }


emptyModel : Model
emptyModel =
    { symptomsData = emptySymptomsData
    , physicalExamData = emptyPhysicalExamData
    , laboratoryData = emptyLaboratoryData
    , exposureData = emptyExposureData
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
    , symptomsGIForm = SymptomsGIForm Dict.empty
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
    , recomendations : Maybe (List HCRecomendation)
    , responsePeriod : Maybe ResponsePeriod
    , ambulanceArrivalPeriod : Maybe ResponsePeriod
    }
