module Pages.AcuteIllnessActivity.Model exposing (ContactHCForm, ExposureData, ExposureForm, ExposureTask(..), HCRecomendation(..), IsolationForm, LaboratoryData, LaboratoryTask(..), MalariaTestingForm, Model, Msg(..), PhysicalExamData, PhysicalExamTask(..), ReasonsForNotIsolating(..), ResponsePeriod(..), SymptomsData, SymptomsGIForm, SymptomsGeneralForm, SymptomsRespiratoryForm, SymptomsTask(..), TravelForm, VitalsForm, emptyExposureData, emptyLaboratoryData, emptyModel, emptyPhysicalExamData, emptySymptomsData)

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
    { travelForm : TravelForm
    , exposureForm : ExposureForm
    , isolationForm : IsolationForm
    , contactHCForm : ContactHCForm
    , activeTask : ExposureTask
    }


emptyExposureData : ExposureData
emptyExposureData =
    { travelForm = TravelForm Nothing
    , exposureForm = ExposureForm Nothing Nothing
    , isolationForm = IsolationForm Nothing Nothing Nothing Nothing
    , contactHCForm = ContactHCForm Nothing Nothing Nothing Nothing
    , activeTask = ExposureTravel
    }


type ExposureTask
    = ExposureTravel
    | ExposureExposure
    | ExposureIsolation
    | ExposureContactHC


type alias TravelForm =
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
    , reasonsForNotIsolating : Maybe (List ReasonsForNotIsolating)
    }


type ReasonsForNotIsolating
    = NoSpace
    | Other
    | TooIll
    | ReasonsForNotIsolatingNotApplicable


type alias ContactHCForm =
    { contactedHC : Maybe Bool
    , recomendations : Maybe (List HCRecomendation)
    , responseTime : Maybe ResponsePeriod
    , ambulanceArrivalTime : Maybe ResponsePeriod
    }


type HCRecomendation
    = SendAmbulance
    | HomeIsolation
    | ComeToHC
    | ChwMonitoring
    | HCRecomendationNotApplicable


type ResponsePeriod
    = LessThan30Min
    | Between30min1Hour
    | Between1Hour2Hour
    | Between2Hour1Day
    | MoreThan1Day
    | ResponsePeriodNotApplicable
