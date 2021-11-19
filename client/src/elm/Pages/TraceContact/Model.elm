module Pages.TraceContact.Model exposing (..)

import Backend.Entities exposing (..)
import Backend.Measurement.Model exposing (SymptomsGISign, SymptomsGeneralSign, SymptomsRespiratorySign)
import EverySet exposing (EverySet)
import Pages.AcuteIllnessActivity.Types exposing (SymptomsTask(..))
import Pages.Page exposing (Page)


type alias Model =
    { step : TraceContactStep }


emptyModel : Model
emptyModel =
    { step = StepInitiateContact emptyStepInitiateContactData }


type TraceContactStep
    = StepInitiateContact StepInitiateContactData
    | StepRecordSymptoms StepRecordSymptomsData


type alias StepInitiateContactData =
    { contactInitiated : Maybe Bool
    , noContactReason : Maybe NoContactReason
    }


emptyStepInitiateContactData : StepInitiateContactData
emptyStepInitiateContactData =
    StepInitiateContactData Nothing Nothing


type NoContactReason
    = ReasonNoAnser
    | ReasonWrongContactInfo
    | ReasonDeclinedFollowUp


type alias StepRecordSymptomsData =
    { symptomsGeneralForm : SymptomsGeneralForm
    , symptomsRespiratoryForm : SymptomsRespiratoryForm
    , symptomsGIForm : SymptomsGIForm
    , activeTask : Maybe SymptomsTask
    }


emptyStepRecordSymptomsData : StepRecordSymptomsData
emptyStepRecordSymptomsData =
    { symptomsGeneralForm = SymptomsGeneralForm EverySet.empty False
    , symptomsRespiratoryForm = SymptomsRespiratoryForm EverySet.empty False
    , symptomsGIForm = SymptomsGIForm EverySet.empty False
    , activeTask = Nothing
    }


type alias SymptomsGeneralForm =
    { signs : EverySet SymptomsGeneralSign
    , completed : Bool
    }


type alias SymptomsRespiratoryForm =
    { signs : EverySet SymptomsRespiratorySign
    , completed : Bool
    }


type alias SymptomsGIForm =
    { signs : EverySet SymptomsGISign
    , completed : Bool
    }


type Msg
    = SetActivePage Page
    | SetTraceContactStep TraceContactStep
      -- StepInitiateContact
    | SetContactInitiated Bool
    | SetNoContactReason NoContactReason
    | SaveStepInitiateContact
      -- StepRecordSymptoms
    | SetActiveSymptomsTask SymptomsTask
    | ToggleSymptomsGeneralSign SymptomsGeneralSign
    | ToggleSymptomsRespiratorySign SymptomsRespiratorySign
    | ToggleSymptomsGISign SymptomsGISign
    | SaveSymptomsGeneral (Maybe SymptomsTask)
    | SaveSymptomsRespiratory (Maybe SymptomsTask)
    | SaveSymptomsGI (Maybe SymptomsTask)
