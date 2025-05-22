module Pages.TraceContact.Model exposing (..)

import Backend.Measurement.Model exposing (ContactTraceItem, SymptomsGISign, SymptomsGeneralSign, SymptomsRespiratorySign)
import EverySet exposing (EverySet)
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
    = ReasonNoAnswer
    | ReasonWrongContactInfo
    | ReasonDeclinedFollowUp


type SymptomsTask
    = SymptomsGeneral
    | SymptomsRespiratory
    | SymptomsGI


type alias StepRecordSymptomsData =
    { symptomsGeneralForm : SymptomsGeneralForm
    , symptomsRespiratoryForm : SymptomsRespiratoryForm
    , symptomsGIForm : SymptomsGIForm
    , activeTask : Maybe SymptomsTask
    , popupState : Maybe RecordSymptomsPopupState
    }


emptyStepRecordSymptomsData : StepRecordSymptomsData
emptyStepRecordSymptomsData =
    { symptomsGeneralForm = SymptomsGeneralForm EverySet.empty False
    , symptomsRespiratoryForm = SymptomsRespiratoryForm EverySet.empty False
    , symptomsGIForm = SymptomsGIForm EverySet.empty False
    , activeTask = Nothing
    , popupState = Nothing
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


type RecordSymptomsPopupState
    = StateSymptomsFound
    | StateSymptomsNotFound


type Msg
    = SetActivePage Page
    | SetTraceContactStep TraceContactStep
      -- StepInitiateContact
    | SetContactInitiated Bool
    | SetNoContactReason NoContactReason
    | SaveStepInitiateContact ContactTraceItem
      -- StepRecordSymptoms
    | SetActiveSymptomsTask SymptomsTask
    | ToggleSymptomsGeneralSign SymptomsGeneralSign
    | ToggleSymptomsRespiratorySign SymptomsRespiratorySign
    | ToggleSymptomsGISign SymptomsGISign
    | SaveSymptomsGeneral ContactTraceItem (Maybe SymptomsTask)
    | SaveSymptomsRespiratory ContactTraceItem (Maybe SymptomsTask)
    | SaveSymptomsGI ContactTraceItem (Maybe SymptomsTask)
    | SetRecordSymptomsPopupState (Maybe RecordSymptomsPopupState)
    | GenerateRecommendation ContactTraceItem
