module Pages.TraceContact.Model exposing (..)

--import Backend.Measurement.Model exposing (ChildTraceContactSign, TraceContactMeasurements, ObstetricHistoryValue)

import Backend.Entities exposing (..)
import Pages.Page exposing (Page)


type alias Model =
    { step : TraceContactStep }


emptyModel : Model
emptyModel =
    { step = StepInitiateContact emptyStepInitiateContactData }


type TraceContactStep
    = StepInitiateContact StepInitiateContactData
    | StepRecordSymptoms
    | StepReferToHealthCenter


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


type Msg
    = SetActivePage Page
    | SetTraceContactStep TraceContactStep
      -- StepInitiateContact
    | SetContactInitiated Bool
    | SetNoContactReason NoContactReason
    | SaveStepInitiateContact
