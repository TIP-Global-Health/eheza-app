module PatientManager.Model exposing (..)

import Activity.Model exposing (ActivityType)
import Child.Model exposing (ChildId)
import Dict exposing (Dict)
import Http
import Measurement.Model
import Pages.Activities.Model
import Pages.Patient.Model
import Pages.Patients.Model
import Patient.Model exposing (Patient, PatientId, PatientsDict)
import Pusher.Model exposing (PusherEvent)
import RemoteData exposing (RemoteData(..), WebData)


{-| We track any Patients we are currently subscribed to.

In theory, we'll only typically have one at a time. However, the logic of
subscribing and unsubscribing will be required in any event. Thus, it's
simpler to just track whatever we're subscribed to. That is, we could limit
ourselves to one subscription at a time, but that would actually be extra
logic, not less.

Each `Pages.Patient.Model.Model` is wrapped in a `WebData`, because we
derive it from fetching a `Patient` through `WebData` ... it's simplest to
just stay within the `WebData` container.

-}
type alias Model =
    { activitiesPage : Pages.Activities.Model.Model
    , patients : Dict PatientId (WebData Patient)
    , patientsPage : Pages.Patients.Model.Model
    , patientPage : Dict PatientId Pages.Patient.Model.Model
    }


{-| Our messages:

  - `Subscribe` means "fetch the Patient and listen to its pusher events"

  - `Unsubscribe` means "forget the Patient and stop listening to its pusher events"

  - `MsgPagesPatient` is a message to route to a Patient viewer

-}
type Msg
    = Subscribe PatientId
    | Unsubscribe PatientId
    | FetchAll
    | MsgMeasurement Measurement.Model.Msg
    | MsgPagesActivities Pages.Activities.Model.Msg
    | MsgPagesPatient PatientId Pages.Patient.Model.Msg
    | MsgPagesPatients Pages.Patients.Model.Msg
    | HandleFetchedPatient PatientId (Result Http.Error Patient)
    | HandleFetchedPatients (Result Http.Error PatientsDict)
    | HandlePusherEvent (Result String PusherEvent)
    | SetActivityTypeFilters (List ActivityType)


emptyModel : Model
emptyModel =
    { activitiesPage = Pages.Activities.Model.emptyModel
    , patients = Dict.empty
    , patientsPage = Pages.Patients.Model.emptyModel
    , patientPage = Dict.empty
    }
