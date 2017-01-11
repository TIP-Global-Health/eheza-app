module Pusher.Model exposing (..)

import Patient.Model exposing (Patient, PatientId)


type alias PusherEvent =
    { patientId : PatientId
    , data : PusherEventData
    }


type PusherEventData
    = PatientUpdate Patient
