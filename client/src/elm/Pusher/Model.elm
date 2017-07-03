module Pusher.Model exposing (..)

import Patient.Model exposing (Patient, PatientId)


type Cluster
    = ApSouthEast1
    | EuWest1
    | UsEast1


type alias PusherAppKey =
    { key : String
    , cluster : Cluster
    }


type alias PusherEvent =
    { patientId : PatientId
    , data : PusherEventData
    }


type PusherEventData
    = PatientUpdate Patient


{-| Return the event names that should be added via JS.
-}
eventNames : List String
eventNames =
    [ "item__update" ]
