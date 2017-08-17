module Pusher.Model exposing (..)

import Participant.Model exposing (Participant, ParticipantId)


type Cluster
    = ApSouthEast1
    | EuWest1
    | UsEast1


type alias PusherAppKey =
    { key : String
    , cluster : Cluster
    }


type alias PusherEvent =
    { participantId : ParticipantId
    , data : PusherEventData
    }


type PusherEventData
    = ParticipantUpdate Participant


{-| Return the event names that should be added via JS.
-}
eventNames : List String
eventNames =
    [ "item__update" ]
