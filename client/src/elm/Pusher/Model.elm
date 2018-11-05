module Pusher.Model exposing (Cluster(..), PusherAppKey, PusherEvent, PusherEventData(..), eventNames)

{-| This isn't used at the moment ... we can revive it at some
point when we have a sense of what we want to do with Pusher. It will be a
little copmlicated by the fact that we anticipate going offline and online, so
we'll need to think about how to manage "missed" Pusher events -- we won't
necessarily get them all.
-}


type Cluster
    = ApSouthEast1
    | EuWest1
    | UsEast1


type alias PusherAppKey =
    { key : String
    , cluster : Cluster
    }


type alias PusherEvent =
    { data : PusherEventData
    }


type PusherEventData
    = ParticipantUpdate


{-| Return the event names that should be added via JS.
-}
eventNames : List String
eventNames =
    [ "item__update" ]
