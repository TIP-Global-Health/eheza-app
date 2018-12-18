module LocalConfig exposing (localConfigs)

import Config.Model as Config exposing (Model)
import Dict exposing (..)
import Pusher.Model exposing (Cluster(..), PusherAppKey)
import Rollbar


local : Model
local =
    { backendUrl = "http://server.local"
    , name = "local"
    , pusherKey = PusherAppKey "" UsEast1
    , debug = True
    , rollbarToken = Rollbar.token "***REMOVED***"
    , sandbox = False
    }


localhost : Model
localhost =
    { backendUrl = "http://localhost"
    , name = "local"
    , pusherKey = PusherAppKey "" UsEast1
    , debug = True
    , rollbarToken = Rollbar.token "***REMOVED***"
    , sandbox = False
    }


localConfigs : Dict String Model
localConfigs =
    Dict.fromList
        -- Change "localhost" if you are serving this from a different local
        -- URL.
        [ ( "server.local", local )
        , ( "localhost", localhost )
        ]
