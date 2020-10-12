module LocalConfig exposing (localConfigs)

import AssocList as Dict exposing (..)
import Config.Model as Config exposing (Model)
import Pusher.Model exposing (Cluster(..), PusherAppKey)


local : Model
local =
    { backendUrl = "http://localhost/ihangane/server/www"
    , name = "local"
    , pusherKey = PusherAppKey "" UsEast1
    , debug = True
    , sandbox = False
    }


localConfigs : Dict String Model
localConfigs =
    Dict.fromList
        -- Change "localhost" if you are serving this from a different local
        -- URL.
        [ ( "localhost", local )
        ]
