module LocalConfig exposing (localConfigs)

import Config.Model as Config exposing (Model)
import Dict exposing (..)
import Rollbar


local : Model
local =
    { backendUrl = "http://server.local"
    , name = "local"
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
        ]
