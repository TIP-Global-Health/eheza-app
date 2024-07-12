module LocalConfig.Example exposing (localConfigs)

import AssocList as Dict exposing (..)
import Config.Model exposing (Model)


local : Model
local =
    { backendUrl = "https://eheza-app.ddev.site:4443"
    , name = "local"
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
