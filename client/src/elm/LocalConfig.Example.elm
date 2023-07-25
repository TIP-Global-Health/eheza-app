module LocalConfig exposing (localConfigs)

import AssocList as Dict exposing (..)
import Config.Model as Config exposing (Model)


local : Model
local =
    { site = SiteRwanda
    , backendUrl = "https://eheza-app.ddev.site:4443"
    , name = "local"
    , debug = True
    }


localConfigs : Dict String Model
localConfigs =
    Dict.fromList
        -- Change "localhost" if you are serving this from a different local
        -- URL.
        [ ( "localhost", local )
        ]
