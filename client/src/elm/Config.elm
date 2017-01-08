module Config exposing (..)

import Config.Model as Config exposing (Model)
import Dict exposing (..)


local : Model
local =
    { backendUrl = "http://localhost/ihangane/server/www"
    , name = "local"
    , pusherKey = "ba4608c38aa09c23227b"
    }


production : Model
production =
    { backendUrl = "https://***REMOVED***"
    , name = "gh-pages"
    , pusherKey = "f0d7df56f0d4928ea6d8"
    }


configs : Dict String Model
configs =
    Dict.fromList
        [ ( "localhost", local )
        , ( "dev-drupal-elm-starter.pantheonsite.io", production )
        ]
