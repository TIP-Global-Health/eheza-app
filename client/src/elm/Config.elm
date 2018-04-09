module Config exposing (..)

import LocalConfig exposing (localConfigs)
import Config.Model as Config exposing (Model)
import Dict exposing (..)
import Pusher.Model exposing (Cluster(..), PusherAppKey)


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"
    , pusherKey = PusherAppKey "***REMOVED***" UsEast1
    , debug = False
    }


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"
    , pusherKey = PusherAppKey "***REMOVED***" UsEast1
    , debug = False
    }


livePantheon : Model
livePantheon =
    { backendUrl = "https://***REMOVED***"
    , name = "livePantheon"
    , pusherKey = PusherAppKey "***REMOVED***" UsEast1
    , debug = False
    }


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"

    -- We're not actually using Pusher at the moment, so just filling in a
    -- blank key for now.
    , pusherKey = PusherAppKey "" UsEast1
    , debug = False
    }


configs : Dict String Model
configs =
    Dict.fromList
        [ ( "***REMOVED***", ***REMOVED*** )
        , ( "***REMOVED***", ***REMOVED*** )
        , ( "***REMOVED***", livePantheon )
        , ( "***REMOVED***", ***REMOVED*** )
        ]
        |> Dict.union localConfigs
