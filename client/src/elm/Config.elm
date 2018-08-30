module Config exposing (..)

import Config.Model as Config exposing (Model)
import Dict exposing (..)
import LocalConfig exposing (localConfigs)
import Pusher.Model exposing (Cluster(..), PusherAppKey)


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"
    , pusherKey = PusherAppKey "***REMOVED***" UsEast1
    , debug = False
    , sandbox = False
    }


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"
    , pusherKey = PusherAppKey "***REMOVED***" UsEast1
    , debug = False
    , sandbox = False
    }


livePantheon : Model
livePantheon =
    { backendUrl = "https://***REMOVED***"
    , name = "livePantheon"
    , pusherKey = PusherAppKey "***REMOVED***" UsEast1
    , debug = False
    , sandbox = False
    }


counselingPantheon : Model
counselingPantheon =
    { backendUrl = "https://counseling-ihangane.pantheonsite.io"
    , name = "counselingPantheon"
    , pusherKey = PusherAppKey "" UsEast1
    , debug = False
    , sandbox = False
    }


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"
    , pusherKey = PusherAppKey "" UsEast1
    , debug = False
    , sandbox = False
    }


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"

    -- We're not actually using Pusher at the moment, so just filling in a
    -- blank key for now.
    , pusherKey = PusherAppKey "" UsEast1
    , debug = False
    , sandbox = True
    }


caregiverPantheon : Model
caregiverPantheon =
    { backendUrl = "https://caregiver-ihangane.pantheonsite.io"
    , name = "caregiverPantheon"
    , pusherKey = PusherAppKey "" UsEast1
    , debug = False
    , sandbox = False
    }


configs : Dict String Model
configs =
    Dict.fromList
        [ ( "***REMOVED***", ***REMOVED*** )
        , ( "***REMOVED***", ***REMOVED*** )
        , ( "***REMOVED***", livePantheon )
        , ( "***REMOVED***", ***REMOVED*** )
        , ( "counseling-ihangane.pantheonsite.io", counselingPantheon )
        , ( "***REMOVED***", ***REMOVED*** )
        , ( "caregiver-ihangane.pantheonsite.io", caregiverPantheon )
        ]
        |> Dict.union localConfigs
