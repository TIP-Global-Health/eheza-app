module Config exposing (configs)

import AssocList as Dict exposing (Dict)
import Config.Model as Config exposing (Model)
import LocalConfig exposing (localConfigs)
import Pusher.Model exposing (Cluster(..), PusherAppKey)
import Rollbar


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"
    , pusherKey = PusherAppKey "***REMOVED***" UsEast1
    , debug = False
    , rollbarToken = Rollbar.token "***REMOVED***"
    , sandbox = False
    }


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"
    , pusherKey = PusherAppKey "***REMOVED***" UsEast1
    , debug = False
    , rollbarToken = Rollbar.token "***REMOVED***"
    , sandbox = False
    }


livePantheon : Model
livePantheon =
    { backendUrl = "https://***REMOVED***"
    , name = "livePantheon"
    , pusherKey = PusherAppKey "***REMOVED***" UsEast1
    , debug = False
    , rollbarToken = Rollbar.token "***REMOVED***"
    , sandbox = False
    }


ehezaGlobal : Model
ehezaGlobal =
    { backendUrl = "https://***REMOVED***"
    , name = "eheza-global"
    , pusherKey = PusherAppKey "***REMOVED***" UsEast1
    , debug = False
    , rollbarToken = Rollbar.token "***REMOVED***"
    , sandbox = False
    }


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"
    , pusherKey = PusherAppKey "" UsEast1
    , debug = False
    , rollbarToken = Rollbar.token "***REMOVED***"
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
    , rollbarToken = Rollbar.token "***REMOVED***"
    , sandbox = True
    }


prenatalPantheon : Model
prenatalPantheon =
    { backendUrl = "https://prenatal-ihangane.pantheonsite.io"
    , name = "prenatalPantheon"
    , pusherKey = PusherAppKey "" UsEast1
    , debug = False
    , rollbarToken = Rollbar.token "***REMOVED***"
    , sandbox = False
    }


elm19Pantheon : Model
elm19Pantheon =
    { backendUrl = "https://elm19-ihangane.pantheonsite.io"
    , name = "elm19Pantheon"
    , pusherKey = PusherAppKey "" UsEast1
    , debug = False
    , rollbarToken = Rollbar.token "***REMOVED***"
    , sandbox = False
    }


configs : Dict String Model
configs =
    Dict.fromList
        [ ( "***REMOVED***", ***REMOVED*** )
        , ( "***REMOVED***", ***REMOVED*** )
        , ( "***REMOVED***", livePantheon )
        , ( "***REMOVED***", ehezaGlobal )
        , ( "***REMOVED***", ***REMOVED*** )
        , ( "prenatal-ihangane.pantheonsite.io", prenatalPantheon )
        , ( "***REMOVED***", ***REMOVED*** )
        , ( "elm19-ihangane.pantheonsite.io", elm19Pantheon )
        ]
        |> Dict.union localConfigs
