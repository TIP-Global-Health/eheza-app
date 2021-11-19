module Config exposing (configs)

import AssocList as Dict exposing (Dict)
import Config.Model as Config exposing (Model)
import LocalConfig exposing (localConfigs)


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "live-aos"
    , debug = False
    , sandbox = False
    }


configs : Dict String Model
configs =
    Dict.fromList
        [ ( "***REMOVED***", ***REMOVED*** ) ]
        |> Dict.union localConfigs
