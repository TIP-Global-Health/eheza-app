module Config exposing (configs)

import AssocList as Dict exposing (Dict)
import Config.Model as Config exposing (Model)
import LocalConfig exposing (localConfigs)


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"
    , debug = False
    , sandbox = False
    }


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"
    , debug = False
    , sandbox = False
    }


livePantheon : Model
livePantheon =
    { backendUrl = "https://***REMOVED***"
    , name = "livePantheon"
    , debug = False
    , sandbox = False
    }


ehezaGlobal : Model
ehezaGlobal =
    { backendUrl = "https://***REMOVED***"
    , name = "eheza-global"
    , debug = False
    , sandbox = False
    }


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "live-aos"
    , debug = False
    , sandbox = False
    }


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"
    , debug = False
    , sandbox = False
    }


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"
    , debug = False
    , sandbox = False
    }


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"
    , debug = False
    , sandbox = True
    }


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"
    , debug = False
    , sandbox = False
    }


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"
    , debug = False
    , sandbox = False
    }


***REMOVED*** : Model
***REMOVED*** =
    { backendUrl = "https://***REMOVED***"
    , name = "***REMOVED***"
    , debug = False
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
        , ( "***REMOVED***", ***REMOVED*** )
        , ( "***REMOVED***", ***REMOVED*** )
        , ( "***REMOVED***", ***REMOVED*** )
        , ( "***REMOVED***", ***REMOVED*** )
        , ( "***REMOVED***", ***REMOVED*** )
        , ( "***REMOVED***", ***REMOVED*** )
        ]
        |> Dict.union localConfigs
