module Config exposing (configs)

import AssocList as Dict exposing (Dict)
import Config.Model exposing (Model)
import LocalConfig exposing (localConfigs)


liveExample : Model
liveExample =
    { backendUrl = "https://example-live.pantheonsite.io"
    , name = "live-example"
    , debug = False
    , sandbox = False
    }


configs : Dict String Model
configs =
    Dict.fromList
        [ ( "example-live.pantheonsite.io", liveExample ) ]
        |> Dict.union localConfigs
