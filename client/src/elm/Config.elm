module Config exposing (configs)

import AssocList as Dict exposing (Dict)
import Config.Model as Config exposing (Model, Site(..))
import LocalConfig exposing (localConfigs)


liveExample : Model
liveExample =
    { site = SiteRwanda
    , backendUrl = "https://example-live.pantheonsite.io"
    , name = "live-example"
    , debug = False
    }


configs : Dict String Model
configs =
    Dict.fromList
        [ ( "example-live.pantheonsite.io", liveExample ) ]
        |> Dict.union localConfigs
