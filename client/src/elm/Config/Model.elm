module Config.Model exposing (BackendUrl, Model, Site(..))


type alias BackendUrl =
    String


type alias Model =
    { site : Site
    , backendUrl : BackendUrl
    , name : String
    , debug : Bool
    }


type Site
    = SiteRwanda
    | SiteBurundi
