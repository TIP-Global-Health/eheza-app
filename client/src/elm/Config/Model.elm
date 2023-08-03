module Config.Model exposing (BackendUrl, Model)


type alias BackendUrl =
    String


type alias Model =
    { backendUrl : BackendUrl
    , name : String
    , debug : Bool
    , sandbox : Bool
    }
