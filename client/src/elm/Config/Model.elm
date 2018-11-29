module Config.Model exposing (BackendUrl, Model)

import Pusher.Model exposing (PusherAppKey)
import Rollbar


type alias BackendUrl =
    String


type alias Model =
    { backendUrl : BackendUrl
    , name : String
    , pusherKey : PusherAppKey
    , debug : Bool
    , rollbarToken : Rollbar.Token
    , sandbox : Bool
    }
