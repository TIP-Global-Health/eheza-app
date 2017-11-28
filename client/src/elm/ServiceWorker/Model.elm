module ServiceWorker.Model exposing (..)

{-| Some state we maintain relating to service workers.
-}

import Json.Encode exposing (Value)


{-| We're not tracking much (yet) ... we'll just flip this to true
when we get a message that we have an active worker. Eventually, we
may want to do more.
-}
type alias Model =
    { active : Bool
    }


emptyModel : Model
emptyModel =
    { active = False
    }


type Msg
    = HandlePortMsg Value
    | Register
    | SetActive Bool
    | Unregister
