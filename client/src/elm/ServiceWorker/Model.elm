module ServiceWorker.Model exposing (IncomingMsg(..), Model, Msg(..), OutgoingMsg(..), emptyModel)

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
    = HandleIncomingMsg Value
    | SendOutgoingMsg OutgoingMsg


type IncomingMsg
    = SetActive Bool


type OutgoingMsg
    = Register
    | Unregister
