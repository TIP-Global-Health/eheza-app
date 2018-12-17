module ServiceWorker.Model exposing (IncomingMsg(..), Model, Msg(..), OutgoingMsg(..), emptyModel)

{-| Some state we maintain relating to service workers.
-}

import Json.Encode exposing (Value)


{-| The state of the service worker system.

  - `active` tracks whether this page is currently controlled by some
    service worker.

-}
type alias Model =
    { active : Bool
    }


{-| We use flags, so that we know whether we're active as early as possible.
-}
emptyModel : Bool -> Model
emptyModel active =
    { active = active
    }


type Msg
    = HandleIncomingMsg Value
    | SendOutgoingMsg OutgoingMsg


type IncomingMsg
    = SetActive Bool


type OutgoingMsg
    = Register
    | Unregister
