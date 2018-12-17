module ServiceWorker.Model exposing (IncomingMsg(..), Model, Msg(..), OutgoingMsg(..), emptyModel)

{-| Some state we maintain relating to service workers.
-}

import Json.Encode exposing (Value)
import RemoteData exposing (RemoteData(..))


{-| The state of the service worker system.

  - `active` tracks whether this page is currently controlled by some
    service worker.

  - `registration` tracks our attempt to register the service worker.

-}
type alias Model =
    { active : Bool
    , registration : RemoteData Value ()
    }


{-| We use flags, so that we know whether we're active as early as possible.
-}
emptyModel : Bool -> Model
emptyModel active =
    { active = active
    , registration = NotAsked
    }


type Msg
    = BackToLoginPage
    | HandleIncomingMsg Value
    | SendOutgoingMsg OutgoingMsg


type IncomingMsg
    = SetActive Bool
    | RegistrationSucceeded
    | RegistrationFailed Value


type OutgoingMsg
    = Register
    | Unregister
