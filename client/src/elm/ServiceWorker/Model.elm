module ServiceWorker.Model exposing (IncomingMsg(..), Model, Msg(..), NewWorker(..), OutgoingMsg(..), emptyModel)

{-| Some state we maintain relating to service workers.
-}

import Json.Encode exposing (Value)
import RemoteData exposing (RemoteData(..))


{-| The state of the service worker system.

  - `active` tracks whether this page was being controlled by a
    service worker when the app started up.

  - `registration` tracks our attempt to register the service worker.

  - `newWorker` tracks the status of a new worker which is being installed

-}
type alias Model =
    { active : Bool
    , registration : RemoteData Value ()
    , newWorker : Maybe NewWorker
    }


type NewWorker
    = Installing
    | Installed
    | Activating
    | Activated
    | Redundant


{-| We use flags, so that we know whether we're active as early as possible.
-}
emptyModel : Bool -> Model
emptyModel active =
    { active = active
    , registration = NotAsked
    , newWorker = Nothing
    }


type Msg
    = BackToLoginPage
    | HandleIncomingMsg Value
    | SendOutgoingMsg OutgoingMsg


type IncomingMsg
    = RegistrationSucceeded
    | RegistrationFailed Value
    | SetNewWorker NewWorker


type OutgoingMsg
    = Register
    | SkipWaiting
    | Update
