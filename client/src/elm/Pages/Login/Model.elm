module Pages.Login.Model exposing (..)

{-| This models the username and password entered in the UI.
-}


type alias Model =
    { name : String
    , pass : String
    }


type Msg
    = ClearNameAndPassword
    | SetName String
    | SetPassword String
    | HandleLoginClicked
    | HandleLogoutClicked


{-| The message we return when we want to actually attempt a login, or logout.
Whoever calls `update` needs to detect this and (eventually) route the
appropriate message to `Restful.Login.update`.

Note that we can't just create a `Restful.Login.Msg` in our `update` function
directly, because we don't know the BackendUrl. I suppose we could ask for a
backendUrl in our `update` function, but it seems nicer for the UI layer not
to know that sort of thing.

-}
type OutMsg
    = TryLogin String String
    | Logout


emptyModel : Model
emptyModel =
    { name = ""
    , pass = ""
    }
