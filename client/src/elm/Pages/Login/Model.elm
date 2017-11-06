module Pages.Login.Model exposing (..)


type alias LoginForm =
    { name : String
    , pass : String
    }


type alias Model =
    { loginForm : LoginForm
    }


type Msg
    = Clear
    | SetName String
    | SetPassword String
    | HandleLoginClicked


{-| The message we return when we want to actually attempt
a login.
-}
type OutMsg
    = TryLogin String String


emptyModel : Model
emptyModel =
    { loginForm = LoginForm "" ""
    }
