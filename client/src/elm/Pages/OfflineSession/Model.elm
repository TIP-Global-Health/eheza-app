module Pages.OfflineSession.Model exposing (..)

{-| This represents data we track locally with respect to an
offline session, which is not saved to the backend. (That is,
data relevant to the UI, which we can throw away if the browser
is refreshed).
-}


type alias Model =
    {}


emptyModel : Model
emptyModel =
    {}


type Msg
    = NoOp
