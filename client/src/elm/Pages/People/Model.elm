module Pages.People.Model exposing (Model, Msg(..), emptyModel)

import Debouncer.Basic as Debouncer exposing (Debouncer, debounce, toDebouncer)
import Pages.Page exposing (Page)


{-| The `input` field represents what the user has typed as an input.

The `search` field represents what we're searching for. (We delay
searches briefly while the user is typing).

-}
type alias Model =
    { debouncer : Debouncer Msg Msg
    , search : Maybe String
    , input : String
    }


emptyModel : Model
emptyModel =
    { debouncer = debounce 500 |> toDebouncer
    , search = Nothing
    , input = ""
    }


type Msg
    = MsgDebouncer (Debouncer.Msg Msg)
    | SetInput String
    | SetSearch String
    | SetActivePage Page
