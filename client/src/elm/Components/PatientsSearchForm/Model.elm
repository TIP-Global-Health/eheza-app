module Components.PatientsSearchForm.Model exposing (..)

import Debouncer.Basic as Debouncer exposing (Debouncer, debounce, toDebouncer)


{-| The `input` field represents what the user has typed as an input.

The `search` field represents what we're searching for. (We delay
searches briefly while the user is typing).

-}
type alias Model =
    { debouncer : Debouncer Msg Msg
    , mode : PatientsSearchFormMode
    , search : Maybe String
    , input : String
    }


type PatientsSearchFormMode
    = ModeSearchByName
    | ModeSearchByNationalId


emptyModel : Model
emptyModel =
    { debouncer = debounce 500 |> toDebouncer
    , mode = ModeSearchByName
    , search = Nothing
    , input = ""
    }


type Msg
    = MsgDebouncer (Debouncer.Msg Msg)
    | SetMode Bool
    | SetInput String
    | SetSearch String
