module Pages.People.Model exposing (Model, Msg(..), emptyModel)

import Components.PatientsSearchForm.Model
import Pages.Page exposing (Page)


{-| The `input` field represents what the user has typed as an input.

The `search` field represents what we're searching for. (We delay
searches briefly while the user is typing).

-}
type alias Model =
    Components.PatientsSearchForm.Model.Model


emptyModel : Model
emptyModel =
    Components.PatientsSearchForm.Model.emptyModel


type Msg
    = SetActivePage Page
    | MsgPatientsSearchForm Components.PatientsSearchForm.Model.Msg
