module Pages.IndividualEncounterParticipants.Model exposing (Model, Msg(..))

import Components.PatientsSearchForm.Model
import Pages.Page exposing (Page)


{-| The `input` field represents what the user has typed as an input.

The `search` field represents what we're searching for. (We delay
searches briefly while the user is typing).

-}
type alias Model =
    Components.PatientsSearchForm.Model.Model


type Msg
    = SetActivePage Page
    | MsgPatientsSearchForm Components.PatientsSearchForm.Model.Msg
