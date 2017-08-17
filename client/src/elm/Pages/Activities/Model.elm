module Pages.Activities.Model exposing (..)

import App.PageType exposing (Page(..))
import Participant.Model exposing (ParticipantTypeFilter(..))


type alias Model =
    { participantTypeFilter : ParticipantTypeFilter }


type Msg
    = SetParticipantTypeFilter String
    | SetRedirectPage Page


emptyModel : Model
emptyModel =
    { participantTypeFilter = All }
