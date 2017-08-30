module Pages.Activities.Model exposing (..)

import App.PageType exposing (Page(..))
import Participant.Model exposing (ParticipantTypeFilter(..))


type alias Model =
    { participantTypeFilter : ParticipantTypeFilter
    , activeTab : Tab
    }


type Msg
    = SetActiveTab Tab
    | SetParticipantTypeFilter String
    | SetRedirectPage Page


type Tab
    = Completed
    | Pending


emptyModel : Model
emptyModel =
    { participantTypeFilter = All
    , activeTab = Pending
    }
