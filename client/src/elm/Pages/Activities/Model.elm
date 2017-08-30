module Pages.Activities.Model exposing (..)

import App.PageType exposing (Page(..))
import Participant.Model exposing (ParticipantTypeFilter(..))


type alias Model =
    { participantTypeFilter : ParticipantTypeFilter
    , selectedTab : Tab
    }


type Msg
    = SetParticipantTypeFilter String
    | SetRedirectPage Page
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending


emptyModel : Model
emptyModel =
    { participantTypeFilter = All
    , selectedTab = Pending
    }
