module Pages.Participants.Model exposing (..)

import Activity.Model exposing (ActivityType)
import Activity.Utils exposing (getActivityTypeList)
import App.PageType exposing (Page(..))
import Participant.Model exposing (ParticipantTypeFilter(..))
import Table


type alias Model =
    { selectedTab : Tab
    }


type Msg
    = SetRedirectPage Page
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    }


thumbnailDimensions =
    { width = 122
    , height = 122
    }
