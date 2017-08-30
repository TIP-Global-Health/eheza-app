module Pages.Activity.Model exposing (..)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import App.PageType exposing (Page)
import Participant.Model exposing (ParticipantId)


type alias Model =
    { selectedActivity : ActivityType
    , selectedParticipantId : Maybe ParticipantId
    , selectedTab : Tab
    }


type Msg
    = SetRedirectPage Page
    | SetSelectedParticipant (Maybe ParticipantId)
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending


emptyModel : Model
emptyModel =
    { selectedActivity = Child Height
    , selectedParticipantId = Nothing
    , selectedTab = Pending
    }
