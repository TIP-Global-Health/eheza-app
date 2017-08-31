module Pages.Activity.Model exposing (..)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import App.PageType exposing (Page)
import Measurement.Model
import Participant.Model exposing (ParticipantId, Participant)


type alias Model =
    { measurements : Measurement.Model.Model
    , selectedActivity : ActivityType
    , selectedParticipantId : Maybe ParticipantId
    , selectedTab : Tab
    }


type Msg
    = SetRedirectPage Page
    | MsgMeasurement ( ParticipantId, Participant ) Measurement.Model.Msg
    | SetSelectedParticipant (Maybe ParticipantId)
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending


emptyModel : Model
emptyModel =
    { measurements = Measurement.Model.emptyModel
    , selectedActivity = Child Height
    , selectedParticipantId = Nothing
    , selectedTab = Pending
    }
