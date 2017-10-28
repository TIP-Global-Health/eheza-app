module Pages.Activity.Model exposing (..)

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import App.PageType exposing (Page)
import Backend.Entities exposing (..)
import FilePicker.Model
import Measurement.Model
import Participant.Model exposing (ParticipantId)


{-| This models the UI state for the page which is oriented around
the following flow:

  - The user sees a list of activities
  - The user selects an activity
  - The user sees the participants for whom the activity is pending
    (or for whom the activity is complete)
  - The user can select a participant in order to perform (or re-perform)
    the activity.

Given that flow, we necessarily have an ActivityType selected ... we'll
initially default to one, if necessary. But we don't necessarily have a
participant selected ... you could simply be reviewing the list of
participants who have completed (or not completed) the activity, before
having selected one. Whether you're looking at the `Completed` or
`Pending` list is tracked by the `selectedTab`.

Note we're "middle management" here ... we don't model the state that
the measurement UI needs, and we don't know how to change it. That gets
handled above and below us ... we just specialize based on user selections,
possibly ask the `Masurement` moules to show a UI, and pass what they tell
us back up the chain. Isn't division of labour fun?

-}
type alias Model =
    { selectedActivity : ActivityType
    , selectedParticipant : Maybe ParticipantId
    , selectedTab : Tab
    }


type Msg
    = MsgMeasurementChild ChildId Measurement.Model.MsgChild
    | MsgMeasurementMother MotherId Measurement.Model.MsgMother
    | SetSelectedParticipant (Maybe ParticipantId)
    | SetSelectedActivity ActivityType
    | SetSelectedTab Tab


type Tab
    = Completed
    | Pending


emptyModel : Model
emptyModel =
    { selectedActivity = ChildActivity Height
    , selectedParticipant = Nothing
    , selectedTab = Pending
    }
