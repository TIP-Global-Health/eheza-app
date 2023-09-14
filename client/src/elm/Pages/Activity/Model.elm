module Pages.Activity.Model exposing (ChildUpdateReturns, Model, MotherUpdateReturns, Msg(..), Tab(..), emptyModel)

{-| This models (part of) the UI state for the page which is oriented around
the following flow:

  - The user sees a list of activities (in `Pages.Activities`)
  - The user selects an activity (in `Pages.Activities`)
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
possibly ask the `Measurement` moules to show a UI, and pass what they tell
us back up the chain. Isn't division of labour fun?

Also note that we don't manage the selection of activities here (that is,
the first two steps in flow above). That's done by `Pages.Activities`, which
just supplies an activity to us ... we can't change it in this part of the UI.
We can only change the `selectedParticipant` and `selectedTab`.

-}

import Backend.Entities exposing (..)
import Measurement.Model
import Pages.Page exposing (Page)


type alias Model id =
    { selectedParticipant : Maybe id
    , selectedTab : Tab
    , filter : String
    }


type Msg id measurement
    = GoBackToActivitiesPage SessionId
    | MsgMeasurement measurement
    | SetFilter String
    | SetSelectedParticipant (Maybe id)
    | SetSelectedTab Tab


{-| This bears a kind of resemblance to the `Tab` type in `Pages.Activites.Model`.

Now, they don't play quite the same role ... here it controls which paricipants
we see for a given activity (the participants for whom the activity is
completed or pending), whereas there it controls which activities we see (those
which are entirely completed, or those which have at least one participant
pending).

Nevertheless, there may be something we might do to relate them.

-}
type Tab
    = Completed
    | Pending


emptyModel : Model any
emptyModel =
    { selectedParticipant = Nothing
    , selectedTab = Pending
    , filter = ""
    }


type alias ChildUpdateReturns =
    { model : Model PersonId
    , cmd : Cmd (Msg PersonId Measurement.Model.MsgChild)
    , form : Maybe Measurement.Model.ModelChild
    , outMsg : Maybe Measurement.Model.OutMsgChild
    , page : Maybe Page
    }


type alias MotherUpdateReturns =
    { model : Model PersonId
    , cmd : Cmd (Msg PersonId Measurement.Model.MsgMother)
    , form : Maybe Measurement.Model.ModelMother
    , outMsg : Maybe Measurement.Model.OutMsgMother
    , page : Maybe Page
    }
