module Pages.Participant.Model exposing (..)

{-| This module models the UI state for showing a particular participant.

  - We don't track the `participantId` here ... that's tracked in
    `Pages.Page.Page` ... we just use what is provided.

  - We do show a `Measurement` page for a selected activity, which we
    do track here. However, we don't "own" the `Measurement.Model`, because
    we show the measurement UI through `Pages.Activity` as well, and we
    want to use the same model for the same participant. So, that gets
    owned at a higher level, and we use what we're supplied.

The model and msg types are parameterized so we can use them for
mother and child with the appropriate types.

-}


type Msg activity measurement
    = MsgMeasurement measurement
    | SetSelectedActivity activity
    | SetSelectedTab Tab


{-| Activity is a `Maybe` so we can model the case where the user
hasn't made a selection, and we should show a default.
-}
type alias Model activity =
    { selectedActivity : Maybe activity
    , selectedTab : Tab
    }


{-| Like our various `Tab` types, except that `Progress Report` is singled out
here for special treatment.

TODO: This probably means that `PogressReport` should also be handled specailly
in `Activity.Utils` when we're calculating how many activitie are completed or
pending ... it ought to be neither, I suppose.

Alternatively, perhaps `ProgressReport` ought not to be handled specially.
Perhaps `ProgressReport` ought to be a normal activity, where the activity is
showing it to the mother, and this is "saved" to the backend to confirm that it
was shown to the mother.

-}
type Tab
    = Completed
    | Pending
    | ProgressReport


emptyModel : Model any
emptyModel =
    { selectedActivity = Nothing
    , selectedTab = Pending
    }
