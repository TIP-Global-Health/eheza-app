module Pages.Model exposing (MsgSession(..), SessionPages, emptySessionPages)

{-| This module manages the "pages" of the UI.

What is a "page" exactly, since this is, after all, a single-page app? Perhaps
a better word would be `UserAttention` ... that is, the `Page` answers the
question: "what does the user want to be looking at right now". So, we might
rename `Page` to be `UserAttention`, or something like that.

Each of the components under `Page` manages one or another "top-level" choices
about what the user might be paying attention to. That is, each manages some
major division in the UI. Within that, a page may make a variety of choices
about what to show the user.

The state encapsulated in the various `Model` files under `Pages` should
generally be state that is relevant only to the UI, not the fundamental state
of the app that is persisted to the backend. These modules should ask for
backend state in their `view` functions (and, if needed, in their `update`
functions), but they don't "own" it. To modify that state, the `update`
functions should return an extra parameter indicating what kind of modification
is desired. The caller can then either perform that update, or just return it
itself, so that one of its callers can do so -- eventually, we'll get to a
caller that knows how.

The way in which UI state is often "persisted" is via the URL, to allow going
back-and-forward in the UI state. What gets persisted to the URL is often
mostly the `Page` type, but one actually has choices as to how much of the UI
state is reflected in the `Page` type itself, and how much in the various
`Model` files under `Pages`. It's probably best to keep the `Page` type
itself pretty simple, on the prinicple of locating the structure of things
near their implementation, but (as noted below) we don't always do it that
way ... there are advantages and disadvantage.

The various things under `Pages` may well use "widgets" that are defined at
the top-level (or, we may put them in a "Widgets" folder eventually). This
is especially useful for re-usable widgets that may be used on more than
one page -- but may be useful in other cases just for clarity. (In the sense
that the things under `Pages` most clearly need to implement just the top-level
choices about what to show the user, rather than the details).

In fact, you will see below that this is actually specialized to `SessionPages`
... that is, those pages which we can show if we have an `EditableSession`.
Eventually, we may want to make this more general, and put the `SessionPages`
in their own folder. But leaving it here for the moment is convenient.

-}

import Activity.Model exposing (Activity(..), ChildActivity(..), MotherActivity)
import Backend.Entities exposing (..)
import Backend.Session.Model exposing (MsgEditableSession(..))
import EveryDict exposing (EveryDict)
import Measurement.Model
import Pages.Activities.Model
import Pages.Activity.Model
import Pages.Page exposing (Page)
import Pages.Participant.Model
import Pages.Participants.Model


{-| This is where we track all the UI state that relates to an EditableSession
... that is, UI pages which will need an EditableSession to be supplied in
order to view or update them.

TODO: Most (or evven all) of the state (and messages) in Pages.Activity, Pages.Activites,
Pages.Participant, and Pages.Participants could be consolidated here, so that all of
them could work in the same base type (as Pages.Attendance already does). That would
simplify things. Also, this should probably then move to its own folder (i.e.
`Pages.EditableSession` or something like that).

-}
type alias SessionPages =
    -- Shows a list of activities ... user can select one.
    { activitiesPage : Pages.Activities.Model.Model

    -- Shows a page for a single activity. We keep separate UI state for
    -- each activity.
    , childActivityPages : EveryDict ChildActivity (Pages.Activity.Model.Model ChildId)
    , motherActivityPages : EveryDict MotherActivity (Pages.Activity.Model.Model MotherId)

    -- Shows a list of participants ... user can select one.
    , participantsPage : Pages.Participants.Model.Model

    -- We keep separate page state per mother and child ... just models the
    -- selectedActivity and selectedTab, so it's not expensive. This means that
    -- we keep a separate selectedTab and selectedActivity per mother or child
    -- ... we could just keep a single state here if we wanted the selectedTab
    -- and selectedActivity to stay the same when you switch from one
    -- participant to another.
    , childPages : EveryDict ChildId (Pages.Participant.Model.Model ChildActivity)
    , motherPages : EveryDict MotherId (Pages.Participant.Model.Model MotherActivity)
    }


emptySessionPages : SessionPages
emptySessionPages =
    { activitiesPage = Pages.Activities.Model.emptyModel
    , childActivityPages = EveryDict.empty
    , motherActivityPages = EveryDict.empty
    , childPages = EveryDict.empty
    , motherPages = EveryDict.empty
    , participantsPage = Pages.Participants.Model.emptyModel
    }


{-| All the messages which relate to session pages.
-}
type MsgSession
    = MsgActivities Pages.Activities.Model.Msg
    | MsgChildActivity ChildActivity (Maybe ChildId) (Pages.Activity.Model.Msg ChildId Measurement.Model.MsgChild)
    | MsgMotherActivity MotherActivity (Maybe MotherId) (Pages.Activity.Model.Msg MotherId Measurement.Model.MsgMother)
    | MsgChild ChildId (Pages.Participant.Model.Msg ChildActivity Measurement.Model.MsgChild)
    | MsgMother MotherId (Pages.Participant.Model.Msg MotherActivity Measurement.Model.MsgMother)
    | MsgParticipants Pages.Participants.Model.Msg
    | MsgEditableSession MsgEditableSession
    | SetActivePage Page
