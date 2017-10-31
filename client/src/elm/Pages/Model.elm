module Pages.Model exposing (..)

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

-}

import Activity.Model exposing (ActivityType(..), ChildActivityType(..))
import EveryDict exposing (EveryDict)
import Pages.Activities.Model
import Pages.Activity.Model
import Pages.Participants.Model
import Pages.Page exposing (Page(..))


type alias Model =
    -- Shows a list of activities ... user can select one.
    { activitiesPage : Pages.Activities.Model.Model

    -- Shows a page for a single activity. We keep separate UI state for
    -- each activity.
    , activityPages : EveryDict ActivityType Pages.Activity.Model.Model

    -- Shows a list of participants ... user can select one.
    , participantsPage : Pages.Participants.Model.Model

    -- What does the user want to see?
    , userAttention : Page
    }


emptyModel : Model
emptyModel =
    { activitiesPage = Pages.Activities.Model.emptyModel
    , activityPages = EveryDict.empty
    , participantsPage = Pages.Participants.Model.emptyModel
    , userAttention = ActivityPage (ChildActivity Height)
    }


type Msg
    = MsgActivities Pages.Activities.Model.Msg
    | MsgActivity ActivityType Pages.Activity.Model.Msg
    | MsgParticipants Pages.Participants.Model.Msg
    | SetUserAttention Page
