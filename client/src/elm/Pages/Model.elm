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
near their implementation.

That is, we should only really use a `UserAttention` (or `Page`) type in cases
where some more specific representation of the user's attention isn't possible.
For instance, in our `Activity` page, we model a `selectedActivity` ... it
probably makes more sense to do this specifically than to include the
`ActivityType` in a `UserAttention` type. (This does mean that our URL
routing will need to be delegated, in part, but that is also probably
sensible).

The various things under `Pages` may well use "widgets" that are defined at
the top-level (or, we may put them in a "Widgets" folder eventually). This
is especially useful for re-usable widgets that may be used on more than
one page -- but may be useful in other cases just for clarity. (In the sense
that the things under `Pages` most clearly need to implement just the top-level
choices about what to show the user, rather than the details).

-}

import Pages.Activity.Model


type UserAttention
    = ActivityPage


type alias Model =
    { activityPage : Pages.Activity.Model.Model
    , userAttention : UserAttention
    }


emptyModel : Model
emptyModel =
    { activityPage = Pages.Activity.Model.emptyModel
    , userAttention = ActivityPage
    }


type Msg
    = MsgActivity Pages.Activity.Model.Msg
    | SetUserAttention UserAttention
