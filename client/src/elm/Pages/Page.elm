module Pages.Page exposing (Page(..), SessionPage(..), UserPage(..))

{-| A module that defines a type which controls what the user wishes
to be shown at the moment.

What is a "page" exactly, since this is, after all, a single-page app? Perhaps
a better word would be `WantsToSee` ... that is, the `Page` answers the
question: "what does the user want to be looking at right now".

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

import Activity.Model exposing (Activity(..))
import Backend.Entities exposing (..)


{-| What does the user want to see?

It's debatable how much detail to model here vs. elsewhere. For instance,
we could just have

    ActivtyPage

here, and model the `ActivityType` as a `selectedActivity` in `Pages.Activity.Model`.

There probably isn't a universally right answer to that, so we'll do it this
way for now and see how it goes.

One of the considerations to apply would be this. Any state we store in an
`activePage` or `userAttention` will be "lost" when we switch to a different page.
So, one consideration is whether that's a feature or a bug. Sometimes it's nice
to "lose" information (yuo don't want to remember everything, after all), but
sometimes we want to remember. (Of course, we can sort-of remember in the routing
layer if we are serializing to forward-back steps in the URL, but that's not
the same as remembering things in our data model itself, since it relies on the
user to use the back-forward button).

It's also debatable how much of the "make impossible states unrepresentable"
philosophy one ought to use in this type. For instance, some pages can only
be shown to logged in users. Some pages can only be shown if we already have
downloaded certain kinds of data. So, you could imagine re-arranging this type
to make the impossible states unrepresentable.

However, it's not impossible for the user to **want** to view something which
they currently can't (e.g. via the URL). So, if what we're modelling here is
"desired user attention" then it isn't really impossible ... we just have to
tell the user that what they want isn't available at the moment. Or, kick off
a fetch automatically. Or, redirect to a different kind of page (e.g. login).

Nevertheless, it might be useful to have sub-types that group together things
that have certain characteristics. For instance, you could group together
things that require login, things that require a downloaded session, etc. ...
it might make some of the code for checking those things a little clearer.
(Indeed it does ... see the `SessionPage` type below).

In a way, you can expect to have a hierarchy with `Page` that somewhat mirrors
the hierarchy you have in the `Model` and `Msg` types. This is sensible, since
the `Page` functions as a kind of "lens" into the Model and Msg types ... that
is, it allows you to pick out certain things from the Model and Msg. And, those
things may or may not actually exist at the moment, which is why you need a
separate type to express the users's **desire** for them to exist. (And, I
suppose that answers the question about why some things are in the `Page` type
itself, and some things in the main model. The things that need to be in the
`Page` type itself are the things which we want the user to be able to
express a desire for, even if the rest of the model is not now in a state
to represent that.)

Note that we've removed `AccessDenied`, since that's never what the user
**wants** to see ... we might **show** an access denied in the UI, but what we
should track in the model is what the user wanted.

We do, however, still have a `PageNotFound`, because when we translate from
a URL, we could end up with a URL we don't understand.

-}
type Page
    = DevicePage -- page that shows the status of the device
    | PinCodePage -- page that allows for local login
    | UserPage UserPage -- page that requires a logged-in user
    | PageNotFound String -- we couldn't interpret the URL ... the parameter is the URL
    | ServiceWorkerPage -- shows status of the service worker


{-| A page which you must be logged in to view. If you're not logged in, we'll show you
the login page instead.
-}
type UserPage
    = AdminPage -- a page that shows administrative tasks
    | ClinicsPage (Maybe ClinicId) -- shows a list of clinics, allows you to choose one
    | SessionPage SessionId SessionPage -- pages that manipulate a group session
    | MyAccountPage -- shows information about the logged-in user
    | PatientRegistrationPage -- alllows registration of new patients.


{-| We group together the pages that can only be viewed with an EditableSession ... it
makes function signatures nicer, since we can specify require a `SessionPage` with
an `EditableSession`. There is also some markup which is relevant just to these,
so we can specify this as a parameter to som functions which produce that markup.
-}
type SessionPage
    = ActivitiesPage -- page that shows list of activities
    | ActivityPage Activity -- page that focuses on a single activity
    | AttendancePage -- page where mothers can be marked present / absent
    | ParticipantsPage -- page that shows a list of participants
    | ChildPage ChildId -- page that focuses on a particular child
    | MotherPage MotherId -- page that focuses on a particular mother
    | ProgressReportPage ChildId -- shows progress report for child with ID
