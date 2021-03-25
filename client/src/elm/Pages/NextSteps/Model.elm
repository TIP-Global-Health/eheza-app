module Pages.NextSteps.Model exposing (Model, Msg(..), emptyModel)

import Pages.Page exposing (Page)


{-| This module manages the state for the first part of the UI
flow described in `Pages.Activity.Model`. That is, it shows a list of
activities, and allows the user to click on an activity to see participants
related to that activity.

Note that we don't actually model the `selectedActivity` here, at least for now
... instead, at least for the moment, that is modeled as part of the
`UserAttention` (or `Page`). That might be worth changing at some point, or
perhaps not -- it's not absolutely clear what is best modeled in the `Page`
type itself vs. more specific types.

Also note that we don't manage the `Page.Activity.Model` here (again, at least
for the moment). Instead, we redirect the `Page` in such as way as to show the
desired activity. So, we're not drawing a wrapper around the `Page.Activity`
... we're merely selecting & redirecting.

-}
type alias Model =
    {}


type Msg
    = SetRedirectPage Page


emptyModel : Model
emptyModel =
    {}
