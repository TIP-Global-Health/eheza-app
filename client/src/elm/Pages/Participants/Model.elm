module Pages.Participants.Model exposing (Model, Msg(..), Tab(..), emptyModel)

{-| This module is analogous to `Pages.Activities`, but it manages the
selection of participants first, rather than activities first. That is

  - in `Pages.Activities` you first select an activity, and then a participant

  - here, first you select a participant, and then you select an activity.

Note that we don't model the selected participant here ... instead, that
is modeled in `Pages.Page.Page`. So, we're not a wrapper around showing
a participant ... we merely select a participant and then redirect to
show the participant.

-}

import Pages.Page exposing (Page)


type alias Model =
    { selectedTab : Tab
    , showEndSessionDialog : Bool
    , filter : String
    }


type Msg
    = CloseSession
    | SetFilter String
    | SetRedirectPage Page
    | SetSelectedTab Tab
    | ShowEndSessionDialog Bool


{-| Once again we have a `Tab` type, roughly analogous to the type in
`Pages.Activities.Model` and `Pages.Activity.Model`. Though, here it
again signifies something slightly different ... it signifies those
participants for whom activities are completed, vs. those for whom
at least one activity is pending.
-}
type Tab
    = Completed
    | Pending


emptyModel : Model
emptyModel =
    { selectedTab = Pending
    , showEndSessionDialog = False
    , filter = ""
    }
