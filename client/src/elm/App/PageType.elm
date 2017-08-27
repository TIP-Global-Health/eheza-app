module App.PageType exposing (Page(..), DashboardPage(..))

import Activity.Model exposing (ActivityType)


{-| Prevent circular dependency.
-}
type alias ParticipantId =
    Int


type Page
    = AccessDenied
    | Activities
    | Dashboard (List ActivityType)
    | Login
    | MyAccount
    | PageNotFound
    | Participant ParticipantId


type DashboardPage
    = ActivitiesDashboard
    | ParticipantsDashboard
