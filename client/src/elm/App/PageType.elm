module App.PageType exposing (DashboardPage(..), Page(..))

import Activity.Model exposing (ActivityType)


{-| Prevent circular dependency.
-}
type alias ParticipantId =
    Int


type Page
    = AccessDenied
    | Activities
    | Activity (Maybe ActivityType)
    | Dashboard (List ActivityType)
    | Login
    | MyAccount
    | PageNotFound
    | Participant ParticipantId
    | OpenSessions


type DashboardPage
    = ActivitiesDashboard
    | ParticipantsDashboard
